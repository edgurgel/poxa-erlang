-compile([{parse_transform, lager_transform}]).
-module(presence_subscription).

-export([subscribe/2]).
-export([unsubscribe/1]).
-export([check_and_remove/0]).
-export([user_count/1]).
-export([users/1]).
-export([is_presence_channel/1]).

subscribe(Channel, ChannelData) ->
  try jsx:decode(ChannelData) of
    DecodedChannelData ->
      case subscription:is_subscribed(Channel) of
        true -> lager:info("Already subscribed ~p on channel ~p", [self(), Channel]);
        false -> lager:info("Registering ~p to channel ~p", [self(), Channel]),
          {UserId, UserInfo} = extract_userid_and_userinfo(DecodedChannelData),
          case user_id_already_on_presence_channel(UserId, Channel) of
            false ->
              Message = pusher_event:presence_member_added(Channel, UserId, UserInfo),
              gproc:add_shared_local_counter({presence, Channel, UserId}, 1),
              gproc:send({p, l, {pusher, Channel}}, {self(), Message});
            true ->
              gproc:update_shared_counter({c, l, {presence, Channel, UserId}}, 1)
          end,
          gproc:reg({p, l, {pusher, Channel}}, {UserId, UserInfo})
      end,
      {presence, Channel, gproc:lookup_values({p, l, {pusher, Channel}})}
  catch
    error:badarg ->
      lager:error("Invalid channel data"),
      error
  end.

extract_userid_and_userinfo(ChannelData) ->
  UserId = sanitize_user_id(proplists:get_value(<<"user_id">>, ChannelData)),
  UserInfo = proplists:get_value(<<"user_info">>, ChannelData),
  {UserId, UserInfo}.

unsubscribe(Channel) ->
  case gproc:get_value({p, l, {pusher, Channel}}) of
    {UserId, _} ->
      Message = pusher_event:presence_member_removed(Channel, UserId),
      gproc:send({p, l, {pusher, Channel}}, {self(), Message});
    _ -> undefined
  end.

check_and_remove() ->
  Match = {{p, l, {pusher, '$1'}}, self(), {'$2', '_'}},
  ChannelUserId = gproc:select([{Match, [], [['$1','$2']]}]),
  MemberRemoveFun = fun([Channel, UserId]) ->
      case Channel of
        <<"presence-", _PresenceChannel/binary>> ->
          case is_one_connection_on_user_id(Channel, UserId) of
            true ->
              gproc:unreg_shared({c, l, {presence, Channel, UserId}}),
              Message = pusher_event:presence_member_removed(Channel, UserId),
              gproc:send({p, l, {pusher, Channel}}, {self(), Message});
            false->
              gproc:update_shared_counter({c, l, {presence, Channel, UserId}}, -1)
          end;
        _ -> undefined
      end
  end,
  lists:foreach(MemberRemoveFun, ChannelUserId),
  ok.

sanitize_user_id(UserId) ->
  case jsx:is_term(UserId) of
    true -> jsx:encode(UserId);
    false -> UserId
  end.

user_id_already_on_presence_channel(UserId, Channel) ->
  Match = {{p, l, {pusher, Channel}}, '_', {UserId, '_'}},
  case gproc:select([{Match, [], ['$$']}]) of
    [] -> false;
    _ -> true
  end.

is_one_connection_on_user_id(Channel, UserId) ->
  case gproc:get_value({c, l, {presence, Channel, UserId}}, shared) of
    1 -> true;
    _ -> false
  end.

user_count(Channel) ->
  Match = {{c, l, {presence, Channel, '_'}}, '_', '_'},
  gproc:select_count([{Match, [], [true]}]).

users(Channel) ->
  Match = {{c, l, {presence, Channel, '$1'}}, '_', '_'},
  gproc:select([{Match, [], ['$1']}]).

is_presence_channel(Channel) ->
  case Channel of
    <<"presence-", _PresenceChannel/binary>> -> true;
    _ -> false
  end.
