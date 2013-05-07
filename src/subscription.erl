-compile([{parse_transform, lager_transform}]).
-module(subscription).

-export([subscribe/2, unsubscribe/1]).
-export([is_subscribed/1]).

subscribe(Data, SocketId) ->
  Channel = proplists:get_value(<<"channel">>, Data),
  case Channel of
    <<"private-", _PrivateChannel/binary>> ->
      Auth = proplists:get_value(<<"auth">>, Data),
      ToSign = <<SocketId/binary, ":", Channel/binary>>,
      case auth_signature:validate(ToSign, Auth) of
        ok -> subscribe_channel(Channel);
        error -> subscribe_error(Channel)
      end;
    <<"presence-", _PresenceChannel/binary>> ->
      Auth = proplists:get_value(<<"auth">>, Data),
      ChannelData = proplists:get_value(<<"channel_data">>, Data, <<"undefined">>),
      ToSign = <<SocketId/binary, ":", Channel/binary, ":", ChannelData/binary>>,
      case auth_signature:validate(ToSign, Auth) of
        ok -> subscribe_presence_channel(Channel, ChannelData);
        error -> subscribe_error(Channel)
      end;
    undefined ->
      lager:info("Missing channel"),
      error;
    _ -> subscribe_channel(Channel)
  end.

subscribe_error(Channel) ->
  lager:info("Error while subscribing to channel ~p", [Channel]),
  error.

subscribe_presence_channel(Channel, ChannelData) ->
  try jsx:decode(ChannelData) of
    DecodedChannelData ->
      UserId = proplists:get_value(<<"user_id">>, DecodedChannelData),
      SanitizedUserId = sanitize_user_id(UserId),
      UserInfo = proplists:get_value(<<"user_info">>, DecodedChannelData),
      case is_subscribed(Channel) of
        true -> lager:info("Already subscribed ~p on channel ~p", [self(), Channel]);
        false -> lager:info("Registering ~p to channel ~p", [self(), Channel]),
          Message = pusher_event:presence_member_added(Channel, SanitizedUserId, UserInfo),
          gproc:send({p, l, {pusher, Channel}}, {self(), Message}),
          gproc:reg({p, l, {pusher, Channel}}, {SanitizedUserId, UserInfo})
      end,
      {presence, Channel, gproc:lookup_values({p, l, {pusher, Channel}})}
  catch
    error:badarg ->
      lager:error("Invalid channel data"),
      error
  end.

sanitize_user_id(UserId) ->
  case jsx:is_term(UserId) of
    true -> jsx:encode(UserId);
    false -> UserId
  end.

subscribe_channel(Channel) ->
  lager:info("Subscribing to channel ~p", [Channel]),
  case is_subscribed(Channel) of
    true -> lager:info("Already subscribed ~p on channel ~p", [self(), Channel]);
    false -> lager:info("Registering ~p to channel ~p", [self(), Channel]),
      gproc:reg({p, l, {pusher, Channel}})
  end,
  ok.

unsubscribe(Data) ->
  Channel = proplists:get_value(<<"channel">>, Data, undefined),
  case Channel of
    <<"presence-", _PresenceChannel/binary>> ->
      case gproc:get_value({p, l, {pusher, Channel}}) of
        {UserId, _} ->
          Message = pusher_event:presence_member_removed(Channel, UserId),
          gproc:send({p, l, {pusher, Channel}}, {self(), Message});
        _ -> undefined
      end;
    _ -> undefined
  end,
  unsubscribe_channel(Channel).
unsubscribe_channel(Channel) ->
  lager:info("Unsubscribing to channel ~p", [Channel]),
  case is_subscribed(Channel) of
    true -> gproc:unreg({p, l, {pusher, Channel}});
    false -> lager:debug("Already subscribed")
  end.

is_subscribed(Channel) ->
  Match = {{p, l, {pusher, Channel}}, self(), '_'},
  case gproc:select([{Match, [], ['$$']}]) of
    [] -> false;
    [_] -> true
  end.
