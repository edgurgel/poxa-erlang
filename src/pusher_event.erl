-compile([{parse_transform, lager_transform}]).
-module(pusher_event).

-export([subscription_succeeded/0]).
-export([subscription_error/0]).
-export([connection_established/1]).
-export([pong/0]).
-export([presence_subscription_succeeded/2]).
-export([presence_member_added/3]).
-export([presence_member_removed/2]).
-export([send_message_to_channels/3]).
-export([send_message_to_channel/3]).
-export([parse_channels/1]).

connection_established(SocketId) ->
  jsx:encode([{<<"event">>, <<"pusher:connection_established">>},
              {<<"data">>,
               [{<<"socket_id">>, SocketId }]
              }]).

subscription_succeeded() ->
  jsx:encode([{<<"event">>, <<"pusher_internal:subscription_succeeded">>},
              {<<"data">>, []}]).

subscription_error() ->
  jsx:encode([{<<"event">>, <<"pusher:subscription_error">>},
              {<<"data">>, []}]).
pong() ->
  jsx:encode([{<<"event">>, <<"pusher:pong">>},
              {<<"data">>, []}]).

% PresenceData looks like:
% [{<0.45.0>,{id, userinfo}},
% {<0.47.0>,{id2, userinfo2}}
% ,{<0.49.0>,{id3, userinfo3}}]
presence_subscription_succeeded(Channel, PresenceData) ->
  % This may be too slow in the future...let's wait and see :)
  IdsHash = [{UserId, UserInfo} || {_Pid, {UserId, UserInfo}} <- PresenceData],
  {Ids, Hash} = lists:unzip(IdsHash),
  Count = length(Ids),
  jsx:encode([{<<"event">>, <<"pusher_internal:subscription_succeeded">>},
              {<<"channel">>, Channel},
              {<<"data">>,
               [{<<"presence">>, [
                {<<"ids">>, Ids},
                {<<"hash">>, Hash},
                {<<"count">>, Count}
                ]}]
              }]).
presence_member_added(Channel, UserId, UserInfo) ->
  jsx:encode([{<<"event">>, <<"pusher_internal:member_added">>},
              {<<"channel">>, Channel},
              {<<"data">>, [
                {<<"user_id">>, UserId},
                {<<"user_info">>, UserInfo}]
              }]).

presence_member_removed(Channel, UserId) ->
  jsx:encode([{<<"event">>, <<"pusher_internal:member_removed">>},
              {<<"channel">>, Channel},
              {<<"data">>, [
                {<<"user_id">>, UserId}]
              }]).

parse_channels(Req) ->
  Exclude = proplists:get_value(<<"socket_id">>, Req, undefined),
  case proplists:get_value(<<"channel">>, Req) of
    undefined ->
      case proplists:get_value(<<"channels">>, Req) of
        undefined -> {Req, undefined, Exclude}; % channel/channels not found
        Channels -> Req2 = proplists:delete(<<"channels">>, Req),
          {Req2, Channels, Exclude}
      end;
    Channel -> Req2 = proplists:delete(<<"channel">>, Req),
      {Req2, [Channel], Exclude}
  end.

send_message_to_channels(Channels, Message, Exclude) ->
  lager:debug("Sending message to channels ~p", [Channels]),
  PidToExclude = case Exclude of
    undefined -> [];
    _ -> gproc:lookup_pids({n, l, Exclude})
  end,
  [send_message_to_channel(Channel, Message, PidToExclude) || Channel <- Channels].

send_message_to_channel(Channel, Message, PidToExclude) ->
  Message2 = lists:append(Message, [{<<"channel">>, Channel}]),
  Pids = gproc:lookup_pids({p, l, {pusher, Channel}}),
  Pids2 = Pids -- PidToExclude,
  [Pid ! {self(), jsx:encode(Message2)} || Pid <- Pids2].
