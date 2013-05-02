-module(pusher_event).

-export([subscription_succeeded/0]).
-export([subscription_error/0]).
-export([connection_established/1]).
-export([pong/0]).
-export([presence_subscription_succeeded/2]).
-export([presence_member_added/3]).
-export([presence_member_removed/2]).

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
  jsx:encode([{<<"event">>, <<"pusher:pong">>}]).

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
