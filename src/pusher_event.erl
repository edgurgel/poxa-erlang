-module(pusher_event).

-export([subscription_succeeded/0]).
-export([subscription_error/0]).
-export([connection_established/1]).
-export([pong/0]).

connection_established(SocketId) ->
  jsx:encode([{<<"event">>, <<"pusher:connection_established">>},
              {<<"data">>,
               [{<<"socket_id">>, SocketId }]
              }]).

subscription_succeeded() ->
  jsx:encode([{<<"event">>, <<"pusher:subscription_succeeded">>},
              {<<"data">>, []}]).

subscription_error() ->
  jsx:encode([{<<"event">>, <<"pusher:subscription_error">>},
              {<<"data">>, []}]).
pong() ->
  jsx:encode([{<<"event">>, <<"pusher:pong">>},
              {<<"data">>, []}]).
