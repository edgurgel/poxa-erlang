-module(websocket_handler_test).
-include_lib("eunit/include/eunit.hrl").

websocket_handler_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"Handle websocket_info with message", fun websocket_info_normal/0},
    {"Handle websocket_info on start", fun websocket_info_start/0},
    {"Handle websocket_info without a message", fun websocket_info_undefined/0},
    {"Handle websocket_handle undefined event", fun websocket_handle_undefined/0},
    {"Handle websocket_handle invalid json", fun websocket_handle_invalid_json/0},
    {"Handle websocket_handle ping", fun websocket_handle_ping/0},
    {"Handle websocket_handle subscribe on private channel", fun websocket_handle_subscribe_private_channel/0},
    {"Handle websocket_handle subscribe on private channel bad auth", fun websocket_handle_subscribe_private_channel_bad_auth/0},
    {"Handle websocket_handle subscribe", fun websocket_handle_subscribe/0},
    {"Handle websocket_handle subscribe on already subscribed", fun websocket_handle_subscribe_on_already_subscribed/0},
    {"Handle websocket_handle unsubscribe", fun websocket_handle_unsubscribe/0},
    {"Handle websocket_handle unsubscribe on non subscribed", fun websocket_handle_unsubscribe_on_non_subscribed/0},
    {"Handle init and upgrade to websocket", fun init/0},
    {"Handle websocket_init", fun websocket_init/0},
    {"Handle websocket_init wrong app key", fun websocket_init_wrong_app_key/0}]}.

websocket_info_normal() ->
  ?assertEqual({reply, {text, msg}, req, state},
               websocket_handler:websocket_info({pid, msg}, req, state)).

websocket_info_start() ->
  meck:expect(jsx, encode, 1, encodedData),
  meck:expect(uuid, uuid1, fun() -> meck:passthrough([]) end),
  meck:expect(uuid, to_string, 1, "uuid"),
  meck:expect(gproc, reg, 1, registered),
  meck:expect(pusher_event, connection_established, 1, connection_established),

  ?assertEqual({reply, {text, connection_established}, req, <<"uuid">>},
               websocket_handler:websocket_info(start, req, state)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(uuid)),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)).

websocket_info_undefined() ->
  ?assertEqual({ok, req, state},
               websocket_handler:websocket_info(undefined, req, state)).

websocket_handle_undefined() ->
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:undefined">>}]),
  ?assertEqual({ok, req, state},
               websocket_handler:websocket_handle({text, undefined_event_json}, req, state)),
  ?assert(meck:validate(jsx)).

websocket_handle_invalid_json() ->
  meck:expect(jsx, decode, fun(_) -> meck:exception(error, badarg) end),
  ?assertEqual({ok, req, state},
               websocket_handler:websocket_handle({text, undefined_event_json}, req, state)),
  ?assert(meck:validate(jsx)).

websocket_handle_ping() ->
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:ping">>}]),
  meck:expect(pusher_event, pong, 0, pong),
  ?assertEqual({reply, {text, pong}, req, state},
               websocket_handler:websocket_handle({text, subscribe_json}, req, state)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(jsx)).

websocket_handle_subscribe_private_channel() ->
  meck:expect(application, get_env, 2, <<"secret">>),
  Auth = hmac:hmac256(<<"secret">>, <<"SocketId:private-channel">>),
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:subscribe">>},
                               {<<"data">>, [{<<"channel">>, <<"private-channel">>},
                                             {<<"auth">>, Auth}]}]),
  meck:expect(subscription, subscribe, 2, ok),
  meck:expect(pusher_event, subscription_succeeded, 0, subscription_succeeded),
  ?assertEqual({reply, {text, subscription_succeeded}, req, <<"SocketId">>},
               websocket_handler:websocket_handle({text, subscribe_json}, req, <<"SocketId">>)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(jsx)).

websocket_handle_subscribe_private_channel_bad_auth() ->
  meck:expect(application, get_env, 2, <<"secret">>),
  Auth = hmac:hmac256(<<"secret">>, <<"badauth">>),
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:subscribe">>},
                               {<<"data">>, [{<<"channel">>, <<"private-channel">>},
                                             {<<"auth">>, Auth}]}]),
  meck:expect(subscription, subscribe, 2, error),
  meck:expect(pusher_event, subscription_error, 0, subscription_error),
  ?assertEqual({reply, {text, subscription_error}, req, <<"SocketId">>},
               websocket_handler:websocket_handle({text, subscribe_json}, req, <<"SocketId">>)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(jsx)).

websocket_handle_subscribe() ->
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:subscribe">>},
                               {<<"data">>, [{<<"channel">>, <<"test_channel">>}]}]),
  meck:expect(pusher_event, subscription_succeeded, 0, subscription_succeeded),
  meck:expect(subscription, subscribe, 2, ok),
  ?assertEqual({reply, {text, subscription_succeeded}, req, state},
               websocket_handler:websocket_handle({text, subscribe_json}, req, state)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(jsx)).

websocket_handle_subscribe_on_already_subscribed() ->
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:subscribe">>},
                               {<<"data">>, [{<<"channel">>, <<"test_channel">>}]}]),
  meck:expect(pusher_event, subscription_succeeded, 0, subscription_succeeded),
  meck:expect(subscription, subscribe, 2, ok),
  ?assertEqual({reply,{text, subscription_succeeded}, req, state},
               websocket_handler:websocket_handle({text, subscribe_json}, req, state)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(jsx)).

websocket_handle_unsubscribe() ->
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:unsubscribe">>},
                               {<<"data">>, [{<<"channel">>, <<"test_channel">>}]}]),
  meck:expect(subscription, unsubscribe, 1, undefined),
  ?assertEqual({ok, req, state},
               websocket_handler:websocket_handle({text, subscribe_json}, req, state)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(jsx)).

websocket_handle_unsubscribe_on_non_subscribed() ->
  meck:expect(jsx, decode, 1, [{<<"event">>, <<"pusher:unsubscribe">>},
                               {<<"data">>, [{<<"channel">>, <<"test_channel">>}]}]),
  meck:expect(subscription, unsubscribe, 1, undefined),
  ?assertEqual({ok, req, state},
               websocket_handler:websocket_handle({text, subscribe_json}, req, state)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(jsx)).

init() ->
  ?assertEqual({upgrade, protocol, cowboy_websocket},
               websocket_handler:init({tcp, http}, req, opts)).

websocket_init() ->
  meck:expect(application, get_env, 2, {ok, <<"app_key">>}),
  meck:expect(pusher_event, connection_established, 1, connection_established),
  meck:expect(cowboy_req, binding, 2, {<<"app_key">>, req} ),
  ?assertEqual({ok, req, empty},
               websocket_handler:websocket_init(transport, req, opts)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(cowboy_req)).

websocket_init_wrong_app_key() ->
  meck:expect(application, get_env, 2, {ok, <<"app_key">>}),
  meck:expect(cowboy_req, binding, 2, {<<"different_app_key">>, req}),
  ?assertEqual({shutdown, req, empty},
               websocket_handler:websocket_init(transport, req, opts)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(cowboy_req)).

start() ->
  meck:new(pusher_event),
  meck:new(application, [unstick]),
  meck:new(subscription),
  meck:new(cowboy_req),
  meck:new(jsx),
  meck:new(gproc),
  meck:new(uuid).

stop(_) ->
  meck:unload(pusher_event),
  meck:unload(application),
  meck:unload(subscription),
  meck:unload(cowboy_req),
  meck:unload(jsx),
  meck:unload(gproc),
  meck:unload(uuid).
