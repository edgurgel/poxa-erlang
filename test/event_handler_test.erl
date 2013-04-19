-module(event_handler_test).
-include_lib("eunit/include/eunit.hrl").

event_handler_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"Handle single channel event", fun handle_single_channel_event/0},
    {"Handle multiple channel event", fun handle_multiple_channel_event/0},
    {"Handle invalid json", fun handle_invalid_json/0},
    {"Handle having no channel on request", fun handle_error_undefined_channel/0},
    {"Handle having failed authentication", fun handle_error_failing_authentication/0}]}.

handle_single_channel_event() ->
  meck:expect(authentication, check, 3, ok),
  meck:expect(cowboy_req, body, 1,
              {ok, body, req1}),
  meck:expect(cowboy_req, qs_vals, 1, {qsvals, req2}),
  meck:expect(cowboy_req, path, 1, {path, req3}),
  meck:expect(jsx, decode, 1,
              [{<<"channel">>, <<"channel_name">>},
               {<<"name">>, <<"event_etc">>} ]),
  meck:expect(jsx, encode, 1,
              [{<<"channel">>, <<"channel_name">>},
               {<<"event">>, <<"event_etc">>}]),
  meck:expect(gproc, send, 2, undefined),
  meck:expect(cowboy_req, reply, 4, {ok, req4}),
  ?assertEqual({ok, req4, state},
               event_handler:handle(req, state)),
  ?assert(meck:validate(authentication)),
  ?assert(meck:validate(cowboy_req)),
  ?assert(meck:validate(jsx)),
  ?assert(meck:validate(gproc)).

handle_multiple_channel_event() ->
  meck:expect(authentication, check, 3, ok),
  meck:expect(cowboy_req, body, 1,
              {ok, body, req1}),
  meck:expect(cowboy_req, qs_vals, 1, {qsvals, req2}),
  meck:expect(cowboy_req, path, 1, {path, req3}),
  meck:expect(jsx, decode, 1,
              [{<<"channels">>, [<<"channel_name">>, <<"other_channel">>]},
               {<<"name">>, <<"event_etc">>} ]),
  meck:expect(jsx, encode, 1,
              [{<<"channel">>, <<"channel_name">>},
               {<<"event">>, <<"event_etc">>}]),
  meck:expect(gproc, send, 2, undefined),
  meck:expect(cowboy_req, reply, 4, {ok, req4}),
  ?assertEqual({ok, req4, state},
               event_handler:handle(req, state)),
  ?assert(meck:validate(authentication)),
  ?assert(meck:validate(cowboy_req)),
  ?assert(meck:validate(jsx)),
  ?assert(meck:validate(gproc)).

handle_invalid_json() ->
  meck:expect(authentication, check, 3, ok),
  meck:expect(cowboy_req, body, 1,
              {ok, body, req1}),
  meck:expect(cowboy_req, qs_vals, 1, {qsvals, req2}),
  meck:expect(cowboy_req, path, 1, {path, req3}),
  meck:expect(jsx, decode, fun(_) -> meck:exception(error, badarg) end),
  meck:expect(cowboy_req, reply, 4, {ok, req4}),
  ?assertEqual({ok, req4, state},
               event_handler:handle(req, state)),
  ?assert(meck:validate(authentication)),
  ?assert(meck:validate(cowboy_req)),
  ?assert(meck:validate(jsx)).

handle_error_undefined_channel() ->
  meck:expect(authentication, check, 3, ok),
  meck:expect(cowboy_req, body, 1,
              {ok, undefined, req1}),
  meck:expect(cowboy_req, qs_vals, 1, {qsvals, req2}),
  meck:expect(cowboy_req, path, 1, {path, req3}),
  meck:expect(jsx, decode, 1,
              [{<<"no_channel">>, <<"etc">>}]),
  ?assertEqual({ok, req3, state},
               event_handler:handle(req, state)),
  ?assert(meck:validate(authentication)),
  ?assert(meck:validate(cowboy_req)),
  ?assert(meck:validate(jsx)),
  ?assert(meck:validate(gproc)).

handle_error_failing_authentication() ->
  meck:expect(authentication, check, 3, error),
  meck:expect(cowboy_req, body, 1,
              {ok, undefined, req1}),
  meck:expect(cowboy_req, qs_vals, 1, {qsvals, req2}),
  meck:expect(cowboy_req, path, 1, {path, req3}),
  meck:expect(jsx, decode, 1,
              [{<<"no_channel">>, <<"etc">>}]),
  ?assertEqual({ok, req3, state},
               event_handler:handle(req, state)),
  ?assert(meck:validate(authentication)),
  ?assert(meck:validate(cowboy_req)),
  ?assert(meck:validate(jsx)),
  ?assert(meck:validate(gproc)).


start() ->
  meck:new(authentication),
  meck:new(jsx),
  meck:new(gproc),
  meck:new(cowboy_req).

stop(_) ->
  meck:unload(authentication),
  meck:unload(jsx),
  meck:unload(gproc),
  meck:unload(cowboy_req).
