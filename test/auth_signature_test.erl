-module(auth_signature_test).
-include_lib("eunit/include/eunit.hrl").

auth_signature_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"valid signature", fun valid_signature/0},
   {"invalid signature", fun invalid_signature/0},
   {"invalid key", fun invalid_key/0}]
  }.

valid_signature() ->
  meck:expect(application, get_env, 2, {ok, <<"secret">>}),
  AppKey = <<"appkey">>,
  Signature = list_to_binary(string:to_lower(hmac:hexlify(hmac:hmac256(<<"secret">>, <<"SocketId:private-channel">>)))),
  Auth = <<AppKey/binary, ":" ,Signature/binary>>,
  meck:expect(authentication, check_key, 1, ok),
  ?assertEqual(ok, auth_signature:validate(<<"private-channel">>, Auth, <<"SocketId">>)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(authentication)).

invalid_key() ->
  meck:expect(authentication, check_key, fun() -> meck:exception(throw, {badauth, "error"}) end),
  ?assertEqual(error, auth_signature:validate(<<"private-channel">>, <<"Auth">>, <<"SocketId">>)),
  ?assert(meck:validate(authentication)).

invalid_signature() ->
  meck:expect(application, get_env, 2, {ok, <<"secret">>}),
  meck:expect(authentication, check_key, 1, ok),
  ?assertEqual(error, auth_signature:validate(<<"private-channel">>, <<"Wrong:Auth">>, <<"SocketId">>)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(authentication)).

start() ->
  meck:new(authentication),
  meck:new(application, [unstick]).

stop(_) ->
  meck:unload(authentication),
  meck:unload(application).

