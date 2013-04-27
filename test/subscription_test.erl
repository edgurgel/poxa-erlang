-module(subscription_test).
-include_lib("eunit/include/eunit.hrl").

subscrition_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"subscribe to a public channel", fun subscribe_public/0},
   {"subscribe with missing channel", fun subscribe_missing_channel/0},
   {"subscribe to an already subscribed channel", fun subscribe_already_subscribed/0},
   {"subscribe to a private channel", fun subscribe_private/0},
   {"subscribe to a private channel and bad auth", fun subscribe_private_bad_auth/0}]
  }.

subscribe_public() ->
  meck:expect(gproc, lookup_local_properties, 1, []),
  meck:expect(gproc, reg, 1, registered),
  ?assertEqual(ok, subscription:subscribe([{<<"channel">>, <<"public-channel">>}],
                                          undefined)),
  ?assert(meck:validate(gproc)).

subscribe_missing_channel() ->
  ?assertEqual(error, subscription:subscribe([], undefined)).

subscribe_already_subscribed() ->
  meck:expect(gproc, lookup_local_properties, 1, [self()]),
  ?assertEqual(ok, subscription:subscribe([{<<"channel">>, <<"public-channel">>}],
                                          <<"SocketId">>)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(gproc)).

subscribe_private() ->
  meck:expect(application, get_env, 2, {ok, <<"secret">>}),
  AppKey = <<"appkey">>,
  Signature = list_to_binary(string:to_lower(hmac:hexlify(hmac:hmac256(<<"secret">>, <<"SocketId:private-channel">>)))),
  Auth = <<AppKey/binary, ":" ,Signature/binary>>,
  meck:expect(gproc, lookup_local_properties, 1, []),
  meck:expect(gproc, reg, 1, registered),
  meck:expect(authentication, check_key, 1, ok),
  ?assertEqual(ok, subscription:subscribe([{<<"channel">>, <<"private-channel">>},
                                           {<<"auth">>, Auth}],
                                          <<"SocketId">>)),
  ?assert(meck:validate(authentication)),
  ?assert(meck:validate(application)),
  ?assert(meck:validate(gproc)).

subscribe_private_bad_auth() ->
  meck:expect(application, get_env, 2, {ok, <<"secret">>}),
  Auth = hmac:hmac256(<<"secret">>, <<"WrongAuth">>),
  ?assertEqual(error, subscription:subscribe([{<<"channel">>, <<"private-channel">>},
                                              {<<"auth">>, Auth}],
                                             <<"SocketId">>)),
  ?assert(meck:validate(application)).

start() ->
  meck:new(authentication),
  meck:new(application, [unstick]),
  meck:new(gproc).

stop(_) ->
  meck:unload(authentication),
  meck:unload(application),
  meck:unload(gproc).
