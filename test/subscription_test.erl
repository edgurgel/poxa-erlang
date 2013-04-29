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
  ?assert(meck:validate(gproc)).

subscribe_private() ->
  meck:expect(gproc, lookup_local_properties, 1, []),
  meck:expect(gproc, reg, 1, registered),
  meck:expect(auth_signature, validate, 3, ok),
  ?assertEqual(ok, subscription:subscribe([{<<"channel">>, <<"private-channel">>},
                                           {<<"auth">>, <<"signeddata">>}],
                                          <<"SocketId">>)),
  ?assert(meck:validate(auth_signature)),
  ?assert(meck:validate(gproc)).

subscribe_private_bad_auth() ->
  meck:expect(auth_signature, validate, 3, error),
  ?assertEqual(error, subscription:subscribe([{<<"channel">>, <<"private-channel">>},
                                              {<<"auth">>, <<"signeddata">>}],
                                             <<"SocketId">>)),
  ?assert(meck:validate(auth_signature)).

start() ->
  meck:new(auth_signature),
  meck:new(gproc).

stop(_) ->
  meck:unload(auth_signature),
  meck:unload(gproc).
