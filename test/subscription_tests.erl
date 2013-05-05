-module(subscription_tests).
-include_lib("eunit/include/eunit.hrl").

subscrition_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"subscribe to a public channel", fun subscribe_public/0},
   {"subscribe with missing channel", fun subscribe_missing_channel/0},
   {"subscribe to an already subscribed channel", fun subscribe_already_subscribed/0},
   {"subscribe to a private channel", fun subscribe_private/0},
   {"subscribe to a presence channel", fun subscribe_presence/0},
   {"subscribe to a presence_channel and bad auth", fun subscribe_presence_bad_auth/0},
   {"subscribe to a private channel and bad auth", fun subscribe_private_bad_auth/0},
   {"unsubscribe from a channel", fun unsubscribe_channel/0},
   {"unsubscribe from a channel without being subscribed", fun unsubscribe_channel_without_being_subscribed/0},
   {"unsubscribe from a presence channel", fun unsubscribe_presence_channel/0},
   {"returns true if a channel is subscribed", fun is_subscribed_true/0},
   {"returns false if a channel is not subscribed", fun is_subscribed_false/0}]
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
  meck:expect(auth_signature, validate, 2, ok),
  ?assertEqual(ok, subscription:subscribe([{<<"channel">>, <<"private-channel">>},
                                           {<<"auth">>, <<"signeddata">>}],
                                          <<"SocketId">>)),
  ?assert(meck:validate(auth_signature)),
  ?assert(meck:validate(gproc)).

subscribe_presence() ->
  meck:expect(gproc, lookup_local_properties, 1, []),
  meck:expect(gproc, lookup_values, 1, values),
  meck:expect(gproc, reg, 2, registered),
  meck:expect(gproc, send, 2, sent),
  meck:expect(auth_signature, validate, 2, ok),
  meck:expect(pusher_event, presence_member_added, 3, event_message),
  ?assertEqual({presence, <<"presence-channel">>, values},
               subscription:subscribe([{<<"channel">>, <<"presence-channel">>},
                                       {<<"auth">>, <<"signeddata">>},
                                       {<<"channel_data">>,
                                        <<"{\"user_id\" : \"id123\", \"user_info\" : \"info456\" }">>
                                       }
                                      ],
                                          <<"SocketId">>)),
  ?assert(meck:validate(auth_signature)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(gproc)).

subscribe_private_bad_auth() ->
  meck:expect(auth_signature, validate, 2, error),
  ?assertEqual(error, subscription:subscribe([{<<"channel">>, <<"private-channel">>},
                                              {<<"auth">>, <<"signeddata">>}],
                                             <<"SocketId">>)),
  ?assert(meck:validate(auth_signature)).

subscribe_presence_bad_auth() ->
  meck:expect(auth_signature, validate, 2, error),
  ?assertEqual(error, subscription:subscribe([{<<"channel">>, <<"presence-channel">>},
                                              {<<"auth">>, <<"signeddata">>}],
                                             <<"SocketId">>)),
  ?assert(meck:validate(auth_signature)).

unsubscribe_channel() ->
  meck:expect(gproc, lookup_local_properties, 1, [self()]),
  meck:expect(gproc, unreg, 1, ok),
  ?assertEqual(ok, subscription:unsubscribe([{<<"channel">>, <<"a_channel">>}])),
  ?assert(meck:validate(gproc)).

unsubscribe_channel_without_being_subscribed() ->
  meck:expect(gproc, lookup_local_properties, 1, []),
  ?assertEqual(ok, subscription:unsubscribe([{<<"channel">>, <<"a_channel">>}])),
  ?assert(meck:validate(gproc)).

unsubscribe_presence_channel() ->
  meck:expect(gproc, lookup_local_properties, 1, [self()]),
  meck:expect(gproc, get_value, 1, {userid, userinfo}),
  meck:expect(gproc, unreg, 1, ok),
  meck:expect(gproc, send, 2, ok),
  meck:expect(pusher_event, presence_member_removed, 2, event_message),
  ?assertEqual(ok, subscription:unsubscribe([{<<"channel">>, <<"presence-channel">>}])),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(gproc)).

is_subscribed_true() ->
  meck:expect(gproc, info, 2, {gproc,[{{p,l,{pusher,<<"channel">>}},undefined},
                                      {{p,n,<<"SocketId">>},undefined}]}),
  ?assertEqual(true, subscription:is_subscribed(<<"channel">>)),
  ?assert(meck:validate(gproc)).

is_subscribed_false() ->
  meck:expect(gproc, info, 2, {gproc,[{{p,l,{pusher,<<"channel">>}},undefined},
                                      {{p,n,<<"SocketId">>},undefined}]}),
  ?assertEqual(false, subscription:is_subscribed(<<"a-channel">>)),
  ?assert(meck:validate(gproc)).

start() ->
  meck:new(pusher_event),
  meck:new(auth_signature),
  meck:new(gproc).

stop(_) ->
  meck:unload(pusher_event),
  meck:unload(auth_signature),
  meck:unload(gproc).
