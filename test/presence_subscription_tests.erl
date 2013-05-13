-module(presence_subscription_tests).

-include_lib("eunit/include/eunit.hrl").
presence_subscription_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"subscribe to a presence channel", fun subscribe_presence/0},
   {"subscribe to a presence channel on already subscribed channel", fun subscribe_presence_already_subscribed/0},
   {"subscribe to a presence channel having the same userid", fun subscribe_presence_same_userid/0},
    {"unsubscribe to a presence channel being subscribed", fun unsubscribe_presence_if_subscribed/0},
    {"unsubscribe to a presence channel without a subscription", fun unsubscribe_presence_if_not_subscribed/0},
    {"check subscribed presence channels and remove having only one connection on userid", fun check_and_remove_channels_only_one_connection/0},
    {"check subscribed presence channels and remove having more than one connection on userid", fun check_and_remove_channels_more_than_one_connection/0},
    {"check subscribed presence channels, but none found and do nothing", fun check_and_remove_channels_empty/0}]}.

subscribe_presence() ->
  meck:expect(subscription, is_subscribed, 1, false),
  meck:expect(gproc, select, 1, []), % false for user_id_already_on_presence_channel/2
  meck:expect(gproc, add_shared_local_counter, 2, ok),
  meck:expect(gproc, send, 2, sent),
  meck:expect(gproc, reg, 2, registered),
  meck:expect(pusher_event, presence_member_added, 3, event_message),
  meck:expect(gproc, lookup_values, 1, values),
  ?assertEqual({presence, <<"presence-channel">>, values},
               presence_subscription:subscribe(<<"presence-channel">>,
                                               <<"{\"user_id\" : \"id123\", \"user_info\" : \"info456\" }">>)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(gproc)).

subscribe_presence_already_subscribed() ->
  meck:expect(subscription, is_subscribed, 1, true),
  meck:expect(gproc, lookup_values, 1, values),
  ?assertEqual({presence, <<"presence-channel">>, values},
               presence_subscription:subscribe(<<"presence-channel">>,
                                               <<"{\"user_id\" : \"id123\", \"user_info\" : \"info456\" }">>)),
  ?assert(meck:validate(subscription)).

subscribe_presence_same_userid() ->
  meck:expect(subscription, is_subscribed, 1, false),
  meck:expect(gproc, select, 1, [something]), % true for user_id_already_on_presence_channel/2
  meck:expect(gproc, update_shared_counter, 2, ok),
  meck:expect(gproc, reg, 2, registered),
  meck:expect(gproc, lookup_values, 1, values),
  ?assertEqual({presence, <<"presence-channel">>, values},
               presence_subscription:subscribe(<<"presence-channel">>,
                                               <<"{\"user_id\" : \"id123\", \"user_info\" : \"info456\" }">>)),
  ?assert(meck:validate(subscription)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(gproc)).

unsubscribe_presence_if_subscribed() ->
  meck:expect(gproc, get_value, 1, {userid, userinfo}),
  meck:expect(gproc, send, 2, sent),
  meck:expect(pusher_event, presence_member_removed, 2, event_message),
  ?assertEqual(sent,
               presence_subscription:unsubscribe(<<"presence-channel">>)),
  ?assert(meck:validate(pusher_event)),
  ?assert(meck:validate(gproc)).

unsubscribe_presence_if_not_subscribed() ->
  meck:expect(gproc, get_value, 1, empty),
  ?assertEqual(undefined,
               presence_subscription:unsubscribe(<<"presence-channel">>)),
  ?assert(meck:validate(gproc)).

check_and_remove_channels_only_one_connection() ->
  meck:expect(gproc, select, 1, [[<<"presence-channel">>, userid]]),
  meck:expect(gproc, get_value, 2, 1),
  meck:expect(gproc, unreg_shared, 1, ok),
  meck:expect(pusher_event, presence_member_removed, 2, msg),
  meck:expect(gproc, send, 2, ok),
  ?assertEqual(ok,
               presence_subscription:check_and_remove()),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(pusher_event)).

check_and_remove_channels_more_than_one_connection() ->
  meck:expect(gproc, select, 1, [[<<"presence-channel">>, userid]]),
  meck:expect(gproc, get_value, 2, 5),
  meck:expect(gproc, update_shared_counter, 2, ok),
  ?assertEqual(ok,
               presence_subscription:check_and_remove()),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(pusher_event)).


check_and_remove_channels_empty() ->
  meck:expect(gproc, select, 1, []),
  ?assertEqual(ok,
               presence_subscription:check_and_remove()),
  ?assert(meck:validate(gproc)).


start() ->
  meck:new(pusher_event),
  meck:new(subscription),
  meck:new(gproc).

stop(_) ->
  meck:unload(pusher_event),
  meck:unload(subscription),
  meck:unload(gproc).
