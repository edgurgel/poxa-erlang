-module(pusher_event_tests).
-include_lib("eunit/include/eunit.hrl").

pusher_event_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"Output connection established JSON", fun connection_established/0},
    {"Output subscription succeeded JSON", fun subscription_succeeded/0},
    {"Output subscription error JSON", fun subscription_error/0},
    {"Output pong JSON", fun pong/0},
    {"Output presence member added JSON", fun presence_member_added/0},
    {"Output presence member removed JSON", fun presence_member_removed/0},
    {"Output presence presence subscription succeeded JSON", fun presence_subscription_succeeded/0},
    {"Parse a single channel and return data, channel list", fun parse_channels_single_channel/0},
    {"Parse multiple channels and return data, channels list", fun parse_channels_multiple_channels/0},
    {"Parse with no channels and return data and undefined channel", fun parse_channels_no_channel/0},
    {"Parse channels and return data, channels list and excluding socket_id", fun parse_channels_excluding_socket_id/0},
    {"Send message to channel", fun send_message_to_channel/0},
    {"Send message to channel excluding a pid", fun send_message_to_channel_excluding_a_pid/0},
    {"Send message to channels", fun send_message_to_channels/0},
    {"Send message to channels excluding a socket_id", fun send_message_to_channels_excluding_a_socket_id/0}]}.

connection_established() ->
  ?assertEqual(<<"{\"event\":\"pusher:connection_established\",\"data\":{\"socket_id\":\"SocketId\"}}">>,
               pusher_event:connection_established(<<"SocketId">>)).

subscription_succeeded() ->
  ?assertEqual(<<"{\"event\":\"pusher_internal:subscription_succeeded\",\"data\":[]}">>,
               pusher_event:subscription_succeeded()).

subscription_error() ->
  ?assertEqual(<<"{\"event\":\"pusher:subscription_error\",\"data\":[]}">>,
               pusher_event:subscription_error()).
pong() ->
  ?assertEqual(<<"{\"event\":\"pusher:pong\",\"data\":[]}">>,
               pusher_event:pong()).

presence_member_added() ->
  ?assertEqual(<<"{\"event\":\"pusher_internal:member_added\",\"channel\":\"channel\",\"data\":{\"user_id\":\"userid\",\"user_info\":\"userinfo\"}}">>,
               pusher_event:presence_member_added(<<"channel">>, <<"userid">>, <<"userinfo">>)).

presence_member_removed() ->
  ?assertEqual(<<"{\"event\":\"pusher_internal:member_removed\",\"channel\":\"channel\",\"data\":{\"user_id\":\"userid\"}}">>,
               pusher_event:presence_member_removed(<<"channel">>, <<"userid">>)).

presence_subscription_succeeded() ->
  ?assertEqual(<<"{\"event\":\"pusher_internal:subscription_succeeded\",\"channel\":\"presence-channel\",\"data\":{\"presence\":{\"ids\":[\"userid\"],\"hash\":{\"userid\":\"userinfo\"},\"count\":1}}}">>,
               pusher_event:presence_subscription_succeeded(<<"presence-channel">>, [{pid, {<<"userid">>, <<"userinfo">>}}])).

parse_channels_single_channel() ->
  Data = [{<<"channel">>, <<"channel_name">>},
   {<<"name">>, <<"event_etc">>} ],
  ExpectedData = [{<<"name">>, <<"event_etc">>}],
  ?assertEqual({ExpectedData, [<<"channel_name">>], undefined},
               pusher_event:parse_channels(Data)).

parse_channels_multiple_channels() ->
  Data = [{<<"channels">>, [<<"channel_name1">>, <<"channel_name2">>]},
   {<<"name">>, <<"event_etc">>} ],
  ExpectedData = [{<<"name">>, <<"event_etc">>}],
  ?assertEqual({ExpectedData, [<<"channel_name1">>, <<"channel_name2">>], undefined},
               pusher_event:parse_channels(Data)).

parse_channels_no_channel() ->
  Data = [{<<"name">>, <<"event_etc">>}],
  ?assertEqual({Data, undefined, undefined},
               pusher_event:parse_channels(Data)).

parse_channels_excluding_socket_id() ->
  Data = [{<<"channel">>, <<"channel_name">>},
          {<<"name">>, <<"event_etc">>},
          {<<"socket_id">>, <<"SocketId">>}],
  ExpectedData = [{<<"name">>, <<"event_etc">>}, {<<"socket_id">>, <<"SocketId">>}],
  ?assertEqual({ExpectedData, [<<"channel_name">>], <<"SocketId">>},
               pusher_event:parse_channels(Data)).

send_message_to_channel() ->
  meck:new(jsx),
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([{self(), ok}],
               pusher_event:send_message_to_channel(channel, [], [])),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)),
  meck:unload(jsx).

send_message_to_channel_excluding_a_pid() ->
  meck:new(jsx),
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([],
               pusher_event:send_message_to_channel(channel, [], [self()])),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)),
  meck:unload(jsx).

send_message_to_channels() ->
  meck:new(jsx),
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([[{self(), ok}]],
               pusher_event:send_message_to_channels([channel], [], undefined)),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)),
  meck:unload(jsx).

send_message_to_channels_excluding_a_socket_id() ->
  meck:new(jsx),
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([[]],
               pusher_event:send_message_to_channels([channel], [], <<"SocketId">>)),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)),
  meck:unload(jsx).

start() ->
  meck:new(gproc).

stop(_) ->
  meck:unload(gproc).
