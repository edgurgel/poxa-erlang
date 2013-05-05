-module(pusher_event_tests).
-include_lib("eunit/include/eunit.hrl").

pusher_event_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"Parse a single channel and return data, channel list", fun parse_channels_single_channel/0},
    {"Parse multiple channels and return data, channels list", fun parse_channels_multiple_channels/0},
    {"Parse with no channels and return data and undefined channel", fun parse_channels_no_channel/0},
    {"Parse channels and return data, channels list and excluding socket_id", fun parse_channels_excluding_socket_id/0},
    {"Send message to channel", fun send_message_to_channel/0},
    {"Send message to channel excluding a pid", fun send_message_to_channel_excluding_a_pid/0},
    {"Send message to channels", fun send_message_to_channels/0},
    {"Send message to channels excluding a socket_id", fun send_message_to_channels_excluding_a_socket_id/0}]}.

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
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([{self(), ok}],
               pusher_event:send_message_to_channel(channel, [], [])),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)).

send_message_to_channel_excluding_a_pid() ->
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([],
               pusher_event:send_message_to_channel(channel, [], [self()])),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)).

send_message_to_channels() ->
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([[{self(), ok}]],
               pusher_event:send_message_to_channels([channel], [], undefined)),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)).

send_message_to_channels_excluding_a_socket_id() ->
  meck:expect(gproc, lookup_pids, 1, [self()]),
  meck:expect(jsx, encode, 1, ok),
  ?assertEqual([[]],
               pusher_event:send_message_to_channels([channel], [], <<"SocketId">>)),
  ?assert(meck:validate(gproc)),
  ?assert(meck:validate(jsx)).

start() ->
  meck:new(jsx),
  meck:new(gproc).

stop(_) ->
  meck:unload(jsx),
  meck:unload(gproc).
