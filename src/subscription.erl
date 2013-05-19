-compile([{parse_transform, lager_transform}]).
-module(subscription).

-export([subscribe/2, unsubscribe/1]).
-export([is_subscribed/1]).
-export([count/1]).

subscribe(Data, SocketId) ->
  Channel = proplists:get_value(<<"channel">>, Data),
  case Channel of
    <<"private-", _PrivateChannel/binary>> ->
      Auth = proplists:get_value(<<"auth">>, Data),
      ToSign = <<SocketId/binary, ":", Channel/binary>>,
      case auth_signature:validate(ToSign, Auth) of
        ok -> subscribe_channel(Channel);
        error -> subscribe_error(Channel)
      end;
    <<"presence-", _PresenceChannel/binary>> ->
      Auth = proplists:get_value(<<"auth">>, Data),
      ChannelData = proplists:get_value(<<"channel_data">>, Data, <<"undefined">>),
      ToSign = <<SocketId/binary, ":", Channel/binary, ":", ChannelData/binary>>,
      case auth_signature:validate(ToSign, Auth) of
        ok -> presence_subscription:subscribe(Channel, ChannelData);
        error -> subscribe_error(Channel)
      end;
    undefined ->
      lager:info("Missing channel"),
      error;
    _ -> subscribe_channel(Channel)
  end.

subscribe_error(Channel) ->
  lager:info("Error while subscribing to channel ~p", [Channel]),
  error.

subscribe_channel(Channel) ->
  lager:info("Subscribing to channel ~p", [Channel]),
  case is_subscribed(Channel) of
    true -> lager:info("Already subscribed ~p on channel ~p", [self(), Channel]);
    false -> lager:info("Registering ~p to channel ~p", [self(), Channel]),
      gproc:reg({p, l, {pusher, Channel}})
  end,
  ok.

unsubscribe(Data) ->
  Channel = proplists:get_value(<<"channel">>, Data, undefined),
  case Channel of
    <<"presence-", _PresenceChannel/binary>> ->
      presence_subscription:unsubscribe(Channel);
    _ -> undefined
  end,
  unsubscribe_channel(Channel).

unsubscribe_channel(Channel) ->
  lager:info("Unsubscribing to channel ~p", [Channel]),
  case is_subscribed(Channel) of
    true -> gproc:unreg({p, l, {pusher, Channel}});
    false -> lager:debug("Already subscribed")
  end.

is_subscribed(Channel) ->
  Match = {{p, l, {pusher, Channel}}, self(), '_'},
  case gproc:select([{Match, [], ['$$']}]) of
    [] -> false;
    [_] -> true
  end.

count(Channel) ->
  Match = {{p, l, {pusher, Channel}}, '_', '_'},
  gproc:select_count([{Match, [], [true]}]).
