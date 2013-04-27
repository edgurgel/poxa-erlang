-compile([{parse_transform, lager_transform}]).
-module(subscription).

-export([subscribe/2, unsubscribe/1]).

subscribe(Data, SocketId) ->
  Channel = proplists:get_value(<<"channel">>, Data),
  CheckChannel = case Channel of
    <<"private-", _PrivateChannel/binary>> ->
      Auth = proplists:get_value(<<"auth">>, Data),
      validate(Channel, Auth, SocketId);
    undefined -> lager:info("Missing channel"),
      error;
    _ -> ok % Public channel
  end,
  subscribe_channel(CheckChannel, Channel).

subscribe_channel(error, Channel) ->
  lager:info("Error while subscribing to channel ~p", [Channel]),
  error;

subscribe_channel(ok, Channel) ->
  lager:info("Subscribing to channel ~p", [Channel]),
  Pids = gproc:lookup_local_properties({pusher, Channel}),
  case lists:any(fun(Pid) -> Pid == self() end, Pids) of
    true -> lager:info("Already subscribed ~p on channel ~p", [self(), Channel]);
    false -> lager:info("Registering ~p to channel ~p", [self(), Channel]),
      gproc:reg({p, l, {pusher, Channel}})
  end,
  ok.

unsubscribe(Data) ->
  Channel = proplists:get_value(<<"channel">>, Data, undefined),
  lager:info("Unsubscribing to channel ~p", [Channel]),
  Pids = gproc:lookup_local_properties({pusher, Channel}),
  case lists:any(Pids, self()) of
    true -> gproc:unreg({p, l, {pusher, Channel}});
    false -> lager:debug("Already subscribed")
  end.

% This function should be moved to authentication
validate(_Channel, undefined, _SocketId) -> error;
validate(Channel, Auth, SocketId) ->
  ToSign = <<SocketId/binary, ":", Channel/binary>>,
  try
    [AppKey, RemoteSignedData] = split_auth(Auth),
    ok = authentication:check_key(AppKey),
    case application:get_env(pusherl_api, app_secret) of
      {ok, AppSecret} ->
        SignedData = list_to_binary(string:to_lower(hmac:hexlify(hmac:hmac256(AppSecret, ToSign)))),
        case SignedData of
          RemoteSignedData -> ok;
          _ -> lager:info("Auth failed."),
            error
        end;
      _ -> error
    end
  catch
    error:{badmatch, BadMatch} -> lager:info("Badmatch ~p", [BadMatch]),
      error;
    throw:{badauth, Error} -> lager:info("Error during authentication ~p", [Error]),
      error
  end.

split_auth(Auth) ->
  binary:split(Auth, <<":">>).
