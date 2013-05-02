-compile([{parse_transform, lager_transform}]).
-module(auth_signature).

-export([validate/2]).
% http://pusher.com/docs/auth_signatures

validate(ToSign, Auth) ->
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
    error:{badmatch, BadMatch} -> lager:info("Badmatch during validation", [BadMatch]),
      error;
    throw:{badauth, Error} -> lager:info("Error during authentication ~p", [Error]),
      error
  end.

split_auth(Auth) ->
  binary:split(Auth, <<":">>).
