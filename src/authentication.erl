-compile([{parse_transform, lager_transform}]).
-module(authentication).

-export([check/3]).
-export([check_key/1]).
-export([check_timestamp/1]).
-export([check_version/1]).
-export([check_body/2]).
-export([check_signature/6]).

% http://pusher.com/docs/rest_api#authentication

check(Path, Body, QsVals) ->
  try
    % If any of these values are not avaiable -> badmatch
    AuthKey = proplists:get_value(<<"auth_key">>, QsVals),
    AuthTimestamp = proplists:get_value(<<"auth_timestamp">>, QsVals),
    AuthVersion = proplists:get_value(<<"auth_version">>, QsVals),
    BodyMD5 = proplists:get_value(<<"body_md5">>, QsVals),
    AuthSignature = proplists:get_value(<<"auth_signature">>, QsVals),

    ok = check_key(AuthKey),
    ok = check_timestamp(AuthTimestamp),
    ok = check_version(AuthVersion),
    ok = check_body(Body, BodyMD5),
    ok = check_signature(Path, AuthKey, AuthTimestamp,
                         AuthVersion, BodyMD5, AuthSignature)
  catch
    error:{badmatch, BadMatch} -> lager:info("Badmatch ~p", [BadMatch]),
      {badauth, badmatch};
    throw:{badauth, Error} -> lager:info("Error during authentication ~p", [Error]),
      {badauth, Error}
  end.

check_key(AuthKey) ->
  {ok, AppKey} = application:get_env(pusherl_api, app_key),
  case AuthKey of
    AppKey -> ok;
    _ -> throw({badauth, "app_key and auth_key dont match."})
  end.

check_timestamp(AuthTimestamp) ->
  IntAuthTimestamp = list_to_integer(binary_to_list(AuthTimestamp)),
  {Mega,Sec,_Micro} = os:timestamp(),
  Timestamp = Mega * 1000000 + Sec,
  case Timestamp - IntAuthTimestamp of
    Diff when Diff < 600000 -> ok;
    _ -> throw({badauth, "Old event, timestamp bigger than 600 s"})
  end.

check_version(AuthVersion) ->
  case AuthVersion of
    <<"1.0">> -> ok;
    _ -> throw({badauth, "auth_version is not 1.0"})
  end.

check_body(Body, BodyMD5) ->
  MD5 = hmac:hexlify(crypto:md5(Body)),
  case MD5 of
    BodyMD5 -> ok;
    _ -> throw({badauth, "body_md5 does not match"})
  end.

check_signature(Path, AuthKey, AuthTimestamp,
                AuthVersion, BodyMD5, AuthSignature) ->
%"POST\n/apps/3/events\nauth_key=278d425bdf160c739803&auth_timestamp=1353088179&auth_version=1.0&body_md5=ec365a775a4cd0599faeb73354201b6f"
  ToSign = list_to_binary([<<"POST\n">>, Path,
                           <<"\nauth_key=">>, AuthKey,
                           <<"&auth_timestamp=">>, AuthTimestamp,
                           <<"&auth_version=">>, AuthVersion,
                           <<"&body_md5=">>, BodyMD5]),
  case application:get_env(pusherl_api, app_secret) of
    {ok, AppSecret} ->
      SignedData = list_to_binary(string:to_lower(hmac:hexlify(hmac:hmac256(AppSecret, ToSign)))),
      case SignedData of
        AuthSignature -> ok;
        _ -> throw({badauth, "auth_signature does not match"})
      end;
    _ -> throw({badauth, "app_secret not found"})
  end.
