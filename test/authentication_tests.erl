-module(authentication_tests).
-include_lib("eunit/include/eunit.hrl").

authentication_test_() ->
  {foreach,
  fun start/0,
  fun stop/1,
   [{"check correct authentication", fun check_ok/0},
    {"check valid auth_key", fun valid_auth_key/0},
    {"check invalid auth_key", fun invalid_auth_key/0},
    {"check valid timestamp", fun valid_timestamp/0},
    {"check invalid timestamp", fun invalid_timestamp/0},
    {"check valid version", fun valid_version/0},
    {"check invalid version", fun invalid_version/0},
    {"check valid body", fun valid_body/0},
    {"check invalid body", fun invalid_body/0},
    {"check valid signature", fun valid_signature/0},
    {"check invalid signature", fun invalid_signature/0}
   ]}.

check_ok() ->
  ok.

valid_auth_key() ->
  meck:expect(application, get_env, 2, {ok, authkey}),
  ?assertEqual(ok, authentication:check_key(authkey)),
  ?assert(meck:validate(application)).

invalid_auth_key() ->
  meck:expect(application, get_env, 2, {ok, authkey}),
  ?assertThrow({badauth, "app_key and auth_key dont match."}, authentication:check_key(invalidauthkey)),
  ?assert(meck:validate(application)).

valid_timestamp() ->
  {Mega,Sec,_Micro} = os:timestamp(),
  Timestamp = Mega * 1000000 + Sec,
  ?assertEqual(ok, authentication:check_timestamp(list_to_binary(integer_to_list(Timestamp)))).

invalid_timestamp() ->
  {Mega,Sec,_Micro} = os:timestamp(),
  Timestamp = Mega * 1000000 + Sec,
  ?assertThrow({badauth, "Old event, timestamp bigger than 600 s"},
               authentication:check_timestamp(list_to_binary(integer_to_list(Timestamp - 600001)))).

valid_version() ->
  ?assertEqual(ok, authentication:check_version(<<"1.0">>)).

invalid_version() ->
  ?assertThrow({badauth, "auth_version is not 1.0"}, authentication:check_version(<<"2.5">>)).

valid_body() ->
  Body = "md5",
  MD5 = <<"md5">>,
  % Could not mock crypto module
  meck:expect(hmac, hexlify, 1, "md5"),
  ?assertEqual(ok, authentication:check_body(Body, MD5)),
  ?assert(meck:validate(hmac)).

invalid_body() ->
  Body = <<"YOLO">>,
  MD5 = <<"MD5">>,
  meck:expect(hmac, hexlify, 1, "WRONGMD5"),
  ?assertThrow({badauth, "body_md5 does not match"}, authentication:check_body(Body, MD5)),
  ?assert(meck:validate(hmac)).

valid_signature() ->
  meck:expect(application, get_env, 2, {ok, app_secret}),
  meck:expect(hmac, hmac256, 2, <<"auth_signature">>),
  meck:expect(hmac, hexlify, 1, "auth_signature"),
  ?assertEqual(ok, authentication:check_signature(<<"path">>, <<"auth_key">>, <<"auth_timestamp">>,
                                                  <<"auth_version">>, <<"body_md5">>, <<"auth_signature">>)),
  ?assert(meck:validate(hmac)),
  ?assert(meck:validate(application)).

invalid_signature() ->
  meck:expect(application, get_env, 2, {ok, app_secret}),
  meck:expect(hmac, hmac256, 2, <<"auth_signature">>),
  meck:expect(hmac, hexlify, 1, "invalid_auth_signature"),
  ?assertThrow({badauth, "auth_signature does not match"},
               authentication:check_signature(<<"path">>, <<"auth_key">>, <<"auth_timestamp">>,
                                              <<"auth_version">>, <<"body_md5">>, <<"auth_signature">>)),
  ?assert(meck:validate(hmac)),
  ?assert(meck:validate(application)).

start() ->
  meck:new(hmac),
  meck:new(application, [unstick]).

stop(_) ->
  meck:unload(hmac),
  meck:unload(application).
