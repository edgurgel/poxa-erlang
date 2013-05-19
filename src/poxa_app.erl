-module(poxa_app).
-compile([{parse_transform, lager_transform}]).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
  Dispatch = cowboy_router:compile([
        {'_', [
            {"/ping", ping_handler, []},
            {"/apps/:app_id/events", event_handler, []},
            {"/apps/:app_id/channels/[:channel]", channels_handler, []},
            {"/app/:app_key", websocket_handler, []}
            ]}
        ]),
  case load_config() of
    {ok, Configs} ->
      lager:info("Starting Poxa using app_key: ~p, app_id: ~p, app_secret: ~p on port ~p", Configs),
      Port = lists:last(Configs),
      {ok, _} = cowboy:start_http(http, 100,
                                  [{port, Port}],
                                  [{env, [{dispatch, Dispatch}]}]),
      run_ssl(Dispatch),
      poxa_sup:start_link();
    invalid_configuration -> lager:error("Error on start, set app_key, app_id and app_secret"),
      exit(invalid_configuration)
  end.

stop(_State) ->
  ok.

run_ssl(Dispatch) ->
  case application:get_env(poxa, ssl) of
    {ok, SSLConfig} ->
      Port = proplists:get_value(port, SSLConfig),
      CacertFile = proplists:get_value(cacertfile, SSLConfig),
      CertFile = proplists:get_value(certfile, SSLConfig),
      KeyFile = proplists:get_value(keyfile, SSLConfig),
      if Port /= undefined, CacertFile /= undefined,
         CertFile /= undefined, KeyFile /= undefined ->
          {ok, _} = cowboy:start_https(https, 100,
                                       [{port, Port},
                                        {cacertfile, CacertFile},
                                        {certfile, CertFile},
                                        {keyfile, KeyFile}],
                                       [{env, [{dispatch, Dispatch}]}]),
          lager:info("Starting Poxa using SSL on port ~p", [Port]);
        true -> lager:error("Please specify port, cacertfile, certfile and keyfile")
      end;
    undefined -> lager:info("SSL not configured/started")
  end.


load_config() ->
  try
    {ok, AppKey} = application:get_env(poxa, app_key),
    {ok, AppId} = application:get_env(poxa, app_id),
    {ok, AppSecret} = application:get_env(poxa, app_secret),
    {ok, Port} = application:get_env(poxa, port),
    {ok, [AppKey, AppId, AppSecret, Port]}
  catch
    error:{badmatch, BadMatch} -> lager:info("Badmatch ~p", [BadMatch]),
      invalid_configuration
  end.
