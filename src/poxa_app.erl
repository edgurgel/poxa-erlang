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
            {"/app/:app_key", websocket_handler, []}
            ]}
        ]),
  case load_config() of
    {ok, Configs} -> lager:info("Starting Poxa using app_key: ~p, app_id: ~p, app_secret: ~p on port ~p", Configs),
      Port = lists:last(Configs),
      {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
            {env, [{dispatch, Dispatch}]}
            ]),
      poxa_sup:start_link();
    invalid_configuration -> lager:error("Error on start, set app_key, app_id and app_secret"),
      exit(invalid_configuration)
  end.

stop(_State) ->
  ok.

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
