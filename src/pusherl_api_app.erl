-module(pusherl_api_app).
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
    {ok, Configs} -> lager:info("Starting pusherl using app_key: ~p, app_id: ~p, app_secret: ~p on port ~p", Configs),
      Port = lists:last(Configs),
      {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
            {env, [{dispatch, Dispatch}]}
            ]),
      pusherl_api_sup:start_link();
    invalid_configuration -> lager:error("Error on start, set app_key, app_id and app_secret"),
      exit(invalid_configuration)
  end.

stop(_State) ->
  ok.

load_config() ->
  try
    {ok, AppKey} = application:get_env(pusherl_api, app_key),
    {ok, AppId} = application:get_env(pusherl_api, app_id),
    {ok, AppSecret} = application:get_env(pusherl_api, app_secret),
    {ok, Port} = application:get_env(pusherl_api, port),
    {ok, [AppKey, AppId, AppSecret, Port]}
  catch
    error:{badmatch, BadMatch} -> lager:info("Badmatch ~p", [BadMatch]),
      invalid_configuration
  end.
