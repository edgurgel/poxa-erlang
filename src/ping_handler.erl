-compile([{parse_transform, lager_transform}]).
-module(ping_handler).
-behavior(cowboy_http_handler).

-export([init/3, handle/2, terminate/3]).

init(_Transport, Req, _Opts) ->
      {ok, Req, undefined_state}.

handle(Req, State) ->
      {ok, Req2} = cowboy_req:reply(200, [], <<"Pong!">>, Req),
      lager:info("Ping requested"),
      {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
      ok.
