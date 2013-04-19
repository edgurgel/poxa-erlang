-module(pusherl_api).

-export([start/0]).

start() ->
  ok = application:start(crypto),
  ok = application:start(ranch),
  ok = application:start(cowboy),
  ok = application:start(gproc),
  ok = application:start(erlsha2),
  ok = lager:start(),
  ok = application:start(pusherl_api).
