-compile([{parse_transform, lager_transform}]).
-module(event_handler).

-behavior(cowboy_http_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([content_types_provided/2]).
-export([content_types_accepted/2]).

init({tcp, http}, Req, _Opts) ->
  lager:info("Event requested"),
  {ok, Req, undefined_state}.

handle(Req, State) ->
  {ok, Body, Req1} = cowboy_req:body(Req),
  {QsVals, Req2} = cowboy_req:qs_vals(Req1),
  {Path, Req3} = cowboy_req:path(Req2),
  % http://pusher.com/docs/rest_api#authentication
  case authentication:check(Path,Body, QsVals) of
    ok -> lager:debug("json decoding..."),
      try jsx:decode(Body) of
        RequestData -> {RequestData2, Channels} = get_channels(RequestData),
          case Channels of
            undefined -> lager:debug("no channel defined"),
              {ok, Req3, State};
            _ -> Message = prepare_message(RequestData2),
              send_message_to_channels(Channels, Message),
              {ok, Req4} = cowboy_req:reply(200, [], <<"{}">>, Req3),
              {ok, Req4, State}
          end
      catch
        error:badarg -> lager:error("Invalid json"),
          {ok, Req4} = cowboy_req:reply(400, [], <<"{ invalid json }">>, Req3), % FIXME
          {ok, Req4, State}
      end;
    _ -> lager:info("Authentication failed"),
      {ok, Req3, State}
  end.


get_channels(Req) ->
  case proplists:get_value(<<"channel">>, Req) of
    undefined ->
      case proplists:get_value(<<"channels">>, Req) of
        undefined -> {Req, undefined}; % channel/channels not found
        Channels -> Req2 = proplists:delete(<<"channels">>, Req),
          {Req2, Channels}
      end;
    Channel -> Req2 = proplists:delete(<<"channel">>, Req),
      {Req2, [Channel]}
  end.

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, handle}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, handle}], Req, State}.

terminate(_Reason, _Req, _State) ->
  ok.

send_message_to_channels(Channels, Message) ->
  lager:debug("Sending message to channels ~p", [Channels]),
  [send_message_to_channel(Channel, Message) || Channel <- Channels].

send_message_to_channel(Channel, Message) ->
  Message2 = lists:append(Message, [{<<"channel">>, Channel}]),
  gproc:send({p, l, {pusher, Channel}}, {self(), jsx:encode(Message2)}).


% Remove name and add event to the response
prepare_message(Message) ->
  Event = proplists:get_value(<<"name">>, Message, undefined),
  Message2 = lists:append(Message, [{<<"event">>, Event}]),
  proplists:delete(<<"name">>, Message2).
