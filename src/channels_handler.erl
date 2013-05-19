-compile([{parse_transform, lager_transform}]).
-module(channels_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([get_json/2]).

init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, get_json}], Req, State}.

get_json(Req, State) ->
  {ChannelName, Req2} = cowboy_req:binding(channel, Req, undefined),
   Response = case ChannelName of
    undefined -> index();
    _ -> show(ChannelName)
  end,
  {Response, Req2, State}.

index() ->
  jsx:encode([{<<"error">>, <<"not yet implemented">>}]).

show(ChannelName) ->
  InfoList = [occupied(ChannelName), subscription_count(ChannelName)],
  InfoList2 = case presence_subscription:is_presence_channel(ChannelName) of
    true -> InfoList ++ [user_count(ChannelName)];
    false -> InfoList
  end,
  jsx:encode(InfoList2).

user_count(ChannelName) ->
  UserCount = presence_subscription:user_count(ChannelName),
  {<<"user_count">>, UserCount}.

subscription_count(ChannelName) ->
  SubscriptionCount = subscription:count(ChannelName),
  {<<"subscription_count">>, SubscriptionCount}.

occupied(ChannelName) ->
  SubscriptionCount = subscription:count(ChannelName),
  Occupied = case SubscriptionCount of
    0 -> false;
    _ -> true
  end,
  {<<"occupied">>, Occupied}.
