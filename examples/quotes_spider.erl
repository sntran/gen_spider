% File: quotes_spider.erl
-module(quotes_spider).
-behaviour(gen_spider).

%% API
-export([
  start_link/0,
  stop/1
]).

%% gen_spider callbacks
-export([
  init/1,
  parse/2
]).

start_link() ->
  gen_spider:start_link(?MODULE, [], [
    {start_urls, [<<"http://quotes.toscrape.com/page/1/">>]}
  ]).

stop(Spider) ->
  gen_spider:stop(Spider).

%% gen_spider callbacks

init(_) ->
  {ok, []}.

parse(#{status := 200, headers := _Headers, body := Body}, State) ->
  io:format("~p~n", [Body]),
  {ok, [Body] ++ State}.
