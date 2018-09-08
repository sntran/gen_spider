-module(gen_spider).
-author ('dev@sntran.com').
-behaviour(gen_server).

%% API
-export([
  start/3,
  start_link/3,
  stop/1
]).

%% gen_server callbacks
-export([
  init/1,
  handle_call/3,
  handle_cast/2,
  handle_info/2,
  terminate/2,
  code_change/3
]).

-include("gen_spider.hrl").
-include_lib("gen_spider_internal.hrl").

-record(spider, {
  module :: module(),
  options :: [option()],
  state :: state()
}).

-callback init(Args :: term()) -> {ok, State :: state()} | ignore | {stop, Reason :: any()}.

%% ==================================================================
%% API
%% ==================================================================
-spec start(module(), term(), [option()]) -> on_start().
start(Module, Args, Options) ->
  do_start(nolink, Module, Args, Options).

-spec start_link(module(), term(), [option()]) -> on_start().
start_link(Module, Args, Options) ->
  do_start(link, Module, Args, Options).

%% @private
do_start(Link, Module, Args, Options)
when is_atom(Link), is_atom(Module), is_list(Options) ->
  {Name, SpiderOpts, GenServerOpts} = parse_opts(Options),
  GenServerArgs = {Module, Args, SpiderOpts},
  gen_start(Link, Name, GenServerArgs, GenServerOpts).

-spec stop(spider()) -> ok.
stop(Spider) ->
  gen_server:stop(Spider).

%% @private
gen_start(Link, nil, Args, Opts) ->
  gen:start(gen_server, Link, gen_spider, Args, Opts);
gen_start(Link, Name, Args, Opts) when is_atom(Name) ->
  gen:start(gen_server, Link, {local, Name}, gen_spider, Args, Opts);
gen_start(Link, Name, Args, Opts) when is_tuple(Name) ->
  gen:start(gen_server, Link, Name, gen_spider, Args, Opts).

%% ==================================================================
%% gen_server Callbacks
%% ==================================================================

%% @private
init({Module, Args, Opts}) ->
  % traps exit signals so we can clean up when terminated by supervisor.
  process_flag(trap_exit, true),

  Spider = #spider{module=Module, options=Opts},

  case erlang:apply(Module, init, [Args]) of
    {ok, State} -> {ok, Spider#spider{state=State}};
    ignore -> ignore;
    {stop, Reason} -> {stop, Reason}
  end.

%% @private
handle_call(_, _From, Spider) ->
  {reply, ok, Spider}.

%% @private
handle_cast(stop, Spider) ->
  {stop, normal, Spider}.

%% @private
% handles exit message, if the gen_server is linked to other processes (than
% the supervisor) and trapping exit signals.
handle_info({'EXIT', _Pid, _Reason}, Spider) ->
  % ..code to handle exits here..
  {noreply, Spider}.

%% @private
terminate(normal, _Spider) ->
  % handles normal termination when callback retruns `{stop, normal, Spider}`
  ok;

terminate(shutdown, _Spider) ->
  io:format("terminate ~p~n", [shutdown]),
  % ..code for cleaning up here..
  ok;

terminate(_, _Spider) ->
  ok.

code_change(_OldVsn, Spider, _Extra) ->
  % ..code to convert state (and more) during code change
  {ok, Spider}.

%% ==================================================================
%% Internal Funtions
%% ==================================================================

%%-------------------------------------------------------------------
%% @doc Splits options passed to `start_*` functions.
%%
%% Takes options relevant to the spider itself, and leaves the rest for
%% gen_server options.
%% @end
%%-------------------------------------------------------------------
%% @private
-spec parse_opts([option()]) -> {name(), [option()], [option()]}.
parse_opts(Options) ->
  OptionKeys = [name, start_urls, allowed_domains],
  {Lists, GenServerOpts} = proplists:split(Options, OptionKeys),
  SpiderOpts = lists:flatten(Lists),

  case lists:keytake(name, 1, SpiderOpts) of
    false ->
      {nil, SpiderOpts, GenServerOpts};
    {value, {name, Name}, SpiderOpts1} ->
      {Name, SpiderOpts1, GenServerOpts}
  end.
