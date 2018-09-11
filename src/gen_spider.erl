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

-export_type([
  option/0
]).

-include("gen_spider.hrl").
-include("gen_spider_internal.hrl").

-record(spider, {
  module :: module(),
  options :: [option()],
  state :: state()
}).

-callback init(Args :: term()) -> {ok, State :: state()} | ignore | {stop, Reason :: any()}.

-callback start_requests(State :: state()) ->
  {ok, Requests :: list(), NewState :: state()}.

-optional_callbacks([
  start_requests/1
]).

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
    {ok, State} ->
      % returns 0 timeout to start the spider immediately.
      {ok, Spider#spider{state=State}, 0};
    {ok, State, Delay} when is_integer(Delay) ->
      {ok, Spider#spider{state=State}, Delay};
    Else ->
      Else
  end.

%% @private
handle_call(_, _From, Spider) ->
  {reply, ok, Spider}.

%% @private
handle_cast(stop, Spider) ->
  {stop, normal, Spider}.

%% @private
%% This get called on the initial crawl due to the timeout of 0.
handle_info(timeout, Spider) ->
  #spider{module=Module, options=Opts, state=State} = Spider,
  case proplists:get_value(start_urls, Opts, []) of
    [] ->
      case Module:start_requests(State) of
        {ok, _Requests, NewState} ->
          {noreply, Spider#spider{state=NewState}};
        _Else ->
          {stop, invalid_return, Spider}
      end;
    Urls when is_list(Urls) ->
      {noreply, Spider}
  end;

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
