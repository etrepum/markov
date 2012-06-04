-module(markov_server).
-behaviour(gen_server).

-export([input/1, output/1, output/0]).
-export([start_link/0, start_link/1, start/1]).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-record(state, {t :: term()}).

-define(APP, markov).

output() ->
    output([]).

output(L) ->
    gen_server:call(?MODULE, {output, L}).

input(B) ->
    ok = gen_server:cast(?MODULE, {input, B}).

start_link() ->
    Args = [{K, V} || K <- [module, etf],
                      {ok, V} <- [application:get_env(?APP, K)]],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start(Args) ->
    gen_server:start(?MODULE, Args, []).

init(Args) ->
    Mod = proplists:get_value(module, Args, markov_cstack),
    T = markov:new(Mod),
    T1 = case proplists:get_value(etf, Args) of
             undefined ->
                 T;
             FName ->
                 {ok, B} = file:read_file(FName),
                 markov:from_list(binary_to_term(B), T)
         end,
    {ok, #state{t=T1}, hibernate}.

handle_cast({input, B}, S=#state{t=T}) ->
    {noreply, S#state{t=markov:input(B, T)}};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Req, State) ->
    {noreply, State}.

handle_call({output, L}, _From, S=#state{t=T}) ->
    {reply, markov:output(L, T), S};
handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
