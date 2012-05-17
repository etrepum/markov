-module(markov_server).
-behaviour(gen_server).

-export([start_link/1, start/1]).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-record(state, {}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start(Args) ->
    gen_server:start(?MODULE, Args, []).

init([]) ->
    {ok, #state{}}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
