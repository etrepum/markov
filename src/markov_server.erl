-module(markov_server).
-behaviour(gen_server).

-export([input/1, output/1, output/0]).
-export([start_link/0, start_link/1, start/1]).
-export([checkpoint_async/2]).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).

-record(state, {t :: undefined | term(),
                new_args :: [term()],
                log_file :: string(),
                writes :: non_neg_integer(),
                checkpoint_writes :: non_neg_integer(),
                checkpoint_monitor :: reference() | undefined,
                log :: disk_log:log()}).

-define(APP, markov).

output() ->
    output(<<>>).

output(L) ->
    gen_server:call(?MODULE, {output, L}).

input(B) ->
    ok = gen_server:cast(?MODULE, {input, B}).

start_link() ->
    Args = [{K, V} || K <- [module, etf, opts, log_name, log_file,
                            checkpoint_writes],
                      {ok, V} <- [application:get_env(?APP, K)]],
    gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

start(Args) ->
    gen_server:start(?MODULE, Args, []).

init(Args) ->
    NewArgs = [proplists:get_value(module, Args, markov_cstack),
               proplists:get_value(opts, Args, [])],
    Name = proplists:get_value(log_name, Args, markov_input_log),
    LogFile = filename:join(
                [lists:concat([proplists:get_value(log_dir, Args, "log")]),
                 lists:concat([Name, ".LOG"])]),
    CWrites = proplists:get_value(checkpoint_writes, Args, 100000),
    ok = filelib:ensure_dir(LogFile),
    Log = case disk_log:open([{name, Name},
                              {file, LogFile}]) of
              {ok, L} ->
                  L;
              {repaired,
               L,
               {recovered, _GoodBytes},
               {badbytes, _BadBytes}} ->
                  L
          end,
    {ok,
     read_any(proplists:get_value(etf, Args),
              #state{writes=0,
                     checkpoint_writes=CWrites,
                     log=Log,
                     log_file=LogFile,
                     t=undefined,
                     checkpoint_monitor=undefined,
                     new_args=NewArgs}),
     hibernate}.

handle_cast({input, B}, S) ->
    {noreply, maybe_checkpoint_log(log_input(B, input(B, S)))};
handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({'DOWN', MRef, _Type, _Obj, _Info},
            State=#state{checkpoint_monitor=MRef,
                         writes=W,
                         log_file=LogFile}) ->
    error_logger:info_msg("Checkpoint of ~p finished", [LogFile]),
    {noreply, State#state{checkpoint_monitor=undefined, writes=1 + W}};
handle_info(_Req, State) ->
    {noreply, State}.

handle_call({output, B}, _From, S=#state{t=T}) ->
    {reply, markov:output(B, T), S};
handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

terminate(_Reason, #state{log=Log}) ->
    disk_log:close(Log).

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

log_input(B, S=#state{log=L}) ->
    ok = disk_log:alog(L, B),
    S.

input(B, S=#state{t=T, writes=W}) ->
    S#state{t=markov:input(B, T), writes=1 + W}.

read_any(ETF, S=#state{log=Log, log_file=LogFile}) ->
    try_read(
      [fun (SAcc) -> read_log(Log, SAcc) end,
       fun (SAcc) ->
               case disk_log:open([{name, markov_server_prev_log},
                                   {file, LogFile ++ ".prev"},
                                   {mode, read_only},
                                   {repair, false}]) of
                   {ok, L} ->
                       try
                           read_log(L, SAcc)
                       after
                           disk_log:close(L)
                       end;
                   _Err ->
                       SAcc
               end
       end,
       fun (SAcc) -> read_etf(ETF, SAcc) end,
       fun ({S0, Acc}) ->
               error_logger:info_msg("Starting a new log from scratch"),
               {checkpoint_log(from_list([], S0)), Acc} end],
      {S, []}).

read_log(Log, SAcc) ->
    {_K, LogFile} = lists:keyfind(file, 1, disk_log:info(Log)),
    error_logger:info_msg("Reading log ~p", [LogFile]),
    read_chunk(Log, disk_log:chunk(Log, start), SAcc).

try_read([F | Rest], SAcc) ->
    case F(SAcc) of
        SAcc1 = {#state{t=undefined}, _Acc} ->
            try_read(Rest, SAcc1);
        {S0, Acc} ->
            {S1, []} = lists:foldl(fun chunk_fold/2, {S0, []}, Acc),
            S1
    end.

read_chunk(Log, {Cont, Terms}, SAcc) ->
    read_chunk(Log,
               disk_log:chunk(Log, Cont),
               lists:foldl(fun chunk_fold/2, SAcc, Terms));
read_chunk(Log, {Cont, Terms, _BadBytes}, SAcc) ->
    %% Ignore bad bytes
    read_chunk(Log, {Cont, Terms}, SAcc);
read_chunk(_Log, eof, SAcc) ->
    SAcc.

read_etf(undefined, SAcc) ->
    SAcc;
read_etf(FName, {S, Acc}) ->
    error_logger:info_msg("Reading ETF ~p", [FName]),
    {ok, B} = file:read_file(FName),
    {checkpoint_log(from_list(binary_to_term(B), S)), Acc}.

chunk_fold(B, {S=#state{t=undefined}, Acc}) when is_binary(B) ->
    %% Messages that were received after the checkpoint started, but before
    %% the checkpoint was written.
    {S, [B | Acc]};
chunk_fold(B, {S, []}) when is_binary(B) ->
    {input(B, S), []};
chunk_fold(L, {S, Acc}) when is_list(L) ->
    lists:foldl(fun chunk_fold/2, {from_list(L, S), []}, Acc).

from_list(L, S=#state{new_args=[Mod, Args]}) ->
    S#state{writes=0,
            t=markov:from_list(L, markov:new(Mod, Args))}.

maybe_checkpoint_log(S=#state{checkpoint_writes=W,
                              writes=N,
                              checkpoint_monitor=undefined}) when N >= W ->
    checkpoint_log(S);
maybe_checkpoint_log(S) ->
    S.

checkpoint_log(S=#state{log_file=LogFile, log=Log, t=T,
                        checkpoint_monitor=undefined}) ->
    error_logger:info_msg("Checkpoint of ~p starting", [LogFile]),
    disk_log:reopen(Log, LogFile ++ ".prev"),
    MRef = monitor(process, spawn_link(?MODULE, checkpoint_async, [Log, T])),
    S#state{writes=0, checkpoint_monitor=MRef}.

checkpoint_async(Log, T) ->
    disk_log:log(Log, markov:to_list(T)).
