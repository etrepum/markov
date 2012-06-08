%% @doc Minimal IRC client.
-module(min_irc).
-behaviour(gen_server).

-define(APP, markov).

-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2,
         init/1, terminate/2]).
-export([start_link/0, start_link/1, start/0]).

-record(state, {socket :: gen_tcp:socket(),
                nick :: binary(),
                identify :: undefined | binary(),
                host :: string(),
                port :: non_neg_integer(),
                rng_state :: term(),
                channels :: [binary()],
                tokenizers :: undefined | [{re:mp(), atom()}],
                current_nick :: undefined | binary()}).

start_link() ->
    start_link([]).

start_link([]) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, app_args(), []).

start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, app_args(), []).

init(Args) ->
    Host = proplists:get_value(irc_host, Args, "irc.freenode.net"),
    Port = proplists:get_value(irc_port, Args, 6667),
    Nick = iolist_to_binary(proplists:get_value(nick, Args, "antihector")),
    Identify = proplists:get_value(identify, Args),
    Channels = lists:map(
                 fun iolist_to_binary/1,
                 proplists:get_value(channels, Args, ["#antihector"])),
    {ok, Sock} = gen_tcp:connect(Host,
                                 Port,
                                 [{packet, line},
                                  {mode, binary},
                                  {active, true}]),
    write(Sock, cmd(["NICK", Nick])),
    %% <username> <hostname> <servername> <realname>
    write(Sock, cmd(["USER", Nick, Nick, Nick, Nick])),
    S = #state{socket=Sock,
               nick=Nick,
               identify=Identify,
               host=Host,
               port=Port,
               channels=Channels,
               tokenizers=undefined,
               rng_state=now(),
               current_nick=Nick},
    {ok, update_tokenizers(S)}.

handle_cast(_Req, State) ->
    {noreply, State}.

handle_info({tcp, Socket, Data}, State=#state{socket=Socket, tokenizers=T}) ->
    Msg = tokenize(Data, T),
    io:format("MSG: ~p~n", [Msg]),
    handle_msg(Msg, State);
handle_info({tcp_closed, Socket}, State=#state{socket=Socket}) ->
    {stop, tcp_closed, State#state{socket=undefined}};
handle_info({tcp_error, Socket, Reason}, State=#state{socket=Socket}) ->
    {stop, {tcp_error, Reason}, State#state{socket=undefined}};
handle_info(_Req, State) ->
    {noreply, State}.

handle_call(_Req, _From, State) ->
    {reply, ignored, State}.

terminate(_Reason, #state{socket=undefined}) ->
    ok;
terminate(_Reason, #state{socket=Socket}) ->
    write(Socket, cmd(["QUIT", "Died in a fire"])),
    ok = gen_tcp:close(Socket),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

write(Sock, Raw) ->
    io:format("WRITE: ~s", [Raw]),
    ok = gen_tcp:send(Sock, Raw).

handle_msg({ping, [Server]}, State=#state{socket=Socket}) ->
    write(Socket, cmd(["PONG", Server])),
    {noreply, State};
handle_msg({nick_mode, []}, State=#state{socket=Socket, channels=Channels}) ->
    lists:foreach(
      fun (Channel) -> write(Socket, cmd(["JOIN", Channel])) end,
      Channels),
    {noreply, State};
handle_msg({nick_in_use, [Nick]}, State=#state{socket=Socket}) ->
    NewNick = <<Nick/binary, $_>>,
    write(Socket, cmd(["NICK", NewNick])),
    {noreply, State#state{current_nick=NewNick}};
handle_msg({Type, [From, To, Msg]},
           State=#state{channels=Channels}) when Type =:= privmsg orelse
                                                 Type =:= mention ->
    case lists:member(To, Channels) of
        true ->
            msg(Type, From, To, Msg, State);
        false ->
            {noreply, State}
    end;
handle_msg(_Other, State) ->
    {noreply, State}.

msg(Type, _From, To, Msg, State=#state{socket=Socket}) ->
    Chance = chance(Type),
    markov_server:input(Msg),
    case uniform(State) of
        {R, State1} when R < Chance ->
            write(Socket, cmd(["PRIVMSG", To, markov_server:output(Msg)])),
            {noreply, State1};
        {_R, State1} ->
            {noreply, State1}
    end.

chance(privmsg) ->
    0.1;
chance(mention) ->
    1.0.

uniform(State=#state{rng_state=RNG}) ->
    {Res, RNG1} = random:uniform_s(RNG),
    {Res, State#state{rng_state=RNG1}}.

compile([{Template, Token} | Rest]) ->
    {ok, R} = re:compile([$^, Template, "\r?\n$"]),
    [{R, Token} | compile(Rest)];
compile([]) ->
    [].

update_tokenizers(S) ->
    S#state{tokenizers=compile(tokenizers(S))}.

tokenize(Data, [{R, Token} | Rest]) ->
    case re:run(Data, R, [{capture, all_but_first, binary}]) of
        nomatch ->
            tokenize(Data, Rest);
        {match, Groups} ->
            {Token, Groups}
    end.

tokenizers(#state{nick=Nick, current_nick=CurrentNick}) ->
    AnyNick = <<"(?:", Nick/binary, $|, CurrentNick/binary, ")">>,
    [{<<"PING :(\\S+)">>, ping},
     {<<":\\S+ 433 (", CurrentNick/binary, ") :.*?">>,
      nick_in_use},
     {<<":", CurrentNick/binary, " MODE ", CurrentNick/binary, " :.*?">>,
      nick_mode},
     {<<":([^!]+)!\\S+ PRIVMSG (\\S+) :", AnyNick/binary, "[:,] (.*?)">>,
      mention},
     {<<":([^!]+)!\\S+ PRIVMSG (\\S+) :(.*?)">>, privmsg},
     {<<"(.*?)">>, raw}].

cmd([Last]) ->
    [$:, Last, <<"\r\n">>];
cmd([S | Rest]) ->
    [S, $\s | cmd(Rest)].

app_args() ->
    [{K, V} || K <- [irc_host, irc_port, nick, channels, identify],
                         {ok, V} <- [application:get_env(?APP, K)]].
