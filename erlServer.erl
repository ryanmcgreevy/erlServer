-module(erlServer).
-export([listen/1]).
-record(subscription, {name, subscribers = []}).
%%self() gets the Pid of the process its evoked in

%% TCP options for our listening socket.  The initial list atom
%% specifies that we should receive data as lists of bytes (ie
%% strings) rather than binary objects and the rest are explained
%% better in the Erlang docs than I can do here.

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).

%% Listen on the given port, accept the first incoming connection and
%% launch the echo loop on it.

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    register(client_manager, spawn(fun() -> client_manager(dict:new()) end)),
    do_accept(LSocket).

%% The accept gets its own function so we can loop easily.  Yay tail
%% recursion!

do_accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> do_echo(Socket)end),
    do_accept(LSocket).

%% Sit in a loop, echoing everything that comes in on the socket.
%% Exits cleanly on client disconnect.

do_echo(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    handle_client( Socket, Data),
        {error, closed} ->
            ok
    end.

handle_client(Socket, Data) ->
    [Cmd1, Cmd2 | Rest] = Data,
    case [Cmd1, Cmd2] of
        "dt" ->
	    client_manager ! {data, Socket, Rest};
        "nm" ->
            client_manager ! {name, Socket, Rest}
    end.

client_manager(Players) ->
    receive
        {data, Socket, Data} ->
            {ok, CSocket} = dict:find("Ryan", Players),
            {match, Matches} = parse_packets(Data),
            io:format("~w",[Matches]);
	    %gen_tcp:send(CSocket, Data);    
        {name, Socket, Name} ->
            PlayersMod = dict:store(Name, Socket, Players),
            gen_tcp:send(Socket, Name),
	    client_manager(PlayersMod)
    end,
    client_manager(Players).

parse_packets(Packet) ->
    {ok, Reg} = re:compile("<([A-Z][A-Z0-9]*)\\b[^>]*>(.*?)</\\1>", [unicode, caseless]),
    Result = re:run(Packet, Reg, [global, {capture, [1,2], list}]),
    Result.

