-module(erlServer).
-export([listen/1]).

%% TCP options for our listening socket.  The initial list atom
%% specifies that we should receive data as lists of bytes (ie
%% strings) rather than binary objects and the rest are explained
%% better in the Erlang docs than I can do here.

-define(TCP_OPTIONS,[list, {packet, 0}, {active, false}, {reuseaddr, true}]).

%% Listen on the given port, accept the first incoming connection and
%% launch the echo loop on it.

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    Pid = spawn(fun() -> client_manager([]) end),
    register(client_manager, Pid),
    do_accept(LSocket).

%% The accept gets its own function so we can loop easily.  Yay tail
%% recursion!

do_accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    Pid = spawn(fun() -> do_echo(Socket) end),
    spawn(fun() -> identify_client(Socket, Pid) end),
    do_accept(LSocket).

%% Sit in a loop, echoing everything that comes in on the socket.
%% Exits cleanly on client disconnect.

do_echo(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
	    handle_client(Socket, Data),
            do_echo(Socket);
        {error, closed} ->
            ok
    end.

handle_client(Socket, Data) ->
    case Data of
        "Data" ->
	    client_manager ! {data, Socket, Data};
        "Name" ->
            client_manager ! {name, Socket, Data};
        "GetPid" ->
            client_manager ! {get, Socket, Data}
    end.

client_manager(Players) ->
    receive
        {get, Socket, Data} ->
            {Data, Socket, Pid} = Players,
            

        {data, Socket, Data} ->
            
        {name, Socket, Data} ->
            Players = [{name, Socket, Pid} | Players]
    end,
    client_manager(Players).

identify_client(Socket, Pid) ->
    gen_tcp:send(Socket, "Name"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Name} ->
            client_manager ! {name, Socket, Pid};
        {error, closed} ->
            ok
    end.

