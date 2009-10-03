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
    register(game_manager, spawn(fun() -> game_manager(dict:new()) end)),
    do_accept(LSocket).

%% The accept gets its own function so we can loop easily.
do_accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> do_echo(Socket)end),
    do_accept(LSocket).

%% Sit in a loop, waiting for messages on the socket, sends data to handle_client
%% Exits cleanly on client disconnect.

do_echo(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
	    handle_client(Socket, Data),
            do_echo(Socket);
        {error, closed} ->
            ok
    end.

%sends data from socket to be parsed and routes the message according to first tag
handle_client(Socket, Data) ->
    {match, Matches} = parse_packets(Data),
    [[Tag, Content] | Rest] = Matches,
    case Tag of
        "Data" ->
	    client_manager ! {data, Socket, Content};
        "Name" ->
            client_manager ! {name, Socket, Content};
        "Players" ->
            client_manager ! {players, Socket};
        "Game" ->
            [[_Tag, MyName] | _] = Rest,
            client_manager ! {game, Socket, Content, MyName};
        "Move" ->
            [[_Tag, MyName] | _] = Rest,
            game_manager ! {move, Socket, Content, MyName}
    end.

%maintains a list of players (note: add delete on disconnect) and options on what to do with them
client_manager(Players) ->
    receive
        {data, Socket, Data} ->
            {ok, CSocket} = dict:find("Ryan", Players),
	    gen_tcp:send(CSocket, Data);
        {name, Socket, Name} ->
            PlayersMod = dict:store(Name, Socket, Players),
	    client_manager(PlayersMod);
        {players, Socket} ->
            gen_tcp:send(Socket, list_players("<Players>" ++ dict:to_list(Players) ++ "</Players>", []));
        {game, Socket, Opponent, MyName} ->
            {ok, CSocket} = dict:find(Opponent, Players),
            Pid = spawn(fun() -> game(Socket, CSocket) end),
            game_manager ! {new, Opponent, MyName, Pid}
    end,
    client_manager(Players).

%use regex to parse the xml-like messages used to send data
parse_packets(Packet) ->
    {ok, Reg} = re:compile("<([A-Z][A-Z0-9]*)\\b[^>]*>(.*?)</\\1>", [unicode, caseless]),
    Result = re:run(Packet, Reg, [global, {capture, [1,2], list}]),
    Result.

%returns a list of players separated by a single space
list_players([], ReturnList) ->
    ReturnList;
list_players(Players, ReturnList) ->
    [ {Person, Socket} | Rest ] = Players,
    UpdatedPlayers = [Person, " " | ReturnList],
    list_players(Rest, UpdatedPlayers).

%game process, runs when new game is made and basically makes sure the players
%are updated on the game state
game(P1S, P2S) ->
    receive
        {move, Socket, Move} ->
            case Socket of
                P1S ->
                    gen_tcp:send(P2S, Move);
                P2S ->
                    gen_tcp:send(P1S, Move)
            end
    end,
    game(P1S, P2S).

%manages the creation of a new game, moves made by players and a list of active games
game_manager(Games) ->
    receive
        {new, Name1, Name2, Pid} ->
            Update1 = dict:store(Name1, Pid, Games),
            Update2 = dict:store(Name2, Pid, Update1),
            game_manager(Update2);
        {move, Socket, Move, Name} ->
            {ok, Pid} = dict:find(Name, Games),     
            Pid ! {move, Socket, Move}
    end,
    game_manager(Games).      

