-module(erlchat_server).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

% Call echo:listen(Port) to start the service.
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("Started tcp server on port ~p~n", [Port]),

    ClientDict = dict:new(),
	PidDict = dict:new(),
    DictPid = spawn_link(fun() -> mem:loop(ClientDict, PidDict) end),

    accept(LSocket, DictPid).

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket, DictPid) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_tcp:send(Socket, <<"Welcome! Insert your username: ">>),
    case gen_tcp:recv(Socket, 0) of
        {ok, Username} ->
            UsernameStr = binary_to_list(Username),
            TrimmedUsername = string:trim(UsernameStr),
            io:format("New client connected: ~p~n", [TrimmedUsername]),
            % Start the dict handler process to manage clients
            mem:add_client(DictPid, Socket, TrimmedUsername),
            spawn(fun() -> loop(Socket, DictPid) end)
    end,
    accept(LSocket, DictPid).

% Echo back whatever data we receive on Socket.
loop(Socket, DictPid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {Command, Payload} = process_message(Data),
            case string:lowercase(Command) of
                "whoami" ->
                    Username = mem:get_username(DictPid, self(), Socket),
                    gen_tcp:send(Socket, list_to_binary("You are: " ++ Username ++ "\n"));
                "say" ->
                    Username = mem:get_username(DictPid, self(), Socket),
                    SocketList = mem:get_all_sockets(DictPid, self()),
                    io:format("Writing to: ~p~n", [SocketList]),
                    lists:foreach(fun(S) -> 
                        case S == Socket of
                            true -> ok;
                            false -> 
                                gen_tcp:send(S, list_to_binary(Username ++ ": " ++ Payload ++ "\n"))
                        end
                    end, SocketList);
                _ ->
                    gen_tcp:send(Socket, list_to_binary("Unknown command!\n"))
            end,
            loop(Socket, DictPid);
        {error, closed} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            DictPid ! {remove_client, self(), Socket},
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("TCP error on socket ~p: ~p~n", [Socket, Reason]),
            ok
    end.

process_message(Data) ->
    io:format("Data: ~p~n", [binary_to_list(Data)]),
    Message = binary_to_list(Data),
    case lists:member(hd(":"), Message) of
        true ->
            {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message);
        false ->
            Command = Message,
            Content = ""
    end,    
    Command1 = string:trim(Command),
    Content1 = string:trim(Content),
    io:format("Command: ~p~n", [Command1]),
    io:format("Content: ~p~n", [Content1]),
    {Command1, Content1}.
