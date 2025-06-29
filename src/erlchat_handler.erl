-module(erlchat_handler).
-export([loop/1]).

-include("records.hrl").

loop(State = #handler_state{socket = Socket, dictpid = DictPid, username = Username, roomname = RoomName}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {Command, Payload} = process_message(Data),
            case string:lowercase(Command) of
                "whoami" ->
                    gen_tcp:send(Socket, list_to_binary("You are: " ++ Username ++ "\n"));

                "say" ->
                    case RoomName of
                        "" ->
                            SocketList = storage:get_all_sockets(DictPid, self()),
                            Label = "Global";
                        _ ->
                            io:format("Sending message to room: ~p~n", [RoomName]),
                            SocketList = storage:get_all_sockets(DictPid, self(), RoomName),
                            Label = RoomName
                    end,
            
                    lists:foreach(fun(S) -> 
                        case S == Socket of
                            true -> ok;
                            false -> 
                                gen_tcp:send(S, list_to_binary("[" ++ Label ++ "] " ++ Username ++ ": " ++ Payload ++ "\n"))
                        end
                    end, SocketList);

                "create" ->
                    io:format("Creating room with payload: ~p~n", [Payload]),
                    case string:trim(Payload) of
                        "" ->
                            gen_tcp:send(Socket, list_to_binary("Please specify a room name to create.\n"));
                        RoomToCreate ->
                            case storage:create_room(DictPid, RoomToCreate, self(), Socket) of
                                ok ->
                                    gen_tcp:send(Socket, list_to_binary("Room created: " ++ RoomToCreate ++ "\n")),
                                    NewState = State#handler_state{roomname = list_to_binary(RoomToCreate)},
                                    loop(NewState);
                                already_exists ->
                                    gen_tcp:send(Socket, list_to_binary("Room already exists: " ++ RoomToCreate ++ "\n")),
                                    loop(State)
                            end
                    end;


                "join" ->
                    case string:trim(Payload) of
                        "" ->
                            gen_tcp:send(Socket, list_to_binary("Please specify a room name to join.\n"));
                        SelectedRoom ->
                            io:format("Joining room with payload: ~p~n", [Payload]),
                            case storage:join_room(DictPid, SelectedRoom, self(), Socket) of
                                ok ->
                                    io:format("~p joined room: ~p~n", [Username, SelectedRoom]),
                                    gen_tcp:send(Socket, list_to_binary("You have joined room: " ++ SelectedRoom ++ "\n")),
                                    NewState = State#handler_state{
                                        socket = Socket,
                                        roomname = SelectedRoom,
                                        username = Username,
                                        dictpid = DictPid
                                    },
                                    loop(NewState);
                                room_not_found ->
                                    gen_tcp:send(Socket, list_to_binary("Room not found: " ++ SelectedRoom ++ "\n")),
                                    loop(State)
                            end
                    end;

                "leave" ->
                    case RoomName of
                        <<"">> ->
                            gen_tcp:send(Socket, list_to_binary("You are not in any room.\n"));
                        _ ->
                            case storage:leave_room(DictPid, RoomName, self(), Socket) of
                                ok ->
                                    gen_tcp:send(Socket, list_to_binary("You have left room: " ++ RoomName ++ "\n")),
                                    NewState = State#handler_state{
                                        socket = Socket,
                                        roomname = "",
                                        username = Username,
                                        dictpid = DictPid
                                    },
                                    loop(NewState);
                                room_not_found ->
                                    gen_tcp:send(Socket, list_to_binary("You are not in room: " ++ RoomName ++ "\n")),
                                    loop(State);
                                timeout ->
                                    gen_tcp:send(Socket, list_to_binary("Operation timed out.\n")),
                                    loop(State)
                            end
                    end;

                _ ->
                    gen_tcp:send(Socket, list_to_binary("Unknown command!\n"))
            end,
            loop(State);
        {error, closed} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            DictPid ! {remove_client, self(), Socket},
            ok;
        {error, enotconn} ->
            io:format("Socket ~p is not connected~n", [Socket]),
            DictPid ! {remove_client, self(), Socket},
            ok;

        {tcp_error, Socket, Reason} ->
            io:format("TCP error on socket ~p: ~p~n", [Socket, Reason]),
            ok
        
    end.

process_message(Data) ->
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
    {Command1, Content1}.