-module(erlchat_handler).
-export([loop/1]).

-include("records.hrl").

loop(State = #handler_state{socket = Socket, dictpid = DictPid, username = Username}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {Command, Payload} = process_message(Data),
            case string:lowercase(Command) of
                "whoami" ->
                    gen_tcp:send(Socket, list_to_binary("You are: " ++ Username ++ "\n"));

                "whereami" ->
                    RoomName = storage:get_current_room(DictPid, Socket),
                    gen_tcp:send(Socket, list_to_binary("You are in room: " ++ RoomName ++ "\n"));

                "say" ->
                    RoomName = storage:get_current_room(DictPid, Socket),
                    SocketList = storage:get_all_sockets_in_same_room(DictPid, Socket),
                    io:format("Sending message to room ~p with payload: ~p~n", [RoomName, Payload]),
                    io:format("SocketList: ~p~n", [SocketList]),
                    send_message(Socket, Username, SocketList, Payload, RoomName);

                "create" ->
                    io:format("Creating room with payload: ~p~n", [Payload]),
                    case string:trim(Payload) of
                        "" ->
                            gen_tcp:send(Socket, list_to_binary("Please specify a room name to create.\n"));
                        RoomToCreate ->
                            case storage:create_room(DictPid, RoomToCreate, Socket) of
                                ok ->
                                    gen_tcp:send(Socket, list_to_binary("Room created: " ++ RoomToCreate ++ "\n")),
                                    loop(State);
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
                            case storage:join_room(DictPid, SelectedRoom, Socket) of
                                ok ->
                                    io:format("~p joined room: ~p~n", [Username, SelectedRoom]),
                                    gen_tcp:send(Socket, list_to_binary("You have joined room: " ++ SelectedRoom ++ "\n")),
                                    loop(State);
                                room_not_found ->
                                    gen_tcp:send(Socket, list_to_binary("Room not found: " ++ SelectedRoom ++ "\n")),
                                    loop(State);
                                not_invited ->
                                    gen_tcp:send(Socket, list_to_binary("You are not invited to this room: " ++ SelectedRoom ++ "\n")),
                                    loop(State)
                            end
                    end;

                "leave" ->
                    RoomName = storage:get_current_room(DictPid, Socket),
                    case RoomName of
                        "" ->
                            gen_tcp:send(Socket, list_to_binary("You are not in any room.\n"));
                        _ ->
                            case storage:leave_room(DictPid, Socket) of
                                ok ->
                                    gen_tcp:send(Socket, list_to_binary("You have left room: " ++ RoomName ++ "\n")),
                                    loop(State);
                                room_not_found ->
                                    gen_tcp:send(Socket, list_to_binary("You are not in room: " ++ RoomName ++ "\n")),
                                    loop(State)
                            end
                    end;

                "destroy" ->
                    case string:trim(Payload) of
                        "" ->
                            gen_tcp:send(Socket, <<"Please specify a room name to destroy.\n">>);
                        RoomName ->
                            case storage:delete_room(DictPid, RoomName, Socket) of
                                {ok, RoomMembers} ->
                                    send_system_message(RoomMembers, "Room destroyed: " ++ RoomName ++ "\n"),
                                    loop(State);
                                not_owner ->
                                    gen_tcp:send(Socket, list_to_binary("Only the creator of the room can destroy it!\n")),
                                    loop(State);
                                room_not_found ->
                                    gen_tcp:send(Socket, list_to_binary("Room not found: " ++ RoomName ++ "\n")),
                                    loop(State)
                            end
                    end;

                "whisper" ->
                    case string:tokens(Payload, " ") of
                        [ToUser | MsgList] when MsgList =/= [] ->
                            Message = string:join(MsgList, " "),
                            io:format("~p whispers ~p: ~p", [Socket, ToUser, MsgList]),
                            case storage:get_client_pid(DictPid, ToUser) of
                                {ok, SendeeSocket} ->
                                    gen_tcp:send(SendeeSocket, list_to_binary("[whisper] " ++ Username ++ ": " ++ Message ++ "\n")),
                                    gen_tcp:send(Socket, list_to_binary("[whisper to " ++ ToUser ++ "] " ++ Message ++ "\n")),
                                    loop(State);
                                user_not_found ->
                                    gen_tcp:send(Socket, list_to_binary("User not found: " ++ ToUser ++ "\n")),
                                    loop(State)
                            end;
                        _ ->
                            gen_tcp:send(Socket, list_to_binary("Usage: whisper: <username> <message>\n")),
                            loop(State)
                    end;

                "private" ->
                    case string:trim(Payload) of
                        "" ->
                            gen_tcp:send(Socket, list_to_binary("Please specify a room name for the private room.\n"));
                        RoomName ->
                            case storage:create_private_room(DictPid, RoomName, Socket) of
                                ok ->
                                    gen_tcp:send(Socket, list_to_binary("Private room created: " ++ RoomName ++ "\n")),
                                    loop(State);
                                already_exists ->
                                    gen_tcp:send(Socket, list_to_binary("Private room already exists: " ++ RoomName ++ "\n")),
                                    loop(State)
                            end
                    end;

                "invite" ->
                    case storage:get_current_room(DictPid, Socket) of
                        "" ->
                            gen_tcp:send(Socket, list_to_binary("You are not in any room to invite users.\n")),
                            loop(State);
                        RoomName ->
                            io:format("Inviting user with payload: ~p~n", [Payload]),
                            case string:trim(Payload) of
                                "" ->
                                    gen_tcp:send(Socket, list_to_binary("Please specify a username to invite.\n"));
                                InviteeName ->
                                    case storage:invite(DictPid, Socket, InviteeName) of
                                        ok ->
                                            gen_tcp:send(Socket, list_to_binary("Invitation sent to " ++ InviteeName ++ "\n")),
                                            {ok, InviteeSocket} = storage:get_client_pid(DictPid, InviteeName),
                                            RoomName = storage:get_current_room(DictPid, Socket),
                                            gen_tcp:send(InviteeSocket, list_to_binary("You have been invited to join " ++ RoomName ++ " by " ++ Username ++ ".\n")),
                                            loop(State);
                                        not_owner ->
                                            gen_tcp:send(Socket, list_to_binary("Only the room owner can invite users!\n")),
                                            loop(State);
                                        user_not_found ->
                                            gen_tcp:send(Socket, list_to_binary("No user found.\n")),
                                            loop(State);
                                        public_room ->
                                            gen_tcp:send(Socket, list_to_binary("You cannot invite users to a public room.\n")),
                                            loop(State)
                                    end
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

send_message(SenderSocket, SenderUsername, Sendees, Message, RoomLabel) ->
    lists:foreach(fun(S) -> 
        case S == SenderSocket of
            true -> ok;
            false -> 
                gen_tcp:send(S, list_to_binary("[" ++ RoomLabel ++ "] " ++ SenderUsername ++ ": " ++ Message ++ "\n"))
        end
    end, Sendees).

send_system_message(Sendees, Message) ->
    lists:foreach(fun(S) -> 
        gen_tcp:send(S, list_to_binary("System: " ++ Message ++ "\n"))
    end, Sendees).