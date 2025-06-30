-module(storage_memory).
-export([loop/1, start_link/0]).


-record(state, {
    client_dict, % Username -> ClientSocket
    pid_dict, % ClientSocket -> Username
    room_dict, % RoomName -> ClientSockets
    room_owner_dict, % RoomName -> ClientSocket
    client_to_room_dict % ClientSocket -> RoomName
}).

start_link() ->
        State = #state{
            client_dict = dict:new(),
            pid_dict = dict:new(),
            room_dict = dict:new(),
            room_owner_dict = dict:new(),
            client_to_room_dict = dict:new()
        },
        DictPid = spawn_link(fun() -> storage_memory:loop(State) end),
        DictPid.

loop(State) ->
    #state{
        client_dict = ClientDict,
        pid_dict = PidDict,
        room_dict = RoomDict,
        room_owner_dict = RoomOwnerDict,
        client_to_room_dict = ClientToRoomDict
    } = State,
    receive
        {add_new_client, Socket, Username} ->
            io:format("Adding ~p~n", [Username]),
            TmpClients = dict:store(Username, Socket, ClientDict),
            TmpPids = dict:store(Socket, Username, PidDict),
            NewState = State#state{
                client_dict = TmpClients,
                pid_dict = TmpPids
            },
            loop(NewState);

        {get_client_pid, ReceiverPid, Username} ->
            case dict:find(Username, ClientDict) of 
                {ok, Cpid} ->
                    ReceiverPid ! {ok, Cpid};
                error ->
                    ReceiverPid ! user_not_found
            end,
            loop(State);

        {get_username, ReceiverPid, Pid} ->
            {ok, Username} = dict:find(Pid, PidDict),
            ReceiverPid ! {username, Username},
            loop(State);

        {get_current_room, ReceiverPid, ClientSocket} ->
            RoomName = get_room(ClientSocket, ClientToRoomDict),
            ReceiverPid ! {room, RoomName},
            loop(State);

        {remove_client, Socket} ->
            {ok, Username} = dict:find(Socket, PidDict),
            io:format("Removing ~p~n", [Username]),
            TmpClients = dict:erase(Username, ClientDict),
            TmpPids = dict:erase(Socket, PidDict),
            NewState = State#state{
                client_dict = TmpClients,
                pid_dict = TmpPids
            },
            loop(NewState);

        {create_room, ReceiverPid, RoomName, ClientSocket} ->
            case dict:is_key(RoomName, RoomDict) of 
                true -> ReceiverPid ! {already_exists}, loop(State);
                false ->
                    io:format("Creating room ~p~n", [RoomName]),
                    TmpRooms = dict:store(RoomName, [], RoomDict),
                    NewRoomOwnerDict = dict:store(RoomName, ClientSocket, RoomOwnerDict),
                    io:format("RoomDict: ~p~n", [dict:to_list(TmpRooms)]),
                    io:format("ClientDict: ~p~n", [dict:to_list(ClientDict)]),
                    io:format("RoomOwnerDict: ~p~n", [dict:to_list(NewRoomOwnerDict)]),
                    ReceiverPid ! room_created,
                    NewState = State#state{
                        room_dict = TmpRooms,
                        room_owner_dict = NewRoomOwnerDict
                    },
                    loop(NewState)
            end;

        {get_all_pids_in_room, ReceiverPid, ClientSocket} ->
            Room = get_room(ClientSocket, ClientToRoomDict),
            io:format("Getting all pids in room ~p~n", [Room]),
            io:format("RoomDict: ~p~n", [dict:to_list(RoomDict)]),
            case Room of
                "" ->
                    AllSockets = dict:fetch_keys(PidDict),
                    ReceiverPid ! {all_pids_in_room, AllSockets};
                _ ->
                    case dict:find(Room, RoomDict) of
                        {ok, Sockets} ->
                            ReceiverPid ! {all_pids_in_room, Sockets};
                        error ->
                            ReceiverPid ! room_not_found
                    end
            end,
            loop(State);

        {join_room, ReceiverPid, RoomName, ClientSocket} ->
            case dict:find(RoomName, RoomDict) of
                {ok, CurrentSockets} ->
                    io:format("Joining room ~p~n", [RoomName]),
                    NewSockets = [ClientSocket | CurrentSockets],
                    TmpRooms = dict:store(RoomName, NewSockets, RoomDict),
                    NewClientToRoomDict = dict:store(ClientSocket, RoomName, ClientToRoomDict),
                    ReceiverPid ! room_joined,
                    NewState = State#state{
                        room_dict = TmpRooms,
                        client_to_room_dict = NewClientToRoomDict
                    },
                    loop(NewState);
                error ->
                    ReceiverPid ! room_not_found,
                    loop(State)
            end;

        {leave_room, ReceiverPid, ClientSocket} ->
            RoomName = get_room(ClientSocket, ClientToRoomDict),
            case RoomName of
                "" ->
                    ReceiverPid ! not_in_room,
                    loop(State);
                RoomName ->
                    NewClientToRoomDict = dict:erase(ClientSocket, ClientToRoomDict),
                    NewSockets = lists:delete(ClientSocket, dict:fetch(RoomName, RoomDict)),
                    TmpRooms = dict:store(RoomName, NewSockets, RoomDict),
                    NewState = State#state{
                        client_to_room_dict = NewClientToRoomDict,
                        room_dict = TmpRooms
                    },
                    ReceiverPid ! room_left,
                    loop(NewState)
            end;

        {delete_room, ReceiverPid, RoomName, ClientSocket} ->
            case dict:find(RoomName, RoomDict) of
                {ok, Sockets} ->
                    case dict:find(RoomName, RoomOwnerDict) of
                        {ok, OwnerSocket} when OwnerSocket == ClientSocket ->
                            % Remove the room from room_dict and room_owner_dict
                            NewRoomDict = dict:erase(RoomName, RoomDict),
                            NewRoomOwnerDict = dict:erase(RoomName, RoomOwnerDict),
                            % Remove all clients in this room from client_to_room_dict
                            NewClientToRoomDict = lists:foldl(
                                fun(Socket, AccDict) -> dict:erase(Socket, AccDict) end,
                                ClientToRoomDict,
                                Sockets
                            ),
                            NewState = State#state{
                                room_dict = NewRoomDict,
                                room_owner_dict = NewRoomOwnerDict,
                                client_to_room_dict = NewClientToRoomDict
                            },
                            ReceiverPid ! {ok, Sockets},
                            loop(NewState);
                        {ok, OwnerSocket} ->
                            io:format("~p tried to delete ~p, while ~p is the owner", [ClientSocket, RoomName, OwnerSocket]),
                            ReceiverPid ! not_owner,
                            loop(State);
                        error ->
                            ReceiverPid ! room_not_found,
                            loop(State)
                    end;
                error ->
                    ReceiverPid ! room_not_found,
                    loop(State)
            end;

        % error handling
        _ ->
            {error, "That's not an acceptable action!"}
    end.


get_room(Client, ClientToRoomDict) ->
    case dict:find(Client, ClientToRoomDict) of
        {ok, RoomName} -> RoomName;
        error -> ""
    end.