-module(storage_memory).
-export([loop/1, start_link/0]).


-record(state, {
    client_dict, % Username -> ClientSocket
    pid_dict, % ClientSocket -> Username
    room_dict, % RoomName -> Room objects
    client_to_room_dict % ClientSocket -> RoomName
}).

-record(room, {
    owner,        % Username (string)
    members,      % [Username]
    invitations,   % [Username]
    is_private      % boolean
}).

start_link() ->
        State = #state{
            client_dict = dict:new(),
            pid_dict = dict:new(),
            room_dict = dict:new(),
            client_to_room_dict = dict:new()
        },
        DictPid = spawn_link(fun() -> storage_memory:loop(State) end),
        DictPid.

loop(State) ->
    #state{
        client_dict = ClientDict,
        pid_dict = PidDict,
        room_dict = RoomDict,
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

            % Remove the client from the room if they are in one
            RoomName = get_room(Socket, ClientToRoomDict),
            case RoomName of
                "" -> 
                    NewState = State#state{
                        client_dict = TmpClients,
                        pid_dict = TmpPids
                    },
                    loop(NewState);
                _ ->
                    io:format("Removing ~p from room ~p~n", [Username, RoomName]),
                    TmpClientToRoomDict = dict:erase(Socket, ClientToRoomDict),
                    TmpRooms = dict:erase(RoomName, RoomDict),
                    NewState = State#state{
                        client_dict = TmpClients,
                        pid_dict = TmpPids,
                        room_dict = TmpRooms,
                        client_to_room_dict = TmpClientToRoomDict
                    },
                    loop(NewState)
            end;

        {create_room, ReceiverPid, RoomName, ClientSocket} ->
            case dict:is_key(RoomName, RoomDict) of 
                true -> ReceiverPid ! {already_exists}, loop(State);
                false ->
                    io:format("Creating room ~p~n", [RoomName]),
                    OwnerName = dict:fetch(ClientSocket, PidDict),
                    Room = #room{
                        owner = OwnerName,
                        members = [],
                        invitations = [],
                        is_private = false
                    },
                    TmpRooms = dict:store(RoomName, Room, RoomDict),
                    io:format("RoomDict: ~p~n", [dict:to_list(TmpRooms)]),
                    io:format("ClientDict: ~p~n", [dict:to_list(ClientDict)]),
                    ReceiverPid ! room_created,
                    NewState = State#state{
                        room_dict = TmpRooms
                    },
                    loop(NewState)
            end;

        {get_all_pids_in_room, ReceiverPid, ClientSocket} ->
            RoomName = get_room(ClientSocket, ClientToRoomDict),
            io:format("Getting all pids in room ~p~n", [RoomName]),
            case RoomName of
                "" ->
                    AllSockets = dict:fetch_keys(PidDict),
                    ReceiverPid ! {all_pids_in_room, AllSockets};
                _ ->
                    Room = dict:fetch(RoomName, RoomDict),
                    ReceiverPid ! {all_pids_in_room, Room#room.members}
            end,
            loop(State);

        {join_room, ReceiverPid, RoomName, ClientSocket} ->
            case dict:find(RoomName, RoomDict) of
                {ok, Room} ->
                    case Room#room.is_private of
                        true ->
                            % Check if the client is invited
                            case lists:member(dict:fetch(ClientSocket, PidDict), Room#room.invitations) of
                                true ->
                                    io:format("Joining private room ~p~n", [RoomName]),
                                    NewSockets = [ClientSocket | Room#room.members],
                                    NewRoom = Room#room{members = NewSockets},
                                    TmpRooms = dict:store(RoomName, NewRoom, RoomDict),
                                    NewClientToRoomDict = dict:store(ClientSocket, RoomName, ClientToRoomDict),
                                    ReceiverPid ! room_joined,
                                    NewState = State#state{
                                        room_dict = TmpRooms,
                                        client_to_room_dict = NewClientToRoomDict
                                    },
                                    loop(NewState);
                                false ->
                                    ReceiverPid ! not_invited,
                                    loop(State)
                            end;
                        false -> 
                            io:format("Joining public room ~p~n", [RoomName]),
                            NewSockets = [ClientSocket | Room#room.members],
                            NewRoom = Room#room{members = NewSockets},
                            TmpRooms = dict:store(RoomName, NewRoom, RoomDict),
                            NewClientToRoomDict = dict:store(ClientSocket, RoomName, ClientToRoomDict),
                            ReceiverPid ! room_joined,
                            NewState = State#state{
                                room_dict = TmpRooms,
                                client_to_room_dict = NewClientToRoomDict
                            },
                            loop(NewState)
                    end;
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
                    Room = dict:fetch(RoomName, RoomDict),
                    NewSockets = lists:delete(ClientSocket, Room#room.members),
                    NewRoom = Room#room{members = NewSockets},
                    TmpRooms = dict:store(RoomName, NewRoom, RoomDict),
                    NewState = State#state{
                        client_to_room_dict = NewClientToRoomDict,
                        room_dict = TmpRooms
                    },
                    ReceiverPid ! room_left,
                    loop(NewState)
            end;

        {delete_room, ReceiverPid, RoomName, ClientSocket} ->
            ClientName = dict:fetch(ClientSocket, PidDict),
            case dict:find(RoomName, RoomDict) of
                {ok, Room} ->
                    case Room#room.owner == ClientName of
                        true ->
                            % Remove the room from room_dict and room_owner_dict
                            NewRoomDict = dict:erase(RoomName, RoomDict),
                            % Remove all clients in this room from client_to_room_dict
                            NewClientToRoomDict = lists:foldl(
                                fun(Socket, AccDict) -> dict:erase(Socket, AccDict) end,
                                ClientToRoomDict,
                                Room#room.members
                            ),
                            NewState = State#state{
                                room_dict = NewRoomDict,
                                client_to_room_dict = NewClientToRoomDict
                            },
                            ReceiverPid ! {ok, Room#room.members},
                            loop(NewState);
                        false ->
                            io:format("~p tried to delete ~p, while ~p is the owner.~n", [ClientSocket, RoomName, Room#room.owner]),
                            ReceiverPid ! not_owner,
                            loop(State)
                    end;
                error ->
                    ReceiverPid ! room_not_found,
                    loop(State)
            end;

        {create_private_room, ReceiverPid, RoomName, CreatorSocket} ->
            case dict:is_key(RoomName, RoomDict) of
                true ->
                    ReceiverPid ! already_exists,
                    loop(State);
                false ->
                    io:format("Creating private room ~p~n", [RoomName]),
                    OwnerName = dict:fetch(CreatorSocket, PidDict),
                    PrivateRoom = #room{
                        owner = OwnerName,
                        is_private = true,
                        members = [],
                        invitations = [OwnerName]
                    },
                    NewRoomDict = dict:store(RoomName, PrivateRoom, RoomDict),
                    NewState = State#state{room_dict = NewRoomDict},
                    ReceiverPid ! ok,
                    loop(NewState)
            end;

        {invite, ReceiverPid, InviterSocket, InviteeName} ->
            case dict:find(InviteeName, ClientDict) of
                {ok, _} ->
                    RoomName = get_room(InviterSocket, ClientToRoomDict),
                    Room = dict:fetch(RoomName, RoomDict),
                    InviterName = dict:fetch(InviterSocket, PidDict),
                    
                    if 
                        Room#room.is_private andalso
                        InviterName == Room#room.owner ->
                            NewInvitations = [InviteeName | Room#room.invitations],
                            io:format("Total invitations: ~p~n", [NewInvitations]),
                            UpdatedRoom = Room#room{invitations = NewInvitations},
                            NewRoomDict = dict:store(RoomName, UpdatedRoom, RoomDict),
                            NewState = State#state{room_dict = NewRoomDict},
                            ReceiverPid ! ok,
                            loop(NewState);

                        not Room#room.is_private ->
                            ReceiverPid ! public_room,
                            loop(State);

                        Room#room.is_private andalso
                        InviterName /= Room#room.owner ->
                            ReceiverPid ! not_owner,
                            loop(State)
                    end;
                error ->
                    ReceiverPid ! user_not_found,
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