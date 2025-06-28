-module(storage).
-export([loop/4, get_username/3, add_client/3, get_all_sockets/2, get_all_sockets/3,
        create_room/4, delete_room/2, list_rooms/2, join_room/4, leave_room/4]).

loop(ClientDict, PidDict, RoomDict, RoomOwnerDict) ->
    receive
        {add_new_client, Socket, Username} ->
            io:format("Adding ~p~n", [Username]),
            TmpClients = dict:store(Username, Socket, ClientDict),
            TmpPids = dict:store(Socket, Username, PidDict),
            loop(TmpClients, TmpPids, RoomDict, RoomOwnerDict);

        {get_client_pid, ReceiverPid, Username} ->
            {ok, Cpid} = dict:find(Username, ClientDict),
            ReceiverPid ! {pid, Cpid},
            loop(ClientDict, PidDict, RoomDict, RoomOwnerDict);

        {get_username, ReceiverPid, Pid} ->
            {ok, Username} = dict:find(Pid, PidDict),
            ReceiverPid ! {username, Username},
            loop(ClientDict, PidDict, RoomDict, RoomOwnerDict);

        {get_all_pids, ReceiverPid} ->
            ReceiverPid ! {all_pids, PidDict},
            loop(ClientDict, PidDict, RoomDict, RoomOwnerDict);

        {remove_client, Socket} ->
            {ok, Username} = dict:find(Socket, PidDict),
            io:format("Removing ~p~n", [Username]),
            TmpClients = dict:erase(Username, ClientDict),
            TmpPids = dict:erase(Socket, PidDict),
            loop(TmpClients, TmpPids, RoomDict, RoomOwnerDict);

        {create_room, RoomName, ReceiverPid, ClientSocket} ->
            io:format("Creating room ~p~n", [RoomName]),
            TmpRooms = dict:store(RoomName, [], RoomDict),
            TmpRoomOwners = dict:store(RoomName, ClientSocket, RoomOwnerDict),
            ReceiverPid ! {room_created, RoomName},
            loop(ClientDict, PidDict,
                  TmpRooms,
                  TmpRoomOwners);

        _ ->
            {error,
             "That's not an acceptable action!"}
    end.

add_client(MemPid, Socket, Username) ->
    MemPid ! {add_new_client, Socket, Username},
    ok.

get_all_sockets(MemPid, ReceiverPid) ->
    MemPid ! {get_all_pids, ReceiverPid},
    receive
        {all_pids, PidDict} ->
            dict:fetch_keys(PidDict)
    end.

get_all_sockets(MemPid, ReceiverPid, RoomName) ->
    MemPid ! {get_all_pids_in_room, ReceiverPid, RoomName},
    receive
        {all_pids_in_room, Sockets} ->
            Sockets
    end.

get_username(MemPid, ReceiverPid, ClientPid) ->
    MemPid ! {get_username, ReceiverPid, ClientPid},
    receive
        {username, Username} ->
            Username
    end.

create_room(MemPid, RoomName, ReceiverPid, ClientSocket) ->
    MemPid ! {create_room, RoomName, ReceiverPid, ClientSocket},
    receive
        {room_created, RoomName} ->
            ok
    end.

delete_room(MemPid, RoomName) ->
    MemPid ! {delete_room, RoomName},
    ok.

list_rooms(MemPid, ReceiverPid) ->
    MemPid ! {list_rooms, ReceiverPid},
    receive
        {rooms, Rooms} ->
            Rooms
    end.

join_room(MemPid, RoomName, ReceiverPid, ClientSocket) ->
    MemPid ! {join_room, RoomName, ReceiverPid, ClientSocket},
    receive
        {room_joined, RoomName} ->
            ok
    end.

leave_room(MemPid, RoomName, ReceiverPid, ClientSocket) ->
    MemPid ! {leave_room, RoomName, ReceiverPid, ClientSocket},
    ok.

