-module(storage).
-export([get_username/3, add_client/3, get_all_sockets/2, get_all_sockets/3,
        create_room/4, delete_room/2, list_rooms/2, join_room/4, leave_room/4]).

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
            Sockets;
        room_not_found ->
            room_not_found
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
        room_created ->
            ok;
        already_exists ->
            already_exists
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
        room_joined ->
            ok;
        room_not_found ->
            room_not_found
    end.

leave_room(MemPid, RoomName, ReceiverPid, ClientSocket) ->
    MemPid ! {leave_room, RoomName, ReceiverPid, ClientSocket},
    receive
        room_left ->
            ok;
        room_not_found ->
            room_not_found
    after
        10 -> timetout
    end.

