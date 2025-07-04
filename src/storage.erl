-module(storage).
-export([get_username/2, add_client/3, get_client_pid/2, get_current_room/2, get_all_sockets_in_same_room/2,
        create_room/3, delete_room/3, list_rooms/1, join_room/3, leave_room/2, create_private_room/3,
        invite/3]).

add_client(MemPid, Socket, Username) ->
    MemPid ! {add_new_client, Socket, Username},
    ok.

get_client_pid(MemPid, Username) ->
    MemPid ! {get_client_pid, self(), Username},
    receive
        {ok, ClientPid} ->
            {ok, ClientPid};
        user_not_found ->
            user_not_found
    end.

get_current_room(MemPid, ClientSocket) ->
    MemPid ! {get_current_room, self(), ClientSocket},
    receive
        {room, RoomName} ->
            case RoomName of
                "" -> "Global";  % Default room name is "Global"
                _ -> RoomName
            end
    end.

get_all_sockets_in_same_room(MemPid, Socket) ->
    MemPid ! {get_all_pids_in_room, self(), Socket},
    receive
        {all_pids_in_room, Sockets} ->
            Sockets;
        room_not_found ->
            room_not_found
    end.

get_username(MemPid, ClientPid) ->
    MemPid ! {get_username, self(), ClientPid},
    receive
        {username, Username} ->
            Username
    end.

create_room(MemPid, RoomName, ClientSocket) ->
    MemPid ! {create_room, self(), RoomName, ClientSocket},
    receive
        room_created ->
            ok;
        already_exists ->
            already_exists
    end.

delete_room(MemPid, RoomName, ClientSocket) ->
    MemPid ! {delete_room, self(), RoomName, ClientSocket},
    receive
        {ok, RoomMembers} ->
            {ok, RoomMembers};
        not_owner -> not_owner;
        room_not_found ->
            room_not_found
    end.

list_rooms(MemPid) ->
    MemPid ! {list_rooms, self()},
    receive
        {rooms, Rooms} ->
            Rooms
    end.

join_room(MemPid, RoomName, ClientSocket) ->
    MemPid ! {join_room, self(), RoomName, ClientSocket},
    receive
        room_joined ->
            ok;
        not_invited ->
            not_invited;
        room_not_found ->
            room_not_found
    end.

leave_room(MemPid, ClientSocket) ->
    MemPid ! {leave_room, self(), ClientSocket},
    receive
        room_left ->
            ok;
        not_in_room ->
            not_in_room
    end.

create_private_room(MemPid, RoomName, Owner) ->
    MemPid ! {create_private_room, self(), RoomName, Owner},
    receive
        ok ->
            ok;
        already_exists ->
            already_exists
    end.

invite(MemPid, InviterSocket, InviteeName) ->
    MemPid ! {invite, self(), InviterSocket, InviteeName},
    receive
        ok ->
            ok;
        not_owner ->
            not_owner;
        public_room ->
            public_room;
        user_not_found ->
            user_not_found
    end.
