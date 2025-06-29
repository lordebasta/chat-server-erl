-module(storage_memory).
-export([loop/4, start_link/0]).

start_link() ->
        ClientDict = dict:new(),
        PidDict = dict:new(),
        RoomDict = dict:new(),
        RoomOwnerDict = dict:new(),
        DictPid = spawn_link(fun() -> storage_memory:loop(ClientDict, PidDict, RoomDict, RoomOwnerDict) end),
        DictPid.

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
                        case dict:is_key(RoomName, RoomDict) of 
                                true -> ReceiverPid ! {already_exists};
                                false ->
                                        io:format("Creating room ~p~n", [RoomName]),
                                        TmpRooms = dict:store(RoomName, [], RoomDict),
                                        NewRoomOwnerDict = dict:store(RoomName, ClientSocket, RoomOwnerDict),
                                        io:format("RoomDict: ~p~n", [dict:to_list(TmpRooms)]),
                                        io:format("ClientDict: ~p~n", [dict:to_list(ClientDict)]),
                                        io:format("RoomOwnerDict: ~p~n", [dict:to_list(NewRoomOwnerDict)]),
                                        ReceiverPid ! room_created,
                                        loop(ClientDict, PidDict, TmpRooms, NewRoomOwnerDict)
                        end;

                {get_all_pids_in_room, ReceiverPid, Room} ->
                        io:format("Getting all pids in room ~p~n", [Room]),
                        io:format("RoomDict: ~p~n", [dict:to_list(RoomDict)]),
                        case dict:find(Room, RoomDict) of
                                {ok, Sockets} ->
                                        ReceiverPid ! {all_pids_in_room, Sockets};
                                error ->
                                        ReceiverPid ! room_not_found
                        end,
                        loop(ClientDict, PidDict, RoomDict, RoomOwnerDict);

                {join_room, RoomName, ReceiverPid, ClientSocket} ->
                        case dict:find(RoomName, RoomDict) of
                                {ok, CurrentSockets} ->
                                        io:format("Joining room ~p~n", [RoomName]),
                                        NewSockets = [ClientSocket | CurrentSockets],
                                        TmpRooms = dict:store(RoomName, NewSockets, RoomDict),
                                        ReceiverPid ! room_joined,
                                        loop(ClientDict, PidDict, TmpRooms, RoomOwnerDict);
                                error ->
                                        ReceiverPid ! room_not_found,
                                        loop(ClientDict, PidDict, RoomDict, RoomOwnerDict)
                        end;

                {leave_room, RoomName, ReceiverPid, ClientSocket} ->
                        case dict:find(RoomName, RoomDict) of
                                {ok, CurrentSockets} ->
                                        NewSockets = lists:delete(ClientSocket, CurrentSockets),
                                        TmpRooms = dict:store(RoomName, NewSockets, RoomDict),
                                        ReceiverPid ! room_left,
                                        loop(ClientDict, PidDict, TmpRooms, RoomOwnerDict);
                                error ->
                                        ReceiverPid ! room_not_found,
                                        loop(ClientDict, PidDict, RoomDict, RoomOwnerDict)
                        end;
                
                % error handling
                _ ->
                {error, "That's not an acceptable action!"}
        end.
