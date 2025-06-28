-module(storage_memory).
-export([loop/4]).

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
                TmpRooms = dict:store(RoomName, [], ClientDict),
                dict:store(RoomName, ClientSocket, RoomOwnerDict),
                ReceiverPid ! {room_created, RoomName},
                loop(ClientDict, PidDict, TmpRooms, RoomOwnerDict);
                
                % error handling
                _ ->
                {error, "That's not an acceptable action!"}
        end.
