-module(mem).
-export([loop/2, get_username/3, add_client/3, get_all_sockets/2]).

loop(ClientDict, PidDict) ->
	receive
		{add_new_client, Socket, Username} ->
                  io:format("Adding ~p~n", [Username]),
                  TmpClients = dict:store(Username, Socket, ClientDict),
                  TmpPids = dict:store(Socket, Username, PidDict),
                  loop(TmpClients, TmpPids);
		
		{get_client_pid, ReceiverPid, Username} ->
                  {ok, Cpid} = dict:find(Username, ClientDict),
                  ReceiverPid ! {pid, Cpid},
                  loop(ClientDict, PidDict);
		
		{get_username, ReceiverPid, Pid} ->
                  {ok, Username} = dict:find(Pid, PidDict),
                  ReceiverPid ! {username, Username},
                  loop(ClientDict, PidDict);
		
		{get_all_pids, ReceiverPid} ->
                  ReceiverPid ! {all_pids, PidDict},
                  loop(ClientDict, PidDict);

            {remove_client, Socket} ->
                  {ok, Username} = dict:find(Socket, PidDict),
                  io:format("Removing ~p~n", [Username]),
                  TmpClients = dict:erase(Username, ClientDict),
                  TmpPids = dict:erase(Socket, PidDict),
                  loop(TmpClients, TmpPids);
		
		% error handling
		_ ->
                  {error, "That's not an acceptable action!"}
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

get_username(MemPid, ReceiverPid, ClientPid) ->
    MemPid ! {get_username, ReceiverPid, ClientPid},
    receive
        {username, Username} ->
            Username
    end.