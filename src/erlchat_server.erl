-module(erlchat_server).
-export([listen/1]).

-define(TCP_OPTIONS, [binary, {packet, 0}, {active, false}, {reuseaddr, true}]).

-include("records.hrl").
% Call echo:listen(Port) to start the service.
listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    io:format("Started tcp server on port ~p~n", [Port]),

    DictPid = storage_memory:start_link(),
    accept(LSocket, DictPid).

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket, DictPid) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    gen_tcp:send(Socket, <<"Welcome! Insert your username: ">>),
    case gen_tcp:recv(Socket, 0) of
        {ok, Username} ->
            UsernameStr = binary_to_list(Username),
            TrimmedUsername = string:trim(UsernameStr),
            io:format("New client connected: ~p~n", [TrimmedUsername]),
            % Start the dict handler process to manage clients
            storage:add_client(DictPid, Socket, TrimmedUsername),
            State = #handler_state{
                socket = Socket,
                username = TrimmedUsername,
                dictpid = DictPid,
                roomname = ""  % Default room name is empty
            },
            spawn(fun() -> erlchat_handler:loop(State) end)
    end,
    accept(LSocket, DictPid).
