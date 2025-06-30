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

    spawn(fun() -> erlchat_handler:init(Socket, DictPid) end),
    accept(LSocket, DictPid).
