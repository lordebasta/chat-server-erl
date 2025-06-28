-module(erlchat_handler).
-export([loop/1]).

-include("records.hrl").

loop(State = #handler_state{socket = Socket, dictpid = DictPid, username = Username}) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            {Command, Payload} = process_message(Data),
            case string:lowercase(Command) of
                "whoami" ->
                    gen_tcp:send(Socket, list_to_binary("You are: " ++ Username ++ "\n"));
                "say" ->
                    SocketList = storage:get_all_sockets(DictPid, self()),
                    io:format("Writing to: ~p~n", [SocketList]),
                    lists:foreach(fun(S) -> 
                        case S == Socket of
                            true -> ok;
                            false -> 
                                gen_tcp:send(S, list_to_binary(Username ++ ": " ++ Payload ++ "\n"))
                        end
                    end, SocketList);
                _ ->
                    gen_tcp:send(Socket, list_to_binary("Unknown command!\n"))
            end,
            loop(State);
        {error, closed} ->
            io:format("Client disconnected: ~p~n", [Socket]),
            DictPid ! {remove_client, self(), Socket},
            ok;
        {tcp_error, Socket, Reason} ->
            io:format("TCP error on socket ~p: ~p~n", [Socket, Reason]),
            ok
    end.

process_message(Data) ->
    io:format("Data: ~p~n", [binary_to_list(Data)]),
    Message = binary_to_list(Data),
    case lists:member(hd(":"), Message) of
        true ->
            {Command, [_|Content]} = lists:splitwith(fun(T) -> [T] =/= ":" end, Message);
        false ->
            Command = Message,
            Content = ""
    end,    
    Command1 = string:trim(Command),
    Content1 = string:trim(Content),
    io:format("Command: ~p~n", [Command1]),
    io:format("Content: ~p~n", [Content1]),
    {Command1, Content1}.