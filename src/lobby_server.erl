-module(lobby_server).
-behavior(e2_task).
-export([start_link/1, start_link/0]).
-export([init/1, handle_task/1]).

-define(TCP_OPTIONS, [binary, % data comes in as a binary
                      %inet, IPv4
                      {packet, 0},
                      {active, false},
                      {reuseaddr, true}]). % because Unix


start_link() ->
    start_link(4242).
start_link(Port) ->
    e2_task:start_link(?MODULE, Port).

init(Port) ->
    {ok, listen(Port)}.

handle_task(Socket) ->
    dispatch_client(wait_for_client(Socket)),
    {repeat, Socket}.

listen(Port) ->
    {ok, Socket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    Socket.

wait_for_client(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    Socket.

dispatch_client(Socket) ->
    % io:format("dispatching client ~p~n", [Client]).
    {ok, V} = lobby_client_handler:start_link(Socket),
    io:format("dispatching client ~p~n", [V]).
