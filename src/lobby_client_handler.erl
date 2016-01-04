-module(lobby_client_handler).
-behavior(e2_task).

-export([start_link/1]).
-export([handle_task/1, terminate/2]).


%% Exports
start_link(Socket) ->
    e2_task:start_link(?MODULE, Socket).

handle_task(Socket) ->
    handle_command_line(read_bytes(Socket), Socket).

terminate(_Reason, Socket) ->
    gen_tcp:close(Socket).


%% Handlers
handle_command({"LOGIN", Bar}, Socket) ->
    handle_reply(try_login(Bar), Socket);
handle_command({"LOGOUT", Bar}, Socket) ->
    handle_reply(try_logout(Bar), Socket);
handle_command(_, Socket) ->
    handle_reply(error, Socket).

send_reply({ok, {login, _Name}}, Socket) ->
    gen_tcp:send(Socket, "{'status': \"ok\"}\r\n");
send_reply({ok, {logout, _Name}}, Socket) ->
    gen_tcp:send(Socket, "{'status': \"ok\"}\r\n");
send_reply(error, Socket) ->
    gen_tcp:send(Socket, "{'status': \"error\"}\r\n").


%% Actions
try_login(Name) ->
    {ok, {login, Name}}.

try_logout(Name) ->
    {ok, {logout, Name}}.


%% Helpers
handle_command_line({ok, Data}, Socket) ->
    io:format("### Got ~p from client~n", [Data]),
    handle_command(parse_command(Data), Socket);
handle_command_line({error, _Err}, _) ->
    {stop, normal}.

handle_reply(Reply, Socket) ->
    send_reply(Reply, Socket),
    {repeat, Socket}.

read_bytes(Socket) ->
    inet:setopts(Socket, [binary, {active, false}, {packet, line}]),
    gen_tcp:recv(Socket, 0).

parse_command(Data) ->
    Map = parse_json(Data),
    io:format("### Parsed JSON: ~w~n", [Map]),
    Map.

parse_json(Data) ->
    jsx:decode(Data, [return_maps]).
