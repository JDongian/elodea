-module(lobby_client_handler).
-behavior(e2_task).
-include("player.hrl").

-export([start_link/1]).
-export([handle_task/1, terminate/2]).


%% E2
start_link(Socket) ->
    e2_task:start_link(?MODULE, Socket).

handle_task(Socket) ->
    handle_command_line(read_bytes(Socket), Socket).

terminate(_Reason, Socket) ->
    gen_tcp:close(Socket).


%% Handlers
handle_command({<<"LOGIN">>, Username}, Socket) ->
    handle_reply(try_login(Username), Socket);
handle_command({<<"LOGOUT">>, Bar}, Socket) ->
    handle_reply(try_logout(Bar), Socket);
% perhaps handle_reply -> send_reply, disconnecting user on bad input
handle_command(bad_format, Socket) ->
    handle_reply(bad_format, Socket);
handle_command(_, Socket) ->
    handle_reply(error, Socket).

send_reply({ok, {login, #player{ref=Ref,handle=Username}}}, Socket) ->
    RefString = list_to_binary(erlang:ref_to_list(Ref)),
    gen_tcp:send(Socket,
                 json_encode_reply([{status, ok},
                                    {login, [{username, Username},
                                             {ref, RefString}]}]));
send_reply({ok, {logout, _Name}}, Socket) ->
    gen_tcp:send(Socket,
                 json_encode_reply([{status, ok}]));
send_reply(bad_format, Socket) ->
    gen_tcp:send(Socket,
                 json_encode_reply([{status, error},
                                    {error, <<"could not parse command">>}]));
send_reply(error, Socket) ->
    gen_tcp:send(Socket,
                 json_encode_reply([{status, error},
                                    {error, <<"unknown command">>}])).


%% Actions
try_login(Username) ->
    lobby_data:login(Username).

try_logout(Username) ->
    lobby_data:logout(Username).


%% Helpers
handle_command_line({ok, Data}, Socket) ->
    %% io:format("### Got ~p from client~n", [Data]),
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
    case jsx:is_json(Data) of
        true ->
            Command = json_decode(Data);
        false ->
            Command = bad_format
    end,
    Command.

json_decode(Data) ->
    [Result] = jsx:decode(Data),
    Result.

json_encode_reply(Reply) ->
    jsx:encode(Reply).
