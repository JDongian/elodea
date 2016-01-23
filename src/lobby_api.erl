-module(lobby_api).
-include_lib("stdlib/include/ms_transform.hrl").
-include("player.hrl").
-define(ID, ets:fun2ms(fun(X) -> X end)).

-export([create_players_ets/1,
         player_login/2,
         player_logout/2,
         get_player/2,
         get_all_players_online/1]).


create_players_ets(Name) ->
    io:format("ETS create with name: ~w.~n", [Name]),
    Players = ets:new(Name,
                      [set, {keypos,#player.handle}, named_table]),
    {ok, Players}.

player_login(Db, Player) ->
    Success = ets:insert_new(Db, Player#player{status=idle}),
    % io:format("Adding player: ~w.~n", [Player]),
    {ok, {login_success, Success, Player}}.

player_logout(Db, Player=#player{handle=Username}) ->
    Success = ets:delete(Db, Username),
    {ok, {logout_success, Success, Player}}.

get_player(Db, Ref) ->
    ets:lookup(Db, Ref).

% There has to be a better way to SELECT * ...
get_all_players_online(Db) ->
    ets:select(Db, ?ID).
