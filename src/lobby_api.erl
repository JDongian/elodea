-module(lobby_api).
-include_lib("stdlib/include/ms_transform.hrl").
-export([init/0,
         login_player/1,
         logout_player/1,
         get_player/1,
         get_all_players_online/0]).

-define(ID, ets:fun2ms(fun(X) -> X end)).

-record(player, {ref, handle, status}).
% rd(player, {ref, handle, status}).

init() ->
    Players = ets:new(players_online,
                      [set, {keypos,#player.ref}, named_table]),
    {ok, Players}.

login_player(Player) ->
    ets:insert(players_online, Player#player{status=idle}),
    {ok, Player}.

logout_player(Player=#player{ref=Ref}) ->
    ets:delete(players_online, Ref),
    {ok, Player}.

get_player(Ref) ->
    ets:lookup(players_online, Ref).

get_all_players_online() ->
    ets:select(players_online, ?ID).
