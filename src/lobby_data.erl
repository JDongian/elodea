-module(lobby_data).
-behavior(e2_service).

-export([start_link/1, login/1, logout/1, get_all_players_online/0]).
-export([init/1, handle_msg/3]).

-include("player.hrl").


%% E2
start_link(Name) ->
    e2_service:start_link(?MODULE, Name, [registered]).


%% Public API
init(Name) ->
    {ok, new_ets(Name)}.

login(User) ->
    e2_service:call(?MODULE, {login, #player{ref=make_ref(),handle=User,status=null}}).

logout(User) ->
    e2_service:call(?MODULE, {logout, #player{ref=null,handle=User,status=null}}).

get_all_players_online() ->
    e2_service:call(?MODULE, {get_all}).


%% Handlers
handle_msg({login, User}, _From, Ets) ->
    {reply, lobby_api:player_login(Ets, User), Ets};
handle_msg({logout, User}, _From, Ets) ->
    {reply, lobby_api:player_logout(Ets, User), Ets};
handle_msg({get_all}, _From, Ets) ->
    {reply, lobby_api:get_all_players_online(Ets), Ets}.


%% Helpers
new_ets(Name) ->
    {ok, Players} = lobby_api:create_players_ets(Name),
    Players.

