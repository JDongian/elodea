-module(lobby_data).
-behavior(e2_service).

-export([start_link/1, login/1, logout/1]).
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


%% Handlers
handle_msg({login, User}, _From, Db) ->
    {reply, lobby_api:player_login(Db, User), Db};
handle_msg({logout, User}, _From, Db) ->
    {reply, lobby_api:player_logout(Db, User), Db}.


%% Helpers
new_ets(Name) ->
    {ok, Players} = lobby_api:create_players_ets(Name),
    Players.

