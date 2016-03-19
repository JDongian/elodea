-module(elodea_app).
-behavior(e2_application).

-export([init/0]).

-define(DEFAULT_PLAYER_ETS_NAME, players).
-define(DEFAULT_PORT, 4242).


%% E2
init() ->
    {ok, [{lobby_data, start_link, [ets_name()]},
          {lobby_client_handler_sup, [supervisor]},
          {lobby_server, start_link, [server_port()]}
         ]}.


%% Hardcoded values
ets_name() ->
        app_config(player_ets_name, ?DEFAULT_PLAYER_ETS_NAME).

server_port() ->
        app_config(server_port, ?DEFAULT_PORT).


%% Configuration boilerplate
app_config(Name, Default) ->
        handle_app_env(application:get_env(Name), Default).

handle_app_env({ok, Value}, _Default) -> Value;
handle_app_env(undefined, Default) -> Default.
