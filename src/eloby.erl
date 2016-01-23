-module(eloby).

-export([start/0, stop/0]).


%% Public API
start() ->
    e2_application:start_with_dependencies(eloby).

stop() ->
    application:stop(eloby).
