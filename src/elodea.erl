-module(elodea).

-export([start/0, stop/0]).


%% Public API
start() ->
    e2_application:start_with_dependencies(elodea).

stop() ->
    application:stop(elodea).
