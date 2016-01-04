-module(pushfight_app).

-behavior(e2_application).

-export([init/0]).

%%%===================================================================
%%% e2_application callbacks
%%%===================================================================

init() ->
    {ok, [hello]}.
