-module(erlgraph_app).

-behaviour(application).

-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() ->
    application:ensure_all_started(erlgraph).

start(_Type, _Args) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, erlgraph, "index.html"}},
            {"/s/[...]", cowboy_static, {priv_dir, erlgraph, "static"}},
            {"/ws", erlgraph_ws, []}
        ]}
    ]),
    {ok, Port} = application:get_env(port),
    {ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    erlgraph_tracer:start(),
    erlgraph_sup:start_link().

stop(_State) ->
    ok.
