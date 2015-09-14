-module(erlgraph_ws).

-behaviour(cowboy_websocket_handler).

-export([init/3, send/1]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

%% API
send(Msg) ->
    gproc:send({p, l, ?MODULE}, Msg).

%% cowboy callbacks
init({tcp, http}, _Req, _Opts) ->
    self() ! post_init,
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	{ok, Req, undefined_state}.

websocket_handle(_Data, Req, State) ->
	{ok, Req, State}.

websocket_info(post_init, Req, State) ->
    true = gproc:reg({p, l, ?MODULE}),
    erlgraph_tracer:init(),
	{ok, Req, State};
websocket_info(Json, Req, State) ->
	{reply, {text, Json}, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
