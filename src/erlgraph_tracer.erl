-module(erlgraph_tracer).

-export([start/0, init/0, stop/0]).

-record(state, {}).

start() ->
    {ok, _DbgServer} = dbg:tracer(process, {fun trace_handler/2, #state{}}).

init() ->
    record_processes(),
    {ok, Tracer} = dbg:get_tracer(),
    erlang:trace(all, true, [procs, {tracer, Tracer}]).

stop()->
    dbg:ctpl(),
    dbg:stop().

record_processes() ->
    Processes = erlang:processes() -- [self()],
    [add_node(P) || P <- Processes].

trace_handler({trace, Pid, spawn, Pid2, _MFA}, State = #state{}) ->
    add_node(Pid),
    add_node(Pid2),
    add_link(Pid, Pid2),
    State;
trace_handler({trace, _Pid, exit, _Reason}, State) ->
    State;
trace_handler({trace, _Pid, link, _Pid2}, State) ->
    State;
trace_handler({trace, _Pid, unlink, _Pid2}, State) ->
    State;
trace_handler({trace, _Pid, getting_linked, _Pid2}, State) ->
    State;
trace_handler({trace, _Pid, getting_unlinked, _Pid2}, State) ->
    State;
trace_handler({trace, _Pid, register, _Pid2}, State) ->
    State;
trace_handler({trace, _Pid, unregister, _Pid2}, State) ->
    State;
trace_handler({trace, _Pid, 'send', {notify, _}, _To}, State) ->
    State;
trace_handler({trace, _Pid, 'send', _Msg, _To}, State = #state{}) ->
    State.

add_node(Pid) ->
    Json = jiffy:encode({[
                          {<<"action">>, <<"add_node">>},
                          {<<"process">>, to_json(Pid)}
                         ]}),
    erlgraph_ws:send(Json).

add_link(From, To) ->
    Json = jiffy:encode({[
                          {<<"action">>, <<"add_link">>},
                          {<<"from">>, {[{<<"process">>, to_json(From)
                                       }]}},
                          {<<"to">>, {[{<<"process">>, to_json(To)
                                       }]}}
                         ]}),
    erlgraph_ws:send(Json).

to_json(Pid) ->
    {[
      {<<"pid">>, format_pid(Pid)},
      {<<"registered_name">>, get_name(Pid)},
      {<<"links">>, get_links(Pid)}
     ]}.

get_name(Pid) ->
    case erlang:process_info(Pid, registered_name) of
        {registered_name, Name} ->
            atom_to_binary(Name, latin1);
        _ ->
            format_pid(Pid)
    end.

get_links(Pid) ->
    case erlang:process_info(Pid, links) of
        {links, Links} ->
            Children = [P || P <- Links, is_pid(P), P > Pid],
            format_links(Children);
        _ ->
            format_links([])
    end.

format_links([]) ->
    [];
format_links([Link|Links]) ->
    Bin = format_pid(Link),
    [Bin | format_links(Links)].

format_pid(Pid) when is_pid(Pid) ->
    list_to_binary(erlang:pid_to_list(Pid));
format_pid(Pid) when is_port(Pid) ->
    list_to_binary(erlang:port_to_list(Pid)).

