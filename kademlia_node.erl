%%%-------------------------------------------------------------------
%%% @doc Kademlia Node Process
%%% Each node in the DHT is an Erlang process running this module
%%%-------------------------------------------------------------------
-module(kademlia_node).

-export([
    start/0,
    start/1,
    start/2,
    stop/1,
    loop/1
]).

-record(node_state, {
    id,                 % This node's ID
    routing_table,      % K-buckets routing table
    storage,            % Local key-value storage (map)
    republish_timer     % Timer reference for republishing
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Start a new bootstrap node (first node in network)
start() ->
    Id = kademlia_utils:generate_id(),
    start(Id, none).

%% @doc Start a node with a specific ID (for testing)
start(Id) ->
    start(Id, none).

%% @doc Start a node and join via bootstrap node
%% BootstrapNode is {NodeId, NodePid} or 'none'
start(Id, BootstrapNode) ->
    Pid = spawn(?MODULE, loop, [init_state(Id)]),
    
    %% If there's a bootstrap node, join the network
    case BootstrapNode of
        none ->
            ok;
        {BootstrapId, BootstrapPid} ->
            %% Add bootstrap node to routing table
            Pid ! {internal_add_node, BootstrapId, BootstrapPid},
            %% Perform node lookup on ourselves to populate routing table
            Pid ! {internal_join, Id}
    end,
    
    %% Start republish timer
    Pid ! start_republish_timer,
    
    {Id, Pid}.

%% @doc Stop a node
stop(NodePid) ->
    NodePid ! stop.

%%%===================================================================
%%% Node Process Loop
%%%===================================================================

init_state(Id) ->
    #node_state{
        id = Id,
        routing_table = kademlia_routing:new(Id),
        storage = #{},
        republish_timer = undefined
    }.

loop(State) ->
    receive
        %% === PING ===
        {ping, FromId, FromPid, RequestId} ->
            %% Respond with pong
            FromPid ! {pong, State#node_state.id, self(), RequestId},
            %% Add sender to routing table
            NewState = add_node_to_routing(State, FromId, FromPid),
            loop(NewState);
        
        {pong, FromId, FromPid, _RequestId} ->
            %% Node is alive, update routing table
            NewState = add_node_to_routing(State, FromId, FromPid),
            loop(NewState);
        
        %% === STORE ===
        {store, Key, Value, FromId, FromPid, RequestId} ->
            %% Store the key-value pair locally
            NewStorage = maps:put(Key, Value, State#node_state.storage),
            %% Send acknowledgment
            FromPid ! {store_ok, State#node_state.id, RequestId},
            %% Add sender to routing table
            NewState = add_node_to_routing(
                State#node_state{storage = NewStorage}, 
                FromId, FromPid),
            loop(NewState);
        
        {store_ok, FromId, _RequestId} ->
            %% Store confirmed, update routing table
            NewState = add_node_to_routing(State, FromId, self()),
            loop(NewState);
        
        %% === FIND_NODE ===
        {find_node, TargetId, FromId, FromPid, RequestId} ->
            %% Find K closest nodes to TargetId
            K = kademlia_utils:k_value(),
            ClosestNodes = kademlia_routing:get_closest_nodes(
                State#node_state.routing_table, TargetId, K),
            %% Send response
            FromPid ! {find_node_result, State#node_state.id, ClosestNodes, RequestId},
            %% Add sender to routing table
            NewState = add_node_to_routing(State, FromId, FromPid),
            loop(NewState);
        
        {find_node_result, FromId, _Nodes, _RequestId} ->
            %% Just update routing table (actual processing done by caller)
            NewState = add_node_to_routing(State, FromId, self()),
            loop(NewState);
        
        %% === FIND_VALUE ===
        {find_value, Key, FromId, FromPid, RequestId} ->
            case maps:get(Key, State#node_state.storage, not_found) of
                not_found ->
                    %% Key not found, return closest nodes instead
                    K = kademlia_utils:k_value(),
                    ClosestNodes = kademlia_routing:get_closest_nodes(
                        State#node_state.routing_table, Key, K),
                    FromPid ! {find_value_nodes, State#node_state.id, ClosestNodes, RequestId};
                Value ->
                    %% Key found, return value
                    FromPid ! {find_value_result, State#node_state.id, Value, RequestId}
            end,
            %% Add sender to routing table
            NewState = add_node_to_routing(State, FromId, FromPid),
            loop(NewState);
        
        %% === INTERNAL MESSAGES ===
        {internal_add_node, NodeId, NodePid} ->
            NewState = add_node_to_routing(State, NodeId, NodePid),
            loop(NewState);
        
        {internal_join, TargetId} ->
            %% Perform iterative lookup on ourselves to populate routing table
            do_iterative_find_node(State, TargetId),
            loop(State);
        
        start_republish_timer ->
            %% Cancel existing timer if any
            case State#node_state.republish_timer of
                undefined -> ok;
                OldTimer -> erlang:cancel_timer(OldTimer)
            end,
            %% Start new timer
            Timer = erlang:send_after(
                kademlia_utils:republish_time(), 
                self(), 
                republish),
            loop(State#node_state{republish_timer = Timer});
        
        republish ->
            %% Republish all stored data
            do_republish(State),
            %% Restart timer
            Timer = erlang:send_after(
                kademlia_utils:republish_time(), 
                self(), 
                republish),
            loop(State#node_state{republish_timer = Timer});
        
        %% === SYNCHRONOUS API CALLS ===
        {api_ping, TargetPid, From} ->
            RequestId = make_ref(),
            TargetPid ! {ping, State#node_state.id, self(), RequestId},
            receive
                {pong, TargetId, _, RequestId} ->
                    NewState = add_node_to_routing(State, TargetId, TargetPid),
                    From ! {ping_result, ok},
                    loop(NewState)
            after 5000 ->
                From ! {ping_result, timeout},
                loop(State)
            end;
        
        {api_store, Key, Value, From} ->
            %% Find nodes closest to key and store on them
            Result = do_store(State, Key, Value),
            From ! {store_result, Result},
            loop(State);
        
        {api_find_node, TargetId, From} ->
            Nodes = do_iterative_find_node(State, TargetId),
            From ! {find_node_result, Nodes},
            loop(State);
        
        {api_find_value, Key, From} ->
            Result = do_iterative_find_value(State, Key),
            From ! {find_value_result, Result},
            loop(State);
        
        {api_get_state, From} ->
            From ! {state, State},
            loop(State);
        
        {api_get_local_storage, From} ->
            From ! {local_storage, State#node_state.storage},
            loop(State);
        
        %% === STOP ===
        stop ->
            case State#node_state.republish_timer of
                undefined -> ok;
                Timer -> erlang:cancel_timer(Timer)
            end,
            ok;
        
        _Unknown ->
            %% Ignore unknown messages
            loop(State)
    end.

%%%===================================================================
%%% Internal Functions
%%%===================================================================

%% Add a node to the routing table, handling full buckets
add_node_to_routing(State, NodeId, NodePid) ->
    case kademlia_routing:add_node(State#node_state.routing_table, NodeId, NodePid) of
        {updated, NewRoutingTable} ->
            State#node_state{routing_table = NewRoutingTable};
        {unchanged, _} ->
            State;
        {full, _RoutingTable, {OldestId, OldestPid}} ->
            %% Bucket is full, ping oldest node
            %% If it doesn't respond, we could replace it
            %% For simplicity, we just try to ping and keep the old node
            RequestId = make_ref(),
            OldestPid ! {ping, State#node_state.id, self(), RequestId},
            State
    end.

%% Iterative FIND_NODE lookup
do_iterative_find_node(State, TargetId) ->
    K = kademlia_utils:k_value(),
    Alpha = kademlia_utils:alpha_value(),
    
    %% Start with closest nodes we know
    InitialNodes = kademlia_routing:get_closest_nodes(
        State#node_state.routing_table, TargetId, K),
    
    %% Iteratively query nodes
    iterative_lookup(State, TargetId, InitialNodes, [], Alpha, K, find_node).

%% Iterative FIND_VALUE lookup
do_iterative_find_value(State, Key) ->
    K = kademlia_utils:k_value(),
    Alpha = kademlia_utils:alpha_value(),
    
    %% Start with closest nodes we know
    InitialNodes = kademlia_routing:get_closest_nodes(
        State#node_state.routing_table, Key, K),
    
    %% Check local storage first
    case maps:get(Key, State#node_state.storage, not_found) of
        not_found ->
            iterative_lookup(State, Key, InitialNodes, [], Alpha, K, find_value);
        Value ->
            {ok, Value}
    end.

%% Generic iterative lookup
iterative_lookup(_State, _TargetId, [], Queried, _Alpha, _K, _Type) ->
    %% No more nodes to query
    case _Type of
        find_node -> Queried;
        find_value -> {error, not_found}
    end;
iterative_lookup(State, TargetId, ToQuery, Queried, Alpha, K, Type) ->
    %% Query up to Alpha nodes in parallel
    {QueryNow, QueryLater} = lists:split(min(Alpha, length(ToQuery)), ToQuery),
    
    %% Send queries
    MyId = State#node_state.id,
    RequestId = make_ref(),
    
    lists:foreach(fun({NodeId, NodePid}) ->
        case Type of
            find_node ->
                NodePid ! {find_node, TargetId, MyId, self(), RequestId};
            find_value ->
                NodePid ! {find_value, TargetId, MyId, self(), RequestId}
        end
    end, QueryNow),
    
    %% Collect responses (with timeout)
    Responses = collect_responses(length(QueryNow), RequestId, 1000),
    
    %% Process responses
    NewQueried = Queried ++ QueryNow,
    QueriedIds = [Id || {Id, _Pid} <- NewQueried],
    
    %% Check if we found a value
    case lists:keyfind(value, 1, Responses) of
        {value, Value} ->
            {ok, Value};
        false ->
            %% Collect new nodes from responses
            NewNodes = lists:flatten([Nodes || {nodes, Nodes} <- Responses]),
            
            %% Filter out already queried nodes and ourselves
            FilteredNew = [{Id, Pid} || {Id, Pid} <- NewNodes,
                                        not lists:member(Id, QueriedIds),
                                        Id =/= MyId],
            
            %% Sort by distance to target
            SortedNew = sort_by_distance(TargetId, FilteredNew),
            
            %% Combine with remaining nodes to query
            AllToQuery = merge_by_distance(TargetId, SortedNew, QueryLater),
            
            %% Take only K closest
            NextToQuery = lists:sublist(AllToQuery, K),
            
            %% Check if we're making progress
            case NextToQuery =:= [] orelse NextToQuery =:= QueryLater of
                true ->
                    %% No progress, return what we have
                    case Type of
                        find_node -> 
                            lists:sublist(sort_by_distance(TargetId, NewQueried), K);
                        find_value -> 
                            {error, not_found}
                    end;
                false ->
                    iterative_lookup(State, TargetId, NextToQuery, NewQueried, Alpha, K, Type)
            end
    end.

%% Collect responses with timeout
collect_responses(0, _RequestId, _Timeout) ->
    [];
collect_responses(Count, RequestId, Timeout) ->
    receive
        {find_node_result, _FromId, Nodes, RequestId} ->
            [{nodes, Nodes} | collect_responses(Count - 1, RequestId, Timeout)];
        {find_value_result, _FromId, Value, RequestId} ->
            [{value, Value} | collect_responses(Count - 1, RequestId, Timeout)];
        {find_value_nodes, _FromId, Nodes, RequestId} ->
            [{nodes, Nodes} | collect_responses(Count - 1, RequestId, Timeout)]
    after Timeout ->
        []
    end.

%% Sort nodes by XOR distance to target
sort_by_distance(TargetId, Nodes) ->
    lists:sort(
        fun({Id1, _}, {Id2, _}) ->
            kademlia_utils:xor_distance(TargetId, Id1) =<
            kademlia_utils:xor_distance(TargetId, Id2)
        end,
        Nodes).

%% Merge two sorted lists by distance
merge_by_distance(_TargetId, [], List2) -> List2;
merge_by_distance(_TargetId, List1, []) -> List1;
merge_by_distance(TargetId, [{Id1, Pid1} | Rest1] = List1, [{Id2, Pid2} | Rest2] = List2) ->
    D1 = kademlia_utils:xor_distance(TargetId, Id1),
    D2 = kademlia_utils:xor_distance(TargetId, Id2),
    case D1 =< D2 of
        true -> [{Id1, Pid1} | merge_by_distance(TargetId, Rest1, List2)];
        false -> [{Id2, Pid2} | merge_by_distance(TargetId, List1, Rest2)]
    end.

%% Store a value on the K closest nodes
do_store(State, Key, Value) ->
    %% Find K closest nodes to the key
    ClosestNodes = do_iterative_find_node(State, Key),
    
    %% Store on each node
    MyId = State#node_state.id,
    RequestId = make_ref(),
    
    lists:foreach(fun({_NodeId, NodePid}) ->
        NodePid ! {store, Key, Value, MyId, self(), RequestId}
    end, ClosestNodes),
    
    %% Wait for acknowledgments
    Acks = collect_store_acks(length(ClosestNodes), RequestId, 2000),
    
    {ok, length(Acks)}.

collect_store_acks(0, _RequestId, _Timeout) ->
    [];
collect_store_acks(Count, RequestId, Timeout) ->
    receive
        {store_ok, FromId, RequestId} ->
            [FromId | collect_store_acks(Count - 1, RequestId, Timeout)]
    after Timeout ->
        []
    end.

%% Republish all stored data
do_republish(State) ->
    maps:foreach(fun(Key, Value) ->
        do_store(State, Key, Value)
    end, State#node_state.storage).
