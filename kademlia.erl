%%%-------------------------------------------------------------------
%%% @doc Kademlia DHT - Main API Module
%%% This module provides the user-facing API for the Kademlia DHT
%%%-------------------------------------------------------------------
-module(kademlia).

-export([
    %% Node management
    start_node/0,
    start_node/1,
    join/1,
    join/2,
    stop_node/1,
    
    %% DHT operations
    ping/2,
    store/3,
    find_node/2,
    find_value/2,
    
    %% Utility
    get_id/1,
    get_state/1,
    get_local_storage/1,
    node_count/1
]).

%%%===================================================================
%%% Node Management API
%%%===================================================================

%% @doc Start a new bootstrap node (first node in network)
%% Returns {NodeId, NodePid}
start_node() ->
    Id = kademlia_utils:generate_id(),
    kademlia_node:start(Id, none).

%% @doc Start a node with a specific ID
%% Returns {NodeId, NodePid}
start_node(Id) ->
    kademlia_node:start(Id, none).

%% @doc Join an existing network via a bootstrap node
%% Returns {NodeId, NodePid}
join(BootstrapNode, Id) ->
    kademlia_node:start(Id, BootstrapNode).

%% @doc Join with auto-generated ID
join(BootstrapNode) ->
    Id = kademlia_utils:generate_id(),
    kademlia_node:start(Id, BootstrapNode).

%% @doc Stop a node
stop_node({_NodeId, NodePid}) ->
    kademlia_node:stop(NodePid);
stop_node(NodePid) when is_pid(NodePid) ->
    kademlia_node:stop(NodePid).

%%%===================================================================
%%% DHT Operations API
%%%===================================================================

%% @doc Ping another node
%% Returns ok | timeout
ping({_MyId, MyPid}, TargetPid) ->
    ping(MyPid, TargetPid);
ping(MyPid, {_TargetId, TargetPid}) ->
    ping(MyPid, TargetPid);
ping(MyPid, TargetPid) when is_pid(MyPid), is_pid(TargetPid) ->
    MyPid ! {api_ping, TargetPid, self()},
    receive
        {ping_result, Result} -> Result
    after 10000 ->
        timeout
    end.

%% @doc Store a key-value pair in the DHT
%% Returns {ok, NumNodesStored}
store({_NodeId, NodePid}, Key, Value) ->
    store(NodePid, Key, Value);
store(NodePid, Key, Value) when is_pid(NodePid) ->
    HashedKey = kademlia_utils:hash_key(Key),
    NodePid ! {api_store, HashedKey, Value, self()},
    receive
        {store_result, Result} -> Result
    after 30000 ->
        {error, timeout}
    end.

%% @doc Find the K closest nodes to a target ID
%% Returns list of {NodeId, NodePid}
find_node({_NodeId, NodePid}, TargetId) ->
    find_node(NodePid, TargetId);
find_node(NodePid, TargetId) when is_pid(NodePid) ->
    NodePid ! {api_find_node, TargetId, self()},
    receive
        {find_node_result, Nodes} -> Nodes
    after 30000 ->
        {error, timeout}
    end.

%% @doc Find a value by key in the DHT
%% Returns {ok, Value} | {error, not_found}
find_value({_NodeId, NodePid}, Key) ->
    find_value(NodePid, Key);
find_value(NodePid, Key) when is_pid(NodePid) ->
    HashedKey = kademlia_utils:hash_key(Key),
    NodePid ! {api_find_value, HashedKey, self()},
    receive
        {find_value_result, Result} -> Result
    after 30000 ->
        {error, timeout}
    end.

%%%===================================================================
%%% Utility Functions
%%%===================================================================

%% @doc Get the ID of a node
get_id({NodeId, _NodePid}) ->
    NodeId.

%% @doc Get the internal state of a node (for debugging)
get_state({_NodeId, NodePid}) ->
    get_state(NodePid);
get_state(NodePid) when is_pid(NodePid) ->
    NodePid ! {api_get_state, self()},
    receive
        {state, State} -> State
    after 5000 ->
        {error, timeout}
    end.

%% @doc Get local storage of a node
get_local_storage({_NodeId, NodePid}) ->
    get_local_storage(NodePid);
get_local_storage(NodePid) when is_pid(NodePid) ->
    NodePid ! {api_get_local_storage, self()},
    receive
        {local_storage, Storage} -> Storage
    after 5000 ->
        {error, timeout}
    end.

%% @doc Get number of nodes in routing table
node_count({_NodeId, NodePid}) ->
    node_count(NodePid);
node_count(NodePid) when is_pid(NodePid) ->
    State = get_state(NodePid),
    case State of
        {error, _} -> 0;
        _ -> kademlia_routing:node_count(element(3, State))
    end.
