%%%-------------------------------------------------------------------
%%% @doc Kademlia Routing Table (K-Buckets)
%%% Manages the routing table with k-buckets for node discovery
%%%-------------------------------------------------------------------
-module(kademlia_routing).

-export([
    new/1,
    add_node/3,
    remove_node/2,
    get_closest_nodes/3,
    get_all_nodes/1,
    get_bucket/2,
    refresh_bucket/2,
    node_count/1
]).

-record(routing_table, {
    my_id,          % This node's ID
    buckets         % Map of bucket_index => list of {NodeId, Pid}
}).

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Create a new routing table for a node with given ID
new(MyId) ->
    BitSize = kademlia_utils:bit_size(),
    %% Initialize empty buckets (0 to BitSize-1)
    EmptyBuckets = maps:from_list([{I, []} || I <- lists:seq(0, BitSize - 1)]),
    #routing_table{my_id = MyId, buckets = EmptyBuckets}.

%% @doc Add a node to the routing table
%% Returns {updated, NewRoutingTable} or {full, RoutingTable, OldestNode}
add_node(RoutingTable, NodeId, NodePid) ->
    #routing_table{my_id = MyId, buckets = Buckets} = RoutingTable,
    
    %% Don't add ourselves
    case NodeId =:= MyId of
        true -> 
            {unchanged, RoutingTable};
        false ->
            BucketIndex = kademlia_utils:bucket_index(MyId, NodeId),
            Bucket = maps:get(BucketIndex, Buckets),
            K = kademlia_utils:k_value(),
            
            %% Check if node already exists
            case lists:keyfind(NodeId, 1, Bucket) of
                {NodeId, _OldPid} ->
                    %% Node exists, move to end (most recently seen)
                    NewBucket = lists:keydelete(NodeId, 1, Bucket) ++ [{NodeId, NodePid}],
                    NewBuckets = maps:put(BucketIndex, NewBucket, Buckets),
                    {updated, RoutingTable#routing_table{buckets = NewBuckets}};
                false ->
                    %% Node doesn't exist
                    case length(Bucket) < K of
                        true ->
                            %% Bucket has space, add node
                            NewBucket = Bucket ++ [{NodeId, NodePid}],
                            NewBuckets = maps:put(BucketIndex, NewBucket, Buckets),
                            {updated, RoutingTable#routing_table{buckets = NewBuckets}};
                        false ->
                            %% Bucket is full, return oldest node for ping check
                            [{OldestId, OldestPid} | _Rest] = Bucket,
                            {full, RoutingTable, {OldestId, OldestPid}}
                    end
            end
    end.

%% @doc Remove a node from the routing table
remove_node(RoutingTable, NodeId) ->
    #routing_table{my_id = MyId, buckets = Buckets} = RoutingTable,
    
    case NodeId =:= MyId of
        true -> 
            RoutingTable;
        false ->
            BucketIndex = kademlia_utils:bucket_index(MyId, NodeId),
            Bucket = maps:get(BucketIndex, Buckets),
            NewBucket = lists:keydelete(NodeId, 1, Bucket),
            NewBuckets = maps:put(BucketIndex, NewBucket, Buckets),
            RoutingTable#routing_table{buckets = NewBuckets}
    end.

%% @doc Get the K closest nodes to a target ID
get_closest_nodes(RoutingTable, TargetId, Count) ->
    AllNodes = get_all_nodes(RoutingTable),
    
    %% Calculate distance for each node
    NodesWithDistance = [{kademlia_utils:xor_distance(TargetId, NodeId), NodeId, Pid} 
                         || {NodeId, Pid} <- AllNodes],
    
    %% Sort by distance
    Sorted = lists:sort(fun({D1, _, _}, {D2, _, _}) -> D1 =< D2 end, NodesWithDistance),
    
    %% Take the closest Count nodes
    Closest = lists:sublist(Sorted, Count),
    
    %% Return as [{NodeId, Pid}]
    [{NodeId, Pid} || {_Dist, NodeId, Pid} <- Closest].

%% @doc Get all nodes in the routing table
get_all_nodes(#routing_table{buckets = Buckets}) ->
    lists:flatten(maps:values(Buckets)).

%% @doc Get a specific bucket
get_bucket(#routing_table{buckets = Buckets}, BucketIndex) ->
    maps:get(BucketIndex, Buckets, []).

%% @doc Get a random ID in a bucket (for refresh)
refresh_bucket(#routing_table{my_id = MyId}, BucketIndex) ->
    kademlia_utils:random_id_in_bucket(MyId, BucketIndex).

%% @doc Count total nodes in routing table
node_count(#routing_table{buckets = Buckets}) ->
    lists:sum([length(B) || B <- maps:values(Buckets)]).
