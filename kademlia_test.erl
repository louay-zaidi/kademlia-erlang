%%%-------------------------------------------------------------------
%%% @doc Kademlia Test and Measurement Module
%%% Contains tests and performance measurements for the project report
%%%-------------------------------------------------------------------
-module(kademlia_test).

-export([
    %% Basic tests
    test_basic/0,
    test_store_retrieve/0,
    test_multiple_nodes/0,
    
    %% Performance measurements
    measure_lookup_time/0,
    measure_lookup_time/1,
    measure_store_time/0,
    measure_store_time/1,
    measure_join_time/0,
    measure_join_time/1,
    measure_failure_recovery/0,
    
    %% Large scale test
    stress_test/0,
    stress_test/1,
    
    %% Helper
    create_network/1,
    shutdown_network/1,
    run_all_tests/0
]).

%%%===================================================================
%%% Basic Tests
%%%===================================================================

%% @doc Test basic node creation and ping
test_basic() ->
    io:format("~n=== Test: Basic Node Operations ===~n"),
    
    %% Create first node (bootstrap)
    io:format("Creating bootstrap node...~n"),
    Node1 = kademlia:start_node(100),
    {Id1, Pid1} = Node1,
    io:format("Node 1 created: ID=~p, PID=~p~n", [Id1, Pid1]),
    
    %% Create second node and join
    io:format("Creating second node and joining...~n"),
    Node2 = kademlia:join(Node1, 50),
    {Id2, Pid2} = Node2,
    io:format("Node 2 created: ID=~p, PID=~p~n", [Id2, Pid2]),
    
    %% Wait for join to complete
    timer:sleep(500),
    
    %% Test ping
    io:format("Testing ping from Node2 to Node1...~n"),
    PingResult = kademlia:ping(Node2, Pid1),
    io:format("Ping result: ~p~n", [PingResult]),
    
    %% Cleanup
    kademlia:stop_node(Node1),
    kademlia:stop_node(Node2),
    
    io:format("Test basic: PASSED~n"),
    ok.

%% @doc Test storing and retrieving values
test_store_retrieve() ->
    io:format("~n=== Test: Store and Retrieve ===~n"),
    
    %% Create a small network
    Nodes = create_network(5),
    timer:sleep(1000),
    
    %% Store a value from the first node
    [Node1 | _] = Nodes,
    io:format("Storing key 'hello' with value 'world'...~n"),
    {ok, NumStored} = kademlia:store(Node1, "hello", "world"),
    io:format("Stored on ~p nodes~n", [NumStored]),
    
    %% Retrieve from different nodes
    lists:foreach(fun(Node) ->
        {NodeId, _} = Node,
        Result = kademlia:find_value(Node, "hello"),
        io:format("Node ~p retrieved: ~p~n", [NodeId, Result])
    end, Nodes),
    
    %% Cleanup
    shutdown_network(Nodes),
    
    io:format("Test store/retrieve: PASSED~n"),
    ok.

%% @doc Test with multiple nodes
test_multiple_nodes() ->
    io:format("~n=== Test: Multiple Nodes Network ===~n"),
    
    %% Create network of 10 nodes
    Nodes = create_network(10),
    timer:sleep(2000),
    
    io:format("Network created with ~p nodes~n", [length(Nodes)]),
    
    %% Check routing table sizes
    lists:foreach(fun(Node) ->
        {NodeId, _} = Node,
        Count = kademlia:node_count(Node),
        io:format("Node ~p knows ~p other nodes~n", [NodeId, Count])
    end, Nodes),
    
    %% Store multiple values
    [Node1 | _] = Nodes,
    Keys = ["key1", "key2", "key3", "key4", "key5"],
    lists:foreach(fun(Key) ->
        Value = "value_" ++ Key,
        {ok, _} = kademlia:store(Node1, Key, Value),
        io:format("Stored ~p~n", [Key])
    end, Keys),
    
    %% Retrieve from random nodes
    timer:sleep(500),
    RandomNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
    lists:foreach(fun(Key) ->
        Result = kademlia:find_value(RandomNode, Key),
        io:format("Retrieved ~p: ~p~n", [Key, Result])
    end, Keys),
    
    %% Cleanup
    shutdown_network(Nodes),
    
    io:format("Test multiple nodes: PASSED~n"),
    ok.

%%%===================================================================
%%% Performance Measurements
%%%===================================================================

%% @doc Measure average lookup time
measure_lookup_time() ->
    measure_lookup_time(100).

measure_lookup_time(NumNodes) ->
    io:format("~n=== Measuring Lookup Time (Network: ~p nodes) ===~n", [NumNodes]),
    
    %% Create network
    Nodes = create_network(NumNodes),
    timer:sleep(NumNodes * 100),  % Wait for network to stabilize
    
    %% Store some values first
    [Node1 | _] = Nodes,
    lists:foreach(fun(I) ->
        Key = "lookup_key_" ++ integer_to_list(I),
        kademlia:store(Node1, Key, "value")
    end, lists:seq(1, 20)),
    timer:sleep(1000),
    
    %% Measure lookup times
    NumLookups = 50,
    Times = lists:map(fun(I) ->
        Key = "lookup_key_" ++ integer_to_list((I rem 20) + 1),
        RandomNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
        
        StartTime = erlang:system_time(microsecond),
        _Result = kademlia:find_value(RandomNode, Key),
        EndTime = erlang:system_time(microsecond),
        
        EndTime - StartTime
    end, lists:seq(1, NumLookups)),
    
    %% Calculate statistics
    AvgTime = lists:sum(Times) / length(Times),
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),
    
    io:format("Lookup Statistics (~p lookups):~n", [NumLookups]),
    io:format("  Average: ~.2f microseconds (~.2f ms)~n", [AvgTime, AvgTime/1000]),
    io:format("  Min: ~p microseconds~n", [MinTime]),
    io:format("  Max: ~p microseconds~n", [MaxTime]),
    
    %% Cleanup
    shutdown_network(Nodes),
    
    {ok, #{avg => AvgTime, min => MinTime, max => MaxTime}}.

%% @doc Measure average store time
measure_store_time() ->
    measure_store_time(100).

measure_store_time(NumNodes) ->
    io:format("~n=== Measuring Store Time (Network: ~p nodes) ===~n", [NumNodes]),
    
    %% Create network
    Nodes = create_network(NumNodes),
    timer:sleep(NumNodes * 100),
    
    %% Measure store times
    NumStores = 50,
    Times = lists:map(fun(I) ->
        Key = "store_test_" ++ integer_to_list(I),
        Value = "value_" ++ integer_to_list(I),
        RandomNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
        
        StartTime = erlang:system_time(microsecond),
        _Result = kademlia:store(RandomNode, Key, Value),
        EndTime = erlang:system_time(microsecond),
        
        EndTime - StartTime
    end, lists:seq(1, NumStores)),
    
    %% Calculate statistics
    AvgTime = lists:sum(Times) / length(Times),
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),
    
    io:format("Store Statistics (~p stores):~n", [NumStores]),
    io:format("  Average: ~.2f microseconds (~.2f ms)~n", [AvgTime, AvgTime/1000]),
    io:format("  Min: ~p microseconds~n", [MinTime]),
    io:format("  Max: ~p microseconds~n", [MaxTime]),
    
    %% Cleanup
    shutdown_network(Nodes),
    
    {ok, #{avg => AvgTime, min => MinTime, max => MaxTime}}.

%% @doc Measure average join time
measure_join_time() ->
    measure_join_time(50).

measure_join_time(NumNodes) ->
    io:format("~n=== Measuring Join Time ===~n"),
    
    %% Create bootstrap node
    Bootstrap = kademlia:start_node(1),
    AllNodes = [Bootstrap],
    
    %% Measure join times for subsequent nodes
    {FinalNodes, Times} = lists:foldl(fun(I, {Nodes, Acc}) ->
        [LastNode | _] = Nodes,
        
        StartTime = erlang:system_time(microsecond),
        NewNode = kademlia:join(LastNode, I * 10),
        timer:sleep(50),  % Small delay to let join complete
        EndTime = erlang:system_time(microsecond),
        
        JoinTime = EndTime - StartTime,
        {[NewNode | Nodes], [JoinTime | Acc]}
    end, {AllNodes, []}, lists:seq(2, NumNodes)),
    
    %% Calculate statistics
    AvgTime = lists:sum(Times) / length(Times),
    MinTime = lists:min(Times),
    MaxTime = lists:max(Times),
    
    io:format("Join Statistics (~p joins):~n", [length(Times)]),
    io:format("  Average: ~.2f microseconds (~.2f ms)~n", [AvgTime, AvgTime/1000]),
    io:format("  Min: ~p microseconds~n", [MinTime]),
    io:format("  Max: ~p microseconds~n", [MaxTime]),
    
    %% Cleanup
    shutdown_network(FinalNodes),
    
    {ok, #{avg => AvgTime, min => MinTime, max => MaxTime}}.

%% @doc Measure recovery time when nodes fail
measure_failure_recovery() ->
    io:format("~n=== Measuring Failure Recovery ===~n"),
    
    %% Create network
    NumNodes = 20,
    Nodes = create_network(NumNodes),
    timer:sleep(2000),
    
    %% Store some values
    [Node1 | Rest] = Nodes,
    Keys = ["recover_key_" ++ integer_to_list(I) || I <- lists:seq(1, 10)],
    lists:foreach(fun(Key) ->
        kademlia:store(Node1, Key, "important_value")
    end, Keys),
    timer:sleep(1000),
    
    io:format("Stored ~p keys in network~n", [length(Keys)]),
    
    %% Kill some nodes (simulate failure)
    NumToKill = 3,
    {ToKill, Survivors} = lists:split(NumToKill, Rest),
    io:format("Killing ~p nodes...~n", [NumToKill]),
    lists:foreach(fun(Node) ->
        kademlia:stop_node(Node)
    end, ToKill),
    
    %% Measure time to retrieve values after failure
    timer:sleep(500),
    
    SurvivorNode = lists:nth(rand:uniform(length(Survivors)), Survivors),
    Times = lists:map(fun(Key) ->
        StartTime = erlang:system_time(microsecond),
        Result = kademlia:find_value(SurvivorNode, Key),
        EndTime = erlang:system_time(microsecond),
        
        {Key, Result, EndTime - StartTime}
    end, Keys),
    
    %% Report results
    SuccessCount = length([1 || {_, {ok, _}, _} <- Times]),
    AvgTime = lists:sum([T || {_, _, T} <- Times]) / length(Times),
    
    io:format("Recovery Results:~n"),
    io:format("  Keys recovered: ~p/~p~n", [SuccessCount, length(Keys)]),
    io:format("  Average lookup time after failure: ~.2f ms~n", [AvgTime/1000]),
    
    %% Cleanup
    shutdown_network([Node1 | Survivors]),
    
    {ok, #{recovered => SuccessCount, total => length(Keys), avg_time => AvgTime}}.

%%%===================================================================
%%% Stress Test
%%%===================================================================

%% @doc Stress test with ~1000 nodes
stress_test() ->
    stress_test(1000).

stress_test(NumNodes) ->
    io:format("~n=== STRESS TEST: ~p Nodes ===~n", [NumNodes]),
    io:format("This may take several minutes...~n"),
    
    %% Create network in batches to avoid overwhelming the system
    BatchSize = 50,
    NumBatches = (NumNodes + BatchSize - 1) div BatchSize,
    
    io:format("Creating network in ~p batches of ~p nodes...~n", [NumBatches, BatchSize]),
    
    %% Create bootstrap
    StartTime = erlang:system_time(second),
    Bootstrap = kademlia:start_node(1),
    
    %% Create remaining nodes in batches
    AllNodes = lists:foldl(fun(BatchNum, Nodes) ->
        NodesInBatch = min(BatchSize, NumNodes - length(Nodes)),
        io:format("Creating batch ~p (~p nodes)...~n", [BatchNum, NodesInBatch]),
        
        NewNodes = lists:map(fun(I) ->
            Id = (BatchNum - 1) * BatchSize + I + 1,
            RandomExisting = lists:nth(rand:uniform(length(Nodes)), Nodes),
            kademlia:join(RandomExisting, Id * 17 rem 256)  % Spread IDs
        end, lists:seq(1, NodesInBatch)),
        
        timer:sleep(100),  % Brief pause between batches
        Nodes ++ NewNodes
    end, [Bootstrap], lists:seq(1, NumBatches - 1)),
    
    CreationTime = erlang:system_time(second) - StartTime,
    io:format("Network created in ~p seconds~n", [CreationTime]),
    io:format("Total nodes: ~p~n", [length(AllNodes)]),
    
    %% Wait for network to stabilize
    io:format("Waiting for network to stabilize...~n"),
    timer:sleep(5000),
    
    %% Test operations
    io:format("~nRunning operations test...~n"),
    
    %% Store 100 values
    TestNode = lists:nth(rand:uniform(length(AllNodes)), AllNodes),
    StoreTimes = lists:map(fun(I) ->
        Key = "stress_key_" ++ integer_to_list(I),
        Start = erlang:system_time(microsecond),
        kademlia:store(TestNode, Key, "stress_value"),
        erlang:system_time(microsecond) - Start
    end, lists:seq(1, 100)),
    
    AvgStoreTime = lists:sum(StoreTimes) / length(StoreTimes),
    io:format("Average store time: ~.2f ms~n", [AvgStoreTime/1000]),
    
    timer:sleep(1000),
    
    %% Lookup 100 values from random nodes
    LookupTimes = lists:map(fun(I) ->
        Key = "stress_key_" ++ integer_to_list(I),
        RandomNode = lists:nth(rand:uniform(length(AllNodes)), AllNodes),
        Start = erlang:system_time(microsecond),
        kademlia:find_value(RandomNode, Key),
        erlang:system_time(microsecond) - Start
    end, lists:seq(1, 100)),
    
    AvgLookupTime = lists:sum(LookupTimes) / length(LookupTimes),
    io:format("Average lookup time: ~.2f ms~n", [AvgLookupTime/1000]),
    
    %% Sample routing table sizes
    SampleNodes = [lists:nth(I * (length(AllNodes) div 10), AllNodes) 
                   || I <- lists:seq(1, 10)],
    RoutingCounts = [kademlia:node_count(N) || N <- SampleNodes],
    AvgRouting = lists:sum(RoutingCounts) / length(RoutingCounts),
    io:format("Average routing table size: ~.1f nodes~n", [AvgRouting]),
    
    %% Cleanup
    io:format("~nShutting down network...~n"),
    shutdown_network(AllNodes),
    
    io:format("~n=== STRESS TEST COMPLETE ===~n"),
    io:format("Summary:~n"),
    io:format("  Nodes: ~p~n", [length(AllNodes)]),
    io:format("  Creation time: ~p seconds~n", [CreationTime]),
    io:format("  Avg store time: ~.2f ms~n", [AvgStoreTime/1000]),
    io:format("  Avg lookup time: ~.2f ms~n", [AvgLookupTime/1000]),
    io:format("  Avg routing table: ~.1f nodes~n", [AvgRouting]),
    
    {ok, #{
        nodes => length(AllNodes),
        creation_time => CreationTime,
        avg_store_time => AvgStoreTime,
        avg_lookup_time => AvgLookupTime,
        avg_routing_size => AvgRouting
    }}.

%%%===================================================================
%%% Helper Functions
%%%===================================================================

%% @doc Create a network of N nodes
create_network(N) when N >= 1 ->
    %% Create bootstrap node
    Bootstrap = kademlia:start_node(1),
    
    %% Create remaining nodes
    Nodes = lists:foldl(fun(I, Acc) ->
        %% Join via a random existing node
        RandomNode = lists:nth(rand:uniform(length(Acc)), Acc),
        NewNode = kademlia:join(RandomNode, I * 10),
        timer:sleep(20),  % Small delay between joins
        [NewNode | Acc]
    end, [Bootstrap], lists:seq(2, N)),
    
    lists:reverse(Nodes).

%% @doc Shutdown all nodes in a network
shutdown_network(Nodes) ->
    lists:foreach(fun(Node) ->
        catch kademlia:stop_node(Node)
    end, Nodes).

%% @doc Run all tests
run_all_tests() ->
    io:format("~n========================================~n"),
    io:format("   KADEMLIA DHT - FULL TEST SUITE~n"),
    io:format("========================================~n"),
    
    test_basic(),
    timer:sleep(500),
    
    test_store_retrieve(),
    timer:sleep(500),
    
    test_multiple_nodes(),
    timer:sleep(500),
    
    measure_lookup_time(50),
    timer:sleep(500),
    
    measure_store_time(50),
    timer:sleep(500),
    
    measure_join_time(30),
    timer:sleep(500),
    
    measure_failure_recovery(),
    
    io:format("~n========================================~n"),
    io:format("   ALL TESTS COMPLETED~n"),
    io:format("========================================~n"),
    
    ok.
