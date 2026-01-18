# Kademlia DHT Implementation in Erlang

A distributed hash table (DHT) implementation based on the Kademlia protocol, written in Erlang for the ADCC (Applicazioni Distribuite e Cloud Computing) course.

## Project Structure

```
kademlia/
├── kademlia.erl           # Main API module
├── kademlia_node.erl      # Node process implementation
├── kademlia_routing.erl   # K-buckets routing table
├── kademlia_utils.erl     # Utility functions (XOR distance, ID generation)
├── kademlia_test.erl      # Tests and performance measurements
└── README.md              # This file
```

## Quick Start

### 1. Compile all modules

```erlang
c(kademlia_utils).
c(kademlia_routing).
c(kademlia_node).
c(kademlia).
c(kademlia_test).
```

### 2. Create a network

```erlang
%% Start bootstrap node
Node1 = kademlia:start_node().

%% Join more nodes
Node2 = kademlia:join(Node1).
Node3 = kademlia:join(Node1).
```

### 3. Store and retrieve values

```erlang
%% Store a value
kademlia:store(Node1, "mykey", "myvalue").

%% Retrieve from any node
kademlia:find_value(Node3, "mykey").
%% Returns: {ok, "myvalue"}
```

### 4. Run tests

```erlang
kademlia_test:run_all_tests().
```

## API Reference

### Node Management

| Function | Description |
|----------|-------------|
| `kademlia:start_node()` | Start a new bootstrap node |
| `kademlia:start_node(Id)` | Start node with specific ID |
| `kademlia:join(BootstrapNode)` | Join network via existing node |
| `kademlia:stop_node(Node)` | Stop a node |

### DHT Operations

| Function | Description |
|----------|-------------|
| `kademlia:ping(Node, TargetPid)` | Check if a node is alive |
| `kademlia:store(Node, Key, Value)` | Store key-value pair |
| `kademlia:find_node(Node, TargetId)` | Find K closest nodes to ID |
| `kademlia:find_value(Node, Key)` | Retrieve value by key |

### Configuration

Edit `kademlia_utils.erl` to change:

- `BIT_SIZE`: Number of bits for IDs (default: 8, use 160 for production)
- `K`: Replication factor / bucket size (default: 4)
- `ALPHA`: Parallel lookup count (default: 3)
- `REPUBLISH_TIME`: Data republish interval in ms (default: 60000)

## Kademlia Protocol

### XOR Distance

Kademlia uses XOR as the distance metric:
```
distance(A, B) = A XOR B
```

This has useful properties:
- `distance(A, A) = 0`
- `distance(A, B) = distance(B, A)`
- Satisfies triangle inequality

### K-Buckets

Each node maintains a routing table of K-buckets:
- Bucket `i` contains nodes with distance `2^i` to `2^(i+1) - 1`
- Each bucket holds at most K nodes
- Most recently seen nodes are preferred

### Iterative Lookup

To find a node or value:
1. Start with K closest known nodes
2. Query ALPHA nodes in parallel
3. Add newly discovered nodes
4. Repeat until no closer nodes found

### Data Republishing

To ensure data persistence:
- Each node periodically republishes stored data
- Data is stored on K closest nodes to the key
- Handles node joins and leaves gracefully

## Performance Measurements

Run `kademlia_test:run_all_tests()` to get:

- Average lookup time
- Average store time  
- Average join time
- Failure recovery statistics

For stress testing with ~1000 nodes:
```erlang
kademlia_test:stress_test(1000).
```

## Author

[LOUAY ZAIDI]
ADCC Course 2025/2026
University of [CARLO BO URBINO]
