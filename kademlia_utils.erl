%%%-------------------------------------------------------------------
%%% @doc Kademlia Utility Functions
%%% Helper functions for ID generation, XOR distance, and bit operations
%%%-------------------------------------------------------------------
-module(kademlia_utils).

-export([
    generate_id/0,
    generate_id/1,
    hash_key/1,
    xor_distance/2,
    bucket_index/2,
    random_id_in_bucket/2,
    bit_size/0,
    k_value/0,
    alpha_value/0,
    republish_time/0,
    max_id/0
]).

%%%===================================================================
%%% Configuration - You can adjust these parameters
%%%===================================================================

%% Number of bits for node IDs (use 16 for testing, 160 for production)
-define(BIT_SIZE, 16).

%% K value: max nodes per bucket, also replication factor
-define(K, 8).

%% Alpha: number of parallel lookups
-define(ALPHA, 3).

%% Time between republishing data (in milliseconds)
-define(REPUBLISH_TIME, 60000).  % 60 seconds for testing

%%%===================================================================
%%% API Functions
%%%===================================================================

%% @doc Returns the configured bit size
bit_size() -> ?BIT_SIZE.

%% @doc Returns the K value
k_value() -> ?K.

%% @doc Returns the alpha value
alpha_value() -> ?ALPHA.

%% @doc Returns the republish time in ms
republish_time() -> ?REPUBLISH_TIME.

%% @doc Returns the maximum ID value
max_id() -> (1 bsl ?BIT_SIZE) - 1.

%% @doc Generate a random node ID
generate_id() ->
    rand:uniform(1 bsl ?BIT_SIZE) - 1.  % Random number from 0 to 2^BIT_SIZE - 1

%% @doc Generate ID from a seed (for reproducible tests)
generate_id(Seed) ->
    rand:seed(exsss, {Seed, Seed, Seed}),
    generate_id().

%% @doc Hash a key (string/atom) to get its ID
hash_key(Key) when is_atom(Key) ->
    hash_key(atom_to_list(Key));
hash_key(Key) when is_list(Key) ->
    %% Simple hash: sum of character codes, modulo max ID
    Hash = lists:foldl(fun(C, Acc) -> (Acc * 31 + C) rem (1 bsl ?BIT_SIZE) end, 0, Key),
    Hash;
hash_key(Key) when is_integer(Key) ->
    Key rem (1 bsl ?BIT_SIZE).

%% @doc Calculate XOR distance between two IDs
xor_distance(Id1, Id2) ->
    Id1 bxor Id2.

%% @doc Find which bucket index a node belongs to based on XOR distance
%% Returns 0 to BIT_SIZE-1
%% Bucket i contains nodes with distance 2^i <= d < 2^(i+1)
bucket_index(MyId, OtherId) when MyId =:= OtherId ->
    0;  % Same node, put in bucket 0
bucket_index(MyId, OtherId) ->
    Distance = xor_distance(MyId, OtherId),
    %% Calculate floor(log2(Distance)) but cap at BIT_SIZE-1
    Index = calc_log2(Distance),
    min(Index, ?BIT_SIZE - 1).

%% @doc Calculate floor of log2 for positive integers
calc_log2(0) -> 0;
calc_log2(N) when N > 0 ->
    calc_log2(N, 0).

calc_log2(1, Acc) -> Acc;
calc_log2(N, Acc) when N > 1 ->
    calc_log2(N bsr 1, Acc + 1).

%% @doc Generate a random ID that would fall into a specific bucket
%% Useful for refreshing buckets
random_id_in_bucket(MyId, BucketIndex) ->
    %% Generate random distance in range [2^BucketIndex, 2^(BucketIndex+1) - 1]
    MinDist = 1 bsl BucketIndex,
    MaxDist = (1 bsl (BucketIndex + 1)) - 1,
    Range = MaxDist - MinDist + 1,
    RandomDist = MinDist + rand:uniform(Range) - 1,
    (MyId bxor RandomDist) band max_id().  % Ensure result is within valid range
