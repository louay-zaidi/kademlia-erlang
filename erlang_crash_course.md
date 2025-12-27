# Erlang Crash Course for Kademlia Project

## 1. Basic Data Types

### Numbers
```erlang
42          % integer
3.14        % float
16#FF       % hexadecimal (255)
2#1010      % binary (10)
```

### Atoms
Atoms are constants. They start with lowercase or are in single quotes.
```erlang
ok
error
hello_world
'Hello World'
```

### Tuples
Fixed-size containers. Use curly braces.
```erlang
{ok, 42}
{error, not_found}
{node, 123, "<0.45.0>"}
{key, value}
```

### Lists
Variable-size containers. Use square brackets.
```erlang
[1, 2, 3, 4]
["hello", "world"]
[{a, 1}, {b, 2}]    % list of tuples (like a dictionary)
```

### Maps (Like dictionaries/hashmaps)
```erlang
#{name => "Alice", age => 25}
#{id => 123, data => "hello"}
```

---

## 2. Variables

Variables start with UPPERCASE. They can only be assigned ONCE!

```erlang
X = 5.          % X is now 5
Y = X + 3.      % Y is now 8
X = 10.         % ERROR! Can't reassign X
```

Underscore `_` ignores a value:
```erlang
{ok, _} = {ok, 42}.    % We don't care about 42
```

---

## 3. Pattern Matching

This is Erlang's superpower. The `=` sign matches patterns.

```erlang
% Extract values from tuples
{Status, Value} = {ok, 42}.
% Now Status = ok, Value = 42

% Extract from lists
[First | Rest] = [1, 2, 3, 4].
% Now First = 1, Rest = [2, 3, 4]

% Match specific patterns
{ok, Result} = {ok, "hello"}.
% Works! Result = "hello"

{ok, Result} = {error, "oops"}.
% CRASH! Patterns don't match
```

---

## 4. Functions

Functions are defined in modules. Create a file `math_example.erl`:

```erlang
-module(math_example).          % Module name (must match filename)
-export([add/2, factorial/1]).  % Functions to export (name/arity)

add(A, B) ->
    A + B.

factorial(0) -> 1;              % Pattern matching in function heads!
factorial(N) -> N * factorial(N - 1).
```

To use:
```erlang
1> c(math_example).             % Compile the module
2> math_example:add(3, 4).      % Call function: 7
3> math_example:factorial(5).   % Result: 120
```

---

## 5. Anonymous Functions (Lambdas)

```erlang
Add = fun(A, B) -> A + B end.
Add(3, 4).    % Returns 7

% Used often with lists:
lists:map(fun(X) -> X * 2 end, [1, 2, 3]).    % Returns [2, 4, 6]
lists:filter(fun(X) -> X > 2 end, [1, 2, 3, 4]).  % Returns [3, 4]
```

---

## 6. Case Expressions

```erlang
case SomeValue of
    {ok, Result} ->
        io:format("Success: ~p~n", [Result]),
        Result;
    {error, Reason} ->
        io:format("Error: ~p~n", [Reason]),
        error;
    _ ->
        io:format("Unknown~n"),
        unknown
end.
```

---

## 7. If Expressions

```erlang
if
    X > 10 -> big;
    X > 5  -> medium;
    true   -> small    % 'true' is like 'else'
end.
```

---

## 8. List Operations

```erlang
% Add to front of list
[1 | [2, 3, 4]].           % Result: [1, 2, 3, 4]

% Concatenate lists
[1, 2] ++ [3, 4].          % Result: [1, 2, 3, 4]

% List comprehensions
[X * 2 || X <- [1, 2, 3]].                    % Result: [2, 4, 6]
[X || X <- [1, 2, 3, 4], X > 2].              % Result: [3, 4]
[{X, Y} || X <- [1, 2], Y <- [a, b]].         % Result: [{1,a},{1,b},{2,a},{2,b}]

% Useful functions
lists:sort([3, 1, 2]).             % [1, 2, 3]
lists:reverse([1, 2, 3]).          % [3, 2, 1]
lists:nth(2, [a, b, c]).           % b (1-indexed!)
lists:member(b, [a, b, c]).        % true
length([1, 2, 3]).                 % 3
```

---

## 9. PROCESSES (Most Important for Kademlia!)

Erlang processes are lightweight "actors". Each Kademlia node will be a process.

### Spawning a Process

```erlang
% spawn(Module, Function, Arguments)
Pid = spawn(my_module, my_function, [Arg1, Arg2]).

% spawn with anonymous function
Pid = spawn(fun() -> io:format("Hello from process!~n") end).
```

### Sending Messages

```erlang
Pid ! {hello, "world"}.           % Send message to process
Pid ! {store, Key, Value}.        % Any term can be a message
```

### Receiving Messages

```erlang
receive
    {hello, Name} ->
        io:format("Hello ~s~n", [Name]);
    {ping, From} ->
        From ! pong;
    stop ->
        io:format("Stopping~n")
after 5000 ->                      % Timeout after 5000ms
    io:format("No message received~n")
end.
```

### Process Loop Pattern (CRITICAL!)

This is how you make a process that stays alive:

```erlang
-module(counter).
-export([start/0, loop/1]).

start() ->
    spawn(counter, loop, [0]).    % Start with count = 0

loop(Count) ->
    receive
        {get, From} ->
            From ! {count, Count},
            loop(Count);          % Continue with same count
        increment ->
            loop(Count + 1);      % Continue with count + 1
        stop ->
            ok                    % Don't call loop = process dies
    end.
```

Usage:
```erlang
1> Pid = counter:start().
2> Pid ! increment.
3> Pid ! increment.
4> Pid ! {get, self()}.     % self() is current process's PID
5> flush().                  % Shows received messages: {count, 2}
```

---

## 10. Bit Operations (Essential for Kademlia!)

Kademlia uses XOR distance. Erlang has built-in bit operations:

```erlang
5 bxor 3.      % XOR: 5 (101) xor 3 (011) = 6 (110)
5 band 3.      % AND: 5 (101) and 3 (011) = 1 (001)
5 bor 3.       % OR:  5 (101) or  3 (011) = 7 (111)
5 bsl 2.       % Shift left: 5 << 2 = 20
5 bsr 2.       % Shift right: 5 >> 2 = 1
```

XOR Distance example:
```erlang
xor_distance(Id1, Id2) -> Id1 bxor Id2.

xor_distance(5, 3).    % Returns 6
xor_distance(5, 5).    % Returns 0 (same node = distance 0)
xor_distance(5, 4).    % Returns 1 (very close!)
```

---

## 11. Maps Operations

```erlang
% Create
M = #{key1 => value1, key2 => value2}.

% Get value
maps:get(key1, M).                    % Returns value1
maps:get(key3, M, default).           % Returns default if not found

% Update/Add
M2 = M#{key1 => new_value}.           % Update existing
M3 = M#{key3 => value3}.              % Add new

% Remove
M4 = maps:remove(key1, M).

% Check if key exists
maps:is_key(key1, M).                 % true

% Get all keys/values
maps:keys(M).                         % [key1, key2]
maps:values(M).                       % [value1, value2]

% Convert to list
maps:to_list(M).                      % [{key1,value1}, {key2,value2}]
```

---

## 12. Useful Built-in Functions

```erlang
self().                    % Get current process's PID
is_pid(X).                 % Check if X is a PID
is_integer(X).             % Check if X is an integer

erlang:system_time(millisecond).  % Current time in ms

io:format("Hello ~p~n", [Value]).  % Print with newline
                                    % ~p = any term
                                    % ~s = string
                                    % ~w = raw term
                                    % ~n = newline

random:uniform(100).       % Random number 1-100 (old)
rand:uniform(100).         % Random number 1-100 (new)
```

---

## 13. Compiling and Running

### Method 1: From Erlang Shell
```erlang
1> c(my_module).           % Compiles my_module.erl
2> my_module:my_function().
```

### Method 2: From Command Line
```bash
erlc my_module.erl         # Compile to .beam file
erl -noshell -s my_module my_function -s init stop
```

---

## Quick Reference for Kademlia

You'll need these patterns constantly:

```erlang
% Process that handles messages
loop(State) ->
    receive
        {ping, From} ->
            From ! {pong, self()},
            loop(State);
        {store, Key, Value} ->
            NewState = store_value(State, Key, Value),
            loop(NewState);
        {find_node, TargetId, From} ->
            Closest = find_closest(State, TargetId),
            From ! {nodes, Closest},
            loop(State)
    end.

% XOR distance
distance(Id1, Id2) -> Id1 bxor Id2.

% Find bucket index (which k-bucket a node belongs to)
bucket_index(MyId, OtherId) ->
    Distance = MyId bxor OtherId,
    math:floor(math:log2(Distance)).
```

---

## Practice Exercise

Create a file `practice.erl` and implement:

```erlang
-module(practice).
-export([start/0, loop/1]).

% A simple key-value store process

start() ->
    spawn(practice, loop, [#{}]).  % Start with empty map

loop(Store) ->
    receive
        {put, Key, Value, From} ->
            NewStore = Store#{Key => Value},
            From ! ok,
            loop(NewStore);
        {get, Key, From} ->
            case maps:get(Key, Store, not_found) of
                not_found -> From ! {error, not_found};
                Value -> From ! {ok, Value}
            end,
            loop(Store);
        stop ->
            io:format("Stopping~n")
    end.
```

Test it:
```erlang
1> c(practice).
2> Pid = practice:start().
3> Pid ! {put, name, "Alice", self()}.
4> flush().
5> Pid ! {get, name, self()}.
6> flush().
```
