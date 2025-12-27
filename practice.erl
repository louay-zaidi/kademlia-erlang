-module(practice).
-export([start/0, loop/1]).

start() ->
    spawn(practice, loop, [#{}]).

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