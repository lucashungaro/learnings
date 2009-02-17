-module(fib).
-import(control, [for/3]).
-export([fib_at/1]).
-export([fib_up_to_pos/1]).

fib_at(N) when N < 2 -> N;
fib_at(N) -> fib_at(N-1) + fib_at(N-2).

fib_up_to_pos(N) -> for(1, N, fun fib_at/1).