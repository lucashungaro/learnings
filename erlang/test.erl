-module(test). 
-compile(export_all). 

start(Atom, Fun) -> 
    register(Atom, spawn(fun loop/0)). 

loop() ->
    receive
        _ ->
             void
    end.
