-module(area_server0).
-export([loop/0]).

loop() ->
    receive
	{rectangle, Width, Ht} ->
	    io:format("Area of the rectangle is ~p~n", [Width * Ht]),
            loop();
        {circle, R} ->
            io:format("Area of the circle is ~p~n", [3.14159 * R * R]),
            loop();
        Other ->
            io:format("Unknow shape ~p~n", [Other]),
            loop()
    end.

