-module(control).
-export([for/3]).

% usage sample: control:for(1,10,fun(I)-> I*I end).
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)].