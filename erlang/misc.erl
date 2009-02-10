-module(misc).
-export ([for/3]).
% TUPLES
Tuple = {value, 2}.
AnotherTuple = {fruit, orange, 3}.

% LISTS
List = [1,2,3,4,5].
AnotherList = [{fruit, orange}, {value, 5}]. % [{fruit,orange},{value,5}]
ListOfLists = [List, AnotherList]. % [[1,2,3,4,5],[{fruit,orange},{value,5}]]
MixAndMatchList = [List, AnotherList, Tuple]. % [[1,2,3,4,5],[{fruit,orange},{value,5}],{value,2}]

% PATTERN MATCHING
% Success
{X,Y} = {1, 900}. % X == 1, Y == 900
{Z,_} = {atom, "string"}. % Z = atom, "sting" is ignored
[X,Y] = [1,2]. % X == 1, Y == 2
[H|T] = [1,2,3,4,5]. % H == 1, T == [2,3,4,5]
[H|T] = MixAndMatchList. % H == [1,2,3,4,5], T == [[{fruit,orange},{value,5}],{value,2}]

% Fail
{X,Y} = {333, atom, "cat"}.
{X,Y,X} = {{abc,12},42,true}
[X,Y] = [1,2,3,4,5].


% FUNS
Double = fun(X) -> 2*X end.
Even = fun(X) -> (X rem 2) =:= 0 end.

TempConvert = fun({c,C}) -> {f, 32 + C*9/5};
								 ({f,F}) -> {c, (F-32)*5/9}
							end.
							
% Funs as arguments
L = [1,2,3,4,5].
lists:map(Double, L).
lists:filter(Even, L).

% Functions that return funs
Fruit = [apple,pear,orange].
MakeTest = fun(L) -> (fun(X) -> lists:member(X, L) end) end.
IsFruit = MakeTest(Fruit).
lists:filter(IsFruit, [dog,orange,cat,apple,bear]).

% Control abstractions
for(Max, Max, F) -> [F(Max)];
for(I, Max, F) -> [F(I)|for(I+1, Max, F)]. 








