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

% List Comprehensions
L = [1,2,3,4,5].
% doubles every element in the list
[2*X || X <- L ]. % means: the list of F(X) where X is taken from the list L.
map(F, L) -> [F(X) || X <- L].

% As filters:
[ X || {a, X} <- [{a,1},{b,2},{c,3},{a,4},{x,10},hello,"wow"]]. % [1,4]

pythag(N) -> 
	[ {A,B,C} || 
							A <- lists:seq(1,N), 
							B <- lists:seq(1,N), 
							C <- lists:seq(1,N), 
							A+B+C =< N, 
							A*A+B*B =:= C*C 
	].
%	Just a few words of explanation: lists:seq(1, N) retur ns a list of all the 
%	integers from 1 to N. Thus, A <- lists:seq(1, N) means that A takes all 
%	possible values from 1 to N. So our program reads, “Take all values of 
%	A from 1 to N, all values of B from 1 to N, and all values of C from 1 to N 
%	such that A + B + C is less than or equal to N and A*A + B*B = C*C.”

% Guards
max(X, Y) when X > Y -> X; 
max(X, Y) -> Y.

% In Guards, commas (,) are "and". Semicolons (;) are "or".

is_tuple(T), size(T) =:= 6, abs(element(3, T)) > 5 
element(4, X) =:= hd(L)

% The ﬁrst line means T is a tuple of six elements, and the absolute value 
% of the third element of T is greater than 5. The second line means that 
% element 4 of the tuple X is identical to the head of the list L. 









