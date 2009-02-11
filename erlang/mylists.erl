-module(mylists).
-compile(export_all).

sum([H|T]) -> H + sum(T);
sum([]) -> 0.

% the function below won't compile, 'cause it is already defined at the standard library
% just for reference
% map(_,[]) -> [].
% map(F, [H|T]) -> [F(H)|map(F,T)].