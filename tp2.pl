% Ex. 3 
exec(X,Y) :- p(X,Y).
exec(X,X) :- s(X).
p(X,Y) :- q(X), r(Y).
p(X,Y) :- s(X), r(Y).
q(a).
q(b).
r(c).
r(d).
s(e).

% Ex. 4
factorial(0, 1).
/*
factorial(N, Value) :-
        N > 0,
        Previous is (N - 1),
        factorial(Previous, PreviousValue),
        Value is PreviousValue * N.
*/
factorial(N, V) :-
        factorialTail(N, 1, V).
factorialTail(1, V, V).
factorialTail(N, Acc, V) :-
        N > 1, N1 is N-1,
        Acc1 is Acc * N,
        factorialTail(N1, Acc1, V).

fibonacci(0,1).
fibonacci(1,1).
fibonacci(N, Fib) :-
        N > 1,
        N1 is N-1,
        N2 is N-2,
        fibonacci(N1, F1),
        fibonacci(N2, F2),
        Fib is (F1 + F2).

% Somatorio
sumatorio(N, V) :-
        sum(N, 1, V).
sum(1, V, V).
sum(N, Acc, V) :-
        N > 1,
        N1 is N-1,
        Acc1 is Acc + N,
        sum(N1, Acc1, V).
