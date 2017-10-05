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

% Ex. 4 - Fatorial e Fibonacci
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

% Modulo function
mod(_, 1, 0).
mod(N, N, 0).
mod(N, Div, R) :-
        Tmp is N // Div,
        R is N - (Div * Tmp).

hasFactor(N, F) :-
        mod(N, F, 0).
hasFactor(N, F) :-
        F * F < N,
        F2 is F + 2,
        hasFactor(N,F2).

% Ex. 5 - Numeros Primos
isPrime(2).
isPrime(3).
isPrime(P) :-
        integer(P),
        P > 3,
        \+mod(P, 2, 0),
        \+hasFactor(P,3).



