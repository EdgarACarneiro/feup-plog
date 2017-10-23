%% Ex. CN 3
dados(um).
dados(dois).
dados(tres).

% 3.a)
cut_teste_a(X) :-
      dados(X).
cut_teste_a('ultima_clausula').
% cut_teste_a(X), write(X), nl, fail.

% 3.b)
cut_teste_b(X) :-
      dados(X), !.
cut_teste_b('ultima_clausula').
% cut_teste_b(X), write(X), nl, fail.

% 3.c)
cut_teste_c(X,Y) :-
      dados(X),
      !,
      dados(Y).
cut_teste_c('ultima_clausula').
% cut_teste_c(X,Y), write(X-Y), nl, fail.


%% Ex. CN 4
max(X, Y, Z, X):- X>=Y, X>=Z, !.
max(_, Y, Z, Y):- Y>=Z, !.
max(_, _, Z, Z).

%% Ex. CN 5
unificavel([], _, []).
unificavel([Car1 | Cdr1], Termo, L2) :-
      \+ Car1 = Termo, !,
      unificavel(Cdr1, Termo, L2).
unificavel([Car1 | Cdr1], Termo, [Car1 | L2]) :-
      unificavel(Cdr1, Termo, L2).


