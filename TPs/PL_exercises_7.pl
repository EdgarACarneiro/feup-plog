% Exercicio 5. Problema dos 2 Baldes

move(s(X,Y),s(Z,4)) :- Z is X - (4 - Y), Z >= 0.
move(s(X,Y),s(Z,0)) :- Z is X + Y, Z =< 3.
move(s(X,Y),s(3,Z)) :- Z is Y - (3 - X), Z >=0.
move(s(X,Y),s(0,Z)) :- Z is X + Y, Z =< 4.

move(s(0,Y),s(3,Y)).
move(s(X,0),s(X,4)).
move(s(X,Y),s(X,0)) :- Y > 0.
move(s(X,Y),s(0,Y)) :- X > 0.

moves(Xs) :- moves([s(0,0)],Xs).
moves([s(X0,Y0)|T], [s(X1,2),s(X0,Y0)|T])
    :- move(s(X0,Y0),s(X1,2)), !.
moves([s(X0,Y0)|T],Xs) :-
    move(s(X0,Y0),s(X1,Y1)), 
    \+ member(s(X1,Y1),[s(X0,Y0)|T]),
    moves([s(X1,Y1),s(X0,Y0)|T],Xs).


% Exercicio 10. Os Degraus da Casa do Paulo
salta_degraus(1).
salta_degraus(2).

casa_degraus(Degraus, L) :-
    salta_degraus(X),
    NDegraus is Degraus - X,
    NDegraus >= 0,
    casa_degraus(NDegraus, [X | L]).
casa_degraus(Degraus, _, _) :-
    Degraus =< 0.

jogo_escadas(Degraus, N, L) :-
    findall(Sol, casa_degraus(Degraus, Sol), L),
    length(L, N).

    
    