translate(0, T) :- T = ' '.
translate(1, T) :- T = 'O'. 

initialBoard([[1,0,0,0],
              [0,1,0,0],
              [0,0,1,0],
              [0,0,0,1]]).

printMatrix([]).
printMatrix([Line|M]) :-
        write('|'),
        printLine(Line),
        write('|'), nl,
        printMatrix(M).

printLine([]).
printLine([Head|Tail]) :-
        translate(Head, T),
        write(T),
        write(' '),
        printLine(Tail).

% Ex. LIST 3
appendList([], L, L).
appendList([X|L1], L2, [X|L3]) :-
        appendList(L1, L2, L3).

% Ex. LIST 4
inverter(Lista,InvLista) :-
        rev(Lista, [], InvLista).
rev([], InvLista, InvLista).
rev([X|L1], L2, Inv) :-
        rev(L1, [X|L2], Inv).

% Ex. LIST 5
% 5.a)
/*
membro(M, [M|_]).
membro(M, [_|L]) :- membro(M,L).
*/

% 5.b)
membro(M, L) :- append(_, [M|_], L). 

% 5.c)
ultimo(L, X) :- append(_, [X], L).

% 5.d)
nth_member(0, M, [M|_]).
nth_member(N, M, [_|L]) :-
        N > 0, N1 is N - 1,
        nth_member(N1, M, L).

/* Ex.6 */
delete_one(X, L1, L2) :- 
        append(Prevlist,[X|Afterlist],L1),
        append(Prevlist, Afterlist, L2).

delete_all(X, L1, L2) :-
        delete_one(X, L1, R),
        delete_all(X, R, L2).

delete_all(X, L, L).

delete_all_list(LX, L1, L2) :-
        LX = [H|T],
        delete_all(H, L1, R),
        delete_all_list(T, R, L2).

delete_all_list([], L, L).

% 7.a)
before(A,B,L) :-
        append(_, [A|L1], L),
        append(_, [B|_], L1).

/* Ex.8 */
conta(Lista, N) :-
        contagem(Lista, 0, N).

contagem(Lista, Acc, N) :-
        Lista = [_| T],
        Acc1 is (Acc + 1),
        contagem(T, Acc1, N).

contagem([], N, N).

/* Ex. 8.b) */
/*
conta_elem(X, Lista, N) :-
                contagem_elem(X, Lista, 0, N).
contagem_elem(X, Lista, Acc, N) :-
        Lista = [H|T],
        H == X,
        Acc1 is (Acc + 1),
        contagem_elem(X, T, Acc1, N).
contagem_elem(X, Lista, Acc, N) :-
        Lista = [H|T],
        H =\= X,
        contagem_elem(X, T, Acc, N).
contagem_elem(_, [], N, N).
*/

% 8.b) - Meh...
conta_elem(_,[],0).
conta_elem(X, [X|L], N) :-
        N > 0, N1 is N - 1,
        conta_elem(X, L, N1).
conta_elem(X, [_|L], N) :-
        N > 0,
        conta_elem(X, L, N).


%10. a)
ordenada([Lista]).

ordenada([H1, H2]) :- H1 =< H2.

ordenada([H1, H2|Lista]) :-
           H1 =< H2,
           Lista = [H3|T],
           ordenada([H2, H3 | T]).

ordenada(H1, H2 ,[]) :- H2 >= H1.