/* TP3 - 09/10/2017 */

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

/* Ex.8 */
conta(Lista, N) :-
        contagem(Lista, 0, N).

contagem(Lista, Acc, N) :-
        Lista = [_| T],
        Acc1 is (Acc + 1),
        contagem(T, Acc1, N).

contagem([], N, N).

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

contagem_elem(X, [], N, N).