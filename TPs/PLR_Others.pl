:- use_module(library(clpfd)).
:- use_module(library(lists)).

% Count restriction
count_eq(_Val, [], 0).
count_eq(Val, [H | T], Cnt) :-
        count_eq(Val, T, Cnt2),
        Val #= H #<=> Bool,
        Cnt #= Cnt2 + Bool.

all_distinct_3by3([_A, _B]).
all_distinct_3by3([A,B,C|T]) :-
        all_distinct([A,B,C]),
        all_distinct_3by3([B,C|T]).

listEquals([], []).
listEquals([X1 | T1], [X2 | T2]) :-
        X1 #= X2,
        listEquals(T1, T2).

numOcurrences(L, Seq, 0) :-
        length(L, LenL),
        length(Seq, LenSeq),
        LenL < LenSeq.
numOcurrences([First | Rest], Seq, Cnt) :-
        append(Head, _Tail, [First | Rest]),
        length(Seq, LenSeq),
        length(Head, LenSeq),
        listEquals(Head, Seq) #<=> Bool,
        numOcurrences(Rest, Seq, Cnt2),
        Cnt #= Cnt2 + Bool.
        

% 12 Carros
% Cores:        Azul - 1 ; Verde - 2 ; Amarelo - 3 ; Preto - 4.
filaDe12Carros(Cores) :-
        length(Cores, 12),
        domain(Cores, 1, 12),
        
        % Restrictions
        global_cardinality(Cores, [1-3, 2-4, 3-2, 4-3]),
        
        % Cor primeiro = Cor ultimo
        element(1, Cores, CorPrimUlt),
        element(12, Cores, CorPrimUlt),
        
        % Cor nth2 = Cor nth11
        element(2, Cores, CorPrimUlt),
        element(11, Cores, CorPrimUlt),
        
        % Quinto carro e azul
        element(5, Cores, 1),
        
        % Subsequencias de 3 carros sao diferentes
        all_distinct_3by3(Cores),
        
        % Sequencia especifica so ocorre uma vez
        numOcurrences(Cores, [2, 3, 4, 1], 1),

        % Labeling
        labeling([], Cores).