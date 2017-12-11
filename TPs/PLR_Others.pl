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


% Consumo de eletrodomesticos
% Duracao de cada; Consumo de cada; Consumo maximo
% Tasks is [task(S1, 16, E1, 2, 1), task(S2, 6, E2, 9, 2), task(S3, 13, E3, 3, 3), task(S4, 7, E4, 7, 4), task(S5, 5, E5, 10, 5), task(S6, 18, E6, 1, 6), task(S7, 4, E7, 11, 7)].
eletrodomesticos(Tasks, Max) :-
        endings(LEnds, Tasks),
        starts(LStarts, Tasks),
        domain(LEnds, 1, 60),
        domain(LStarts, 0, 50),

        /* 
        endingsDomain(Tasks, 1, 60, LEnds),
        startsDomain(Tasks, 1, 50, _),
        */
        cumulative(Tasks, [limit(Max)]),
        maximum(MaxEnd, LEnds),
        
        labeling([minimize(MaxEnd)], LEnds).

endingsDomain([], _, _, []).
endingsDomain([task(_, _, End, _, _) | Rest], Min, Max, [End | RestEnds]) :-
        End #> Min, End #< Max,
        endingsDomain(Rest, Min, Max, RestEnds).

startsDomain([], _, _, []).
startsDomain([task(Start, _, _, _, _) | Rest], Min, Max, [Start | RestStarts]) :-
        Start #> Min, Start #< Max,
        endingsDomain(Rest, Min, Max, RestStarts).

endings([End | LEnds], [task(_, _, End, _, _) | LTasks]) :-
        endings(LEnds, LTasks).
endings([], _).

starts([Start | LStarts], [task(Start, _, _, _, _) | LTasks]) :-
        endings(LStarts, LTasks).
stsarts([], _).