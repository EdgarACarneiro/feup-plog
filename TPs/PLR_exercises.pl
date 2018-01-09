:- use_module(library(clpfd)).
:- use_module(library(lists)).

%% DISCLAIMER :
% Some of these exercises are incomplete,
% take a look in /PLR\ Exercises/ for
% a better collection of solved exercises.


% General PrintBoard
printBoard(Matrix) :-
        length(Matrix, N),
        append(Matrix, Flat),
        printFlatBoard(Flat, N).

printFlatBoard(Board) :-
        length(Board, N),
        Side is integer(sqrt(N)),
        printFlatBoard(Board, Side).
printFlatBoard([], _Side).
printFlatBoard(Board, Side) :-
        length(L, Side),
        append(L, L2, Board),
        write(L), nl,
        printFlatBoard(L2, Side).

%% PLS 16
solve16(NumPeople) :-
  Vars = [NumBoth, NumDust, NumLiquid, NumNone, NumPeople],
  domain(Vars, 1, 10000),

  NumBoth #= 427,

  NumDust #< NumPeople, NumDust #> NumBoth,
  NumLiquid #< NumPeople, NumLiquid #> NumBoth,
  NumBoth #< NumPeople, NumBoth #> 0,
  NumNone #< NumPeople, NumNone #> 0,
  
  NumPeople #= NumDust + NumLiquid - NumBoth + NumNone,

  (NumLiquid + NumNone - NumBoth) * 3 #= NumPeople,
  (NumDust + NumNone - NumBoth) * 7 #= NumPeople * 2,
  NumNone * 5 #= NumPeople,

  labeling([ffc], Vars),
  write('NumNone: '), write(NumNone), nl,
  write('NumDust: '), write(NumDust), nl,
  write('NumLiquid: '), write(NumLiquid), nl,
  write('NumPeople: '), write(NumPeople), nl.
% R: 735 pessoas interrogadas na sondagem.

% IPLR 1. Problema do Quadrado Magico NxN
% [[A, B, C],
%  [D, E, F],
%  [G, H, I]]

magic3(Vars) :- % Solution for magic cube of size 3
        Vars = [A, B, C, D, E, F, G, H, I],
        domain(Vars, 1, 9),
        all_distinct(Vars),
        
        % Rows
        A + B + C #= Sum,
        D + E + F #= Sum,
        G + H + I #= Sum,
        
        % Columns
        A + D + G #= Sum,
        B + E + H #= Sum,
        C + F + I #= Sum,
        
        % Diagonals
        A + E + I #= Sum,
        G + E + C #= Sum,
        
        % Prevent Symmetrical Solutions
        A #< C,
        
        labeling([], Vars).

magicGeneric(Board, N) :- % Solution for magic cube of generic size, N
        NSquared is N * N,
        length(Board, NSquared),
        domain(Board, 1, NSquared),
        all_distinct(Board),
        
        % Apply Restraints
        restrainRows(Board, N),
        restrainColumns(Board, N),
        restrainDiagonals(Board, N),
                
        labeling([], Board).

restrainRows(_Board, _N). % TODO
restrainColumns(_Board, _N). % TODO
restrainDiagonals(_Board, _N). % TODO


% IPLR 4. Problema dos Criptogramas
puzzle(W1, W2, W3, D1, D2, D3) :-
        Vars = [D1, D2, D3],
        length(W1, Len), length(D1, Len),
        length(W2, Len), length(D2, Len),
        length(W3, Len), length(D3, Len),
        domain(D1, 1, 26),
        domain(D2, 1, 26),
        domain(D3, 1, 26),
        
        restrainSum(D1, D2, D3),
        
        labeling([], Vars).
     
restrainSum([], [], []).   
restrainSum([X1 | L1], [X2 | L2], [X3 | L3]) :-
        X3 #= X1 + X2,
        restrainSum(L1, L2, L3).   

% IPLR 7. Peru Assado
costOfTurkey(Cost) :-
        Vars = [T, U, Cost],
        T in 1 .. 9,
        U in 0 .. 9,
        U mod 2 #= 0, % Units digit is even
        Cost in 23 .. 134,
        T * 1000 + 670 + U #= Cost * 72,
        labeling([], Vars).
  
  
% IPLR 8. O Puto na Mercearia
putoMercearia(L):-
        %Valores todos em centimos
        L=[PArroz, PBat, PEspar, PAtum],
        domain(L, 0, 711),
        
        %Falta adcionar a restrição do preço de 2 ser multiplo de 10 -> proxima aula
        PArroz + PBat + PEspar + PAtum #= 711,
        PArroz * PBat * PEspar * PAtum #= 711000000,
        PBat #> PAtum,
        PAtum #> PArroz,
        PEspar #< PArroz,
        
        labeling([], L).


%% PLS Exercises

% PLS 5. A Fila de Carros
% Cores:        Azul - 1 ; Verde - 2 ; Amarelo - 3 ; Preto - 4.
% Tamanhos:     Small - 1 ; Medium - 2 ; Large - 3 ; XLarge - 4.

filaDeCarros(LColors, LSize) :-
        % Variables
        length(LColors, 4),
        length(LSize, 4),
        domain(LColors, 1, 4),
        domain(LSize, 1, 4),
        
        
        % Restrictions
        all_distinct(LColors),
        all_distinct(LSize),

        % Carro imediatamente antes do azul menor que o que esta imediatamente depois 
        element(IdxAzul, LColors, 1),
        IdxAzul in 2..3,
        IdxA1 #= IdxAzul - 1,
        IdxA2 #= IdxAzul + 1,
        element(IdxA1, LSize, SMenor),
        element(IdxA2, LSize, SMaior),
        SMenor #< SMaior,
        
        % Carro verde menor de todos
        element(IdxVerde, LColors, 2),
        element(IdxVerde, LSize, 1),
                
        % Verde depois do azul
        IdxVerde #> IdxAzul,
        
        % Amarelo depois do preto
        element(IdxAmarelo, LColors, 3),
        element(IdxPreto, LColors, 4),
        IdxAmarelo #> IdxPreto,
        
        % Find Solutions - Labeling should be executed only once, to prevent unnecessary backtracking
        append(LColors, LSize, LAnswer),
        labeling([], LAnswer).

%% PLS 16
solve16(NumPeople) :-
  Vars = [NumBoth, NumDust, NumLiquid, NumNone, NumPeople],
  domain(Vars, 1, 10000),

  NumBoth #= 427,

  NumDust #< NumPeople, NumDust #> NumBoth,
  NumLiquid #< NumPeople, NumLiquid #> NumBoth,
  NumBoth #< NumPeople, NumBoth #> 0,
  NumNone #< NumPeople, NumNone #> 0,
  
  NumPeople #= NumDust + NumLiquid - NumBoth + NumNone,

  (NumLiquid + NumNone - NumBoth) * 3 #= NumPeople,
  (NumDust + NumNone - NumBoth) * 7 #= NumPeople * 2,
  NumNone * 5 #= NumPeople,

  labeling([ffc], Vars),
  write('NumNone: '), write(NumNone), nl,
  write('NumDust: '), write(NumDust), nl,
  write('NumLiquid: '), write(NumLiquid), nl,
  write('NumPeople: '), write(NumPeople), nl.
% R: 735 pessoas interrogadas na sondagem.

%% Programacao em Logica Com Restricoes - Problemas de Optimizacao
% PLOP 2: O Carteiro Preguicoso
timeTaken([_El], 0).
timeTaken([First, Second | Rest], Time) :-
        Time #= T2 + abs(Second - First),
        timeTaken([Second | Rest], T2).
        
carteiroPreguicoso(Path, Time) :-
        length(Path, 10),
        domain(Path, 1, 10),
        all_distinct(Path),
        element(10, Path, 6),
        
        timeTaken(Path, Time),
        
        labeling([maximize(Time)], Path).
