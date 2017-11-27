:- use_module(library(clpfd)).
:- use_module(library(lists)).

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


% IPLR 4 Problema dos Criptogramas
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







