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

magic3(Vars) :-
        Vars = [A, B, C, D, E, F, G, H, I],
        domain(Vars, 1, 9),
        all_distinct(Vars),
        
        % Columns
        A + D + G #= Sum,
        B + E + H #= Sum,
        C + F + I #= Sum,
        
        % Rows
        A + B + C #= Sum,
        D + E + F #= Sum,
        G + H + I #= Sum,
        
        % Diagonals
        A + E + I #= Sum,
        G + E + C #= Sum,
        
        labeling([], Vars).
  
