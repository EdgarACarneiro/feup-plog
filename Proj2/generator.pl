:- use_module(library(lists)).
:- use_module(library(random)).
:- include('solver.pl').
:- include('display.pl').

restrictVarsInSides([], _).
restrictVarsInSides([First | Rest], Probability) :-
  restrictVarsInRow(First, Probability),
  restrictVarsInSides(Rest, Probability).

restrictVarsInRow([], _).
restrictVarsInRow([V1 | Vars], Probability) :-
  maybe(Probability), !,
  V1 #\= 0,
  restrictVarsInRow(Vars, Probability).
restrictVarsInRow([_ | Vars], Probability) :-
  restrictVarsInRow(Vars, Probability).

generateRandomBoard(Size, Board, Sides) :-
  setrand(100),
  length(Sides, 4),
  sidesDomain(Size, Sides),
  Probability = 0.70,
  restrictVarsInSides(Sides, Probability),

  solveBoard(Size, Board, Sides).

/*
generateRandomBoardUniqueSol(Size, Board, Sides) :-
  setrand(200),
  length(Sides, 4),
  sidesDomain(Size, Sides),
  Probability = 0.95,
  restrictVarsInSides(Sides, Probability),
  findall(B, solveBoard(Size, B, Sides), [Board]).
*/
  %% badbadnotgood backtracking

% Size = 8, generateRandomBoard(Size, Board, Sides), printBoard(Board, Sides), solveBoard(Size, SameBoard, Sides), printBoard(SameBoard, Sides), Board = SameBoard.
% Size = 8, generateRandomBoard(Size, Board, Sides), printBoard(Board, Sides), solveBoard(Size, SameBoard, Sides), Board = SameBoard.
