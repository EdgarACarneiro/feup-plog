:- use_module(library(lists)).
:- use_module(library(random)).
:- include('solver.pl').

% Restrict the number of constraints shown , according to the probability
restrictVarsInSides([], _).
restrictVarsInSides([First | Rest], Probability) :-
  restrictVarsInRow(First, Probability),
  restrictVarsInSides(Rest, Probability).

% Restrict the number of constraints shown , according to the probability
restrictVarsInRow([], _).
restrictVarsInRow([V1 | Vars], Probability) :-
  maybe(Probability), !,
  V1 #\= 0,
  restrictVarsInRow(Vars, Probability).
restrictVarsInRow([_ | Vars], Probability) :-
  restrictVarsInRow(Vars, Probability).

/**
 * Generates boards of differente difficulty according the given probability.
 * +The probability will control the amount of Restrictions that are shown.
 */
generateBoardEasy(Size, Board, Sides) :-
  generateBoard(Size, Board, Sides, 0.95).
generateBoardMedium(Size, Board, Sides) :-
  generateBoard(Size, Board, Sides, 0.8).
generateBoardHard(Size, Board, Sides) :-
  generateBoard(Size, Board, Sides, 0.65).
generateBoard(Size, Board, Sides, Probability) :-
  setrand(100),
  length(Sides, 4),
  sidesDomain(Size, Sides),
  restrictVarsInSides(Sides, Probability),

  solveBoard(Size, Board, Sides).

% Size = 8, generateBoardEasy(Size, B, S), printBoard(B, S), solveBoard(Size, SB, S), printBoard(SB, S).
% Size = 8, generateRandomBoard(Size, Board, Sides), printBoard(Board, Sides), solveBoard(Size, SameBoard, Sides), printBoard(SameBoard, Sides), Board = SameBoard.
% Size = 6, generateRandomBoard(Size, Board, Sides), printBoard(Board, Sides), solveBoard(Size, SameBoard, Sides), Board = SameBoard.
