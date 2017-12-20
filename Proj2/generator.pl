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
  setrand(200),
  sidesDomain(Size, Sides),
  Probability = 0.5,
  restrictVarsInSides(Sides, Probability),

  solveBoard(Size, Board, Sides).
