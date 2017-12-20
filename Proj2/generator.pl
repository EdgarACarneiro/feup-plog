:- use_module(library(lists)).
:- use_module(library(random)).
:- include('solver.pl').

restrictVarsInSides([], _).
restrictVarsInSides([First | Rest], N) :-
  count(0, First, #=, N),
  restrictVarsInSides(Rest, N).

generateRandomBoard(Size, Board, Sides) :-
  sidesDomain(Size, Sides),
  Probability is 50,
  restrictVarsInSides(Sides, 50),

  solveBoard(Size, Board, Sides).