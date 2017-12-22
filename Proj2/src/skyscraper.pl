:- include('solver.pl').
:- include('display.pl').
:- include('test_data.pl').
:- include('generator.pl').


% fd_statistics.
% testRestrictions4(R), solveBoard(R, B), printBoard(B).


skyscraper :-
  nl, write('Skyscraper!'), nl, nl,
  testRestrictions(R),
  R = [Up | _Rest],
  length(Up, Size),
  solveBoard(Size, B, R),
  printBoard(B, R).