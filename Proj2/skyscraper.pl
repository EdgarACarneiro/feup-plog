:- include('solver.pl').
:- include('display.pl').
:- include('test_data').


% fd_statistics.
% testRestrictions4(R), solveBoard(R, B), printBoard(B).


skyscraper :-
  write('Skyscraper!'), nl,
  testRestrictions(R),
  solveBoard(R, B),
  printBoard(B, R).
