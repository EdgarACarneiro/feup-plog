:- include('solver.pl').
:- include('display.pl').
:- include('test_data.pl').


% fd_statistics.
% testRestrictions4(R), solveBoard(R, B), printBoard(B).


skyscraper :-
  write('Skyscraper!'), nl,
  testRestrictions(R),
  R = [Up | _Rest],
  length(Up, Size),
  solveBoard(Size, B, R),
  printBoard(B, R).
