:- include('solver.pl').
:- include('display.pl').
:- include('test_data.pl').
:- include('generator.pl').

%Predicate to run the default puzzle
skyscraper :-
  nl, write('Skyscraper!'), nl, nl,
  testRestrictions4(R),
  board4(B),
  R = [Up | _Rest],
  length(Up, Size),
  solveBoard(Size, B, R),
  printBoard(B, R).
