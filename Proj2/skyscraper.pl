:- include('solver.pl').

% fd_statistics(backtracks, B).
% testRestrictions(R), solveBoard(R, B), printBoard(B).

skyscraper :-
  write('Skyscraper!'), nl,
  testRestrictions(R),
  solveBoard(R, B),
  printBoard(B).

printBoard([]).
printBoard([Row | Board]) :-
  printRow(Row), nl,
  printBoard(Board).
printRow([]).
printRow([El | Row]) :-
  write(El), write(' '),
  printRow(Row).


/**  Test Boards  **/
testRestrictions([
  [5, 0, 0, 2, 2, 0],
  [0, 2, 3, 4, 0, 0],
  [0, 3, 4, 0, 0, 4],
  [0, 0, 4, 3, 2, 0]
]).