:- include('solver.pl').

% fd_statistics(backtracks, B).
% testRestrictions4(R), solveBoard(R, B), printBoard(B).


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
%% 6 by 6 board -- given example
testRestrictions([
  [5, 0, 0, 2, 2, 0],
  [0, 2, 3, 4, 0, 0],
  [0, 3, 4, 0, 0, 4],
  [0, 0, 4, 3, 2, 0]
]).

%% 5 by 5 board
testRestrictions2([
  [4, 0, 1, 2, 3],
  [0, 2, 0, 4, 0],
  [0, 0, 4, 0, 0],
  [0, 0, 0, 0, 2]
]).

%% 4 by 4 board
testRestrictions3([
  [4, 0, 0, 2],
  [0, 3, 0, 0],
  [0, 0, 4, 0],
  [0, 0, 0, 3]
]).

%% 7 by 7 board -- multiple solutions, rip
testRestrictions4([
  [5, 0, 0, 2, 2, 0, 0],
  [0, 2, 3, 4, 0, 0, 0],
  [0, 3, 4, 0, 0, 4, 0],
  [0, 0, 4, 3, 2, 2, 0]
]).

