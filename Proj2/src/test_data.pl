:- use_module(library(system)).

/**  Test Boards  **/
%% https://www.brainbashers.com/skyscrapers.asp

%% 6 by 6 board -- given example -- 166199 backtracks, 6311 with ffc
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

%% 8 by 8 board
testRestrictions4([
  [0, 0, 5, 3, 0, 2, 0, 4],
  [3, 3, 0, 3, 0, 3, 0, 0],
  [2, 4, 0, 0, 4, 0, 0, 0],
  [0, 0, 2, 0, 4, 4, 0, 1]
]).

board4([
  [_, _, _, 3, _, _, _, _],
  [_, _, _, _, _, _, 8, 2],
  [_, _, _, 5, _, _, _, _],
  [2, _, _, _, _, _, _, _],
  [_, _, 3, _, _, _, _, _],
  [1, _, 5, _, _, _, _, _],
  [_, _, _, 6, _, 2, _, _],
  [_, _, _, _, _, _, _, _]
]).

%% 6 by 6 board
testRestrictions5([
  [0, 1, 3, 0, 0, 0],
  [0, 0, 4, 2, 2, 0],
  [3, 0, 0, 0, 0, 0],
  [3, 3, 2, 0, 2, 4]
]).

board5([
  [1, _, _, _, _, _],
  [_, 4, _, _, _, _],
  [_, _, _, _, _, _],
  [_, _, 2, _, _, _],
  [_, _, _, _, _, _],
  [_, _, _, _, _, _]
]).

%% 7 by 7 board
testRestrictions6([
  [0, 0, 0, 0, 0, 3, 4],
  [0, 0, 4, 2, 0, 0, 5],
  [0, 2, 0, 2, 0, 4, 0],
  [0, 0, 0, 2, 5, 2, 0]
]).

board6([
  [_, _, _, _, _, _, _],
  [4, 3, _, _, _, _, _],
  [2, _, _, _, _, _, 1],
  [_, _, _, _, _, _, _],
  [_, _, _, _, _, _, _],
  [_, _, 1, _, _, _, _],
  [_, _, _, 3, _, _, _]
]).

% 8 by 8 board
testRestrictions7([
  [0, 0, 5, 3, 0, 2, 0, 4],
  [3, 3, 0, 3, 0, 3, 0, 0],
  [2, 4, 0, 0, 4, 0, 0, 0],
  [0, 0, 2, 0, 4, 4, 0, 1]
]).

board7([
  [_, _, _, 3, _, _, _, _],
  [_, _, _, _, _, _, 8, 2],
  [_, _, _, 5, _, _, _, _],
  [2, _, _, _, _, _, _, _],
  [_, _, 3, _, _, _, _, _],
  [1, _, 5, _, _, _, _, _],
  [_, _, _, 6, _, 2, _, _],
  [_, _, _, _, _, _, _, _]
]).

% Functions to get duration time
reset_timer:- statistics(walltime, _).
print_time:-
  statistics(walltime, [_, T]),
  TS is ((T/10)*10)/1000,
  nl, write('Time: '), write(TS), write('s'), nl.

% Print stats for the given restricitons and the given, if HasBoard is 'yes'
% Otherwise only Restrictions are used and board starts empty
% getTestStats(+Restrictions, +HasBoard, +Board)
getTestStats(Restrictions, no, _):-
  call(Restrictions, R),
  R = [Up | _Rest],
  length(Up, Size),
  reset_timer,
  solveBoard(Size, _B, R),
  write('Solved for Restrictions: '), write(Restrictions), nl,
  print_time, fd_statistics, nl.

getTestStats(Restrictions, yes, Board):-
  call(Restrictions, R),
  R = [Up | _Rest],
  length(Up, Size),
  call(Board, B),
  reset_timer,
  solveBoard(Size, B, R),
  write('Solved for Restriction: '), write(Restrictions), nl,
  write('Solved for Board: '), write(Board), nl,
  print_time, fd_statistics, nl.

%Prints the stats of each test
getAllTestsStats:-
  getTestStats(testRestrictions, no, _),
  getTestStats(testRestrictions2, no, _),
  getTestStats(testRestrictions3, no, _),
  getTestStats(testRestrictions4, yes, board4),
  getTestStats(testRestrictions5, yes, board5),
  getTestStats(testRestrictions6, yes, board6),
  getTestStats(testRestrictions7, yes, board7).