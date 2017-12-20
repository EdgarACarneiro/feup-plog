/**  Test Boards  **/
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
