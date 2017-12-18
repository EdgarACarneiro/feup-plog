:- use_module(library(clpfd)).
:- use_module(library(lists)).

/**
 * Input validation
 */
all_equal([_]).
all_equal([X, X | Rest]) :-
  all_equal([X | Rest]).

validateInput(Sides) :-
  maplist(length, Sides, Lengths),
  all_equal(Lengths).


/**
 * Domain restriction
 */
restrictBoardDomain([], _).
restrictBoardDomain([Row | Board], N) :-
  length(Row, N),
  domain(Row, 1, N),
  all_distinct(Row),
  restrictBoardDomain(Board, N).


/**
 * Apply left to right restrictions on a given Row.
 * +Num is the number of visible skyscrapers from LEFT to RIGHT in the given row.
 */
applyLeftToRightRestrictions(Num, Row) :-
  applyLeftToRightRestrictions(Num, Row, 0).
applyLeftToRightRestrictions(0, _, _).
applyLeftToRightRestrictions(Num, [El | Row], Max) :-
  Num > 0,
  El #> Max #<=> Flag,
  nextLRRestriction(Num, Row, Max, El, Flag).
nextLRRestriction(Num, Row, Max, _, 0) :-
  applyLeftToRightRestrictions(Num, Row, Max).
nextLRRestriction(Num, Row, _, El, 1) :-
  NewNum is Num - 1,
  applyLeftToRightRestrictions(NewNum, Row, El).

applyAllLRRestrictions([], []).
applyAllLRRestrictions([0 | Ls], [_ | Rows]) :-
  applyAllLRRestrictions(Ls, Rows).
applyAllLRRestrictions([L1 | Ls], [Row1 | Rows]) :-
  applyLeftToRightRestrictions(L1, Row1),
  applyAllLRRestrictions(Ls, Rows).

/**
 *  +Sides -> a list of lists, each of which represents the restrictions on the side of the board (number of visible buildings).
 *      -> in order: [UpRestrictions, LeftRestrictions, DownRestrictions, RightRestrictions]
 *      -> elements of list are in range [0,N], 0 meaning an undefined restriction
 *      -> elements correspond to restrictions in up->down (left/right lists) or left->right (up/down lists) order
 *  -Board -> a list of lists (a matrix)
 */
solveBoard(Sides, Board) :-
  Sides = [Up, Left, _Down, _Right],

  validateInput(Sides),
  length(Up, N),

  % Domain
  length(Board, N),
  restrictBoardDomain(Board, N),

  % For every ROW in the board, restrict according to Left/Right lists
  applyAllLRRestrictions(Left, Board),

  % For every COL in the board, restrict according to Up/Down lists


  % Other restrictions ?


  % labeling - applied on flattened board
  append(Board, FlatBoard),
  labeling([], FlatBoard).
