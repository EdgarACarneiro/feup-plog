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
 * Apply LEFT to RIGHT restrictions.
 * +Num is the number of visible skyscrapers from LEFT to RIGHT in the given row.
 */
applyLeft(Num, Row) :-
  applyLeft(Num, Row, 0).
applyLeft(0, [], _).
applyLeft(Num, [El | Row], Max) :-
  NewNum #>= 0,
  (El #> Max #/\ NewMax #= El #/\ NewNum #= Num - 1) #\/
  (El #=< Max #/\ NewMax #= Max #/\ NewNum #= Num),
  applyLeft(NewNum, Row, NewMax).

applyAllLeftRestrictions([], []).
applyAllLeftRestrictions([0 | Ls], [_ | Rows]) :-
  applyAllLeftRestrictions(Ls, Rows).
applyAllLeftRestrictions([L1 | Ls], [Row1 | Rows]) :-
  applyLeft(L1, Row1),
  applyAllLeftRestrictions(Ls, Rows).


%% RIGHT to LEFT
applyRight(Num, Row) :-
  applyRight(Num, Row, 0).
applyRight(0, [], _).
applyRight(Num, Row, Max) :-
  append(LeftRow, [El], Row),
  NewNum #>= 0,
  (El #> Max #/\ NewMax #= El #/\ NewNum #= Num - 1) #\/
  (El #=< Max #/\ NewMax #= Max #/\ NewNum #= Num),
  applyRight(NewNum, LeftRow, NewMax).

applyAllRightRestrictions([], []).
applyAllRightRestrictions([0 | Ls], [_ | Rows]) :-
  applyAllRightRestrictions(Ls, Rows).
applyAllRightRestrictions([L1 | Ls], [Row1 | Rows]) :-
  applyRight(L1, Row1),
  applyAllRightRestrictions(Ls, Rows).

%% TOP to BOTTOM
applyAllTopRestrictions(Restrictions, Board) :-
  applyAllTopRestrictions(Restrictions, Board, 1).
applyAllTopRestrictions([0 | Ls], Board, N) :-
  NewN is N + 1,
  applyAllTopRestrictions(Ls, Board, NewN).
applyAllTopRestrictions([L1 | Ls], Board, N) :-
  getBoardCol(Board, N, Col),
  applyLeft(L1, Col),
  NewN is N + 1,
  applyAllTopRestrictions(Ls, Board, NewN).
applyAllTopRestrictions([], [], _).



getBoardCol([], _, []).
getBoardCol([Row | Board], N, [El | Col]) :-
  element(N, Row, El),
  getBoardCol(Board, N, Col).

%% TODO generalize all applyXToY functions with a getElement(Side, Row, Element, RestOfRow) in which Side is one of [top, left, bottom, right]
%% Need to generalize applyAllXToY at the same time, or at least make it compatible with changes

/**
 *  +Sides -> a list of lists, each of which represents the restrictions on the side of the board (number of visible buildings).
 *      -> in order: [TopRestrictions, LeftRestrictions, BottomRestrictions, RightRestrictions]
 *      -> elements of list are in range [0,N], 0 meaning an undefined restriction
 *      -> elements correspond to restrictions in top->bottom (left/right lists) or left->right (top/bottom lists) order
 *  -Board -> a list of lists (a matrix)
 */
solveBoard(Sides, Board) :-
  Sides = [Top, Left, _Bottom, Right],

  validateInput(Sides),
  length(Top, N),

  % Domain
  length(Board, N),
  restrictBoardDomain(Board, N),

  % Apply restrictions to board rows/columns
  applyAllLeftRestrictions(Left, Board),
  applyAllRightRestrictions(Right, Board),
  applyAllTopRestrictions(Top, Board),


  % Other restrictions ?


  % labeling - applied on flattened board
  append(Board, FlatBoard),
  labeling([], FlatBoard).
