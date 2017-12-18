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

all_distinct_columns(_, 0) :- !.
all_distinct_columns(Board, N) :-
  N > 0, !,
  getBoardCol(Board, N, Col),
  all_distinct(Col),
  NewN is N - 1,
  all_distinct_columns(Board, NewN).


% Get the nth1 column of the given Board (1 indexed)
getBoardCol([], _, []).
getBoardCol([Row | Board], N, [El | Col]) :-
  element(N, Row, El),
  getBoardCol(Board, N, Col).


/**
 * Apply LEFT to RIGHT restrictions.
 * +Num is the number of visible skyscrapers from LEFT to RIGHT in the given row.
 */
applyLeftToRight(Num, Row) :-
  applyLeftToRight(Num, Row, 0).
applyLeftToRight(0, [], _).
applyLeftToRight(Num, [El | Row], Max) :-
  NewNum #>= 0,
  (El #> Max #/\ NewMax #= El #/\ NewNum #= Num - 1) #\/
  (El #=< Max #/\ NewMax #= Max #/\ NewNum #= Num),
  applyLeftToRight(NewNum, Row, NewMax).

applyAllLeftRestrictions([], []).
applyAllLeftRestrictions([0 | NList], [_ | Rows]) :-
  applyAllLeftRestrictions(NList, Rows).
applyAllLeftRestrictions([N | NList], [Row1 | Rows]) :-
  applyLeftToRight(N, Row1),
  applyAllLeftRestrictions(NList, Rows).


%% RIGHT to LEFT
applyRightToLeft(Num, Row) :-
  applyRightToLeft(Num, Row, 0).
applyRightToLeft(0, [], _).
applyRightToLeft(Num, Row, Max) :-
  append(LeftRow, [El], Row),
  NewNum #>= 0,
  (El #> Max #/\ NewMax #= El #/\ NewNum #= Num - 1) #\/
  (El #=< Max #/\ NewMax #= Max #/\ NewNum #= Num),
  applyRightToLeft(NewNum, LeftRow, NewMax).

applyAllRightRestrictions([], []).
applyAllRightRestrictions([0 | NList], [_ | Rows]) :-
  applyAllRightRestrictions(NList, Rows).
applyAllRightRestrictions([N | NList], [Row1 | Rows]) :-
  applyRightToLeft(N, Row1),
  applyAllRightRestrictions(NList, Rows).

%% TOP to BOTTOM
applyAllTopRestrictions(Restrictions, Board) :-
  applyAllTopRestrictions(Restrictions, Board, 1).
applyAllTopRestrictions([0 | NList], Board, Count) :-
  NewCount is Count + 1,
  applyAllTopRestrictions(NList, Board, NewCount).
applyAllTopRestrictions([N | NList], Board, Count) :-
  getBoardCol(Board, Count, Col),
  applyLeftToRight(N, Col),
  NewCount is Count + 1,
  applyAllTopRestrictions(NList, Board, NewCount).
applyAllTopRestrictions([], _, _).


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
  all_distinct_columns(Board, N),

  % Apply restrictions to board rows/columns
  applyAllLeftRestrictions(Left, Board),
  applyAllRightRestrictions(Right, Board),
  applyAllTopRestrictions(Top, Board),


  % Other restrictions ?


  % labeling - applied on flattened board
  append(Board, FlatBoard),
  labeling([], FlatBoard).
