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

/**
 * Applies restrictions Horizontally along the board
 * +Predicate is the predicate used to apply restrictions on the fetched Row.
 */
applyAllHorizontalRestrictions([0 | Rest], [_ | Rows], Predicate) :-
  applyAllHorizontalRestrictions(Rest, Rows, Predicate).
applyAllHorizontalRestrictions([N | Rest], [Row1 | Rows], Predicate) :-
  call(Predicate, N, Row1),
  applyAllHorizontalRestrictions(Rest, Rows, Predicate).
applyAllHorizontalRestrictions([], [], _).

/**
 * Applies restrictions Vertically along the board
 * +Predicate is the predicate used to apply restrictions on the fetched Column.
 */applyAllVerticalRestrictions(Restrictions, Board, Predicate) :-
  applyAllVerticalRestrictions(Restrictions, Board, Predicate, 1).
applyAllVerticalRestrictions([0 | Rest], Board, Predicate, Count) :-
  NewCount is Count + 1,
  applyAllVerticalRestrictions(Rest, Board, Predicate, NewCount).
applyAllVerticalRestrictions([N | Rest], Board, Predicate, Count) :-
  NewCount is Count + 1,
  getBoardCol(Board, Count, Col),
  call(Predicate, N, Col),
  applyAllVerticalRestrictions(Rest, Board, Predicate, NewCount).
applyAllVerticalRestrictions([], _, _, _).


/**
 *  +Sides -> a list of lists, each of which represents the restrictions on the side of the board (number of visible buildings).
 *      -> in order: [TopRestrictions, LeftRestrictions, BottomRestrictions, RightRestrictions]
 *      -> elements of list are in range [0,N], 0 meaning an undefined restriction
 *      -> elements correspond to restrictions in top->bottom (left/right lists) or left->right (top/bottom lists) order
 *  -Board -> a list of lists (a matrix)
 */
solveBoard(Sides, Board) :-
  Sides = [Top, Left, Bottom, Right],

  validateInput(Sides),
  length(Top, N),

  % Domain
  length(Board, N),
  restrictBoardDomain(Board, N),
  all_distinct_columns(Board, N),

  % Apply restrictions to board rows/columns
  applyAllHorizontalRestrictions(Left, Board, applyLeftToRight),
  applyAllHorizontalRestrictions(Right, Board, applyRightToLeft),
  applyAllVerticalRestrictions(Top, Board, applyLeftToRight),
  applyAllVerticalRestrictions(Bottom, Board, applyRightToLeft),

  % Other restrictions ?


  % labeling - applied on flattened board
  append(Board, FlatBoard),
  labeling([], FlatBoard).
