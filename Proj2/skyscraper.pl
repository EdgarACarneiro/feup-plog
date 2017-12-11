:- use_module(library(clpfd)).
:- use_module(library(lists)).

skyscraper :-
  write("Hello World!"), nl.


all_equal([_]).
all_equal([X, X | Rest]) :-
  all_equal([X | Rest]).

validateInput(Sides) :-
  maplist(length, Sides, Lengths),
  all_equal(Lengths).

restrictBoardDomain([], _).
restrictBoardDomain([Row | Board], N) :-
  domain(Row, 1, N),
  restrictBoardDomain(Board, N).

/**
 *  +Sides -> a list of lists, each of which represents the restrictions on the side of the board (number of visible buildings).
 *      -> in order: [UpRestrictions, LeftRestrictions, DownRestrictions, RightRestrictions]
 *      -> elements of list are in range [0,N], 0 meaning an undefined restriction
 *  -Board -> a list of lists (a matrix)
 */
solve(Sides, Board) :-
  Sides = [Up, Left, Down, Right],

  validateInput(Sides),
  length(Up, N),

  % Domain
  restrictBoardDomain(Board, N),

  % For every ROW in the board, restrict according to Left/Right lists
  element(1, Board, Row1), % Example with first Row
  applyRowRestrictions(Row1),

  % For every COL in the board, restrict according to Up/Down lists


  % Other restrictions ?


  % labeling - applied on flattened board
  append(Board, FlatBoard),
  labeling([], FlatBoard).
