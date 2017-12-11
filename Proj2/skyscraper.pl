:- use_module(library(clpfd)).
:- use_module(library(lists)).

skyscraper :-
  write("Hello World!"), nl.


all_equal([_]).
all_equal([X, X | Rest]) :-
	all_equal([X | Rest]).

/**
 *	+Sides -> a list of lists, each of which represents the restrictions on the side of the board (number of visible buildings).
 *			-> in order: [UpRestrictions, LeftRestrictions, DownRestrictions, RightRestrictions]
 *			-> elements of list are in range [0,N], 0 meaning an undefined restriction
 *	-Board -> a list of lists (a matrix)
 */
solve(Sides/*, Board*/) :-
	%Sides = [Up, Left, Down, Right],
	
	% Input Validation
	maplist(length, Sides, Lengths),
	all_equal(Lengths).