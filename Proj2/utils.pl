all_equal([_]).
all_equal([X, X | Rest]) :-
  all_equal([X | Rest]).

listsOfEqualSize(Sides) :-
  maplist(length, Sides, Lengths),
  all_equal(Lengths).