%% P9

:- use_module(library(lists)).
:- use_module(library(clpfd)).

% numPecas(Id, Min, Max)
numPecas(1, 5, 15).
numPecas(2, 2, 6).
numPecas(3, 5, 10).
numPecas(4, 7, 12).

% sequencia(?Seq, ?Custo, +N)
sequencia(Seq, Cost, N) :-
  length(Seq, N),
  domain(Seq, 1, 4),

  count(1, Seq, #=, Count1), Count1 in 5..15,
  count(2, Seq, #=, Count2), Count2 in 2..6,
  count(3, Seq, #=, Count3), Count3 in 5..10,
  count(4, Seq, #=, Count4), Count4 in 7..12,

  automaton(
    Seq, _, Seq,
    [source(q0), sink(a), sink(b), sink(c), sink(d)],
    [
      arc(q0, 1, a), arc(q0, 2, b),
      arc(q0, 3, c), arc(q0, 4, d),
      arc(a, 2, b, [C + 5]),
      arc(a, 3, c, [C + 5]),
      arc(a, 4, d, [C + 7]),
      arc(b, 1, a, [C + 3]),
      arc(b, 4, d, [C + 4]),
      arc(c, 1, a, [C + 2]),
      arc(c, 4, d, [C + 6]),
      arc(d, 1, a, [C + 9]),
      arc(d, 2, b, [C + 5])
    ],
    [C], [0], [Cost]
  ),

  append(Seq, Cost, Vars),
  labeling([minimize(Cost)], Seq),

  write(Seq).

% sequencia(Seq,Cost,20).

