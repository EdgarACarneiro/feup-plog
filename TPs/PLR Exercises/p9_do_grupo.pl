:-use_module(library(clpfd)).

% P9 - Do Damas
sequencia2(Seq,Custo,N):-
  length(Seq,N),
  domain(Seq,1,4),
  count(1,Seq,#=,Ocor1),
  (Ocor1#>=5 #/\ Ocor1#=<15),
  count(2,Seq,#=,Ocor2),
  (Ocor2#>=2 #/\ Ocor2#=<6),
  count(3,Seq,#=,Ocor3),
  (Ocor3#>=5 #/\ Ocor3#=<10),
  count(4,Seq,#=,Ocor4),
  (Ocor4#>=7 #/\ Ocor4#=<12),

  automaton(Seq, _, Seq,
    [source(q0),sink(q0),sink(q1),sink(q2),sink(q3),sink(q4)],
    [
      arc(q0,1,q1), arc(q0,2,q2), arc(q0,3,q3), arc(q0,4,q4),
      arc(q1,2,q2,[C+5]), arc(q1,3,q3,[C+5]), arc(q1,4,q4,[C+7]),
      arc(q2,1,q1,[C+3]), arc(q2,4,q4,[C+4]),
      arc(q3,1,q1,[C+2]), arc(q3,4,q4,[C+6]),
      arc(q4,1,q1,[C+9]), arc(q4,2,q2,[C+5])
    ],
    [C],[0],[Custo]
  ),

  labeling([minimize(Custo)],Seq),
  write(Seq),nl.

