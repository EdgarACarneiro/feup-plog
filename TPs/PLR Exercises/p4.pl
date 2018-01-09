:- use_module(library(lists)).
:- use_module(library(clpfd)).

% P 4.a)
table6(L) :-
  %% Indices in list L
  % Asdrubal=1, Bernardete=2, Cristalinda=3
  % Demetrio=4, Eleuterio=5, Felismina=6
  L = [Asdr, Bern, Crist, Demet, Eleut, Felism],
  domain(L, 1, 6),
  all_distinct(L),

  Asdr #= 1, Bern #= 2,
  together6(Asdr, Bern, 1),
  together6(Crist, Demet, 1),

  together6(Eleut, Felism, 0),
  together6(Asdr, Eleut, 0),

  labeling([], L).

together6(I1, I2, Bool) :-
  ((I1 #= 1 #/\ I2 #= 6) #\/
  (I1 #= I2 + 1) #\/
  (I1 #= I2 - 1)) #<=> Bool.
%% table6(L).


% P 4.b)
squareTable(Ppl, N, Together, Apart) :-

  %% Validating Input
  Res is N mod 2, Res = 0, %% N is even
  length(Ppl, N),
  length(Together, LenT),
  LenT > 0,

  %% Domains
  domain(Ppl, 1, N),

  %% Restrictions
  all_distinct(Ppl),
  apply_together_restrictions(N, Together, 1),
  apply_together_restrictions(N, Apart, 0),

  %% Avoid Symmetries
  element(2, Ppl, Second),
  element(N, Ppl, Last),
  Second #< Last,
  %% (horizontal axis)

  TableHeadIdx is (N // 2) + 1,
  element(TableHeadIdx, Ppl, TableHead),
  element(1, Ppl, First),
  First #< TableHead,
  %% (vertical axis)

  %% Solution
  labeling([], Ppl).

apply_together_restrictions(_, [], _).
apply_together_restrictions(N, [P1-P2 | Together], Bool) :-
  together(N, P1, P2, Bool),
  apply_together_restrictions(N, Together, Bool).

together(N, P1, P2, Bool) :-
  (
    (P1 #= P2 - 1 #\/ P1 #= P2 + 1) #\/
    (P1 #= 1 #/\ P2 #= N) #\/
    (P2 #= 1 #/\ P1 #= N) #\/
    ((P1 - 2 #= N - P2) #/\ /** on opposite sides of the table **/
     (P1 #< P2)) #\/        /** and not on the head of the table **/
    ((P2 - 2 #= N - P1) #/\
     (P2 #< P1))
  ) #<=> Bool.


%% squareTable([A,B,C,D,E,F,G,H], 8, [A-B, C-D, E-F, G-H], [A-C, B-C, A-E, D-G, D-H, E-G, E-H]).

%% All solutions:
%% squareTable([A,B,C,D,E,F,G,H], 8, [A-B, C-D, E-F, G-H], [A-C, B-C, A-E, D-G, D-H, E-G, E-H]), L=[A,B,C,D,E,F,G,H], write(L), nl, fail.