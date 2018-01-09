:- use_module(library(lists)).
:- use_module(library(clpfd)).

%% Exame PLOG 2015-2016

%% P12
% ups_and_downs(+Min, +Max, +N, -L)
ups_and_downs(Min, Max, N, L) :-
  N > 0, Min > 0, Max > 0,
  length(L, N),
  domain(L, Min, Max),

  applyRestrictions(L),

  labeling([], L).

applyRestrictions([F, S, T | List]) :-
  (S #< F #/\ S #< T) #\/ (S #> F #/\ S #> T),
  applyRestrictions([S, T | List]).
applyRestrictions([F, S]) :-
  F #\= S.


% P13
concelho(x, 120, 410).
concelho(y, 10, 800).
concelho(z, 543, 2387).
concelho(w, 3, 38).
concelho(k, 234, 376).

reverseBetween(Min, Max, Max).
reverseBetween(Min, Max, V) :-
  NMax is Max - 1,
  NMax >= Min,
  reverseBetween(Min, NMax, V).


%% P13
% concelho(Nome, Distancia, NEleitoresIndecisos)
concelho(x, 120, 410).
concelho(y, 10, 800).
concelho(z, 543, 2387).
concelho(w, 3, 38).
concelho(k, 234, 376).

% concelhos(+,+,-,-,-)
concelhos(NDias, MaxDist, ConcelhosVisitados, DistTotal, TotalEleitores) :-
  getConcelhos(Nomes, Distancias, Eleitores),

  length(Nomes, N),
  length(Vars, N),
  domain(Vars, 0, 1),

  DistTotal #< MaxDist,
  sum(Vars, #=<, NDias),

  scalar_product(Distancias, Vars, #=, DistTotal),
  scalar_product(Eleitores, Vars, #=, TotalEleitores),

  labeling([maximize(TotalEleitores)], Vars),

  write(Vars), nl,
  getConcelhosVisitados(Vars, Nomes, ConcelhosVisitados).

getConcelhos(Nomes, Distancias, Eleitores) :-
  findall(Nome, concelho(Nome, _, _), Nomes),
  findall(Dist, concelho(_, Dist, _), Distancias),
  findall(Eleit, concelho(_, _, Eleit), Eleitores).

getConcelhosVisitados([], [], []).
getConcelhosVisitados([0|Vars], [_ | Nomes], ConcelhosVisitados) :-
  getConcelhosVisitados(Vars, Nomes, ConcelhosVisitados).
getConcelhosVisitados([1|Vars], [N | Nomes], [N | ConcelhosVisitados]) :-
  getConcelhosVisitados(Vars, Nomes, ConcelhosVisitados).

%% concelhos(3,700,CVs,Dist,TE).
%% concelhos(3,500,CVs,Dist,TE).
%% concelhos(4,500,CVs,Dist,TE).
%% concelhos(4,290,CVs,Dist,TE).
