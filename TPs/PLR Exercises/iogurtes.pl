:- use_module(library(lists)).
:- use_module(library(clpfd)).


%% constroi(+NEmb, +Orcamento, +EmbPorObjeto, +CustoPorObjeto, -EmbUsadas, -Objetos)

%% constroi(30, 100, [6,4,12,20,6], [20,50,10,20,15], EmbUsadas, Objetos).
%% constroi(50, 100, [6,4,12,20,6], [20,50,10,20,15], EmbUsadas, Objetos).

%% with scalar_product, fastest approach
constroi(NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, Objetos):-
  length(EmbPorObjeto, N),
  length(CustoPorObjeto, N),
  length(Vars, N),
  domain(Vars, 0, 1),

  EmbUsadas #=< NEmb,
  scalar_product(EmbPorObjeto, Vars, #=, EmbUsadas),

  Custo #=< Orcamento,
  scalar_product(CustoPorObjeto, Vars, #=, Custo),

  labeling([maximize(EmbUsadas)], Vars),

  getObjetos(Vars, Objetos).

getObjetos(Vars, Objetos) :-
  getObjetosAux(Vars, Objetos, 1).
getObjetosAux([], [], _).
getObjetosAux([1 | Vars], [Count | Objetos], Count) :-
  NewCount is Count + 1,
  getObjetosAux(Vars, Objetos, NewCount).
getObjetosAux([0 | Vars], Objetos, Count) :-
  NewCount is Count + 1,
  getObjetosAux(Vars, Objetos, NewCount).


%% Another approach - simpler but slower
constroi2(NEmb, Orcamento, EmbPorObjeto, CustoPorObjeto, EmbUsadas, Objetos) :-
  length(Objetos, 4),
  EmbUsadas in 0..NEmb,

  length(EmbPorObjeto, N),
  length(CustoPorObjeto, N),
  domain(Objetos, 1, N),
  all_distinct(Objetos),

  getEmbsUsadas(EmbPorObjeto, Objetos, EmbUsadas),
  getCusto(CustoPorObjeto, Objetos, Custo),
  Custo #=< Orcamento,

  append(Objetos, [EmbUsadas], Vars),
  labeling([maximize(EmbUsadas)], Vars).

getEmbsUsadas(_, [], 0).
getEmbsUsadas(EmbPorObjeto, [X | Objetos], EmbUsadas) :-
  element(X, EmbPorObjeto, E),
  EmbUsadas #= E + NextEmbUsadas,
  getEmbsUsadas(EmbPorObjeto, Objetos, NextEmbUsadas).
getCusto(_, [], 0).
getCusto(CustoPorObjeto, [X | Objetos], Custo) :-
  element(X, CustoPorObjeto, C),
  Custo #= C + NextCusto,
  getCusto(CustoPorObjeto, Objetos, NextCusto).
