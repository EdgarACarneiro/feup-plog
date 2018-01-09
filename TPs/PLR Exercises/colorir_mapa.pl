%% P 7.3
% Versao estendida do problema de colorir um mapa.
% Para alem do problema original, apresenta restricoes de exclusao,
%  condicoes positivas/negativas, diferencas e preferencias flexiveis.

:- use_module(library(lists)).
:- use_module(library(clpfd)).

%% Base de Factos
paises(5).
cores(4,[2,1,1,2]).
 
fronteira(1,2).
fronteira(1,3).
fronteira(1,4).
fronteira(1,5).
fronteira(2,3).
fronteira(2,4).
fronteira(3,4).
fronteira(4,5).
 
exclui(1, [3,4]).
exclui(2, [3]).
seentao(1-4,2-3).
seexclui(1-3,2-1).
diferentes(2,5).
 
preferencias([1-[1,2], 2-[1], 3-[3,4], 4-[1,2]]).
%%

%% colorir(-Cores)
colorir(Cores, NumPreferenciasFulfilled) :-
  paises(NPaises),
  cores(NCores, Cardinalidade),

  length(Cores, NPaises),
  domain(Cores, 1, NCores),

  aplicar_cardinalidade(Cores, Cardinalidade),
  aplicar_fronteiras(Cores),
  aplicar_exclusoes(Cores),
  aplicar_seentao(Cores),
  aplicar_seexclui(Cores),
  aplicar_diferentes(Cores),

  aplicar_preferencias(Cores, NumPreferenciasFulfilled),
  append(Cores, [NumPreferenciasFulfilled], Vars),

  labeling([maximize(NumPreferenciasFulfilled)], Vars).

% CARDINALIDADE
aplicar_cardinalidade(Cores, Cardinalidade) :-
  aplicar_cardinalidade_aux(Cores, Cardinalidade, 1).
aplicar_cardinalidade_aux(_, [], _).
aplicar_cardinalidade_aux(Cores, [MaxNum | List], Count) :-
  count(Count, Cores, #=<, MaxNum),
  NewCount is Count + 1,
  aplicar_cardinalidade_aux(Cores, List, NewCount).

% FRONTEIRAS
aplicar_fronteiras(Cores) :-
  findall(X1-X2, fronteira(X1, X2), Fronteiras),
  aplicar_fronteiras_aux(Cores, Fronteiras).
aplicar_fronteiras_aux(_, []).
aplicar_fronteiras_aux(Cores, [X1-X2 | Fronteiras]) :-
  element(X1, Cores, C1),
  element(X2, Cores, C2),
  C1 #\= C2,
  aplicar_fronteiras_aux(Cores, Fronteiras).

% EXCLUSOES
aplicar_exclusoes(Cores) :-
  findall(X1-X2, exclui(X1, X2), Exclusoes),
  aplicar_exclusoes_aux(Cores, Exclusoes).
aplicar_exclusoes_aux(_, []).
aplicar_exclusoes_aux(Cores, [I-L | Exclusoes]) :-
  element(I, Cores, Cor),
  cor_not_in_set(Cor, L),
  aplicar_exclusoes_aux(Cores, Exclusoes).

cor_not_in_set(_, []).
cor_not_in_set(Cor, [C1 | Cores]) :-
  Cor #\= C1,
  cor_not_in_set(Cor, Cores).

% SE ENTAO
aplicar_seentao(Cores) :-
  findall([P1-C1, P2-C2], seentao(P1-C1, P2-C2), Lista),
  aplicar_seentao_aux(Cores, Lista).
aplicar_seentao_aux(_, []).
aplicar_seentao_aux(Cores, [[P1-C1, P2-C2] | Lista]) :-
  element(P1, Cores, Cor1),
  element(P2, Cores, Cor2),
  (Cor1 #= C1) #=> (Cor2 #= C2),
  aplicar_seentao_aux(Cores, Lista).

% SE EXCLUI
aplicar_seexclui(Cores) :-
  findall([P1-C1, P2-C2], seexclui(P1-C1, P2-C2), Lista),
  aplicar_seexclui_aux(Cores, Lista).
aplicar_seexclui_aux(_, []).
aplicar_seexclui_aux(Cores, [[P1-C1, P2-C2] | Lista]) :-
  element(P1, Cores, Cor1),
  element(P2, Cores, Cor2),
  (Cor1 #= C1) #=> (Cor2 #\= C2),
  aplicar_seexclui_aux(Cores, Lista).

% DIFERENTES
aplicar_diferentes(Cores) :-
  findall(P1-P2, diferentes(P1, P2), Lista),
  aplicar_diferentes_aux(Cores, Lista).
aplicar_diferentes_aux(_, []).
aplicar_diferentes_aux(Cores, [P1-P2 | Lista]) :-
  element(P1, Cores, Cor1),
  element(P2, Cores, Cor2),
  Cor1 #\= Cor2,
  aplicar_diferentes_aux(Cores, Lista).

% PREFERENCIAS
aplicar_preferencias(Cores, NumPreferenciasFulfilled) :-
  preferencias(Lista),
  aplicar_preferencias_aux(Cores, Lista, NumPreferenciasFulfilled).
aplicar_preferencias_aux(_, [], 0).
aplicar_preferencias_aux(Cores, [Idx-Prefs | Lista], Num) :-
  element(Idx, Cores, Cor),
  cor_in_set(Cor, Prefs, Bool),
  Num #= NextNum + Bool,
  aplicar_preferencias_aux(Cores, Lista, NextNum).

cor_in_set(_, [], 0).
cor_in_set(Cor, [CorPreferida | Prefs], Bool) :-
  Cor #= CorPreferida #<=> B,
  Bool #= B + NextBool,
  cor_in_set(Cor, Prefs, NextBool).















