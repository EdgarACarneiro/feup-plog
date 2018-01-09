%% Mini-Teste 2 Modelo 2017/2018

:- use_module(library(clpfd)).
:- use_module(library(lists)).

%% P4
% sweet_recipes(+MaxTime,+NEggs,+RecipeTimes,+RecipeEggs,-Cookings,-Eggs)
sweet_recipes(MaxTime, NEggs, RecipeTimes, RecipeEggs, Cookings, Eggs) :-
  length(Cookings, 3),
  length(RecipeTimes, N),
  length(RecipeEggs, N),

  domain(Cookings, 1, N),
  all_distinct(Cookings),
  Eggs in 1..NEggs,
  TimeTaken in 1..MaxTime,

  getEggsList(RecipeEggs, Cookings, EggsList),
  sum(EggsList, #=, Eggs),
  getTimeList(RecipeTimes, Cookings, TimesList),
  sum(TimesList, #=, TimeTaken),

  append(Cookings, [Eggs], Vars),
  /**
    Eggs must be instantiated after Cookings!!
    Or it'll instantiate the Eggs and backtrack
     hundreads of times on the Cookings' list
  */
  labeling([maximize(Eggs)], Vars).

getTimeList(_, [], []).
getTimeList(RecipeTimes, [FirstC | Cookings], [FirstTime | Times]) :-
  element(FirstC, RecipeTimes, FirstTime),
  getTimeList(RecipeTimes, Cookings, Times).
getEggsList(_, [], []).
getEggsList(RecipeEggs, [FirstC | Cookings], [FirstEggs | Eggs]) :-
  element(FirstC, RecipeEggs, FirstEggs),
  getEggsList(RecipeEggs, Cookings, Eggs).


%% P5
% embrulha(+Rolos,+Presentes,-RolosSelecionados)
embrulha(Rolos, Presentes, RolosSelecionados) :-
  length(Presentes, N),
  length(RolosSelecionados, N),
  length(Rolos, NumRolos),
  domain(RolosSelecionados, 1, NumRolos),
  
  generateTasks(Presentes, RolosSelecionados, Tasks),
  write(Tasks), nl,
  generateResources(Rolos, Resources),
  write(Resources), nl,

  cumulatives(Tasks, Resources, [bound(upper)]),

  labeling([], RolosSelecionados).

generateTasks([], [], []).
generateTasks([P | Presents], [R | Rolos], [task(0, 1, 1, P, R) | Tasks]) :-
  generateTasks(Presents, Rolos, Tasks).

generateResources(Rolos, Resources) :-
  generateResourcesAux(Rolos, Resources, 1).
generateResourcesAux([], [], _).
generateResourcesAux([R | Rolos], [machine(Count, R) | Resources], Count) :-
  NCount is Count + 1,
  generateResourcesAux(Rolos, Resources, NCount).

