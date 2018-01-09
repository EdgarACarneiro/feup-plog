%% Mini-Teste 2 2017 (16-17)

:- use_module(library(lists)).
:- use_module(library(clpfd)).

%% P3
p2(L1,L2) :-
    length(L1,N),
    length(L2,N),
    %
    pos(L1,L2,Is),
    all_distinct(Is),
    %
    test(L2),
    labeling([],Is),
    write(L2).
 
test([]).
test([_]).
test([_,_]).
test([X1,X2,X3|Xs]):-
    ((X1 #< X2 #/\ X2 #< X3) #\/ (X1 #> X2 #/\ X2 #> X3)),
    test(Xs).
 
pos([],_,[]).
pos([X|Xs],L2,[I|Is]) :-
    element(I,L2,X),
    pos(Xs,L2,Is).

% p2([1,5,4,2,9,3],L2).


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
% cut(+Shelves, +Boards, -SelectedBoards)
cut(Shelves, Boards, SelectedBoards) :-
    length(Shelves, LenShelves),
    length(Boards, LenBoards),

    length(SelectedBoards, LenShelves),
    domain(SelectedBoards, 1, LenBoards),

    generateTasks(Shelves, SelectedBoards, Tasks),
    generateMachines(Boards, Machines),

    cumulatives(Tasks, Machines, [bound(upper)]),

    labeling([], SelectedBoards).

generateTasks([], [], []).
generateTasks([S | Shelves], [I | Indices], [task(0, 1, 1, S, I) | Tasks]) :-
    generateTasks(Shelves, Indices, Tasks).

generateMachines(Boards, Machines) :-
    generateMachinesAux(Boards, Machines, 1).
generateMachinesAux([], [], _).
generateMachinesAux([B | Boards], [machine(Count, B) | Machines], Count) :-
    NewCount is Count + 1,
    generateMachinesAux(Boards, Machines, NewCount).

% cut([12,50,14,8,10,90,24], [100,45,70], S).


