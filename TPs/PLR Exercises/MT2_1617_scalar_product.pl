%% Mini-Teste 2 2017

:- use_module(library(lists)).
:- use_module(library(clpfd)).

% P4
% sweet_recipes(+, +, +, +, -, -)
sweet_recipes(MaxTime, NEggs, RecipeTimes, RecipeEggs, Cookings, Eggs) :-
  length(RecipeTimes, N),
  length(RecipeEggs, N),
  
  length(Vars, N),
  domain(Vars, 0, 1),
  sum(Vars, #=, 3),

  scalar_product(RecipeTimes, Vars, #=<, MaxTime),
  scalar_product(RecipeEggs, Vars, #=, Eggs),
  Eggs #=< NEggs,

  labeling([maximize(Eggs)], Vars),
  getCookings(Vars, Cookings).

getCookings(Vars, Cookings) :-
  getCookingsAux(Vars, Cookings, 1).
getCookingsAux([], [], _).
getCookingsAux([0|Vars], Cookings, Id) :-
  NewId is Id + 1,
  getCookingsAux(Vars, Cookings, NewId).
getCookingsAux([1|Vars], [Id | Cookings], Id) :-
  NewId is Id + 1,
  getCookingsAux(Vars, Cookings, NewId).

%% sweet_recipes(60,30,[20,50,10,20,15],[6,4,12,20,6],Cookings,Eggs).
%% sweet_recipes(120,30,[20,50,10,20,15],[6,4,12,20,6],Cookings,Eggs).


% P5
% cut(+, +, -)
cut(Shelves, Boards, SelectedBoards) :-
  length(Shelves, NShelves),
  length(Boards, NBoards),

  length(SelectedBoards, NShelves),
  domain(SelectedBoards, 1, NBoards),

  length(Tasks, NShelves),
  createTasks(Shelves, SelectedBoards, Tasks),

  length(Machines, NBoards),
  createMachines(Boards, Machines),

  cumulatives(Tasks, Machines, [bound(upper)]),

  labeling([], SelectedBoards).

createTasks([], [], []).
createTasks([Cost | Shelves], [Id | MachineIds],
            [task(0, 1, 1, Cost, Id) | Tasks]) :-
  createTasks(Shelves, MachineIds, Tasks).

createMachines(Boards, Machines) :-
  createMachinesAux(Boards, Machines, 1).
createMachinesAux([], [], _).
createMachinesAux([B | Boards], [machine(Id, B) | Machines], Id) :-
  NewId is Id + 1,
  createMachinesAux(Boards, Machines, NewId).

%% cut([12,50,14,8,10,90,24], [100,45,70], S).

