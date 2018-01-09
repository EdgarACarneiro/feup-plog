:- use_module(library(lists)).
:- use_module(library(clpfd)).

% P13

% Cada filme em FilmList e' representado por um tuplo (Start,Duration,Worth)
% attend(+FilmList, -Going, -Worth)
attend(FilmList, Going, Worth) :-
  length(FilmList, N),
  length(Going, N),
  domain(Going, 0, 1),

  length(Tasks, N),
  generateTasks(FilmList, Tasks, Going),
  Machines = [machine(0, N), machine(1, 1)],
  cumulatives(Tasks, Machines, [bound(upper)]),

  getWorths(FilmList, WorthsList),
  scalar_product(WorthsList, Going, #=, Worth),

  labeling([maximize(Worth)], Going).

generateTasks([], [], []).
generateTasks([(S,D,_) | FilmList],
              [task(S, D, _, 1, V) | Tasks],
              [V | Vars]) :-
  generateTasks(FilmList, Tasks, Vars).

getWorths([], []).
getWorths([(_,_,W) | FilmList], [W | List]) :-
  getWorths(FilmList, List).

%% attend([(1,3,2), (1,4,4), (4,4,3)], Goings, Worth).
%% attend([(1,3,2), (1,4,4), (4,4,1)], Goings, Worth).
%% attend([(1,3,3), (1,4,4), (3,5,5), (4,1,1), (4,4,3), (5,4,3), (6,3,1), (8,3,3)], Goings, Worth).
