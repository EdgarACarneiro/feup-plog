%% P 7.2

:- use_module(library(lists)).
:- use_module(library(clpfd)).

% docente(Nome, Sexo, Start, End) -> pode dar aulas que comecem de Start a End
docente(pedro, m, 3, 6).
docente(joana, f, 3, 4).
docente(ana, f, 2, 5).
docente(joao, m, 2, 4).
docente(david, m, 3, 4).
docente(maria, f, 1, 6).

alocarDocentes :-
  getDocentes(Docentes),
  length(Docentes, N),

  length(Tasks, N),
  generateTasks(Docentes, Tasks, FemaleStarts, AllStarts),
  cumulative(Tasks),

  %% Minimize female starts
  sum(FemaleStarts, #=, AbsFemaleStarts),

  labeling([minimize(AbsFemaleStarts)], AllStarts),
  
  writeClasses(Docentes, AllStarts).

getDocentes(Docentes) :-
  findall(Name-Gender-Min-Max,
    docente(Name, Gender, Min, Max),
    Docentes).

generateTasks(Docentes, Tasks, FemaleStarts, AllStarts) :-
  generateTasksAux(Docentes, Tasks, FemaleStarts, AllStarts, 1).

generateTasksAux([], [], [], [], _).
generateTasksAux(
  [_-m-Min-Max | Docentes],
  [task(Start, 1, End, 1, Id) | Tasks],
  FemaleStarts, [Start | AllStarts], Id) :-
  
  Start in Min..Max,
  End #= Start + 1,
  NewId is Id + 1,

  generateTasksAux(Docentes, Tasks, FemaleStarts, AllStarts, NewId).
generateTasksAux(
  [_-f-Min-Max | Docentes],
  [task(Start, 1, End, 1, Id) | Tasks],
  [Start | FemaleStarts], [Start | AllStarts], Id) :-
  
  Start in Min..Max,
  End #= Start + 1,
  NewId is Id + 1,

  generateTasksAux(Docentes, Tasks, FemaleStarts, AllStarts, NewId).

writeClasses([], []).
writeClasses([D-_-_-_ | Docentes], [Start | AllStarts]) :-
  write(D), write(': '), write(Start), nl,
  writeClasses(Docentes, AllStarts).
