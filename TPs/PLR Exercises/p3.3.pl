% Pergunta 3.3

:- use_module(library(lists)).
:- use_module(library(clpfd)).

objecto(piano, 3, 30).
objecto(cadeira, 1, 10).
objecto(cama, 3, 15).
objecto(mesa, 2, 15).
homens(4).
tempo_max(60).

solve33(Starts, Ends) :-
  findall(Name-Cost-Duration, objecto(Name, Cost, Duration), Objects),

  length(Objects, NumObjects),
  length(Starts, NumObjects),
  length(Ends, NumObjects),

  append(Starts, Ends, Vars),
  tempo_max(Max),
  domain(Vars, 0, Max),
  maximum(MaxEnd, Ends),

  makeTasks(Objects, Starts, Ends, Tasks),

  cumulatives(Tasks, [machine(1, 4)], [bound(upper)]),

  labeling([minimize(MaxEnd)], Vars),
  output(Objects, Starts, Ends).

makeTasks([], [], [], []).
makeTasks([_-Cost-Duration | Objects],
          [S|Starts], [E|Ends],
          [task(S, Duration, E, Cost, 1) | Tasks]) :-
  makeTasks(Objects, Starts, Ends, Tasks).

output([], [], []).
output([Obj-Men-_ | Rem1], [S | Rem2], [E | Rem3]):-
    write('Mover '),write(Obj),write(' ('),write(Men),write(' homens) desde t='),write(S),write(' ate t='),write(E),nl,
    output(Rem1, Rem2, Rem3).