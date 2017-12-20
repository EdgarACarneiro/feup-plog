translate(0, 'X').
translate(P, P).

% printBoard/1
%% prints the given board on the screen
printBoard([]).
printBoard([Row | Board]) :-
  printRow(Row), nl,
  printBoard(Board).

% printBoard/2
%% prints the given board, and all the provided side restrictions
printBoard(Board, Restrictions) :-
  Restrictions = [Top, Left, Right, Bottom],
  write('  '), printRow(Top), nl,
  printBoard(Board, Left, Right),
  write('  '), printRow(Bottom), nl.

% printBoard/3
%% prints the given board, and the provided side restrictions for left and right
printBoard([], [], []).
printBoard([Row | Board], [L1 | Left], [R1 | Right]) :-
  translate(L1, SymbL), translate(R1, SymbR),
  write(SymbL), write(' '),
  printRow(Row),
  write(SymbR), nl,
  printBoard(Board, Left, Right).

% printRow/1
%% prints the provided list/row
printRow([]).
printRow([El | Row]) :-
  translate(El, Symb),
  write(Symb), write(' '),
  printRow(Row).
