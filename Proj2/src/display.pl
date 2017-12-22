translate(0, '.').
translate(P, P).

% printBoard/1
%% prints the given board on the screen
printBoard(Board) :-
  Board = [Row | _],
  length(Row, RowLength),
  printHBorder(RowLength),
  printBoardAux(Board),
  printHBorder(RowLength).

printBoardAux([]) :- !.
printBoardAux([Row | Board]) :-  
  printRow(Row), nl,
  printBoardAux(Board).

% printBoard/2
%% prints the given board, and all the provided side restrictions
printBoard(Board, Restrictions) :- !,
  Restrictions = [Top, Left, Bottom, Right],
  write('   '), printRowAux(Top), nl,
  length(Top, RowLength), 
  write('  '), printHBorder(RowLength),
  printBoard(Board, Left, Right),
  write('  '), printHBorder(RowLength),
  write('   '), printRowAux(Bottom), nl, nl.

% printBoard/3
%% prints the given board, and the provided side restrictions for left and right
printBoard([], [], []) :- !.
printBoard([Row | Board], [L1 | Left], [R1 | Right]) :-
  translate(L1, SymbL), translate(R1, SymbR),
  write(SymbL), write(' '),
  printRow(Row),
  write(' '), write(SymbR), nl,
  printBoard(Board, Left, Right).

% printRow/1
%% prints the provided list/row
printRow(Board) :-
  write('|'),
  printRowAux(Board),
  write('|'), !.
printRowAux([]) :- !.
printRowAux([El | Row]) :-
  translate(El, Symb),
  write(Symb), write(' '),
  printRowAux(Row).

% printHorizontalBorder(+Length)
%% prints the top or bottom border for the board
printHBorder(Length):-
  write('+'),
  printHBorderAux(Length),
  write('+'), nl, !.
printHBorderAux(0) :- !.
printHBorderAux(Length) :-
  write('--'),
  NewLength is Length - 1,
  printHBorderAux(NewLength).