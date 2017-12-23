%Dictionary for user friendly visualization of elements
translate(0, '.').
translate(P, P).

% printBoard(+Board)
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

% printBoard(+Board, +Restrictions)
%% prints the given board, and all the provided side restrictions
printBoard(Board, Restrictions) :- !,
  Restrictions = [Top, Left, Bottom, Right],
  write('   '), printRowAux(Top), nl,
  length(Top, RowLength), 
  write('  '), printHBorder(RowLength),
  printBoard(Board, Left, Right),
  write('  '), printHBorder(RowLength),
  write('   '), printRowAux(Bottom), nl, nl.

% printBoard(+Board, +LeftRestrictions, +RightRestrictions)
%% prints the given board, and the provided side restrictions for left and right
printBoard([], [], []) :- !.
printBoard([Row | Board], [L1 | Left], [R1 | Right]) :-
  translate(L1, SymbL), translate(R1, SymbR),
  write(SymbL), write(' '),
  printRow(Row),
  write(' '), write(SymbR), nl,
  printBoard(Board, Left, Right).

% printRow(+Row)
%% prints the provided list/row and adds '|' after and before the list
printRow(Row) :-
  write('|'),
  printRowAux(Row),
  write('|'), !.
printRowAux([]) :- !.
printRowAux([El | Row]) :-
  translate(El, Symb),
  write(Symb), write(' '),
  printRowAux(Row).

% printHorizontalBorder(+Length)
%% prints the top or bottom border for the board, example of a boarder: '+-------+'
printHBorder(Length):-
  write('+'),
  printHBorderAux(Length),
  write('+'), nl, !.
printHBorderAux(0) :- !.
printHBorderAux(Length) :-
  write('--'),
  NewLength is Length - 1,
  printHBorderAux(NewLength).