% Dictionary for Board Elements
translate(none, Code) :- Code = 32.
translate(black, Code) :- Code = 79. %Dark Pieces
translate(white, Code) :- Code = 88. %White Pieces
translate(worker, Code) :- Code = 9608. %Red Workers


% Board Printing - arguments: Board and Board size
printFabrik(Board, N):-
        nl, 
        write('  '), printHorizontalLabel(N, N),
        printBoard(Board, N, 1).

printBoard([], N, _):-
        printRowDivider(N), nl.

printBoard([Line | Board], N, CurrentL) :-
        printRowDivider(N),
        printDesignRow(N),
        printVerticalLabel(CurrentL),
        put_code(9474),
        printLine(Line),
        printDesignRow(N),
        NewL is (CurrentL+1),
        printBoard(Board, N, NewL).

printLine([]) :- nl.
printLine([Head | Tail]) :-
        translate(Head, Code),
        write('   '),
        put_code(Code),
        write('   '), put_code(9474),
        printLine(Tail).

%  AESTHETICS

printRowDivider(N):-
        write('  '),
        put_code(9532),
        printRowDividerRec(N).

printRowDividerRec(0) :- nl.
printRowDividerRec(N) :-
        put_code(9472), put_code(9472), put_code(9472), put_code(9472),
        put_code(9472), put_code(9472), put_code(9472), put_code(9532),
        N1 is (N-1),
        printRowDividerRec(N1).

printDesignRow(N):-
        write('  '),
        put_code(9474),
        printDesignRowRec(N).

printDesignRowRec(0) :- nl.
printDesignRowRec(N) :-
        write('       '), put_code(9474),
        N1 is (N-1),
        printDesignRowRec(N1).

%Dictionary for Labels
getLabel( 0, L):- L = 'A'.
getLabel( 1, L):- L = 'B'.
getLabel( 2, L):- L = 'C'.
getLabel( 3, L):- L = 'D'.
getLabel( 4, L):- L = 'E'.
getLabel( 5, L):- L = 'F'.
getLabel( 6, L):- L = 'G'.
getLabel( 7, L):- L = 'H'.
getLabel( 8, L):- L = 'I'.
getLabel( 9, L):- L = 'J'.
getLabel(10, L):- L = 'K'.
getLabel(11, L):- L = 'L'.

printHorizontalLabel(0, _):- nl.
printHorizontalLabel(N, Total):-
        Pos is (Total-N),
        getLabel(Pos, L),
        write('    '), write(L), write('   '),
        N1 is (N-1),
        printHorizontalLabel(N1, Total).        

printVerticalLabel(CurrentL):-
        CurrentL < 10,
        write(CurrentL),
        write(' ').

printVerticalLabel(CurrentL):-
        write(CurrentL).
