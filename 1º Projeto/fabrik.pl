% Utils
boardSize(9).
boardSize(11).
initGame(N) :-
        boardSize(N),
        createBoard(B, N),
        printFabrik(B, N).


%Generate a board predicate with N x N empty spaces
createBoard(Board, N) :-
        createBoard(Board, N , 0).

createBoard(_, N, N).
createBoard([FirstRow | OtherRows], N, Lines) :-
        Lines1 is (Lines+1),
        createBoardLine(FirstRow, N),
        createBoard(OtherRows, N, Lines1).
        
createBoardLine(_, 0).
createBoardLine([FirstEle | OtherEle], N) :-
        FirstEle = none,
        N1 is (N-1),
        createBoardLine(OtherEle, N1).
        
%Board Printing - Board Elements Translation to Print Code
translate(none, Code) :- Code = 32.
translate(black, Code) :- Code = 79. %Dark Pieces
translate(white, Code) :- Code = 88. %White Pieces
translate(worker, Code) :- Code = 9608. %Red Workers

%Board Printing - arguments: Board and Board size
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

%                                              AESTHETICS‚

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
getLabel(0, L):- L = 'A'.
getLabel(1, L):- L = 'B'.
getLabel(2, L):- L = 'C'.
getLabel(3, L):- L = 'D'.
getLabel(4, L):- L = 'E'.
getLabel(5, L):- L = 'F'.
getLabel(6, L):- L = 'G'.
getLabel(7, L):- L = 'H'.
getLabel(8, L):- L = 'I'.
getLabel(9, L):- L = 'J'.
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
        

%Access the element in the [Row,Col] position of the given board
accessElement(0, [Elem | _], Element):-
        Element = Elem.

accessElement(Col, [_ | Board], Element) :-
        Col1 is (Col-1),
        accessElement(Col1, Board, Element).

accessElement(0, Col, [Line | _], Element):-
        accessElement(Col, Line, Element).

accessElement(Row, Col, [_ | Board], Element):-
        Row1 is (Row-1),
        accessElement(Row1, Col, Board, Element).