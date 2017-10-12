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
        FirstEle = 0,
        N1 is (N-1),
        createBoardLine(OtherEle, N1).
        
%Board Printing - Character Translation
translate(0, Symbol) :- Symbol = ' '.
translate(1, Symbol) :- Symbol = 'O'. %Dark Pieces
translate(2, Symbol) :- Symbol = 'X'. %White Pieces
transalte(3, Symbol) :- Symbol = 'R'. %Red Workers

%Board Printing - arguments: Board and Board size
printBoard([], N):-
        printRowDivider(N).

printBoard([Line|Board], N) :-
        printRowDivider(N), nl,
        write('|'),
        printLine(Line), nl,
        printBoard(Board, N).

printLine([]).
printLine([Head|Tail]) :-
        translate(Head, Symbol),
        write(' '),
        write(Symbol),
        write(' |'),
        printLine(Tail).

printRowDivider(0).
printRowDivider(N) :-
        write(' '),
        put_code(9472), put_code(9472), put_code(9472),
        N1 is (N-1),
        printRowDivider(N1).