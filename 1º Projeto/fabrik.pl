%Generate a board predicate with N x N empty spaces
createBoard(Board, N) :-
        createBoard(Board, N , 0).

createBoard(_, N, N).
createBoard(Board, N, Lines) :-
        Lines1 is (Lines+1),
        Board = [Head | Tail],
        createBoardLine(Head, N),
        createBoard(Tail, N, Lines1).
        
createBoardLine(_, 0).
createBoardLine(Board, N) :-
        Board = [Head | Tail],
        Head = 0,
        N1 is (N-1),
        createBoardLine(Tail, N1).
        
%Board Screen - Printing

translate(0, Symbol) :- Symbol = ' '.
translate(1, Symbol) :- Symbol = 'O'.
translate(2, Symbol) :- Symbol = 'X'.
transalte(3, Symbol) :- Symbol = 'R'.

printBoard([]).
printBoard([Line|Board]) :-
        write('|'),
        printLine(Line),
        write('|'), nl,
        printBoard(Board).

printLine([]).
printLine([Head|Tail]) :-
        translate(Head, Symbol),
        write(Symbol),
        write(' '),
        printLine(Tail).