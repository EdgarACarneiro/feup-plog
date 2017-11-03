:- include('utils.pl').
:- use_module(library(lists)).
:- use_module(library(clpfd)).


% Generate a board predicate with N x N empty spaces
createBoard(Board, N) :-
        createBoard(Board, N , 0).

createBoard(_, N, N).
createBoard([FirstRow | OtherRows], N, Lines) :-
        Lines1 is (Lines + 1),
        createBoardLine(FirstRow, N),
        createBoard(OtherRows, N, Lines1).
        
createBoardLine(_, 0).
createBoardLine([FirstEle | OtherEle], N) :-
        FirstEle = none,
        N1 is (N - 1),
        createBoardLine(OtherEle, N1).


% Access the element in the [Row,Col] position of the given board
getElement(Row, Col, Board, Element):-
        nth0(Row, Board, RowLine, _),
        nth0(Col, RowLine, Element, _), !.


% % Validation Predicates
% Worker can be played if [Row,Col]=none
isValidPlay(worker, Row, Col, Board) :-
        getElement(Row, Col, Board, El),
        El = none.

% White/Black pieces can be played if [Row,Col] is in intersection of Workers' lines of sight
isValidPlay(white, Row, Col, Board) :-
        isIntersection(Row, Col, Board).
isValidPlay(black, Row, Col, Board) :-
        isIntersection(Row, Col, Board).

% PLACEHOLDER - probably receive [Row1,Col1],[Row2,Col2] positions of workers 
isIntersection(_Row, _Col, _Board).
% TODO


% Set piece on board
% Sets the piece of the given type on the given position, on the given Board
setPiece(Piece, Row, Col, Board, NewBoard) :-
        isValidPlay(Piece, Row, Col, Board),
        nth0(Row, Board, RowLine, TmpBoard),
        nth0(Col, RowLine, _, TmpRowLine),
        nth0(Col, NewRowLine, Piece, TmpRowLine),
        nth0(Row, NewBoard, NewRowLine, TmpBoard).


% Finds both workers' positions
findBothWorkers(Board, Row1, Col1, Row2, Col2) :-
        findWorker(Board, Row1, Col1),
        findWorker(Board, Row2, Col2),
        (Row1 \= Row2; Col1 \= Col2), !. % Cut prevents Row/Col permutations over backtracking

% Finds a worker's position
findWorker(Board, OutputRow, OutputCol) :-
        nth0(OutputRow, Board, TmpRow),
        nth0(OutputCol, TmpRow, worker).

% Tests if board has any empty space
boardIsNotFull(Board) :-
        nth0(_, Board, TmpRow),
        nth0(_, TmpRow, none).

% Side has won ?
gameIsWon(PieceSide, Board) :-
        checkHorizontalWin(PieceSide, Board),
        checkVerticalWin(PieceSide, Board),
        checkDiagonalWin(PieceSide, Board).

% Check Horizontal Win for side 'Side'
checkHorizontalWin(Side, [FirstRow | _RestOfBoard]) :-
        checkRowWin(Side, FirstRow, 0).
checkHorizontalWin(Side, [FirstRow | RestOfBoard]) :-
        \+ checkRowWin(Side, FirstRow, 0),
        checkHorizontalWin(Side, RestOfBoard).
checkRowWin(_, _, N) :-
        winningStreakN(N), !.
checkRowWin(Side, [Side | RestOfRow], Count) :-
        NewCount is Count + 1,
        checkRowWin(Side, RestOfRow, NewCount).
checkRowWin(Side, [FirstEl | RestOfRow], _Count) :-
        Side \= FirstEl,
        checkRowWin(Side, RestOfRow, 0).


% Check Vertical Win for side 'Side'
checkVerticalWin(Side, Board) :-
        transpose(Board, TransposedBoard),
        checkHorizontalWin(Side, TransposedBoard).

% Check Diagonal Win for side 'Side'
checkDiagonalWin(Side, [FirstRow | RestOfBoard]) :-
        length(FirstRow, RowLen), !,
        checkDiagonalWin(Side, [FirstRow | RestOfBoard], 0, RowLen).
checkDiagonalWin(Side, [_FirstRow | RestOfBoard], RowLen, RowLen) :-
        checkDiagonalWin(Side, RestOfBoard, 0, RowLen), !.
checkDiagonalWin(Side, Board, Col, RowLen) :-
        length(Board, Len), winningStreakN(N), Len >= N,
        checkDiagonalLine(Side, Board, Col, 0, RowLen), !.
checkDiagonalWin(Side, Board, Col, RowLen) :-
        length(Board, Len), winningStreakN(N),
        Len >= N, NewCol is Col + 1,
        checkDiagonalWin(Side, Board, NewCol, RowLen).

checkDiagonalLine(_, _, _, Count, _) :-
        winningStreakN(Count), !.
checkDiagonalLine(Side, [FirstRow | RestOfBoard], Col, Count, RowLen) :-
        Col < RowLen,
        nth0(Col, FirstRow, Side), % Unifies element at idx Col with Side
        NewCol is Col + 1,
        NewCount is Count + 1,
        checkDiagonalLine(Side, RestOfBoard, NewCol, NewCount, RowLen).
