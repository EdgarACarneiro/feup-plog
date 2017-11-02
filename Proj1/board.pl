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
getElementAux(0, [Elem | _], Element):-
        Element = Elem, !. % added a cut

getElementAux(Col, [_ | RemainderOfLine], Element) :-
        Col1 is (Col - 1),
        getElementAux(Col1, RemainderOfLine, Element).

getElement(0, Col, [Line | _], Element):-
        !, getElementAux(Col, Line, Element).

getElement(Row, Col, [_ | RemainderRows], Element):-
        Row1 is (Row-1),
        getElement(Row1, Col, RemainderRows, Element).


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


% Gets the Intersections between the two workers, into an array of positions
getPossiblePositions(Board, _Positions):- % % TODO - also, assess if this should exist
        findWorker(Board, 0, 0, _WorkerRow, _WorkerCol).

findBothWorkers(Board, Positions):-
        findWorker(Board, 0, 0, WorkerRow1, WorkerCol1),
        nextPos(Board, WorkerRow1, WorkerCol1, NextRow, NextCol),
        findWorker(Board, NextRow, NextCol, WorkerRow2, WorkerCol2),
        Positions = [[WorkerRow1, WorkerCol1], [WorkerRow2, WorkerCol2]].

% Finds the next Worker starting from the given Row and Column.
findWorker(Board, Row, Col, WorkerRow, WorkerCol):-
        getElement(Row, Col, Board, Element),
        Element == worker, !, % TODO Assess cut usefulness.
        WorkerRow = Row,
        WorkerCol = Col.

% Updates the Current Position and retries
findWorker(Board, Row, Col, WorkerRow, WorkerCol):-
        nextPos(Board, Row, Col, NextRow, NextCol),
        findWorker(Board, NextRow, NextCol, WorkerRow, WorkerCol).

% Test End of Game
boardIsNotEmpty(Board) :-
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
        \+ checkRowWin(Side, FirstRow, 0),      %Is this line really needed? Isn't it repeating exactly line 100?
        checkHorizontalWin(Side, RestOfBoard).
checkRowWin(_, _, Count) :-
        winningStreakN(Count), !.
checkRowWin(Side, [FirstEl | RestOfRow], Count) :-
        Side = FirstEl,
        NewCount is Count + 1,
        checkRowWin(Side, RestOfRow, NewCount).
checkRowWin(Side, [FirstEl | RestOfRow], _Count) :-
        Side \= FirstEl,
        checkRowWin(Side, RestOfRow, 0).


% Check Vertical Win for side 'Side'
checkVerticalWin(Side, Board):-
        transpose(Board, TransposedBoard),
        checkHorizontalWin(Side, TransposedBoard).


% Check Diagonal Win for side 'Side'
checkDiagonalWin(Side, Board):-
        length(Board, Size),
        checkDiffDiagonals(Side, Board, Size).

%Check diagonals from [0, 0] to [0, BoardSize]
checkDiffDiagonals(Side, Board, BoardSize):-
        checkDiagonalWinAux(Side, Board, 0, BoardSize).
%Check diagonals from [0, 0] to [BoardSize, 0]
checkDiffDiagonals(Side, Board, BoardSize):-
        transpose(Board, TransposedBoard),
        checkDiagonalWinAux(Side, TransposedBoard, 0, BoardSize).

%Checks the different diagonal lines of the given board starting at [0, 0] ending at [0, BoardSize].
checkDiagonalWinAux(Side, Board, Col, BoardSize):-
        NumPositions is (BoardSize - Col),
        NumPositions >= 5,       %Number of cells there must be to happen N in a row
        checkDiagLineWin(Side, Board, Col, NumPositions).
checkDiagonalWinAux(Side, Board, Col, BoardSize):-
        NewCol is (Col + 1),
        NewCol < BoardSize,
        checkDiagonalWinAux(Side, Board, NewCol, BoardSize).

%Check If there are N in a row, in the given diagonal line.
%Starts at Position [0, Col], and Line has NumPositions to evaluate.
checkDiagLineWin(Side, Board, Col, NumPositions):-
        checkDiagLineWinAux(Side, Board, 0, Col, NumPositions, 0, _Count).

checkDiagLineWinAux(Side, Board, Row, Col, _NumPos, CurrCount, UpdatedCount):-
        checkDiagonalElem(Side, Board, Row, Col, CurrCount, UpdatedCount),
        winningStreakN(UpdatedCount), !.
checkDiagLineWinAux(Side, Board, Row, Col, NumPos, CurrCount, UpdatedCount):-
        checkDiagonalElem(Side, Board, Row, Col, CurrCount, UpdatedCount),
        NumPos > 0,
        NewRow is (Row + 1),
        NewCol is (Col + 1),
        NewNumPos is (NumPos - 1), !,
        checkDiagLineWinAux(Side, Board, NewRow, NewCol, NewNumPos, UpdatedCount, _NewCount).

%Check the Element in the given position and update Count.
checkDiagonalElem(Side, Board, Row, Col, Count, NewCount):-
        getElement(Row, Col, Board, Element),
        Element = Side, !, 
        NewCount is (Count + 1).
checkDiagonalElem(_Side, _Board, _Row, _Col, _Count, NewCount):-
        NewCount is 0.