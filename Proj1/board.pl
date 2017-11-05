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
getElement(Board, Row, Col, Element):-
        nth0(Row, Board, RowLine, _),
        nth0(Col, RowLine, Element, _), !.


%% Validation Predicates
% Worker can be played if [Row,Col]=none
isValidPlay(worker, Row, Col, Board) :- !,
        getElement(Board, Row, Col, none).

% No conditions if there is no piece
isValidPlay(none, _, _, _) :- !.

% White/Black pieces can be played if [Row,Col] is in intersection of Workers' lines of sight
isValidPlay(_, Row, Col, Board) :-
        getElement(Board, Row, Col, none),
        isIntersection(Board, Row, Col).

% Fetches the position of both workers and checks if [Row,Col] is in their lines of sight
isIntersection(Board, Row, Col) :-
        findBothWorkers(Board, Row1, Col1, Row2, Col2),
        getIntersections(Board, Row1, Col1, Row2, Col2, Positions),
        member([Row, Col], Positions).

% Gets the intersections of the workers' lines of sight
getIntersections(Board, Row1, Col1, Row2, Col2, Positions) :-
        positionsInSight(Board, Row1, Col1, Pos1), write(Pos1), nl, % TODO delete write
        positionsInSight(Board, Row2, Col2, Pos2), write(Pos2), nl, % TODO delete write
        intersection(Pos1, Pos2, Positions).

% Position is in Board and is empty ?
isValidPosition(Board, Row, Col) :-
        boardSize(N),
        Row >= 0, Col >= 0,
        Row =< N, Col =< N,
        getElement(Board, Row, Col, none).

% Possible values for coordinates' change
coordinateChange(0).
coordinateChange(1).
coordinateChange(-1).

rowColChange(RowChange, ColChange) :-
        coordinateChange(RowChange),
        coordinateChange(ColChange),
        \+ (RowChange = 0, ColChange = 0).

% Spreads outwards from the worker's position and stops on end of board or when a piece blocks the line of sight
% Returns all positions of the worker's lines of sight
positionsInSight(Board, Row, Col, Positions) :-
        findall(PartialPositions, (rowColChange(RChange,CChange), lineOfSight(Board, Row, Col, RChange, CChange, PartialPositions)), ListOfLists),
        matrixToList(ListOfLists, Positions).

lineOfSight(Board, Row, Col, RowChange, ColChange, Positions) :-
        NewRow is Row + RowChange, NewCol is Col + ColChange,
        isValidPosition(Board, NewRow, NewCol), !,
        lineOfSight(Board, NewRow, NewCol, RowChange, ColChange, OtherPositions),
        append([[NewRow, NewCol]], OtherPositions, Positions).
lineOfSight(_Board, _Row, _Col, _RowChange, _ColChange, []) :- !.

                                                                   
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
        \+ (Row1 = Row2, Col1 = Col2), !. % Cut prevents backtracking over Row/Col permutations 

% Finds a worker's position
findWorker(Board, OutputRow, OutputCol) :-
        nth0(OutputRow, Board, TmpRow),
        nth0(OutputCol, TmpRow, worker).

% Tests if board has any empty space
boardIsNotFull(Board) :-
        nth0(_, Board, TmpRow),
        nth0(_, TmpRow, none).

% Side has won ?
gameIsWon(PieceSide, Board):-
        checkHorizontalWin(PieceSide, Board).
gameIsWon(PieceSide, Board):-
        checkVerticalWin(PieceSide, Board).
gameIsWon(PieceSide, Board):-
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
checkDiagonalWin(Side, Board) :-
        length(Board, Size),
        checkDiffDiagonals(Side, Board, Size).

%Check diagonals from [0, 0] to [0, BoardSize]
checkDiffDiagonals(Side, Board, BoardSize) :-
        checkDiagonalWinAux(Side, Board, 0, BoardSize).
%Check diagonals from [0, 0] to [BoardSize, 0]
checkDiffDiagonals(Side, Board, BoardSize) :-
        transpose(Board, TransposedBoard),
        checkDiagonalWinAux(Side, TransposedBoard, 0, BoardSize).

%Checks the different diagonal lines of the given board starting at [0, 0] ending at [0, BoardSize].
checkDiagonalWinAux(Side, Board, Col, BoardSize) :-
        NumPositions is (BoardSize - Col),
        winningStreakN(WinN),
        NumPositions >= WinN,       %Number of cells there must be to happen N in a row
        checkDiagLineWin(Side, Board, Col, NumPositions).
checkDiagonalWinAux(Side, Board, Col, BoardSize) :-
        NewCol is (Col + 1),
        NewCol < BoardSize,
        checkDiagonalWinAux(Side, Board, NewCol, BoardSize).

%Check If there are N in a row, in the given diagonal line.
%Starts at Position [0, Col], and Line has NumPositions to evaluate.
checkDiagLineWin(Side, Board, Col, NumPositions) :-
        checkDiagLineWinAux(Side, Board, 0, Col, NumPositions, 0, _Count).

checkDiagLineWinAux(Side, Board, Row, Col, _NumPos, CurrCount, UpdatedCount) :-
        checkDiagonalElem(Side, Board, Row, Col, CurrCount, UpdatedCount),
        winningStreakN(UpdatedCount), !.
checkDiagLineWinAux(Side, Board, Row, Col, NumPos, CurrCount, UpdatedCount) :-
        checkDiagonalElem(Side, Board, Row, Col, CurrCount, UpdatedCount),
        NumPos > 0,
        NewRow is (Row + 1),
        NewCol is (Col + 1),
        NewNumPos is (NumPos - 1), !,
        checkDiagLineWinAux(Side, Board, NewRow, NewCol, NewNumPos, UpdatedCount, _NewCount).

%Check the Element in the given position and update Count.
checkDiagonalElem(Side, Board, Row, Col, Count, NewCount) :-
        getElement(Board, Row, Col, Element),
        Element = Side, !, 
        NewCount is (Count + 1).
checkDiagonalElem(_Side, _Board, _Row, _Col, _Count, NewCount) :-
        NewCount is 0.


moveWorker(Board, Row, Col, DestRow, DestCol, UpdatedBoard):-
        getElement(Board, Row, Col, worker),
        setPiece(none, Row, Col, Board, TempBoard),
        setPiece(worker, DestRow, DestCol, TempBoard, UpdatedBoard).