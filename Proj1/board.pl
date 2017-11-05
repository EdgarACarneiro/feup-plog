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


% % Validation Predicates
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

% PLACEHOLDER - probably receive [Row1,Col1],[Row2,Col2] positions of workers 
getIntersections(Board, Row1, Col1, Row2, Col2, Positions) :-
        lineOfSight(Board, Row1, Col1, Pos1),
        lineOfSight(Board, Row2, Col2, Pos2),
        intersection(Pos1, Pos2, Positions).

isValidPosition(Board, Row, Col) :-
        Col >= 0, boardSize(N), Col =< N,
        getElement(Board, Row, Col, none).

% Spreads outwards from the worker's position and stops on end of board or when a piece blocks the line of sight
% Returns all positions of the worker's lines of sight
lineOfSight(Board, Row, Col, Positions) :-
        horizontalLineOfSight(Board, Row, Col, HorPos1, 1),
        horizontalLineOfSight(Board, Row, Col, HorPos2, -1),
        verticalLineOfSight(Board, Row, Col, VerPos1, 1),
        verticalLineOfSight(Board, Row, Col, VerPos2, -1),
        diagonalLineOfSight(Board, Row, Col, DiagPos1, 1, 1),
        diagonalLineOfSight(Board, Row, Col, DiagPos2, 1, -1),
        diagonalLineOfSight(Board, Row, Col, DiagPos3, -1, 1),
        diagonalLineOfSight(Board, Row, Col, DiagPos4, -1, -1),
        append(HorPos1, HorPos2, TmpPos1),
        append(VerPos1, TmpPos1, TmpPos2),
        append(VerPos2, TmpPos2, TmpPos3),
        append(DiagPos1, TmpPos3, TmpPos4),
        append(DiagPos2, TmpPos4, TmpPos5),
        append(DiagPos3, TmpPos5, TmpPos6),
        append(DiagPos4, TmpPos6, Positions).

%% TODO passar validacoes e funcoes auxiliares para outro ficheiro goddamn it

%% TODO
%% TODO Eliminar horizontal e vertical lines of sight e por tudo diagonal com change 0 na Row/Col

horizontalLineOfSight(Board, Row, Col, Positions, ColChange) :-
        NewCol is Col + ColChange,
        isValidPosition(Board, Row, NewCol), !,
        horizontalLineOfSight(Board, Row, NewCol, OtherPositions, ColChange),
        append([[Row, NewCol] | Positions], OtherPositions, Positions).
horizontalLineOfSight(_Row, _Col, _Board, _Positions, _ColChange) :- !. % check if element at Row,Col is not none ?

verticalLineOfSight(Board, Row, Col, Positions, RowChange) :-
        NewRow is Row + RowChange,
        isValidPosition(Board, NewRow, Col), !,
        verticalLineOfSight(Board, NewRow, Col, OtherPositions, RowChange),
        append([[NewRow, Col] | Positions], OtherPositions, Positions).
verticalLineOfSight(_Row, _Col, _Board, _Positions, _ColChange) :- !.          

diagonalLineOfSight(Board, Row, Col, Positions, RowChange, ColChange) :-
        NewRow is Row + RowChange, NewCol is Col + ColChange,
        isValidPosition(Board, NewRow, NewCol), !,
        diagonalLineOfSight(Board, NewRow, NewCol, OtherPositions, RowChange, ColChange),
        append([NewRow, NewCol], OtherPositions, Positions).
diagonalLineOfSight(_Row, _Col, _Board, _Positions, _RowChange, _ColChange) :- !.
                                                               
                                                                   
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