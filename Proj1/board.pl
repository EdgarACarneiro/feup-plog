% Generate a board predicate with N x N empty spaces
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


% Access the element in the [Row,Col] position of the given board
getElementAux(0, [Elem | _], Element):-
        Element = Elem, !. % added a cut

getElementAux(Col, [_ | RemainderOfLine], Element) :-
        Col1 is (Col-1),
        getElementAux(Col1, RemainderOfLine, Element).

getElement(0, Col, [Line | _], Element):-
        getElementAux(Col, Line, Element).

getElement(Row, Col, [_ | RemainderRows], Element):-
        Row1 is (Row-1),
        getElement(Row1, Col, RemainderRows, Element).


% Sets the piece of the given type in the given position, in the given Board
setPiece(PieceType, Row, Col, Board, NewBoard):-
        RealRow is (Row-1),
        RealCol is (Col-1),
        %First remove the old piece
        nth0(RealRow, Board, RemovedLine, TempBoard),
        nth0(RealCol, RemovedLine, _, TempLine),
        %Inserting the new piece
        nth0(RealCol, NewLine, PieceType, TempLine),
        nth0(RealRow, NewBoard, NewLine, TempBoard).


% Gets the Intersections between the two workers, into an array of positions
getPossiblePositions(Board, Positions):-
        findWorker(Board, 0, 0, WorkerRow, WorkerCol).

%Finds the next Worker starting from the given Row and Column.
findWorker(Board, Row, Col, WorkerRow, WorkerCol):-
        getElement(Row, Col, Board, Element),
        Element == worker, !, % This cut useless?
        WorkerRow = Row,
        WorkerCol = Col.

%Trying the next Column
findWorker(Board, Row, Col, WorkerRow, WorkerCol):-
        NewCol is (Col+1),
        length(Board, Size),
        NewCol < Size,
        findWorker(Board, Row, NewCol, WorkerRow, WorkerCol).

%Starting in a new line
findWorker(Board, Row, _, WorkerRow, WorkerCol):-
        NewCol is 0,
        NewRow is (Row+1),
        findWorker(Board, NewRow, NewCol, WorkerRow, WorkerCol).