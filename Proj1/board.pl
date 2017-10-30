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


% Set piece on board -- not yet validating move
%% ...
%% ...

