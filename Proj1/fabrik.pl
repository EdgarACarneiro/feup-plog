:- include('board.pl').
:- include('utils.pl').

% Utils
boardSize(9).
boardSize(11).
initGame(N) :-
        boardSize(N),
        createBoard(B, N),
        printFabrik(B, N).
        

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
