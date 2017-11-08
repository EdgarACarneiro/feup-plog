:- use_module(library(lists)).

evaluateBoard(Side, Board, Value):-
	%Definir protocolo de avaliação aqui
	%Começa em 0 um Board inicial
	%Por cada n em linha ele conta mais, tem que ter fator de escala para fazer urge para quebrar os em linha
	fail.

% Possible values for coordinates' change
rowcolChange(0).
rowcolChange(1).
rowcolChange(2).
rowcolChange(3).
rowcolChange(4).
rowcolChange(5).
rowcolChange(6).
rowcolChange(7).
rowcolChange(8).

nextElement(RowChange, ColChange) :-
        rowcolChange(RowChange),
        rowcolChange(ColChange).

%returns a Board containing all the possibly accessible positions by moving workers
findWorkersPossibleBoards(Board, PositionsBoard):-
	findBothWorkers(Board, Row1, Col1, Row2, Col2),
	getWorkerPossibleBoards(Board, Row1, Col1, PossibleBoards1),
	getWorkerPossibleBoards(Board, Row2, Col2, PossibleBoards2),
	append(PossibleBoards1, PossibleBoards2, PossibleBoards).
	%printAllBoards(PossibleBoards).
	%tryWorkerPositions(Board, Row2, Col2, PossibleBoards2).
	%join das listas e remoção dos seus duplicados -> função joinAndTrim(List1, List2, FinalList)
	%devolução dos positions boards.

% All the boards obtained by moving a worker to a available Board
getWorkerPossibleBoards(Board, Row, Col, ListOfBoards):-
	findall(TempPossibleBoards, (nextElement(RChange,CChange), moveWorker(Board, Row, Col, RChange, CChange, TempPossibleBoards)), ListOfBoards).

printAllBoards([]).
printAllBoards([Board | NextBoards]):-
	printBoard(Board, 9),
	printAllBoards(NextBoards).