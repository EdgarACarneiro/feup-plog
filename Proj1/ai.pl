:- dynamic(rowcolChange/1).

evaluateBoard(Side, Board, Value):-
	%Definir protocolo de avaliação aqui
	%Começa em 0 um Board inicial
	%Por cada n em linha ele conta mais, tem que ter fator de escala para fazer urge para quebrar os em linha
	fail.

% Possible values for Row and Col positions
genRowColFacts:-
	boardSize(N),
	genRowColFactsAux(0, N), !.

genRowColFactsAux(BoardSize, BoardSize).
genRowColFactsAux(Current, BoardSize):-
	asserta(rowcolChange(Current)),
	NewValue is (Current + 1),
	genRowColFactsAux(NewValue, BoardSize).

%Used to backtrace over the possible positions
nextPos(RowChange, ColChange) :-
        rowcolChange(RowChange),
        rowcolChange(ColChange).

%returns a List containing all the possible Boards by moving the workers
findWorkersBoards(Board, PossibleBoards):-
	findBothWorkers(Board, Row1, Col1, Row2, Col2),
	genRowColFacts,
	getWorkerBoards(Board, Row1, Col1, PossibleBoards1),
	getWorkerBoards(Board, Row2, Col2, PossibleBoards2),
	append(PossibleBoards1, PossibleBoards2, PossibleBoards).
	%printAllBoards(PossibleBoards).
	%tryWorkerPositions(Board, Row2, Col2, PossibleBoards2).
	%join das listas e remoção dos seus duplicados -> função joinAndTrim(List1, List2, FinalList)
	%devolução dos positions boards.

% All the boards obtained by moving a worker through a Board
getWorkerBoards(Board, Row, Col, ListOfBoards):-
	findall(TempPossibleBoards, (nextPos(NewRow, NewCol), moveWorker(Board, Row, Col, NewRow, NewCol, TempPossibleBoards)), ListOfBoards).

printAllBoards([]).
printAllBoards([Board | NextBoards]):-
	printBoard(Board, 9),
	printAllBoards(NextBoards).
%board5(B), findWorkersBoards(B, PP).