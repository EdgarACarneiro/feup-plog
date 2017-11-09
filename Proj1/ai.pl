:- dynamic(rowcolChange/1).

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
	union(PossibleBoards1, PossibleBoards2, PossibleBoards). %No duplicates

% All the boards obtained by moving a worker through a Board
getWorkerBoards(Board, Row, Col, ListOfBoards):-
	findall(TempPossibleBoards, (nextPos(NewRow, NewCol), moveWorker(Board, Row, Col, NewRow, NewCol, TempPossibleBoards)), ListOfBoards).

getBoardMoves(Board, Side, PossibleBoards):-
	findWorkersBoards(Board, WorkerBoards), !,
	getPossibleBoards(Side, WorkerBoards, [], PossibleBoards),
	printAllBoards(PossibleBoards).

getPossibleBoards(_, [], AllBoards, AllBoards).
getPossibleBoards(Side, [WorkerBoard | OtherBoards], SoFarBoards, PossibleBoards):-
	findBothWorkers(WorkerBoard, Row1, Col1, Row2, Col2),
	getIntersections(WorkerBoard, Row1, Col1, Row2, Col2, IntersectionsList),
	genNewBoards(Side, WorkerBoard, IntersectionsList, [], NewBoards),
	write('HERE: '), nl, write(NewBoards), %NewBoards = [GG | Res], printBoard(GG, 3), nl,
	append(NewBoards, SoFarBoards, PossibleBoards), !,
	getPossibleBoards(Side, OtherBoards, PossibleBoards, _).

genNewBoards(_, _, [], Boards, Boards):-
	write('BUBa: '), write(Boards), nl, nl, nl, !.
genNewBoards(Side, Board, [Intersec | OtherIntersec], FoundBoards, NewBoards):-
	write('Shit: '), write(NewBoards), nl,
	Intersec = [Row, Col],
	setPiece(Side, Row, Col, Board, TempBoard),
	append([TempBoard], FoundBoards, NewBoards), !,
	genNewBoards(Side, Board, OtherIntersec, NewBoards, _), !.


printAllBoards([]).
printAllBoards([Board | NextBoards]):-
	boardSize(Size),
	printBoard(Board, Size),
	printAllBoards(NextBoards).

evaluateBoard(Side, Board, Value):-
	%Definir protocolo de avaliação aqui
	%Começa em 0 um Board inicial
	%Por cada n em linha ele conta mais, tem que ter fator de escala para fazer urge para quebrar os em linha
	fail.

	%tryWorkerPositions(Board, Row2, Col2, PossibleBoards2).
	%join das listas e remoção dos seus duplicados -> função joinAndTrim(List1, List2, FinalList)
	%devolução dos positions boards.