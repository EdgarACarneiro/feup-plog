:- dynamic(rowcolChange/1).

%TODO - needed a float here for 0.6
enemyFactor(2).

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
findWorkerBoards(Board, PossibleBoards):-
	findBothWorkers(Board, Row1, Col1, Row2, Col2),
	genRowColFacts,
	getWorkerBoards(Board, Row1, Col1, PossibleBoards1),
	getWorkerBoards(Board, Row2, Col2, PossibleBoards2),
	union(PossibleBoards1, PossibleBoards2, PossibleBoards). %No duplicates

% All the boards obtained by moving a worker through a Board
getWorkerBoards(Board, Row, Col, ListOfBoards):-
	findall(TempPossibleBoards, (nextPos(NewRow, NewCol), moveWorker(Board, Row, Col, NewRow, NewCol, TempPossibleBoards)), ListOfBoards).

%Get all the boards where a piece can go, given a List of Boards with different worker positions
getPieceBoards(Side, WorkerBoards, PossibleBoards):-
	getPieceBoardsAux(Side, WorkerBoards, [], PossibleBoards).

getPieceBoardsAux(_, [], PossibleBoards, PossibleBoards) :- !.
getPieceBoardsAux(Side, [WorkerBoard | OtherBoards], SoFarBoards, PossibleBoards):-
	findBothWorkers(WorkerBoard, Row1, Col1, Row2, Col2),
	getIntersections(WorkerBoard, Row1, Col1, Row2, Col2, IntersectionsList),
	genNewBoards(Side, WorkerBoard, IntersectionsList, [], NewBoards),
	append(NewBoards, SoFarBoards, UpdatedBoards), !,
	getPieceBoardsAux(Side, OtherBoards, UpdatedBoards, PossibleBoards).

%Generates all the boards associated to a certain board with fix workers.
%Boards with the different places where the piece can be played
genNewBoards(_, _, [], AllBoards, AllBoards):- !.
genNewBoards(Side, Board, [Intersec | OtherIntersec], FoundBoards, AllBoards):-
	Intersec = [Row, Col],
	setPiece(Side, Row, Col, Board, TempBoard),
	append([TempBoard], FoundBoards, UpdatedBoards), !,
	genNewBoards(Side, Board, OtherIntersec, UpdatedBoards, AllBoards).

%Returns all the possible resulting boards, taking into account the move worker play and the set piece play
getPossibleBoards(Side, Board, PossibleBoards):-
	findWorkerBoards(Board, WorkerBoards), !,
	getPieceBoards(Side, WorkerBoards, PossibleBoards).


%Evaluates a board and returns the correspondent Value
evaluateBoard(Side, Board, BoardValue):-
	horizontalEvaluation(Side, Board, HorizontalValue),
	verticalEvaluation(Side, Board, VerticalValue),
	BoardValue is (HorizontalValue + VerticalValue).

%Makes an Horizontal Evaluation of the given board
horizontalEvaluation(Side, Board, Value):-
	horizontalEvaluationAux(Side, Board, 0 , Value), !.
horizontalEvaluationAux(_, [], FinalValue, FinalValue):- !.
horizontalEvaluationAux(Side, [FirstRow | RestOfBoard], CurrentValue, FinalValue):-
	horizontalRowEvaluation(Side, FirstRow, 0, 0, CurrentValue, LineFValue),
	horizontalEvaluationAux(Side, RestOfBoard, LineFValue, FinalValue).

horizontalRowEvaluation(_, [], _, _, LineFValue, LineFValue):- !.
%Succession of Side Pieces
horizontalRowEvaluation(Side, [Side | OtherCols], Streak, _EnemyStreak, CurrentValue, LineFValue):-
	NewStreak is (Streak + 1),
	NewValue is (CurrentValue + (NewStreak * NewStreak)),
	%neighborEnemyStreaks
	horizontalRowEvaluation(Side, OtherCols, NewStreak, 0, NewValue, LineFValue).
%Succession of Enemy Pieces
horizontalRowEvaluation(Side, [Col | OtherCols], _Streak, EnemyStreak, CurrentValue, LineFValue):-
	changePlayer(Side, Enemy),
	Col = Enemy,
	NewEnemyStreak is (EnemyStreak + 1),
	NewValue is (CurrentValue - (NewEnemyStreak * NewEnemyStreak)),
	horizontalRowEvaluation(Side, OtherCols, 0, NewEnemyStreak, NewValue, LineFValue).
%When nor white nor black
horizontalRowEvaluation(Side, [_Col | OtherCols], _, _, CurrentValue, Value):-
	horizontalRowEvaluation(Side, OtherCols, 0, 0, CurrentValue, Value).

%Makes a vertical Evaluation of the given board
verticalEvaluation(Side, Board, Value):-
	transpose(Board, TransposedBoard),
	horizontalRowEvaluation(Side, TransposedBoard, Value).

%Makes a diagonal Evaluation of the given board
diagonalEvaluation(_Side, _Board, _Value):-
	%TODO,
	fail.

evaluateElement(Row, Col, Board, Streak, Value):-
	getElement(Board, Row, Col, Side),
	NewStreak is (Streak + 1),
	TempValue is (NewStreak * NewStreak),
	%evaluateEnemyNeighborhood(Side, Board,Row, Col, CounterValue),
	Valus is (TempValue + CounterValue),
	getNextPosition(Row, Col, NRow, NCol), !,
	evaluateElement(Side, Board, Row, Col, NewStreak, Value).


getNextPosition(Row, Col, Row, NCol):-
	NCol is (Col + 1),
	boardSize(Size),
	NCol < Size.

getNextPosition(Row, _Col, NRow, 0):-
	NRow is (Row + 1).

printAllBoards([]).
printAllBoards([Board | NextBoards]):-
	boardSize(Size),
	printBoard(Board, Size),
	printAllBoards(NextBoards).