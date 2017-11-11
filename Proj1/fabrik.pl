:- include('board.pl').
:- include('display.pl').
:- include('utils.pl').
:- include('menus.pl').
:- include('input.pl').
:- include('ai.pl').
:- include('test.pl').
:- use_module(library(random)).


fabrik:-
	mainMenuHandler.

initGame(Player1, Player2) :-
	genRowColFacts,
	boardSize(N), !,
	createBoard(B0, N), printBoard(B0),
	setFirstWorker(Player1, black, B0, B1), printBoard(B1),
	setFirstWorker(Player1, black, B1, B2), printBoard(B2),
	chooseStartingPlayer(Player1, Side), printBoard(B2),
	gameLoop(Player1, Player2, Side, B2).

gameLoop(Player1Function, Player2Function, black, Board) :-
	call(Player1Function, black, Board, NewBoard),
	decideNextStep(Player1Function, Player2Function, black, NewBoard), !.

gameLoop(Player1Function, Player2Function, white, Board) :-
	call(Player2Function, white, Board, NewBoard),
	decideNextStep(Player1Function, Player2Function, white, NewBoard), !.

decideNextStep(_Player1Function, _Player2Function, Side, Board) :-
	gameIsWon(Side, Board), !,
	%destroyRowColFacts,
	wonMsg(Side),
	getEnter, !.
decideNextStep(Player1Function, Player2Function, Side, Board) :-
	changePlayer(Side, NewSide), !,
	gameLoop(Player1Function, Player2Function, NewSide, Board).


userFunction(Side, Board, NewBoard) :-
	workerUpdate(Side, Board, TempBoard),
	printBoard(TempBoard),
	pieceInput(Side, Side, TempBoard, NewBoard),
	printBoard(NewBoard), !.

aiFunction(Side, Board, NewBoard) :-
	getBestPlay(Side, Board, NewBoard).

setFirstWorker(PlayerFunction, Side, Board, NewBoard) :-
	PlayerFunction = 'userFunction',
	pieceInput(worker, Side, Board, NewBoard).
setFirstWorker(PlayerFunction, _Side, Board, NewBoard) :-
	PlayerFunction = 'aiFunction',
        boardSize(Size),
        random(0, Size, Row), random(0, Size, Col),
        setPiece(worker, Row, Col, Board, NewBoard).

chooseStartingPlayer(Player1, Side) :- % TODO passar igualdade para a definicao do predicado
        Player1 = 'userFunction',
        getFirstPlayer(Side), !.
chooseStartingPlayer(Player1, white) :- % same
	Player1 = 'aiFunction'.
