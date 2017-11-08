:- include('board.pl').
:- include('display.pl').
:- include('utils.pl').
:- include('menus.pl').
:- include('input.pl').
:- include('ai.pl').
:- include('test.pl').


fabrik:-
	mainMenuHandler.

initGame :-
	boardSize(N), !,
	initGame(N).
initGame(N):-
	boardSize(N),
	createBoard(B0, N),
	printBoard(B0, N),
	pieceInput(worker, black, B0, B1),
	printBoard(B1, N),
	pieceInput(worker, white, B1, B2),
	printBoard(B2, N),
	getFirstPlayer(Side),
	printBoard(B2, N),
	gameLoop(Side, N, B2).

gameLoop(Side, BoardSize, Board):-
	workerUpdate(Side, Board, B1),
	printBoard(B1, BoardSize),
	pieceInput(Side, Side, B1, B2),
	printBoard(B2, BoardSize), !,
	decideNextStep(Side, BoardSize, B2).

decideNextStep(Side, _BoardSize, Board):-
	gameIsWon(Side, Board), !,
	wonMsg(Side),
	getEnter.
decideNextStep(Side, BoardSize, Board):-
	changePlayer(Side, NewSide), !,
	gameLoop(NewSide, BoardSize, Board).