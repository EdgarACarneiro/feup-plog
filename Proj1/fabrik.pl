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
	genRowColFacts,
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
	gameLoop(userFunction, userFunction, Side, N, B2).

gameLoop(Player1Function, _Player2Function, black, BoardSize, Board):-
	call(Player1Function, Side, Board, BoardSize, NewBoard),
	decideNextStep(Side, BoardSize, NewBoard).

gameLoop(_Player1Function, Player2Function, white, BoardSize, Board):-
	call(Player2Function, Side, Board, BoardSize, NewBoard),
	decideNextStep(Side, BoardSize, NewBoard).

userFunction(Side, Board, BoardSize, NewBoard):-
	workerUpdate(Side, Board, TempBoard),
	printBoard(TempBoard, BoardSize),
	pieceInput(Side, Side, TempBoard, NewBoard),
	printBoard(NewBoard, BoardSize), !.

aiFunction(_Side, _Board, _BoardSize, _NewBoard):-
	%Mete aqui a chamada a função André
	fail.

decideNextStep(_Player1Function, _Player2Function, Side, _BoardSize, Board):-
	gameIsWon(Side, Board), !,
	wonMsg(Side),
	getEnter.
decideNextStep(Player1Function, Player2Function, Side, BoardSize, Board):-
	changePlayer(Side, NewSide), !,
	gameLoop(Player1Function, Player2Function, NewSide, BoardSize, Board).