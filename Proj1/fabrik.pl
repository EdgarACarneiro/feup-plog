:- include('board.pl').
:- include('display.pl').
:- include('utils.pl').
:- include('menus.pl').
:- include('input.pl').
:- include('ai.pl').
:- include('test.pl').


fabrik:-
	mainMenuHandler.

initGame(Player1, Player2) :-
	genRowColFacts,
	boardSize(N), !,
	createBoard(B0, N), printBoard(B0, N),
	setWorker(Player1, black, B0, B1), printBoard(B1, N),
	setWorker(Player1, black, B1, B2), printBoard(B2, N),
	chooseStartingPlayer(Player1, Side), printBoard(B2, N),
	gameLoop(Player1, Player2, Side, N, B2).

gameLoop(Player1Function, Player2Function, black, BoardSize, Board):-
	call(Player1Function, black, Board, BoardSize, NewBoard),
	decideNextStep(Player1Function, Player2Function, black, BoardSize, NewBoard), !.

gameLoop(Player1Function, Player2Function, white, BoardSize, Board):-
	call(Player2Function, white, Board, BoardSize, NewBoard),
	decideNextStep(Player1Function, Player2Function, white, BoardSize, NewBoard), !.

decideNextStep(_Player1Function, _Player2Function, Side, _BoardSize, Board):-
	gameIsWon(Side, Board), !,
	%destroyRowColFacts,
	wonMsg(Side),
	getEnter, !.
decideNextStep(Player1Function, Player2Function, Side, BoardSize, Board):-
	changePlayer(Side, NewSide), !,
	gameLoop(Player1Function, Player2Function, NewSide, BoardSize, Board).


userFunction(Side, Board, BoardSize, NewBoard):-
	workerUpdate(Side, Board, TempBoard),
	printBoard(TempBoard, BoardSize),
	pieceInput(Side, Side, TempBoard, NewBoard),
	printBoard(NewBoard, BoardSize), !.

aiFunction(_Side, _Board, _BoardSize, _NewBoard):-
	%Mete aqui a chamada a função André
	fail.

setWorker(PlayerFunction, Side, Board, NewBoard):-
	PlayerFunction = 'userFunction',
	pieceInput(worker, Side, Board, NewBoard).
setWorker(PlayerFunction, _Side, _Board, _NewBoard):-
	PlayerFunction = 'aiFunction',
	%meter um worker random num sitio
	fail.

chooseStartingPlayer(Player1, Side):-
        Player1 = 'userFunction',
        getFirstPlayer(Side), !.
chooseStartingPlayer(Player1, _Side):-
	Player1 = 'aiFunction',
	%meter um random worker no board
	fail.
