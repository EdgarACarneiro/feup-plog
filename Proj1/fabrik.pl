:- include('board.pl').
:- include('display.pl').
:- include('utils.pl').
:- include('menus.pl').
:- include('input.pl').
:- include('test.pl').

/*
initGame:-
	boardSize(N),
	initGame(N).
initGame(N) :-
	boardSize(N),
	createBoard(B, N),
	printBoard(B, N).
*/

fabrik:-
	mainMenuHandler.

initGame:-
	boardSize(N),
	createBoard(B0, N),
	printBoard(B0, N),
	pieceInput(worker, black, B0, B1),
	printBoard(B1, N),
	pieceInput(worker, white, B1, B2),
	printBoard(B2, N),
	getFirstPlayer(Side),
	%gameLoop.
	getEnter.


%gameLoop:-
