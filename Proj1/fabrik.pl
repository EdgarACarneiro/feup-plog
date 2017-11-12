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

%Game Initialization - set Workers and choose who starts
initGame(Player1, Player2) :-
	genRowColFacts,
	boardSize(N), !,
	createBoard(B0, N), printBoard(B0),
	setFirstWorker(Player1, black, B0, B1), printBoard(B1),
	setFirstWorker(Player2, black, B1, B2), printBoard(B2),
	chooseStartingPlayer(Player1, Side), printBoard(B2), !,
	gameLoop(Player1, Player2, Side, B2).
%If somehting failed on initGame, return to Play Menu
initGame(_, _):-
	playMenuHandler.

gameLoop(Player1Function, Player2Function, black, Board) :- !,
	call(Player1Function, black, Board, NewBoard), printBoard(NewBoard),
	decideNextStep(Player1Function, Player2Function, black, NewBoard), !.

gameLoop(Player1Function, Player2Function, white, Board) :- !,
	call(Player2Function, white, Board, NewBoard), printBoard(NewBoard),
	decideNextStep(Player1Function, Player2Function, white, NewBoard), !.

decideNextStep(_Player1Function, _Player2Function, Side, Board) :-
	gameIsWon(Side, Board), !,
	destroyRowColFacts, !,
	wonMsg(Side),
	getEnter, !.
decideNextStep(Player1Function, Player2Function, Side, Board) :-
	changePlayer(Side, NewSide), !,
	gameLoop(Player1Function, Player2Function, NewSide, Board).

%Executes a human Play, getting worker input and piece input
humanPlay(Side, Board, NewBoard) :-
	workerUpdate(Side, Board, TempBoard),
	printBoard(TempBoard),
	isPiecePlayPossible(TempBoard), !,
	pieceInput(Side, Side, TempBoard, NewBoard), !.
humanPlay(Side, _, _) :-
        changePlayer(Side, NewSide),
	wonMsg(NewSide), % Side loses if no play is possible
	getEnter, !, fail.

%Setting the First Workers on the Board - Game beggining
setFirstWorker('humanPlay', Side, Board, NewBoard) :-
	pieceInput(worker, Side, Board, NewBoard).
%For AI Functions
setFirstWorker(_, _Side, Board, NewBoard) :-
        boardSize(Size),
        random(0, Size, Row), random(0, Size, Col),
        setPiece(worker, Row, Col, Board, NewBoard).

%Choose the Player that starts putting pieces on the board
chooseStartingPlayer('humanPlay', Side) :-
        getFirstPlayer(Side), !.
chooseStartingPlayer(_, white). % always white - For AI functions 