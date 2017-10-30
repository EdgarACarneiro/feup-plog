:- include('board.pl').
:- include('display.pl').
:- include('utils.pl').
:- include('menus.pl').
:- use_module(library(lists)).

% Utils
boardSize(9).
boardSize(11).

initGame :-
    boardSize(N),
    initGame(N).
initGame(N) :-
    boardSize(N),
    createBoard(B, N),
    printFabrik(B, N).

% Sets the piece of the given type in the given position, in the given Board
setPiece(PieceType, Row, Col, Board, NewBoard):-
	RealRow is (Row-1),
	RealCol is (Col-1),
	%First remove the old piece
	nth0(RealRow, Board, RemovedLine, TempBoard),
	nth0(RealCol, RemovedLine, _, TempLine),
	%Inserting the new piece
	nth0(RealCol, NewLine, PieceType, TempLine),
	nth0(RealRow, NewBoard, NewLine, TempBoard).