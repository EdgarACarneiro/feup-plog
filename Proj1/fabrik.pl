:- include('board.pl').
:- include('display.pl').
:- include('utils.pl').
:- include('menus.pl').
:- include('test.pl').


initGame:-
    boardSize(N),
    initGame(N).
initGame(N) :-
    boardSize(N),
    createBoard(B, N),
    printBoard(B, N).