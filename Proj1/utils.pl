%% CONSTANTS

boardSize(9) :- !.
%boardSize(11) :- !.

winningStreakN(5).

changePlayer(black, white).
changePlayer(white, black).

unknownInput:-
	write('Invalid option chosen.'), nl,
	getEnter.

clearConsole:-
	clearConsole(60).
clearConsole(0).
clearConsole(N) :-
	nl, 
	N1 is N-1, 
	clearConsole(N1).
