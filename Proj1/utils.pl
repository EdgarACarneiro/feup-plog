%% CONSTANTS
boardSize(9).
boardSize(11).

winningStreakN(4).

% TODO: Change this to handle strings and not only 1 char
getInput(Input):-
	get_code(KbInput),
	Input is KbInput - 48,	%Because of ASCII table
	get_char(_).			%for the Enter

% Site used: https://stackoverflow.com/questions/16441062/how-to-clear-screen-in-sicstus-prolog -> doesnt work
clearConsole:-
	clearConsole(60).

clearConsole(0).
clearConsole(N) :-
	nl, 
	N1 is N-1, 
	clearConsole(N1).

getEnter:-
	write('Press enter to continue.'), nl,
	get_char(_).