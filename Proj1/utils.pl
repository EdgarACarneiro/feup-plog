%% CONSTANTS

boardSize(9) :- !.
%boardSize(11) :- !.

winningStreakN(5).

changePlayer(black, white).
changePlayer(white, black).

% Next Position for the given board
nextPos(Board, CurrRow, CurrCol, CurrRow, NextCol):-
	NextCol is (CurrCol + 1),
    	length(Board, Size),
   	NextCol < Size, !.
nextPos(_Board, CurrRow, _CurrCol, NextRow, 0):-
    	NextRow is (CurrRow + 1).

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


% matrixToList converts a list of lists into a single list containing all elements
matrixToList([FirstList | NextLists], List) :-
	matrixToList(NextLists, TmpList),
	append(FirstList, TmpList, List).
matrixToList([], []).