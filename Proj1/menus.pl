% Main Menu

mainMenu:-
	clearConsole,
	printFabrikTitle, nl,
	write('\t 1. Play'), nl,
	write('\t 2. Rules'), nl,
	write('\t 3. Exit'), nl, nl.

playMenu:-
	clearConsole,
	printFabrikTitle, nl,
	write('\t 1. MultiPlayer'), nl,
	write('\t 2. SinglePlayer'), nl,
	write('\t 3. AI VS AI'), nl,
	write('\t 4. Back'), nl, nl.

printFabrikTitle:-
	write('   *********************************'), nl,
	write('   *    __  __   __   __           *'), nl,
	write('   *   |_  |__| |__) |__| | |__/   *'), nl,
	write('   *   |   |  | |__) | \\  | |  \\   *'), nl,
	write('   *                               *'), nl,
	write('   *********************************'), nl.