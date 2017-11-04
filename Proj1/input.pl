% TODO: Change this to handle strings and not only 1 char
%Ask the User for a int
getInt(Input):-
        get_code(KbInput),
        Input is KbInput - 48,  %Because of ASCII table
        get_char(_), !.         %for the Enter

%Ask The User for a char
getChar(Input):-
        get_char(Input),
        get_char(_), !.

%'Press enter to continue' function
getEnter:-
        write('Press enter to continue.'), nl,
        get_char(_).

%Ask the User for a piece's row and column.
getPosition(PieceType, Row, Col):-
        getRow(PieceType, Row),
        getCol(PieceType, Col), !.

%Ask User for the Piece's row
getRow(PieceType, Row):-
        write('Choose '),
        write(PieceType),
        write('\'s row:'), nl,
        getInt(TempRow),
        Row is  (TempRow - 1).

%Ask User for the Piece's column
getCol(PieceType, Col):-
        write('Choose '), 
        write(PieceType), 
        write('\'s column:'), nl,
        getChar(ColLabel),
        getLabel(Col, ColLabel).

%User interface for placing a new piece on the board
pieceInput(PieceType, Side, Board, UpdatedBoard):-
        currentSideDisplay(Side),
        getPosition(PieceType, InputRow, InputCol),
        setPiece(PieceType, InputRow, InputCol, Board, UpdatedBoard).
pieceInput(PieceType, Side, Board, UpdatedBoard):-
        write('That play is not valid. Try again.'), nl, nl,
        pieceInput(PieceType, Side, Board, UpdatedBoard).


decidePlayerMsg:-
        write('Black Player, decide who goes first:'), nl,
        write('\t1. White Player'), nl,
        write('\t2. BlackPlayer'), nl, nl,
        write('Choose an option:'), nl.

getFirstPlayer(Side):-
        decidePlayerMsg,
        getInt(Choice),
        getFirstPlayerChoice(Choice, Side).
getFirstPlayer(Side):-
        write('Unrecognized choice. Try again.'), nl, nl,
        getFirstPlayer(Side).

getFirstPlayerChoice(1, white).
getFirstPlayerChoice(2, black).
getFirstPlayerChoice(_, _):- fail.


%Function used to update a worker position by moving it
workerUpdate(Side, Board, UpdatedBoard):-
        currentSideDisplay(Side),
        write('\tWorker current position'), nl,
        getPosition(worker, CurrRow, CurrCol),
        write('\tWorker new position'), nl,
        getPosition(worker, DestRow, DestCol),
        moveWorker(Board, CurrRow, CurrCol, DestRow, DestCol, UpdatedBoard).
workerUpdate(Side, Board, UpdatedBoard):-
        write('That play is not valid. Try again.'), nl, 
        write('Help:'), nl,
        write('\t * To maintain the worker in the same place, keep the new position equal to the old one.'), nl,
        write('\t * The Worker must be moved to an empty cell.'), nl, nl,
        workerUpdate(Side, Board, UpdatedBoard).