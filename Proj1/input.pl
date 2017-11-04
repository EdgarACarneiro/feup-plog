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

%Ask User for the Piece's row
getRow(PieceType, Row):-
        write('Choose '),
        write(PieceType),
        write('\'s row:'), nl,
        getInt(Row).

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
        getRow(PieceType, Row),
        InputRow is (Row - 1),
        getCol(PieceType, InputCol),
        setPiece(worker, InputRow, InputCol, Board, UpdatedBoard).
pieceInput(PieceType, Side, Board, UpdatedBoard):-
        write('That play is not valid. Try again:'), nl, nl,
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