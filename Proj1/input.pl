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