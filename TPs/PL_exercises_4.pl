% TP 4

%Ex.1
ligado(a,b). ligado(f,i).
ligado(a,c). ligado(f,j).
ligado(b,d). ligado(f,k).
ligado(b,e). ligado(g,l).
ligado(b,f). ligado(g,m).
ligado(c,g). ligado(k,n).
ligado(d,h). ligado(l,o).
ligado(d,i). ligado(i,f). 

% Problem - Cycles
%path(End, End, [End]).
%path(Start, End, [Start, Resto]) :-
%        ligado(Start, Next),
%        path(Next, End , Resto).
 

path(Start, End, Path):-
        path(Start, End, [Start], Path).

path(End, End, Path, Path). 
path(Start, End, Temp, Path) :-
        ligado(Start, Next),
        \+ member(Next, Temp),
        append(Temp, [Next] , NTemp),
        path(Next, End, NTemp, Path).

%setof(Len-Path, (path(Start, End, Path), begof(Path, Len)), L).