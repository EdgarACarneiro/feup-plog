ligado(a,b).
ligado(a,c).
ligado(b,d).
ligado(b,e).
ligado(b,f).
ligado(c,g).
ligado(d,h).
ligado(d,i).
ligado(f,i).
ligado(f,j).
ligado(f,k).
ligado(g,l).
ligado(g,m).
ligado(k,n).
ligado(l,o).
ligado(i,f).

/* %% Problema: Ciclos
path(Start, End, [Start, End]).
path(Start, End, [Start | Resto]) :-
        ligado(Start, Next),
        path(Next, End, Resto).
*/

% Solution: construir caminho start-to-end em vez de end-to-start
path(Start, End, Path) :-
        path(Start, End, [Start], Path).
path(End, End, Path, Path).
path(Start, End, Temp, Path) :-
        ligado(Start, Next),
        \+ member(Next, Temp),
        append(Temp, [Next], NewTemp),
        path(Next, End, NewTemp, Path).


% setof(Len-Path, (path(Start, End, Path), length(Path, Len)), L).