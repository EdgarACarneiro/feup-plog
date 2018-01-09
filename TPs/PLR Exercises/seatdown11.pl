:- use_module(library(lists)).
:- use_module(library(clpfd)).

%% P11
% Numa viagem de autocarro pretendesse que as pessoas
% que vão sentadas uma lado da outra na viagem de ida
% não venham juntas na viagem de volta.

% seatdown(+Persons, -Seats1, -Seats2)

seatdown(L, Seats1, Seats2) :-
  length(L, NPersons),
  length(Seats1, NPersons), length(Seats2, NPersons),
  domain(Seats1, 1, NPersons), domain(Seats2, 1, NPersons),
  %
  all_distinct(Seats1), all_distinct(Seats2),
  distinct_two_by_two(Seats1, Seats2),
  %
  append(Seats1, Seats2, Seats),

  labeling([time_out(500, success)], Seats),

  show_Seats(L, Seats1),nl,
  show_Seats(L, Seats2),nl.

distinct_two_by_two([], _).
distinct_two_by_two([_], _).
distinct_two_by_two([X1, X2 | Seats1], Seats2) :-
  % for each pair X1,X2 assign distinction from each pair in Seats2
  pair_not_in_list([X1, X2], Seats2),
  distinct_two_by_two(Seats1, Seats2).

pair_not_in_list(_, []).
pair_not_in_list(_, [_]).
pair_not_in_list([X1, X2], [Y1, Y2 | Seats2]) :-
  (X1 #\= Y1 #/\ X1 #\= Y2) #\/ (X2 #\= Y1 #/\ X2 #\= Y2),
  pair_not_in_list([X1, X2], Seats2).

show_Seats(_,[]).
show_Seats(Persons, [Seat]) :-
    nth1(Seat, Persons, Person),
    write(Person),nl.
show_Seats(Persons, [Seat1,Seat2|Seats]) :-
    nth1(Seat1, Persons, Person1),
    nth1(Seat2, Persons, Person2),
    write(Person1 - Person2), nl,
    show_Seats(Persons, Seats).

% seatdown([a,b,c,d,e,f,g], S1, S2).
% S1=[1,2|_], S2=[_,_,1,2|_], seatdown([a,b,c,d,e,f,g], S1, S2).