/* -*- Mode:Prolog; coding:iso-8859-1; indent-tabs-mode:nil; prolog-indent-width:8; prolog-paren-indent:3; tab-width:8; -*- */

test :- write('This is a test'), nl.

%% Exercicio 1 %%
/*
male('Aldo Burrows').
female('Christina Rose Scofield').
female('Lisa Rix').
male('Lincoln Burrows').
male('Michael Scofield').
female('Sara Tracendi').
female('Ella Scofield').
male('Lj Burrows').
parent('Lisa Rix', 'Lj Burrows').
parent('Lincoln Burrows', 'Lj Burrows').
parent('Aldo Burrows', 'Lincoln Burrows').
parent('Christina Rose Scofield', 'Lincoln Burrows').
*/

%% Exercicio 2 %%
pilot(lamb).
pilot(besenyei).
pilot(chambliss).
pilot(macLean).
pilot(mangold).
pilot(jones).
pilot(bonhomme).

team(breitling, lamb).
team(redBull, besenyei).
team(redBull, chambliss).
team('Mediterranean Racing Team', macLean).
team(cobra, mangold).
team(matador, jones).
team(matador, bonhomme).

plane(lamb, mx2).
plane(besenyei, edge540).
plane(chambliss, edge540).
plane(macLean, edge540).
plane(mangold, edge540).
plane(bonhomme, edge540).
plane(jones, edge540).

circuit(istanbul).
circuit(budapest).
circuit(porto).

winner(jones, porto).
winner(mangold, budapest).
winner(mangold, istanbul).

numGates(istanbul, 9).
numGates(porto, 5).
numGates(budapest, 6).

winningTeam(Team, Circuit) :- team(Team,Guy), winner(Circuit, Guy).

/*
pilot(P),winner(P,C),circuit(C),winner(P,C2),circuit(C2), C2 \= C. %c
circuit(X),numGates(X, N), N > 8. %d
pilot(P),plane(P,Plane),Plane \= edge540. %e
*/


%% Exercicio 8 %%
cargo(tecnico, rogerio).
cargo(tecnico, ivone).
cargo(engenheiro, daniel).
cargo(engenheiro, isabel).
cargo(engenheiro, oscar).
cargo(engenheiro, tomas).
cargo(engenheiro, ana).
cargo(supervisor, luis).
cargo(supervisor_chefe, sonia).
cargo(secretaria_exec, laura).
cargo(diretor, santiago).
chefiado_por(tecnico, engenheiro).
chefiado_por(engenheiro, supervisor).
chefiado_por(analista, supervisor).
chefiado_por(supervisor, supervisor_chefe).
chefiado_por(supervisor_chefe, director).
chefiado_por(secretaria_exec, director).



%% Exercicio 9 %%
aluno(joao, paradigmas).
aluno(maria, paradigmas).
aluno(joel, lab2).
aluno(joel, estruturas).
frequenta(joao, feup).
frequenta(maria, feup).
frequenta(joel, ist).
professor(carlos, paradigmas).
professor(ana_paula, estruturas).
professor(pedro, lab2).
funcionario(pedro, ist).
funcionario(ana_paula, feup).
funcionario(carlos, feup).

colega(A1, A2) :- aluno(A1, UC), aluno(A2, UC), A1 \= A2.
colega(A1, A2) :- frequenta(A1, Uni), frequenta(A2, Uni), A1 \= A2.
colega(P1, P2) :- funcionario(P1, Uni), funcionario(P2, Uni), P1 \= P2.



%% Exercicio 10 %%
comprou(joao, honda).
comprou(joao, uno).
ano(honda, 1997).
ano(uno, 1998).
valor(honda, 20000).
valor(uno, 7000).

pode_vender(Pessoa, Carro, AnoAtual) :-
        X is AnoAtual - 10,
        comprou(Pessoa, Carro),
        ano(Carro, AnoCarro),
        AnoCarro > X,
        valor(Carro, ValorCarro),
        ValorCarro < 10000.
        




