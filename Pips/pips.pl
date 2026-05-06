% ============================================================
% pips.pl
% Assignatura : Llenguatges de Programació, curs 2025-26
% Pràctica    : Pràctica final – PROLOG (Pips puzzle)
% Data        : 2025
% Nom         : 
%               - Carolina Marín Sánchez
%               - Nour Iman Mehannek Samah
% Grup        : 101
% Professors  : 
%               - Cabot Nadal, Miquel Àngel
%               - Oliver Tomàs, Antoni
% Convocatòria: Ordinària
%
% ============================================================
% COM FER SERVIR ELS PREDICATS
% ============================================================
%
%   1) Comprovar que una solució coneguda és correcta:
%      ?- puzzle(20250818, easy, R, P, S), solucio_pips(R, P, S).
%
%   2) Calcular la solució d'un puzle i comparar amb l'esperada:
%      ?- puzzle(20250818, easy, R, P, S),
%         solucio_pips(R, P, SC), S = SC.
%
%   3) Calcular la solució directament sense usar puzzle/5:
%      ?- solucio_pips(
%           [region(empty, nil, [[0,0]]),
%            region(equals, nil, [[0,1],[0,2],[1,1],[1,2]]),
%            region(sum, 5, [[0,3]]),
%            region(sum, 12, [[2,1],[2,2]])],
%           [[2,2],[5,2],[2,3],[6,6]],
%           Solucio).
%
%   4) Imprimir el tauler amb la solució (opcional):
%      ?- puzzle(20250818, easy, R, P, S), imprimeix_solucio(R, P, S).
%
% ============================================================
% ASPECTES OPCIONALS IMPLEMENTATS
% ============================================================
%
%   - Impressió del tauler per terminal: imprimeix_solucio/3
%     Mostra els valors de cada casella del tauler, i ' . '
%     per les posicions fora de les regions.
%
% ============================================================
% DISSENY LÒGIC
% ============================================================
%
%   El predicat principal solucio_pips/3 funciona tant per
%   comprovar (tots els arguments instanciats) com per generar
%   la solució (Solucio no instanciada), gracies al backtracking.
%
%   El procés és:
%   1. S'extreuen totes les caselles valides del tauler a partir
%      de les regions (totes_caselles/2).
%   2. Per a cada peça, es trien dues caselles adjacents i lliures
%      on col·locar-la (col_loca_peces/5). El tauler s'acumula
%      com una llista de parells Coordenada-Valor.
%   3. Un cop col·locades totes les peces, es comproven totes les
%      restriccions de les regions (comprova_regions/2).
%   Si alguna restricció falla, Prolog fa backtracking i prova
%   una col·locació diferent.
%
% ============================================================

% ============================================================
% BASE DE CONEIXEMENTS: PUZLES
% Format: puzzle(ID, Dificultat, Regions, Peces, Solucio)
% ============================================================

:- consult('puzzles.pl').

% ============================================================
% solucio_pips(+Regions, +Peces, ?Solucio)
%
%   Predicat principal. Donades les Regions i les Peces,
%   comprova (si Solucio esta instanciada) o genera (si no
%   ho esta) la Solucio del puzle.
%
%   Regions : llista de region(Condicio, Objectiu, Caselles)
%   Peces   : llista de [V1, V2] (valors de cada peca de domino)
%   Solucio : llista de [[F1,C1],[F2,C2]] (posicio de cada peca)
% ============================================================
solucio_pips(Regions, Peces, Solucio) :-
    % Pas 1: obtenir totes les caselles valides del tauler
    totes_caselles(Regions, Caselles),
    % Pas 2: col.locar totes les peces al tauler
    col_loca_peces(Peces, Caselles, [], Tauler, Solucio),
    % Pas 3: verificar que totes les regions compleixen la restriccio
    comprova_regions(Regions, Tauler).

% ============================================================
% totes_caselles(+Regions, -Caselles)
%
%   Extreu la llista (sense duplicats) de totes les caselles
%   que apareixen a les regions del puzle.
%
%   Regions  : llista de region(_, _, Caselles)
%   Caselles : llista de coordenades [Fila, Columna]
% ============================================================
totes_caselles([], []).
totes_caselles([region(_, _, Cells)|Rs], Totes) :-
    totes_caselles(Rs, Rest),
    unio(Cells, Rest, Totes).

% ============================================================
% unio(+L1, +L2, -L3)
%
%   Unio de dues llistes sense duplicats.
% ============================================================
unio([], L, L).
unio([X|Xs], L, R) :-
    membre(X, L), !,
    unio(Xs, L, R).
unio([X|Xs], L, [X|R]) :-
    unio(Xs, L, R).

% ============================================================
% col_loca_peces(+Peces, +Caselles, +Acc, -Tauler, -Solucio)
%
%   Per a cada peca, tria dues caselles adjacents i lliures
%   del tauler i hi col.loca els valors de la peca.
%   El tauler s'acumula com a llista de parells Coord-Valor.
%
%   Peces   : peces que queden per col.locar
%   Caselles: totes les coordenades valides del tauler
%   Acc     : caselles ja ocupades (acumulador)
%   Tauler  : tauler final amb totes les peces col.locades
%   Solucio : llista de posicions de cada peca
% ============================================================
col_loca_peces([], _, Tauler, Tauler, []).
col_loca_peces([[V1,V2]|Peces], Caselles, Acc, Tauler,
               [[[F1,C1],[F2,C2]]|Sol]) :-
    % Tria la casella de la primera meitat de la peca
    membre([F1,C1], Caselles),
    \+ ocupada([F1,C1], Acc),
    % Tria una casella adjacent per a la segona meitat
    adjacent([F1,C1], [F2,C2]),
    membre([F2,C2], Caselles),
    \+ ocupada([F2,C2], Acc),
    % Continua col.locant la resta de peces
    col_loca_peces(Peces, Caselles,
                   [[F1,C1]-V1, [F2,C2]-V2 | Acc],
                   Tauler, Sol).

% ============================================================
% ocupada(+Coord, +Tauler)
%
%   Cert si la coordenada Coord ja esta ocupada al Tauler.
% ============================================================
ocupada(Coord, [Coord-_|_]) :- !.
ocupada(Coord, [_|Rest]) :-
    ocupada(Coord, Rest).

% ============================================================
% adjacent(+Coord1, ?Coord2)
%
%   Cert si les dues coordenades son adjacents (4-connexitat).
% ============================================================
adjacent([F,C], [F,C2]) :- C2 is C + 1.
adjacent([F,C], [F,C2]) :- C2 is C - 1.
adjacent([F,C], [F2,C]) :- F2 is F + 1.
adjacent([F,C], [F2,C]) :- F2 is F - 1.

% ============================================================
% comprova_regions(+Regions, +Tauler)
%
%   Comprova que cada region del puzle compleix la seva
%   restriccio donats els valors del tauler.
% ============================================================
comprova_regions([], _).
comprova_regions([R|Rs], Tauler) :-
    comprova_region(R, Tauler),
    comprova_regions(Rs, Tauler).

% ============================================================
% comprova_region(+Region, +Tauler)
%
%   Comprova la restriccio d'una sola region:
%   - empty  : cap restriccio sobre els valors
%   - equals : tots els valors han de ser iguals
%   - sum    : la suma ha de ser igual a Obj
%   - less   : la suma ha de ser estrictament menor que Obj
%   - greater: la suma ha de ser estrictament major que Obj
%   - unequal: tots els valors han de ser diferents entre si
% ============================================================
comprova_region(region(empty, _, _), _) :- !.

comprova_region(region(equals, _, Cells), Tauler) :-
    !,
    valors_caselles(Cells, Tauler, Vals),
    tots_iguals(Vals).

comprova_region(region(sum, Obj, Cells), Tauler) :-
    !,
    valors_caselles(Cells, Tauler, Vals),
    suma_llista(Vals, Obj).

comprova_region(region(less, Obj, Cells), Tauler) :-
    !,
    valors_caselles(Cells, Tauler, Vals),
    suma_llista(Vals, S),
    S < Obj.

comprova_region(region(greater, Obj, Cells), Tauler) :-
    !,
    valors_caselles(Cells, Tauler, Vals),
    suma_llista(Vals, S),
    S > Obj.

comprova_region(region(unequal, _, Cells), Tauler) :-
    !,
    valors_caselles(Cells, Tauler, Vals),
    tots_diferents(Vals).

% ============================================================
% valors_caselles(+Cells, +Tauler, -Vals)
%
%   Obte la llista de valors del tauler corresponents a una
%   llista de coordenades.
% ============================================================
valors_caselles([], _, []).
valors_caselles([C|Cs], Tauler, [V|Vs]) :-
    valor_casella(C, Tauler, V),
    valors_caselles(Cs, Tauler, Vs).

% ============================================================
% valor_casella(+Coord, +Tauler, -Valor)
%
%   Obte el valor associat a una coordenada al tauler.
% ============================================================
valor_casella(Coord, [Coord-V|_], V) :- !.
valor_casella(Coord, [_|Rest], V) :-
    valor_casella(Coord, Rest, V).

% ============================================================
% tots_iguals(+Llista)
%
%   Cert si tots els elements de la llista son iguals.
% ============================================================
tots_iguals([]).
tots_iguals([_]).
tots_iguals([X,X|Xs]) :-
    tots_iguals([X|Xs]).

% ============================================================
% tots_diferents(+Llista)
%
%   Cert si tots els elements de la llista son distints.
% ============================================================
tots_diferents([]).
tots_diferents([X|Xs]) :-
    \+ membre(X, Xs),
    tots_diferents(Xs).

% ============================================================
% suma_llista(+Llista, -Suma)
%
%   Calcula la suma de tots els elements d'una llista.
% ============================================================
suma_llista([], 0).
suma_llista([X|Xs], S) :-
    suma_llista(Xs, S1),
    S is S1 + X.

% ============================================================
% membre(?X, +Llista)
%
%   Cert si X es membre de la Llista.
% ============================================================
membre(X, [X|_]).
membre(X, [_|L]) :- membre(X, L).

% ============================================================
% imprimeix_solucio(+Regions, +Peces, +Solucio)
%
%   OPCIONAL: imprimeix el tauler per terminal amb els valors
%   de cada casella. Les posicions fora del tauler surten ' . '.
%
%   Regions : regions del puzle (per obtenir les caselles)
%   Peces   : peces de domino
%   Solucio : solucio ja calculada o verificada
% ============================================================
imprimeix_solucio(Regions, Peces, Solucio) :-
    totes_caselles(Regions, Caselles),
    col_loca_peces(Peces, Caselles, [], Tauler, Solucio),
    mida_tauler(Caselles, MaxF, MaxC),
    nl,
    imprimeix_files(0, MaxF, 0, MaxC, Tauler),
    nl.

% ============================================================
% mida_tauler(+Caselles, -MaxFila, -MaxColumna)
%
%   Obte la fila i columna maximes del tauler a partir de les
%   caselles valides, sense usar findall.
% ============================================================
mida_tauler(Caselles, MaxF, MaxC) :-
    extreu_files(Caselles, Fs),
    extreu_columnes(Caselles, Cs),
    max_llista(Fs, MaxF),
    max_llista(Cs, MaxC).

% extreu_files(+Caselles, -Files): extreu la llista de files
extreu_files([], []).
extreu_files([[F,_]|Rest], [F|Fs]) :-
    extreu_files(Rest, Fs).

% extreu_columnes(+Caselles, -Columnes): extreu la llista de columnes
extreu_columnes([], []).
extreu_columnes([[_,C]|Rest], [C|Cs]) :-
    extreu_columnes(Rest, Cs).

% ============================================================
% max_llista(+Llista, -Max)
%
%   Obte el valor maxim d'una llista de nombres.
% ============================================================
max_llista([X], X).
max_llista([X|Xs], X) :-
    max_llista(Xs, M),
    X >= M, !.
max_llista([_|Xs], M) :-
    max_llista(Xs, M).

% ============================================================
% imprimeix_files(+F, +MaxF, +MinC, +MaxC, +Tauler)
%
%   Imprimeix fila a fila el tauler des de F fins MaxF.
% ============================================================
imprimeix_files(F, MaxF, _, _, _) :- F > MaxF, !.
imprimeix_files(F, MaxF, MinC, MaxC, Tauler) :-
    imprimeix_columnes(F, MinC, MaxC, Tauler),
    nl,
    F1 is F + 1,
    imprimeix_files(F1, MaxF, MinC, MaxC, Tauler).

% ============================================================
% imprimeix_columnes(+F, +C, +MaxC, +Tauler)
%
%   Imprimeix les caselles d'una fila, columna per columna.
% ============================================================
imprimeix_columnes(_, C, MaxC, _) :- C > MaxC, !.
imprimeix_columnes(F, C, MaxC, Tauler) :-
    imprimeix_casella(F, C, Tauler),
    C1 is C + 1,
    imprimeix_columnes(F, C1, MaxC, Tauler).

% ============================================================
% imprimeix_casella(+F, +C, +Tauler)
%
%   Imprimeix el valor de la casella [F,C] si existeix,
%   o ' . ' si la casella no pertany al tauler.
% ============================================================
imprimeix_casella(F, C, Tauler) :-
    valor_casella([F,C], Tauler, V), !,
    write('['), write(V), write('] ').
imprimeix_casella(_, _, _) :-
    write(' .  ').