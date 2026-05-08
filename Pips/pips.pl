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
%   El predicat principal solucio_pips/3 funciona en dos modes:
%
%   MODE VERIFICACIÓ (Solucio instanciada):
%     1. Construeix el tauler directament des de Peces+Solucio
%        amb construeix_tauler/3, sense backtracking (O(n)).
%     2. Comprova que cada peça ocupa caselles adjacents i vàlides.
%     3. Comprova totes les restriccions de les regions.
%
%   MODE GENERACIÓ (Solucio no instanciada):
%     1. S'extreuen totes les caselles vàlides del tauler a partir
%        de les regions (totes_caselles/2).
%     2. Per a cada peça, es trien dues caselles adjacents i lliures
%        amb ordre canònic ([F1,C1] @< [F2,C2]) per evitar duplicats.
%        S'usa select/3 per treballar directament amb caselles lliures.
%     3. Un cop col·locades totes les peces, es comproven totes les
%        restriccions de les regions (comprova_regions/2).
%
% ============================================================

:- consult('puzzles.pl').

% ============================================================
% solucio_pips(+Regions, +Peces, ?Solucio)
%
%   Predicat principal.
%
%   MODE VERIFICACIÓ: si Solucio ja està instanciada, construeix
%   el tauler directament i comprova les restriccions sense
%   recórrer l'espai de cerca (eficient, O(n)).
%
%   MODE GENERACIÓ: si Solucio no està instanciada, genera totes
%   les solucions possibles per backtracking.
%
%   Regions : llista de region(Condicio, Objectiu, Caselles)
%   Peces   : llista de [V1, V2] (valors de cada peça de dominó)
%   Solucio : llista de [[F1,C1],[F2,C2]] (posició de cada peça)
% ============================================================
solucio_pips(Regions, Peces, Solucio) :-
    ( nonvar(Solucio) ->
        % Mode verificació: construcció directa del tauler
        totes_caselles(Regions, Caselles),
        construeix_tauler(Peces, Solucio, Tauler),
        valida_peces_solucio(Solucio, Caselles),
        comprova_regions(Regions, Tauler)
    ;
        % Mode generació: backtracking
        totes_caselles(Regions, Caselles),
        col_loca_peces(Peces, Caselles, Solucio),
        construeix_tauler(Peces, Solucio, Tauler),
        comprova_regions(Regions, Tauler)
    ).

% ============================================================
% construeix_tauler(+Peces, +Solucio, -Tauler)
%
%   Construeix el tauler (llista de Coord-Valor) directament
%   a partir de Peces i Solucio, sense backtracking.
%   Usat pel mode verificació i per imprimeix_solucio/3.
%
%   Peces   : llista de [V1, V2]
%   Solucio : llista de [[F1,C1],[F2,C2]]
%   Tauler  : llista de [F,C]-Valor
% ============================================================
construeix_tauler([], [], []).
construeix_tauler([[V1,V2]|Ps], [[[F1,C1],[F2,C2]]|Sol],
                  [[F1,C1]-V1, [F2,C2]-V2 | T]) :-
    construeix_tauler(Ps, Sol, T).

% ============================================================
% valida_peces_solucio(+Solucio, +Caselles)
%
%   Comprova que cada peça de la solució:
%     - Ocupa exactament dues caselles
%     - Ambdues caselles pertanyen al tauler (Caselles)
%     - Les dues caselles són adjacents
%     - No hi ha caselles repetides entre peces
%
%   Usat exclusivament pel mode verificació.
% ============================================================
valida_peces_solucio(Solucio, Caselles) :-
    valida_peces_aux(Solucio, Caselles, []).

valida_peces_aux([], _, _).
valida_peces_aux([[[F1,C1],[F2,C2]]|Rest], Caselles, Usades) :-
    membre([F1,C1], Caselles),
    membre([F2,C2], Caselles),
    adjacent([F1,C1], [F2,C2]),
    \+ membre([F1,C1], Usades),
    \+ membre([F2,C2], Usades),
    valida_peces_aux(Rest, Caselles, [[F1,C1],[F2,C2]|Usades]).

% ============================================================
% totes_caselles(+Regions, -Caselles)
%
%   Extreu la llista (sense duplicats) de totes les caselles
%   que apareixen a les regions del puzle.
% ============================================================
totes_caselles([], []).
totes_caselles([region(_, _, Cells)|Rs], Totes) :-
    totes_caselles(Rs, Rest),
    unio(Cells, Rest, Totes).

% ============================================================
% unio(+L1, +L2, -L3)
%
%   Unió de dues llistes sense duplicats.
% ============================================================
unio([], L, L).
unio([X|Xs], L, R) :-
    membre(X, L), !,
    unio(Xs, L, R).
unio([X|Xs], L, [X|R]) :-
    unio(Xs, L, R).

% ============================================================
% col_loca_peces(+Peces, +Caselles, -Solucio)
%
%   Usa select/3 per treballar amb caselles lliures
%   directament, evitant l'acumulador separat i les cerques
%   dobles (membre + ocupada).
%
%   Ordre canònic [F1,C1] @< [F2,C2] per eliminar
%   duplicats simètrics de posició.
%
%   Peces   : peces que queden per col·locar
%   Caselles: caselles lliures que queden disponibles
%   Solucio : llista de posicions de cada peça
% ============================================================
col_loca_peces([], _, []).
col_loca_peces([_|Peces], Caselles, [[[F1,C1],[F2,C2]]|Sol]) :-
    % select/3: tria [F1,C1] i l'elimina de les lliures
    select([F1,C1], Caselles, Caselles1),
    % Tria una casella adjacent
    adjacent([F1,C1], [F2,C2]),
    % Ordre canònic: evita duplicats per simetria de posició
    [F1,C1] @< [F2,C2],
    % Comprova que [F2,C2] és vàlida i lliure, i l'elimina
    select([F2,C2], Caselles1, Caselles2),
    % Continua amb la resta de peces
    col_loca_peces(Peces, Caselles2, Sol).

% ============================================================
% adjacent(+Coord1, ?Coord2)
%
%   Cert si les dues coordenades són adjacents (4-connexitat).
%   Les coordenades fora del tauler es filtren a posteriori
%   per select/3 (que comprova pertinença a Caselles).
% ============================================================
adjacent([F,C], [F,C2]) :- C2 is C + 1.
adjacent([F,C], [F,C2]) :- C2 is C - 1.
adjacent([F,C], [F2,C]) :- F2 is F + 1.
adjacent([F,C], [F2,C]) :- F2 is F - 1.

% ============================================================
% comprova_regions(+Regions, +Tauler)
%
%   Comprova que cada regió del puzle compleix la seva
%   restricció donats els valors del tauler.
% ============================================================
comprova_regions([], _).
comprova_regions([R|Rs], Tauler) :-
    comprova_region(R, Tauler),
    comprova_regions(Rs, Tauler).

% ============================================================
% comprova_region(+Region, +Tauler)
%
%   Comprova la restricció d'una sola regió:
%   - empty  : cap restricció sobre els valors
%   - equals : tots els valors han de ser iguals
%   - sum    : la suma ha de ser igual a Obj
%   - less   : la suma ha de ser estrictament menor que Obj
%   - greater: la suma ha de ser estrictament major que Obj
%   - unequal: tots els valors han de ser diferents entre si
% ============================================================
comprova_region(region(empty, _, _), _) :- !.

comprova_region(region(equals, _, Cells), Tauler) :-
    !,
    Cells = [_|_],   % guàrdia: la llista no pot ser buida
    valors_caselles(Cells, Tauler, Vals),
    tots_iguals(Vals).

comprova_region(region(sum, Obj, Cells), Tauler) :-
    !,
    Cells = [_|_],
    valors_caselles(Cells, Tauler, Vals),
    suma_llista(Vals, Obj).

comprova_region(region(less, Obj, Cells), Tauler) :-
    !,
    Cells = [_|_],
    valors_caselles(Cells, Tauler, Vals),
    suma_llista(Vals, S),
    S < Obj.

comprova_region(region(greater, Obj, Cells), Tauler) :-
    !,
    Cells = [_|_],
    valors_caselles(Cells, Tauler, Vals),
    suma_llista(Vals, S),
    S > Obj.

comprova_region(region(unequal, _, Cells), Tauler) :-
    !,
    Cells = [_|_],
    valors_caselles(Cells, Tauler, Vals),
    tots_diferents(Vals).

% ============================================================
% valors_caselles(+Cells, +Tauler, -Vals)
%
%   Obté la llista de valors del tauler corresponents a una
%   llista de coordenades.
% ============================================================
valors_caselles([], _, []).
valors_caselles([C|Cs], Tauler, [V|Vs]) :-
    valor_casella(C, Tauler, V),
    valors_caselles(Cs, Tauler, Vs).

% ============================================================
% valor_casella(+Coord, +Tauler, -Valor)
%
%   Obté el valor associat a una coordenada al tauler.
% ============================================================
valor_casella(Coord, [Coord-V|_], V) :- !.
valor_casella(Coord, [_|Rest], V) :-
    valor_casella(Coord, Rest, V).

% ============================================================
% tots_iguals(+Llista)
%
%   Cert si tots els elements de la llista són iguals.
%   La llista ha de tenir almenys un element.
% ============================================================
tots_iguals([_]).
tots_iguals([X,X|Xs]) :-
    tots_iguals([X|Xs]).

% ============================================================
% tots_diferents(+Llista)
%
%   Cert si tots els elements de la llista són distints.
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
%   Cert si X és membre de la Llista.
% ============================================================
membre(X, [X|_]).
membre(X, [_|L]) :- membre(X, L).

% ============================================================
% select(?X, +Llista, -Resta)
%
%   Tria X de Llista i retorna la Resta sense X.
%   Equivalent a membre/2 però elimina l'element en O(n).
%   Definit aquí per si l'entorn no el té a la biblioteca.
%   Si s'usa SWI-Prolog, es pot esborrar i usar el built-in.
% ============================================================
select(X, [X|Rest], Rest).
select(X, [Y|Ys], [Y|Zs]) :-
    select(X, Ys, Zs).

% ============================================================
% imprimeix_solucio(+Regions, +Peces, +Solucio)
%
%   OPCIONAL: imprimeix el tauler per terminal amb els valors
%   de cada casella. Les posicions fora del tauler surten ' . '.
%
%   Usa construeix_tauler/3 directament en lloc
%   de col_loca_peces/5, evitant backtracking innecessari.
% ============================================================
imprimeix_solucio(Regions, Peces, Solucio) :-
    construeix_tauler(Peces, Solucio, Tauler),
    totes_caselles(Regions, Caselles),
    mida_tauler(Caselles, MaxF, MaxC),
    nl,
    imprimeix_files(0, MaxF, 0, MaxC, Tauler),
    nl.

% ============================================================
% mida_tauler(+Caselles, -MaxFila, -MaxColumna)
%
%   Obté la fila i columna màximes del tauler.
% ============================================================
mida_tauler(Caselles, MaxF, MaxC) :-
    extreu_files(Caselles, Fs),
    extreu_columnes(Caselles, Cs),
    max_llista(Fs, MaxF),
    max_llista(Cs, MaxC).

extreu_files([], []).
extreu_files([[F,_]|Rest], [F|Fs]) :-
    extreu_files(Rest, Fs).

extreu_columnes([], []).
extreu_columnes([[_,C]|Rest], [C|Cs]) :-
    extreu_columnes(Rest, Cs).

% ============================================================
% max_llista(+Llista, -Max)
%
%   Obté el valor màxim d'una llista de nombres.
% ============================================================
max_llista([X], X).
max_llista([X|Xs], X) :-
    max_llista(Xs, M),
    X >= M, !.
max_llista([_|Xs], M) :-
    max_llista(Xs, M).

% ============================================================
% imprimeix_files(+F, +MaxF, +MinC, +MaxC, +Tauler)
% ============================================================
imprimeix_files(F, MaxF, _, _, _) :- F > MaxF, !.
imprimeix_files(F, MaxF, MinC, MaxC, Tauler) :-
    imprimeix_columnes(F, MinC, MaxC, Tauler),
    nl,
    F1 is F + 1,
    imprimeix_files(F1, MaxF, MinC, MaxC, Tauler).

% ============================================================
% imprimeix_columnes(+F, +C, +MaxC, +Tauler)
% ============================================================
imprimeix_columnes(_, C, MaxC, _) :- C > MaxC, !.
imprimeix_columnes(F, C, MaxC, Tauler) :-
    imprimeix_casella(F, C, Tauler),
    C1 is C + 1,
    imprimeix_columnes(F, C1, MaxC, Tauler).

% ============================================================
% imprimeix_casella(+F, +C, +Tauler)
% ============================================================
imprimeix_casella(F, C, Tauler) :-
    valor_casella([F,C], Tauler, V), !,
    write('['), write(V), write('] ').
imprimeix_casella(_, _, _) :-
    write(' .  ').