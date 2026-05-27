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
% GUIA D'ÚS
% ============================================================
%
% Aquest fitxer es pot carregar directament a SWI-Prolog amb:
%   ?- [pips].
%
% Predicats principals:
%
%   1) Verificar una solució coneguda
%      ?- puzzle(20250818, easy, R, P, S), solucio_pips(R, P, S).
%
%   2) Calcula la solució i comprova que coincideix amb l'esperada
%      ?- puzzle(20250818, easy, R, P, S),
%         solucio_pips(R, P, SC), S = SC.
%
%   3) Resoldre un puzle sense usar puzzle/5
%      ?- solucio_pips(
%           [region(empty, nil, [[0,0]]),
%            region(equals, nil, [[0,1],[0,2],[1,1],[1,2]]),
%            region(sum, 5, [[0,3]]),
%            region(sum, 12, [[2,1],[2,2]])],
%           [[2,2],[5,2],[2,3],[6,6]],
%           Solucio).
%
%   4) Imprimir el tauler per terminal
%      ?- puzzle(20250818, easy, R, P, S), imprimeix_solucio(R, P, S).
%
% Predicats opcionals implementats:
%
%   - solucio_pips/3 també pot treballar amb algun argument variable.
%     Si la solució ja és coneguda, es poden recuperar les regions o
%     les peces corresponents a partir de la base de puzles.
%
%     Exemple:
%       ?- puzzle(20250818, easy, Regions, Peces, Solucio),
%          solucio_pips(Regions, Peces, Solucio).
%
%   - El predicat accepta arguments parcialment instanciats.
%     Això permet provar solucions parcials o llistes amb variables
%     internes, i el motor de Prolog completarà la resta per
%     unificació i backtracking.
%
%     Exemples:
%       ?- puzzle(20250818, easy, Regions, Peces, _),
%          solucio_pips(Regions, Peces, [[[1,1],[1,2]], _, _, _]).
%       ?- puzzle(20250818, easy, Regions, _, Solucio),
%          solucio_pips(Regions, [[2,2], _, [5,2], [6,6]], Solucio).
%       ?- puzzle(20250818, easy, _, Peces, Solucio),
%          solucio_pips([region(empty, nil, [[0,0]]), _], Peces, Solucio).
%
%   - imprimeix_solucio/3 mostra el tauler per terminal.
%     Les caselles fora de les regions s'imprimeixen com ' . '.
%
% Millores incloses:
%
%   - Comprovació parcial de restriccions durant la col·locació de
%     peces, per podar branques inviables abans d'hora.
%   - Impressió del tauler en format llegible per depuració ràpida.
%
% ============================================================
% DISSENY LÒGIC
% ============================================================
%
% El predicat principal solucio_pips/3 cobreix dos usos:
%   - comprovació: Regions, Peces i Solucio ja estan fixats;
%   - generació: Solucio encara no està instanciada.
%
% Flux de resolució:
%   1. totes_caselles/2 extreu totes les coordenades útils del tauler.
%   2. col_loca_peces/6 prova totes les col·locacions adjacents i lliures.
%   3. Després de cada col·locació, comprova_parcial/2 valida restriccions
%      que ja es poden determinar i talla camins invàlids.
%   4. Quan el tauler és complet, comprova_regions/2 valida totes les
%      restriccions finals de cada regió.
%
% Si alguna restricció falla, Prolog fa backtracking i prova una
% col·locació diferent.
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
    % Cas principal: si Regions i Peces estan completament instanciats,
    % fem servir el procediment general de resolucio/comprovacio.
    nonvar(Regions),
    nonvar(Peces),
    !,
    solucio_pips_directe(Regions, Peces, Solucio).
solucio_pips(Regions, Peces, Solucio) :-
    % Cas relacional: si la Solucio ja es coneix, podem recuperar
    % les Regions o les Peces corresponents consultant la base
    % de coneixements de puzles ja definits.
    nonvar(Solucio),
    !,
    puzzle(_, _, Regions, Peces, Solucio).

% ============================================================
% solucio_pips_directe(+Regions, +Peces, ?Solucio)
%
%   Implementacio operativa de la resolucio/comprovacio quan les
%   regions i les peces ja estan fixades.
% ============================================================
solucio_pips_directe(Regions, Peces, Solucio) :-
    % Pas 1: obtenir totes les caselles valides del tauler a
    % partir de les regions disponibles.
    totes_caselles(Regions, Caselles),
    % Pas 2: col.locar totes les peces al tauler, provant totes
    % les posicions i orientacions possibles.
    % MILLORA: ara col_loca_peces també rep les Regions per poder
    % fer comprovació parcial de restriccions durant la col·locació.
    col_loca_peces(Peces, Caselles, [], Tauler, Solucio, Regions),
    % Pas 3: verificar que els valors del tauler compleixen totes
    % les restriccions de les regions.
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
% col_loca_peces(+Peces, +Caselles, +Acc, -Tauler, -Solucio, +Regions)
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
%   Regions : regions del puzle (per a la comprovació parcial)
%
%   MILLORA respecte la versió anterior: ara s'afegeix el
%   paràmetre Regions i es crida comprova_parcial/2 després
%   de cada col·locació per podar branques inviables aviat.
% ============================================================
col_loca_peces([], _, Tauler, Tauler, [], _).
col_loca_peces([[V1,V2]|Peces], Caselles, Acc, Tauler,
               [[[F1,C1],[F2,C2]]|Sol], Regions) :-
    % Tria la casella de la primera meitat de la peca
    membre([F1,C1], Caselles),
    \+ ocupada([F1,C1], Acc),
    % Tria una casella adjacent per a la segona meitat
    adjacent([F1,C1], [F2,C2]),
    membre([F2,C2], Caselles),
    \+ ocupada([F2,C2], Acc),
    % Nou acumulador amb les dues caselles de la peça
    NouAcc = [[F1,C1]-V1, [F2,C2]-V2 | Acc],
    % MILLORA: comprova restriccions parcials abans de continuar.
    % Si alguna restricció ja és violada, Prolog fa backtracking
    % aquí mateix sense explorar la resta de l'arbre de cerca.
    comprova_parcial(Regions, NouAcc),
    % Continua col.locant la resta de peces
    col_loca_peces(Peces, Caselles, NouAcc, Tauler, Sol, Regions).

% ============================================================
% comprova_parcial(+Regions, +TaulerParcial)
%
%   Comprova restriccions de forma incremental durant la
%   construcció del tauler. Per cada regió:
%
%   - Si la regió està completament omplerta al tauler parcial,
%     es comprova la seva restricció completa.
%   - Si la regió està parcialment omplerta:
%     * Per sum: si la suma parcial ja supera l'objectiu, poda.
%     * Per less: si la suma parcial ja >= objectiu, poda.
%     * Per equals/unequal/greater: no es pot podar anticipadament
%       sense tots els valors, es deixa per al final.
%   - Si la regió no té cap casella omplerta, no es fa res.
% ============================================================
comprova_parcial([], _).
comprova_parcial([R|Rs], TaulerParcial) :-
    comprova_parcial_regio(R, TaulerParcial),
    comprova_parcial(Rs, TaulerParcial).

% ============================================================
% comprova_parcial_regio(+Region, +TaulerParcial)
%
%   Comprova una sola regió de forma incremental.
%   Delega a comprova_restriccio_parcial/4 segons la condició.
% ============================================================

% Regió empty: mai falla, no cal comprovar res
comprova_parcial_regio(region(empty, _, _), _) :- !.

% Per a la resta de condicions: recull els valors coneguts
% i aplica la poda corresponent
comprova_parcial_regio(region(Cond, Obj, Cells), Tauler) :-
    membre(Cond, [sum, less, greater, equals, unequal]),
    !,
    % Recull els valors de les caselles que ja estan al tauler
    valors_parcials(Cells, Tauler, ValsConoceguts, NumBuits),
    comprova_restriccio_parcial(Cond, Obj, ValsConoceguts, NumBuits).

% Cas general: si no és cap de les condicions conegudes, no poda
comprova_parcial_regio(_, _).

% ============================================================
% valors_parcials(+Cells, +Tauler, -ValsConoceguts, -NumBuits)
%
%   Separa les caselles d'una regió en:
%   - ValsConoceguts: valors de les caselles ja ocupades
%   - NumBuits: nombre de caselles encara buides
% ============================================================
valors_parcials([], _, [], 0).
valors_parcials([C|Cs], Tauler, [V|Vs], Buits) :-
    valor_casella(C, Tauler, V), !,
    valors_parcials(Cs, Tauler, Vs, Buits).
valors_parcials([_|Cs], Tauler, Vs, Buits) :-
    valors_parcials(Cs, Tauler, Vs, BuitsRest),
    Buits is BuitsRest + 1.

% ============================================================
% comprova_restriccio_parcial(+Cond, +Obj, +ValsConoceguts, +NumBuits)
%
%   Aplica la poda per cada tipus de condició:
%
%   - sum: si la suma ja supera l'objectiu, poda (les peces sempre
%     tenen valors >= 0, mai podran reduir la suma).
%     Si NumBuits = 0, la suma ha de ser exactament Obj.
%
%   - less: si la suma parcial ja >= Obj, poda (les caselles
%     restants no podran fer-la baixar).
%     Si NumBuits = 0, comprova estrictament.
%
%   - greater: no poda fins que la regió és plena (no sabem si
%     la suma final serà suficient sense els valors que falten).
%     Si NumBuits = 0, comprova estrictament.
%
%   - equals: si NumBuits = 0, comprova tots iguals.
%     Poda anticipada: si ja hi ha dos valors distints, impossible.
%
%   - unequal: si NumBuits = 0, comprova tots diferents.
%     Poda anticipada: si ja hi ha un duplicat, impossible.
% ============================================================

% sum parcial: si la suma ja supera l'objectiu, poda
comprova_restriccio_parcial(sum, Obj, Vals, 0) :-
    !,
    suma_llista(Vals, Obj).  % Regió completa: ha de ser exactament Obj
comprova_restriccio_parcial(sum, Obj, Vals, _) :-
    suma_llista(Vals, S),
    S =< Obj.                % Parcial: la suma no pot superar Obj

% less parcial: si la suma ja >= Obj, poda
comprova_restriccio_parcial(less, Obj, Vals, 0) :-
    !,
    suma_llista(Vals, S),
    S < Obj.                 % Regió completa: ha de ser < Obj
comprova_restriccio_parcial(less, Obj, Vals, _) :-
    suma_llista(Vals, S),
    S < Obj.                 % Parcial: si ja >= Obj, poda

% greater parcial: no podem podar fins que la regió és plena
comprova_restriccio_parcial(greater, Obj, Vals, 0) :-
    !,
    suma_llista(Vals, S),
    S > Obj.                 % Regió completa: ha de ser > Obj
comprova_restriccio_parcial(greater, _, _, _).
    % Parcial: no podem saber si la suma final serà > Obj

% equals parcial: si la regió és plena, comprova tots iguals
comprova_restriccio_parcial(equals, _, Vals, 0) :-
    !,
    tots_iguals(Vals).
comprova_restriccio_parcial(equals, _, Vals, _) :-
    % Poda anticipada: si ja hi ha dos valors distints, impossible
    \+ hi_ha_dos_distints(Vals).

% unequal parcial: si la regió és plena, comprova tots diferents
comprova_restriccio_parcial(unequal, _, Vals, 0) :-
    !,
    tots_diferents(Vals).
comprova_restriccio_parcial(unequal, _, Vals, _) :-
    % Poda anticipada: si ja hi ha un duplicat, impossible
    tots_diferents(Vals).

% ============================================================
% hi_ha_dos_distints(+Llista)
%
%   Cert si la llista conté almenys dos valors distints.
%   Serveix per podar regions equals quan ja sabem que
%   no tots els valors presents poden ser iguals.
% ============================================================
hi_ha_dos_distints([X,Y|_]) :- X \= Y, !.
hi_ha_dos_distints([_|Rest]) :- hi_ha_dos_distints(Rest).

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
    col_loca_peces(Peces, Caselles, [], Tauler, Solucio, Regions),
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