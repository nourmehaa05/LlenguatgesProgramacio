;; ============================================================
;; Pràctica final de Llenguatges de Programació - LISP
;; Paintball - Mòdul Gràfic
;; ============================================================
;; Estudiants:
;;   - Marín Sánchez, Carolina
;;   - Mehannek Samah, Nour Iman
;; Data: 03/05/2026
;; Assignatura: Llenguatges de Programació
;; Grup: 101
;; Professors:
;;   - Cabot Nadal, Miquel Àngel
;;   - Oliver Tomàs, Antoni
;; Lliurament: primera convocatòria.
;; ============================================================
;; Descripció de les funcions d'aquest fitxer:
;;   - Funcions auxiliars de matrius: accés i modificació per índex
;;   - Funcions de dibuix: colors, quadrats, cercles i caselles
;;   - Dibuix de la matriu completa del mapa
;;   - HUD: informació de l'estat (ronda, torn, pintura, bolles, labs)
;;   - Funció principal pinta: dibuixa el mapa i actualitza el HUD
;;   - Ajust dinàmic de mida segons les dimensions del mapa
;; ============================================================


;; ==============================================================
;;  ------------------- FUNCIONS AUXILIARS DE MATRIUS -------------------
;; ==============================================================

(defun troba-dins-fila (fila valor &optional (x 0))
  "Retorna l'índex x de la fila on un element conté valor, o nil si no existeix.
   fila:  llista de caselles d'una fila de la matriu
   valor: valor a cercar dins els elements de la fila
   x:     índex de columna actual (per defecte 0)"
  (cond ((null fila) nil)
        ((member valor (car fila)) x)
        (t (troba-dins-fila (cdr fila) valor (+ x 1)))))

(defun troba-matriu (matriu valor &optional (y 0))
  "Retorna la coordenada (x y) on es troba valor a la matriu, o nil si no existeix.
   matriu: mapa del joc representat com a llista de files (llista de llistes)
   valor:  valor a cercar
   y:      índex de fila actual (per defecte 0)"
  (cond ((null matriu) nil)
        (t (let* ((x (troba-dins-fila (car matriu) valor)))
             (cond (x (list x y))
                   (t (troba-matriu (cdr matriu) valor (+ y 1))))))))

(defun indexa-fila (fila x)
  "Retorna l'element a la posició x d'una fila, o nil si x és fora de rang.
   fila: llista de caselles d'una fila
   x:    índex de columna a obtenir"
  (cond ((null fila) nil)
        ((= x 0) (car fila))
        (t (indexa-fila (cdr fila) (- x 1)))))

(defun indexa-matriu (matriu x y)
  "Retorna l'element a la posició (x y) de la matriu, o nil si és fora de rang.
   matriu: mapa del joc representat com a llista de files (llista de llistes)
   x:      índex de columna
   y:      índex de fila"
  (cond ((null matriu) nil)
        ((= y 0) (indexa-fila (car matriu) x))
        (t (indexa-matriu (cdr matriu) x (- y 1)))))

(defun posa-dins-fila (fila x valor)
  "Retorna una nova fila igual a l'original però amb valor a la posició x.
   fila:  llista de caselles original
   x:     índex de columna on col·locar el valor
   valor: nou valor a inserir"
  (cond ((null fila) nil)
        ((= x 0) (cons valor (cdr fila)))
        (t (cons (car fila) (posa-dins-fila (cdr fila) (- x 1) valor)))))

(defun posa-matriu (matriu x y valor)
  "Retorna una nova matriu igual a l'original però amb valor a la posició (x y).
   matriu: mapa del joc representat com a llista de files (llista de llistes)
   x:      índex de columna on col·locar el valor
   y:      índex de fila on col·locar el valor
   valor:  nou valor a inserir"
  (cond ((null matriu) nil)
        ((= y 0) (cons (posa-dins-fila (car matriu) x valor) (cdr matriu)))
        (t (cons (car matriu) (posa-matriu (cdr matriu) x (- y 1) valor)))))


;; ==============================================================
;;  ------------------- FUNCIONS DE COLOR I FORMES -------------------
;; ==============================================================

(defun aplica-color (clau)
  "Estableix el color de dibuix actiu segons el símbol donat.
   Accepta: 'r, 'g, 'b, 'lila, 'taronja, 'groc, 'aigua o qualsevol altre (negre).
   clau: símbol que identifica el color a aplicar"
  (cond ((eq clau 'r)       (color 220 30 30 220 30 30))
        ((eq clau 'g)       (color 30 180 30 30 180 30))
        ((eq clau 'b)       (color 30 80 220 30 80 220))
        ((eq clau 'lila)    (color 200 100 250 200 100 250))
        ((eq clau 'taronja) (color 255 165 0 255 165 0))
        ((eq clau 'groc)    (color 255 255 0 255 255 0))
        ((eq clau 'aigua)   (color 0 20 60 0 20 60))
        (t                  (color 0 0 0 0 0 0))))

(defun dibuixaquadrat (mida)
  "Dibuixa el contorn d'un quadrat de costat mida a partir de la posició actual.
   mida: costat del quadrat en píxels"
  (drawrel mida 0)
  (drawrel 0 mida)
  (drawrel (- mida) 0)
  (drawrel 0 (- mida)))

(defun quadrat (mida gruix)
  "Dibuixa un quadrat de mida x mida amb gruix de línia gruix, recursivament.
   mida:  costat del quadrat en píxels
   gruix: gruix de la línia en píxels"
  (cond ((plusp gruix)
         (dibuixaquadrat (- mida 1))
         (moverel 1 1)
         (quadrat (- mida 2) (- gruix 1))
         (moverel -1 -1))))

(defun rellena-quadrat (x0 y0 mida)
  "Omple un quadrat de costat mida amb la posició superior esquerra a (x0, y0).
   x0:   coordenada x de la cantonada superior esquerra
   y0:   coordenada y de la cantonada superior esquerra
   mida: costat del quadrat en píxels"
  (rellena-quadrat-rec x0 y0 mida 0))

(defun rellena-quadrat-rec (x0 y0 mida i)
  "Dibuixa les línies horitzontals del quadrat de forma recursiva.
   x0:   coordenada x de la cantonada superior esquerra
   y0:   coordenada y de la cantonada superior esquerra
   mida: costat del quadrat en píxels
   i:    índex de la línia horitzontal actual"
  (cond ((>= i mida) t)
        (t (move x0 (+ y0 i))
           (drawrel mida 0)
           (rellena-quadrat-rec x0 y0 mida (+ i 1)))))

(defun rellena-cercle (cx cy radi)
  "Omple un cercle de centre (cx, cy) i radi donat, dibuixant línies horitzontals.
   cx:   coordenada x del centre del cercle
   cy:   coordenada y del centre del cercle
   radi: radi del cercle en píxels"
  (rellena-cercle-rec cx cy radi (- radi)))

(defun rellena-cercle-rec (cx cy radi desplacament-y)
  "Dibuixa les línies horitzontals del cercle de forma recursiva.
   cx:             coordenada x del centre
   cy:             coordenada y del centre
   radi:           radi del cercle en píxels
   desplacament-y: desplaçament vertical actual respecte al centre"
  (cond ((> desplacament-y radi) t)
        (t (let* ((amplada  (floor (sqrt (- (* radi radi)
                                               (* desplacament-y desplacament-y)))))
                  (x-inici  (- cx amplada))
                  (y-actual (+ cy desplacament-y)))
             (move x-inici y-actual)
             (drawrel (* 2 amplada) 0)
             (rellena-cercle-rec cx cy radi (+ desplacament-y 1))))))


;; ==============================================================
;;  ------------------- DIBUIX DE CASELLES I MAPA -------------------
;; ==============================================================

(defun obtenir-color-casella (casella)
  "Retorna el símbol de color ('r, 'g, 'b o 'aigua) d'una casella del mapa.
   casella: casella del mapa de la qual s'extreu el color"
  (let* ((tipus (car casella)))
    (cond ((eq tipus 'aigua) 'aigua)
          ((eq tipus 'terra)
           (let* ((info-color (cadr casella)))
             (cond ((eq info-color 'r) 'r)
                   ((eq info-color 'g) 'g)
                   ((eq info-color 'b) 'b)
                   (t 'terra))))
          (t 'terra))))

(defun pinta-fila (fila i mida y xi yi g)
  "Dibuixa recursivament totes les caselles d'una fila del mapa.
   Per cada casella pinta el fons, la quadrícula i l'element (base, lab o bolla).
   fila: llista de caselles de la fila a dibuixar
   i:    índex de columna actual dins la fila
   mida: mida en píxels de cada casella
   y:    índex de fila dins la matriu (per calcular posició vertical)
   xi:   coordenada x d'inici del mapa en pantalla
   yi:   coordenada y d'inici del mapa en pantalla
   g:    gruix de la quadrícula en píxels"
  (cond ((null fila) t)
        (t (let* ((casella   (car fila))
                  (x-pos     (+ xi (* i mida)))
                  (y-pos     (+ yi (* y mida)))
                  (color-cas (obtenir-color-casella casella))
                  (equip     (cond ((member 'e1 casella) 'e1)
                                   ((member 'e2 casella) 'e2)
                                   (t nil))))
             ;; Fons de la casella
             (move x-pos y-pos)
             (aplica-color color-cas)
             (rellena-quadrat x-pos y-pos mida)

             ;; Quadrícula negra
             (color 0 0 0 0 0 0)
             (move x-pos y-pos)
             (quadrat mida g)

             ;; Element de la casella
             (cond
               ;; Base: quadrat interior del color de l'equip
               ((member 'base casella)
                (let* ((marge    (floor (* mida 0.2)))
                       (mida-int (- mida (* 2 marge)))
                       (color-eq (cond ((eq equip 'e1) 'lila) (t 'taronja))))
                  (aplica-color color-eq)
                  (rellena-quadrat (+ x-pos marge) (+ y-pos marge) mida-int)
                  (color 0 0 0 0 0 0)
                  (move (+ x-pos marge) (+ y-pos marge))
                  (quadrat mida-int g)))

               ;; Laboratori: creu groga al centre
               ((member 'lab casella)
                (let* ((cx (+ x-pos (/ mida 2)))
                       (cy (+ y-pos (/ mida 2)))
                       (r  (- (/ mida 2) 1)))
                  (aplica-color 'groc)
                  (move (floor (- cx r)) (floor cy))
                  (drawrel (floor (* 2 r)) 0)
                  (move (floor cx) (floor (- cy r)))
                  (drawrel 0 (floor (* 2 r)))))

               ;; Bolla: cercle exterior (equip) + cercle interior (color propi)
               ((member 'bolla casella)
                (let* ((cx          (floor (+ x-pos (/ mida 2))))
                       (cy          (floor (+ y-pos (/ mida 2))))
                       (r-equip     (floor (* mida 0.4)))
                       (r-centre    (floor (* mida 0.2)))
                       (color-bolla (cadr (member 'bolla casella)))
                       (color-eq    (cond ((eq equip 'e1) 'lila) (t 'taronja))))
                  (aplica-color color-eq)
                  (rellena-cercle cx cy r-equip)
                  (aplica-color color-bolla)
                  (rellena-cercle cx cy r-centre)
                  ;; Brillantor proporcional
                  (color 255 255 255 255 255 255)
                  (let ((desplacament (floor (* mida 0.15))))
                    (move cx (+ cy desplacament))
                    (draw cx (+ cy desplacament))))))

             (color 0 0 0 0 0 0)
             (pinta-fila (cdr fila) (+ i 1) mida y xi yi g)))))

(defun pinta-matriu (matriu mida xi yi g)
  "Dibuixa totes les files de la matriu del mapa, de baix a dalt.
   matriu: matriu del mapa a dibuixar
   mida:   mida en píxels de cada casella
   xi:     coordenada x d'inici del mapa en pantalla
   yi:     coordenada y d'inici del mapa en pantalla
   g:      gruix de la quadrícula en píxels"
  (pinta-matriu-rec (reverse matriu) mida 0 xi yi g))

(defun pinta-matriu-rec (matriu mida y xi yi g)
  "Recorre recursivament les files de la matriu i les dibuixa.
   matriu: llista de files restants a dibuixar
   mida:   mida en píxels de cada casella
   y:      índex de fila actual
   xi:     coordenada x d'inici del mapa en pantalla
   yi:     coordenada y d'inici del mapa en pantalla
   g:      gruix de la quadrícula en píxels"
  (cond ((null matriu) (color 0 0 0 255 255 255))
        (t (move xi (+ yi (* y mida)))
           (pinta-fila (car matriu) 0 mida y xi yi g)
           (pinta-matriu-rec (cdr matriu) mida (+ y 1) xi yi g))))


;; ==============================================================
;;  ------------------- HUD (INFORMACIÓ DE L'ESTAT) -------------------
;; ==============================================================

(defun hud-comptar-bolles (unitats equip)
  "Compta les bolles vives de l'equip indicat.
   unitats: llista de totes les unitats del joc
   equip:   símbol de l'equip ('e1 o 'e2)"
  (hud-comptar-bolles-rec unitats equip 0))

(defun hud-comptar-bolles-rec (unitats equip comptador)
  "Recorre recursivament la llista d'unitats comptant les bolles de l'equip.
   unitats:   llista d'unitats restants a recórrer
   equip:     símbol de l'equip ('e1 o 'e2)
   comptador: nombre de bolles trobades fins ara"
  (cond ((null unitats) comptador)
        ((and (eq (nth 2 (car unitats)) 'bolla)
              (eq (nth 3 (car unitats)) equip))
         (hud-comptar-bolles-rec (cdr unitats) equip (+ comptador 1)))
        (t (hud-comptar-bolles-rec (cdr unitats) equip comptador))))

(defun hud-comptar-labs-equip (mapa equip)
  "Compta els laboratoris capturats per l'equip indicat recorrent tot el mapa.
   mapa:  matriu del mapa a recórrer
   equip: símbol de l'equip ('e1 o 'e2)"
  (hud-comptar-labs-mapa-rec mapa equip))

(defun hud-comptar-labs-mapa-rec (mapa equip)
  "Recorre recursivament les files del mapa sumant els labs de l'equip.
   mapa:  llista de files restants a recórrer
   equip: símbol de l'equip ('e1 o 'e2)"
  (cond ((null mapa) 0)
        (t (+ (hud-comptar-labs-fila (car mapa) equip)
              (hud-comptar-labs-mapa-rec (cdr mapa) equip)))))

(defun hud-comptar-labs-fila (fila equip)
  "Recorre una fila del mapa comptant els labs capturats per l'equip.
   fila:  llista de caselles d'una fila del mapa
   equip: símbol de l'equip ('e1 o 'e2)"
  (cond ((null fila) 0)
        (t (let* ((casella  (car fila))
                  (info-lab (member 'lab casella)))
             (+ (cond ((and (eq (car casella) 'terra)
                            info-lab
                            (eq (cadr info-lab) equip)) 1)
                      (t 0))
                (hud-comptar-labs-fila (cdr fila) equip))))))

(defun actualitza-hud (estat)
  "Escriu per pantalla la informació de l'estat: ronda, torn, pintura, bolles i labs.
   estat: estat actual del joc"
  (let* ((ronda     (cadr (assoc 'ronda estat)))
         (torn      (cadr (assoc 'torn estat)))
         (p1        (cadr (assoc 'pintura-e1 estat)))
         (p2        (cadr (assoc 'pintura-e2 estat)))
         (unitats   (cadr (assoc 'unitats estat)))
         (mapa      (cadr (assoc 'mapa estat)))
         (bolles-e1 (hud-comptar-bolles unitats 'e1))
         (bolles-e2 (hud-comptar-bolles unitats 'e2))
         (labs-e1   (hud-comptar-labs-equip mapa 'e1))
         (labs-e2   (hud-comptar-labs-equip mapa 'e2)))
    (goto-xy 0 0)
    (princ "Ronda: ")        (princ ronda)
    (princ " | Torn: ")      (princ torn)
    (princ " | P-E1: ")      (princ p1)
    (princ " | P-E2: ")      (princ p2)
    (princ "                    ")
    (terpri)
    (princ "E1-Bolles: ")    (princ bolles-e1)
    (princ " | E2-Bolles: ") (princ bolles-e2)
    (princ " | E1-Labs: ")   (princ labs-e1)
    (princ " | E2-Labs: ")   (princ labs-e2)
    (princ "                    ")
    (terpri)
    t))


;; ==============================================================
;;  ------------------- FUNCIÓ PRINCIPAL DE DIBUIX -------------------
;; ==============================================================

(defun ajusta-mida-mapa (mapa)
  "Retorna els paràmetres de dibuix (xi yi mida gruix) adaptats a la mida del mapa.
   mapa: matriu del mapa per calcular les dimensions"
  (let* ((files (length mapa))
         (cols  (length (car mapa))))
    (cond
      ;; Mapes petits (20x20 o menys)
      ((and (<= files 20) (<= cols 20))
       (list 100 20 15 1))
      ;; Mapes grans (50x50 o més): gruix 0 per evitar que el tauler es vegi negre
      ((or (>= files 50) (>= cols 50))
       (list 100 30 5 0))
      ;; Mapes mitjans
      (t (list 100 20 10 1)))))

(defun pinta (estat)
  "Dibuixa l'estat complet del joc: matriu del mapa i HUD d'informació.
   estat: estat actual del joc"
  (let* ((mapa       (cadr (assoc 'mapa estat)))
         (parametres (ajusta-mida-mapa mapa))
         (xi         (nth 0 parametres))
         (yi         (nth 1 parametres))
         (m          (nth 2 parametres))
         (g          (nth 3 parametres)))
    (pinta-matriu mapa m xi yi g)
    (actualitza-hud estat)
    estat))