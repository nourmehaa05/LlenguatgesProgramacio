;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

;; ------------------------------------------------------------------
;;  ------------------- DIBUIX PER TORN -------------------
;; ------------------------------------------------------------------

(defun pinta (estat)
  "Pinta l'estat complet de la partida una vegada per torn."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (dades (cadr (assoc 'dades mapa)))
         (amplada (cadr (assoc 'amplada mapa)))
         (alt (cadr (assoc 'alt mapa)))
         (unitats (cadr (assoc 'unitats estat)))
         (mida-casella (grafics-mida-casella amplada alt))
         (origen-x (grafics-origen-x amplada mida-casella))
         (origen-y (grafics-origen-y alt mida-casella)))
    (cls)
    (color 0 0 0 255 255 255)
    (grafics-mostrar-resum estat)
    (grafics-dibuixar-mapa dades unitats origen-x origen-y mida-casella)
    (sleep 1)
    estat))

(defun grafics-mostrar-resum (estat)
  (princ "Ronda: ")
  (princ (cadr (assoc 'ronda estat)))
  (princ " | Torn: ")
  (princ (cadr (assoc 'torn estat)))
  (princ " | Pintura e1: ")
  (princ (cadr (assoc 'pintura-e1 estat)))
  (princ " | Pintura e2: ")
  (princ (cadr (assoc 'pintura-e2 estat)))
  (terpri))

(defun grafics-mida-casella (amplada alt)
  (let* ((ample-util 620)
         (alt-util 320)
         (mida-ample (round (/ ample-util amplada)))
         (mida-alt (round (/ alt-util alt)))
         (mida (min mida-ample mida-alt)))
    (if (< mida 4) 4 mida)))

(defun grafics-origen-x (amplada mida-casella)
  (round (/ (- 640 (* amplada mida-casella)) 2)))

(defun grafics-origen-y (alt mida-casella)
  (+ 28 (round (/ (- 345 (* alt mida-casella)) 2))))

(defun grafics-dibuixar-mapa (dades unitats origen-x origen-y mida-casella)
  (grafics-dibuixar-fila dades unitats origen-x origen-y mida-casella 0))

(defun grafics-dibuixar-fila (files unitats origen-x origen-y mida-casella y)
  (cond
    ((null files) nil)
    (t
     (progn
       (grafics-dibuixar-fila-caselles (car files) unitats origen-x origen-y mida-casella y 0)
       (grafics-dibuixar-fila (cdr files) unitats origen-x origen-y mida-casella (+ y 1))))))

(defun grafics-dibuixar-fila-caselles (fila unitats origen-x origen-y mida-casella y x)
  (cond
    ((null fila) nil)
    (t
     (let* ((coord (list x y))
            (casella (car fila))
            (unitat (grafics-obtenir-unitat-per-coord unitats coord))
            (px (+ origen-x (* x mida-casella)))
            (py (+ origen-y (* y mida-casella))))
       (grafics-dibuixar-casella casella coord unitat px py mida-casella)
       (grafics-dibuixar-fila-caselles (cdr fila) unitats origen-x origen-y mida-casella y (+ x 1))))))

(defun grafics-dibuixar-casella (casella coord unitat px py mida-casella)
  (let* ((tipus (grafics-tipus-casella casella))
         (color-casella (grafics-color-casella casella))
         (element (or (and unitat (nth 2 unitat)) (grafics-element-casella casella)))
         (equip (or (and unitat (nth 3 unitat)) (grafics-equip-casella casella)))
         (colors-pintat (or (and unitat (nth 6 unitat)) (grafics-colors-pintat-casella casella)))
         (color-propi (or (and unitat (nth 5 unitat)) (grafics-color-propi-casella casella)))
         (tr-pintar (or (and unitat (nth 7 unitat)) (grafics-tr-pintar-casella casella)))
         (tr-moure (or (and unitat (nth 8 unitat)) (grafics-tr-moure-casella casella))))
    (cond
      ((eq tipus 'aigua)
       (grafics-pen 'b)
       (move px py)
       (grafics-quadrat mida-casella))
      (t
       (grafics-pen color-casella)
       (move px py)
       (grafics-quadrat mida-casella)
       (cond
         ((eq element 'base)
          (grafics-dibuixar-base equip colors-pintat px py mida-casella))
         ((eq element 'bolla)
          (grafics-dibuixar-bolla color-propi colors-pintat px py mida-casella tr-pintar tr-moure))
         ((eq element 'lab)
          (grafics-dibuixar-lab equip px py mida-casella)))))))

(defun grafics-dibuixar-base (equip colors-pintat px py mida-casella)
  (let ((cini (+ 2 (round (/ mida-casella 4)))))
    (grafics-pen (grafics-color-equip equip))
    (move (+ px cini) (+ py cini))
    (grafics-quadrat (- mida-casella (* 2 cini)))
    (grafics-dibuixar-colors-pintat colors-pintat px py mida-casella)))

(defun grafics-dibuixar-bolla (color-propi colors-pintat px py mida-casella tr-pintar tr-moure)
  (let ((radi (max 2 (round (/ mida-casella 3)))))
    (grafics-pen (grafics-color-simbol color-propi))
    (grafics-cercle (+ px (round (/ mida-casella 2)))
                    (+ py (round (/ mida-casella 2)))
                    radi
                    10)
    (grafics-dibuixar-colors-pintat colors-pintat px py mida-casella)))

(defun grafics-dibuixar-lab (equip px py mida-casella)
  (let ((color-lab (grafics-color-equip equip)))
    (grafics-pen color-lab)
    (move (+ px 2) (+ py 2))
    (drawrel (- mida-casella 4) (- mida-casella 4))
    (move (+ px 2) (- (+ py mida-casella) 2))
    (drawrel (- mida-casella 4) 4)))

(defun grafics-dibuixar-colors-pintat (colors-pintat px py mida-casella)
  (grafics-dibuixar-colors-pintat-rec colors-pintat px py mida-casella 0))

(defun grafics-dibuixar-colors-pintat-rec (colors-pintat px py mida-casella idx)
  (cond
    ((null colors-pintat) nil)
    (t
     (let* ((color (car colors-pintat))
            (offset (+ 2 (* idx 4))))
       (grafics-pen (grafics-color-simbol color))
       (move (+ px offset) (+ py 1))
       (grafics-quadrat 2)
       (grafics-dibuixar-colors-pintat-rec (cdr colors-pintat) px py mida-casella (+ idx 1))))))

(defun grafics-quadrat (mida)
  (drawrel 0 mida)
  (drawrel mida 0)
  (drawrel 0 (- mida))
  (drawrel (- mida) 0))

(defun grafics-cercle (x y radi segments)
  (move (+ x radi) y)
  (grafics-cercle-rec x y radi (/ 360 segments) 0))

(defun grafics-cercle-rec (x y radi pas angle)
  (cond
    ((< angle 360)
     (draw (+ x (* radi (cos (grafics-radians (+ angle pas)))))
           (+ y (* radi (sin (grafics-radians (+ angle pas))))))
     (grafics-cercle-rec x y radi pas (+ angle pas)))
    (t t)))

(defun grafics-radians (graus)
  (/ (* graus (* 2 pi)) 360))

(defun grafics-pen (simbol-color)
  (let ((rgb (grafics-color->rgb simbol-color)))
    (color (car rgb) (cadr rgb) (caddr rgb) 255 255 255)))

(defun grafics-color->rgb (simbol)
  (cond
    ((eq simbol 'r) '(255 0 0))
    ((eq simbol 'g) '(0 180 0))
    ((eq simbol 'b) '(0 80 255))
    ((eq simbol 'e1) '(255 0 0))
    ((eq simbol 'e2) '(0 180 0))
    ((eq simbol 'lab) '(0 0 0))
    ((eq simbol 'terra) '(190 190 190))
    ((eq simbol 'aigua) '(40 100 255))
    (t '(0 0 0))))

(defun grafics-color-equip (equip)
  (cond
    ((eq equip 'e1) 'r)
    ((eq equip 'e2) 'g)
    (t 'lab)))

(defun grafics-color-simbol (simbol)
  (cond
    ((eq simbol 'r) 'r)
    ((eq simbol 'g) 'g)
    ((eq simbol 'b) 'b)
    ((eq simbol 'e1) 'r)
    ((eq simbol 'e2) 'g)
    (t 'lab)))

(defun grafics-tipus-casella (casella)
  (cond
    ((null casella) 'terra)
    ((atom casella) casella)
    (t (car casella))))

(defun grafics-color-casella (casella)
  (cond
    ((null casella) 'terra)
    ((atom casella) 'terra)
    ((and (consp casella) (consp (cdr casella))) (cadr casella))
    (t 'terra)))

(defun grafics-element-casella (casella)
  (cond
    ((and (consp casella) (member 'bolla casella)) 'bolla)
    ((and (consp casella) (member 'base casella)) 'base)
    ((and (consp casella) (member 'lab casella)) 'lab)
    (t nil)))

(defun grafics-equip-casella (casella)
  (cond
    ((and (consp casella) (member 'bolla casella)) (cadr (member 'bolla casella)))
    ((and (consp casella) (member 'base casella)) (cadr (member 'base casella)))
    ((and (consp casella) (member 'lab casella)) (cadr (member 'lab casella)))
    (t nil)))

(defun grafics-colors-pintat-casella (casella)
  (cond
    ((and (consp casella) (member 'bolla casella)) (cadddr (member 'bolla casella)))
    ((and (consp casella) (member 'base casella)) (caddr (member 'base casella)))
    (t nil)))

(defun grafics-color-propi-casella (casella)
  (cond
    ((and (consp casella) (member 'bolla casella)) (caddr (member 'bolla casella)))
    (t nil)))

(defun grafics-tr-pintar-casella (casella)
  (cond
    ((and (consp casella) (member 'bolla casella)) (cadddr (cddddr (member 'bolla casella))))
    (t nil)))

(defun grafics-tr-moure-casella (casella)
  (cond
    ((and (consp casella) (member 'bolla casella)) (cadr (cddddr (member 'bolla casella))))
    (t nil)))

(defun grafics-obtenir-unitat-per-coord (unitats coord)
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord) (car unitats))
    (t (grafics-obtenir-unitat-per-coord (cdr unitats) coord))))
;; ------------------------------------------------------------------
;;  ------------------- MODIFICACIÓ FUNCIONAL DEL MAPA -------------------
;; ------------------------------------------------------------------
;; Aquest bloc és el contracte canònic per pintar el mapa sense mutar-lo.
;; No cal introduir-hi variants alternatives per a la mateixa tasca.

(defun pintar-casella (coord color estat)
  "Pinta la casella de la coordenada indicada del color donat.
   Actualitza el color de la casella al mapa."
  (let* ((mapa      (cadr (assoc 'mapa estat)))
         (dades     (cadr (assoc 'dades mapa)))
         (amplada   (cadr (assoc 'amplada mapa)))
         (alt       (cadr (assoc 'alt mapa)))
         ;; reconstruïm el mapa amb la casella pintada
         (dades-noves (pintar-casella-dades coord color dades))
         (mapa-nou  (list (list 'dades    dades-noves)
                          (list 'amplada  amplada)
                          (list 'alt      alt))))
    ;; reconstruïm l'estat amb el nou mapa
    (substituir-camp 'mapa mapa-nou estat)))

(defun pintar-casella-dades (coord color dades)
  "Reconstrueix la matriu del mapa pintant la casella (x,y) del color indicat.
   Cada cel·la del mapa és una llista: (tipus-casella color-casella)"
  (let ((x (car coord))
        (y (cadr coord)))
    (pintar-fila-rec dades x y color 0)))

(defun pintar-fila-rec (files x y color fila-actual)
  "Recorre les files del mapa recursivament fins trobar la fila y."
  (cond
    ((null files) nil)
    ((= fila-actual y)
     (cons (pintar-columna-rec (car files) x color 0)
           (pintar-fila-rec (cdr files) x y color (+ fila-actual 1))))
    (t
     (cons (car files)
           (pintar-fila-rec (cdr files) x y color (+ fila-actual 1))))))

(defun pintar-columna-rec (fila x color col-actual)
  "Recorre les columnes d'una fila recursivament fins trobar la columna x."
  (cond
    ((null fila) nil)
    ((= col-actual x)
     ;; La cel·la és (tipus-casella color-casella); actualitzem el color
     (cons (list (car (car fila))   ;; mantenim el tipus ('terra o 'aigua)
                 color)             ;; actualitzem el color
           (pintar-columna-rec (cdr fila) x color (+ col-actual 1))))
    (t
     (cons (car fila)
           (pintar-columna-rec (cdr fila) x color (+ col-actual 1))))))