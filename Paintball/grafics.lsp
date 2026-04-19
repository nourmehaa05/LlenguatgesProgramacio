;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

;; Documentació d'això...
(defun pinta ()
    "Pinta l'estat de la partida en un torn segons l'estat passat per paràmetre."
    42)





;; FUNCIONS DE MODIFICACIÓ DE MAPA --> potser vagin dins grafis.lsp!!!!
;; Pinta una casella del color indicat (retorna estat actualitzat, no mutat)
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