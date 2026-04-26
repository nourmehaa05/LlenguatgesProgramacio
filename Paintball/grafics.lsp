;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; Dibuixa el tablero del joc Paintball usant funcions de matrices.

;; ------------------------------------------------------------------
;;  ------------------- FUNCIONS AUXILIARS DE MATRIUS -------------------
;; ------------------------------------------------------------------

;; Trova l'índex de la columna dins d'una fila
(defun troba-dins-fila (fila valor &optional (x 0))
  "Retorna l'índex de la llista on un element conté el valor."
  (cond ((null fila) nil)
        ((member valor (car fila)) x)
        (t (troba-dins-fila (cdr fila) valor (+ x 1)))))

;; Trova la posició (x y) d'un valor dins d'una matriu
(defun troba-matriu (matriu valor &optional (y 0))
  "Retorna una llista (x y) on es troba el valor a la matriu."
  (cond ((null matriu) nil)
        (t (let ((x (troba-dins-fila (car matriu) valor)))
                (cond (x (list x y))
                      (t (troba-matriu (cdr matriu) valor (+ y 1))))))))

;; Indexa una fila: retorna el valor que hi ha a la posició x
(defun indexa-fila (fila x)
  "Retorna l'element a la posició x d'una fila."
  (cond ((null fila) nil)
        ((= x 0) (car fila))
        (t (indexa-fila (cdr fila) (- x 1)))))

;; Indexa una matriu: retorna el valor que hi ha a la posició (x y)
(defun indexa-matriu (matriu x y)
  "Retorna l'element a la posició (x y) de la matriu."
  (cond ((null matriu) nil)
        ((= y 0) (indexa-fila (car matriu) x))
        (t (indexa-matriu (cdr matriu) x (- y 1)))))

;; Posa un valor dins d'una fila a la posició x
(defun posa-dins-fila (fila x valor)
  "Retorna una nova fila on a la posició x hi ha el valor."
  (cond ((null fila) nil)
        ((= x 0) (cons valor (cdr fila)))
        (t (cons (car fila) (posa-dins-fila (cdr fila) (- x 1) valor)))))

;; Posa un valor dins d'una matriu a la posició (x y)
(defun posa-matriu (matriu x y valor)
  "Retorna una nova matriu on a la posició (x y) hi ha el valor."
  (cond ((null matriu) nil)
        ((= y 0) (cons (posa-dins-fila (car matriu) x valor) (cdr matriu)))
        (t (cons (car matriu) (posa-matriu (cdr matriu) x (- y 1) valor)))))

;; ------------------------------------------------------------------
;;  ------------------- FUNCIONS DE DIBUIX -------------------
;; ------------------------------------------------------------------

(defun aplica-color (clau)
  "Aplica el color segons el símbol donat (RGB)."
  (cond ((eq clau 'r)      (color 220 30 30 220 30 30))     ; Vermell
        ((eq clau 'g)      (color 30 180 30 30 180 30))     ; Verd
        ((eq clau 'b)      (color 30 80 220 30 80 220))     ; Blau
        ((eq clau 'lila)   (color 200 100 250 200 100 250)) ; NOU: Lila
        ((eq clau 'taronja)(color 255 165 0 255 165 0))     ; NOU: Taronja
        ((eq clau 'aigua)  (color 0 20 60 0 20 60))         ; Blau fosc
        (t                 (color 0 0 0 0 0 0))))           ; Negre

(defun dibuixaquadrat (mida)
  "Dibuixa un quadrat de mida `mida`, a la posició actual."
  (drawrel mida 0)
  (drawrel 0 mida)
  (drawrel (- mida) 0)
  (drawrel 0 (- mida)))

(defun quadrat (mida &optional (gruix g))
  "Dibuixa un quadrat de mida `mida` - 1, i de gruix `gruix`, a la posició actual."
  (cond ((plusp gruix) 
         (dibuixaquadrat (- mida 1))
         (moverel 1 1)
         (quadrat (- mida 2) (- gruix 1))
         (moverel -1 -1))))

(defun pinta-fila (fila i mida y)
  "Pinta una fila de caselles de forma segura i completa."
  (cond
    ((null fila) t)
    (t
     (let* ((casella (car fila))
            (x-pos (+ xi (* i mida)))
            (y-pos (+ yi (* y mida)))
            (color-casella (extraer-color-casella casella))
            (tipo-elemento (caddr casella))
            (equip-elemento (cadddr casella)))

       ;; 1. POSICIONAMIENTO Y FONDO
       (move x-pos y-pos)
       (aplica-color color-casella)
       (rellena-quadrat x-pos y-pos mida)

       ;; 2. BORDE DE LA CASILLA
       (move x-pos y-pos)
       (color 0 0 0 0 0 0)
       (quadrat mida)

       ;; 3. DIBUJO DEL CONTENIDO
       (cond
         ;; --- BASE ---
         ((member 'base casella)
          (let ((margin 4))
            ;; Si es equipo 1 usamos lila, si es equipo 2 usamos taronja
            (aplica-color (if (eq equip-elemento 'e1) 'lila 'taronja))
            (rellena-quadrat (+ x-pos margin) (+ y-pos margin) (- mida (* 2 margin)))))

         ;; --- LABORATORI (Cruz) ---
         ((member 'lab casella)
          (let ((cx (+ x-pos (/ mida 2)))
                (cy (+ y-pos (/ mida 2)))
                (r (/ mida 3)))
            (aplica-color (cond ((eq equip-elemento 'e1) 'r)
                                ((eq equip-elemento 'e2) 'g)
                                (t 'b)))
            (move (truncate (- cx r)) (truncate cy))
            (drawrel (truncate (* 2 r)) 0)
            (move (truncate cx) (truncate (- cy r)))
            (drawrel 0 (truncate (* 2 r)))))

        ;; --- BOLLA (Círculo Sólido y Limpio) ---
         ((member 'bolla casella)
          (let ((cx (truncate (+ x-pos (/ mida 2))))
                (cy (truncate (+ y-pos (/ mida 2)))))

            (aplica-color (nth 6 casella))

            ;; Dibujamos un círculo de 5x5 píxeles mediante 3 franjas
            ;; Franja superior e inferior (3 píxeles de ancho)
            (move (- cx 1) (- cy 2)) (draw (+ cx 1) (- cy 2))
            (move (- cx 1) (+ cy 2)) (draw (+ cx 1) (+ cy 2))

            ;; Cuerpo central (5 píxeles de ancho, 3 de alto)
            (move (- cx 2) (- cy 1)) (draw (+ cx 2) (- cy 1))
            (move (- cx 2) cy)       (draw (+ cx 2) cy)
            (move (- cx 2) (+ cy 1)) (draw (+ cx 2) (+ cy 1))))
       )

       ;; 4. RESET Y SIGUIENTE CASILLA
       (color 0 0 0 0 0 0)
       (pinta-fila (cdr fila) (+ i 1) mida y)))))
        
(defun extraer-color-casella (casella)
  "Extrae el color de fondo d'una casella."
  (let ((tipo (car casella)))
    (cond
      ((eq tipo 'aigua) 'aigua)
      ((eq tipo 'terra)
       (let ((color-info (cadr casella)))
         (cond
           ((eq color-info 'r) 'r)      ; terra pintada vermell
           ((eq color-info 'g) 'g)      ; terra pintada verd
           ((eq color-info 'b) 'b)      ; terra pintada blau
           (t 'terra))))                ; terra sense pintura (gris)
      (t 'terra))))

(defun grafics-cercle-petit (radi segments)
  "Dibuixa un cercle pequeño (aproximat)."
  (move 0 (- radi))
  (grafics-cercle-petit-rec radi (/ 360 segments) 0))

(defun grafics-cercle-petit-rec (radi pas angle)
  "Recorre els punts d'un cercle per dibuixar-lo."
  (cond
    ((>= angle 360) t)
    (t 
     (let ((x (truncate (* radi (cos (grafics-radians angle)))))
           (y (truncate (* radi (sin (grafics-radians angle))))))
       (draw x y)
       (grafics-cercle-petit-rec radi pas (+ angle pas))))))

(defun grafics-radians (graus)
  "Converteix graus a radians."
  (/ (* graus (* 2 pi)) 360))

(defun pinta-matriu (matriu mida)
  "Pinta la matriu sencera de caselles."
  (pinta-matriu-rec matriu mida 0))

(defun pinta-matriu-rec (matriu mida y)
  "Recorre les files de la matriu recursivament."
  (cond
    ((null matriu) (color 0 0 0 255 255 255))
    (t 
     (move xi (+ yi (* y mida)))
     (pinta-fila (car matriu) 0 mida y)
     (pinta-matriu-rec (cdr matriu) mida (+ y 1)))))

(defun pinta (estat)
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (ronda (cadr (assoc 'ronda estat)))
         (torn (cadr (assoc 'torn estat)))
         (p1 (cadr (assoc 'pintura-e1 estat)))
         (p2 (cadr (assoc 'pintura-e2 estat))))
    (cls)
    (ajusta-mida-mapa mapa)   
    (color 0 0 0 255 255 255)
    (princ "Ronda: ")
    (princ ronda)
    (princ " | Torn: ")
    (princ torn)
    (princ " | Pintura e1: ")
    (princ p1)
    (princ " | Pintura e2: ")
    (princ p2)
    (terpri)
    (pinta-matriu mapa m)
    (sleep 0.1)
    estat))

(defun pinta-marques (colors mida)
  (cond
    ((null colors) t)

    ((eq (car colors) 'r)
     (aplica-color 'r)
     (drawrel (- mida 6) 0)
     (move (- (current-x) (- mida 6)) (current-y))
     (pinta-marques (cdr colors) mida))

    ((eq (car colors) 'g)
     (aplica-color 'g)
     (drawrel 0 (- mida 6))
     (move (current-x) (+ (current-y) (- mida 6)))
     (pinta-marques (cdr colors) mida))

    ((eq (car colors) 'b)
     (aplica-color 'b)
     (drawrel (- mida 6) (- mida 6))
     (move (- (current-x) (- mida 6))
           (- (current-y) (- mida 6)))
     (pinta-marques (cdr colors) mida))

    (t (pinta-marques (cdr colors) mida))))

;; Rellena un quadrat sencer de mida x mida
(defun rellena-quadrat (x0 y0 mida)
  "Omple un quadrat píxel a píxel amb línies horitzontals."
  (rellena-quadrat-rec x0 y0 mida 0))

(defun rellena-quadrat-rec (x0 y0 mida i)
  (cond
    ((>= i mida) t)
    (t
      (move x0 (+ y0 i))
      (drawrel mida 0)
      (rellena-quadrat-rec x0 y0 mida (+ i 1)))))

(defun ajusta-mida-mapa (mapa)
  "Ajusta les globals de dibuix segons la mida del mapa."
  (let* ((files (length mapa))
         (cols (length (car mapa))))
    (cond
      ;; Mapa 20x20
      ((and (= files 20) (= cols 20))
       (setq xi 100)
       (setq yi 40)
       (setq m 15)
       (setq g 1))
      ;; Mapa 60x60
      ((and (= files 50) (= cols 50))
      (setq xi 100)
      (setq yi 40)
      (setq m 6)
      (setq g 1))
      ;; Per defecte
      (t
       (setq xi 10)
       (setq yi 40)
       (setq m 10)
       (setq g 1)))))