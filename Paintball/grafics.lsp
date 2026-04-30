;; ------------------------------------------------------------------
;;  Pràctica final de Llenguatges de Programació.
;;  LISP - Paintball.
;;  Estudiants: [Afegeix els teus noms aquí]
;;  Professor: [Afegeix el nom del professor]
;;  Lliurament: primera convocatòria.
;;  Fitxer del mòdul gràfic.
;; ------------------------------------------------------------------

;; ------------------------------------------------------------------
;;  ------------------- FUNCIONS AUXILIARS DE MATRIUS -------------------
;; ------------------------------------------------------------------

(defun troba-dins-fila (fila valor &optional (x 0))
  "Retorna l'índex de la llista on un element conté el valor."
  (cond ((null fila) nil)
        ((member valor (car fila)) x)
        (t (troba-dins-fila (cdr fila) valor (+ x 1)))))

(defun troba-matriu (matriu valor &optional (y 0))
  "Retorna una llista (x y) on es troba el valor a la matriu."
  (cond ((null matriu) nil)
        (t (let* ((x (troba-dins-fila (car matriu) valor)))
             (cond (x (list x y))
                   (t (troba-matriu (cdr matriu) valor (+ y 1))))))))

(defun indexa-fila (fila x)
  "Retorna l'element a la posició x d'una fila."
  (cond ((null fila) nil)
        ((= x 0) (car fila))
        (t (indexa-fila (cdr fila) (- x 1)))))

(defun indexa-matriu (matriu x y)
  "Retorna l'element a la posició (x y) de la matriu."
  (cond ((null matriu) nil)
        ((= y 0) (indexa-fila (car matriu) x))
        (t (indexa-matriu (cdr matriu) x (- y 1)))))

(defun posa-dins-fila (fila x valor)
  "Retorna una nova fila on a la posició x hi ha el valor."
  (cond ((null fila) nil)
        ((= x 0) (cons valor (cdr fila)))
        (t (cons (car fila) (posa-dins-fila (cdr fila) (- x 1) valor)))))

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
  (cond ((eq clau 'r)      (color 220 30 30 220 30 30))
        ((eq clau 'g)      (color 30 180 30 30 180 30))
        ((eq clau 'b)      (color 30 80 220 30 80 220))
        ((eq clau 'lila)   (color 200 100 250 200 100 250))
        ((eq clau 'taronja)(color 255 165 0 255 165 0))
        ((eq clau 'groc)   (color 255 255 0 255 255 0))
        ((eq clau 'aigua)  (color 0 20 60 0 20 60))
        (t                 (color 0 0 0 0 0 0))))

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

(defun rellena-quadrat (x0 y0 mida)
  "Omple un quadrat dibuixant línies horitzontals recursivament."
  (rellena-quadrat-rec x0 y0 mida 0))

(defun rellena-quadrat-rec (x0 y0 mida i)
  (cond ((>= i mida) t)
        (t (move x0 (+ y0 i))
           (drawrel mida 0)
           (rellena-quadrat-rec x0 y0 mida (+ i 1)))))

(defun pinta-fila (fila i mida y)
  (cond ((null fila) t)
        (t (let* ((casella (car fila))
                  (x-pos (+ xi (* i mida)))
                  (y-pos (+ yi (* y mida)))
                  (color-casella (extraer-color-casella casella))
                  (equipo (cond ((member 'e1 casella) 'e1)
                                ((member 'e2 casella) 'e2)
                                (t nil))))
             ;; 1. FONDO
             (move x-pos y-pos)
             (aplica-color color-casella)
             (rellena-quadrat x-pos y-pos mida)
             
             ;; 2. CUADRÍCULA
             (color 0 0 0 0 0 0)
             (move x-pos y-pos)
             (quadrat mida)
             
             ;; 3. UNIDADES REESCALABLES
             (cond 
               ;; --- BASES ---
               ((member 'base casella)
                (let* ((margen (truncate (* mida 0.2)))
                       (mida-int (- mida (* 2 margen)))
                       (color-eq (if (eq equipo 'e1) 'lila 'taronja)))
                  (aplica-color color-eq)
                  (rellena-quadrat (+ x-pos margen) (+ y-pos margen) mida-int)
                  (color 0 0 0 0 0 0)
                  (move (+ x-pos margen) (+ y-pos margen))
                  (quadrat mida-int)))

               ;; --- LABORATORIOS ---
               ((member 'lab casella)
                (let* ((cx (+ x-pos (/ mida 2))) (cy (+ y-pos (/ mida 2)))
                       (r (- (/ mida 2) 1)))
                  (aplica-color 'groc)
                  (move (truncate (- cx r)) (truncate cy)) (drawrel (truncate (* 2 r)) 0)
                  (move (truncate cx) (truncate (- cy r))) (drawrel 0 (truncate (* 2 r)))))

               ;; --- BOLAS (REESCALADO DINÁMICO) ---
               ((member 'bolla casella)
                (let* ((cx (truncate (+ x-pos (/ mida 2))))
                       (cy (truncate (+ y-pos (/ mida 2))))
                       ;; Radios proporcionales al tamaño de la celda
                       (r-equipo (truncate (* mida 0.4)))   ; 40% radio (80% total)
                       (r-centro (truncate (* mida 0.2)))   ; 20% radio (40% total)
                       (color-bola (cadr (member 'bolla casella)))
                       (color-eq (if (eq equipo 'e1) 'lila 'taronja)))
                  
                  (aplica-color color-eq)
                  (rellena-cercle cx cy r-equipo)
                  
                  (aplica-color color-bola)
                  (rellena-cercle cx cy r-centro)
                  
                  ;; Brillo proporcional
                  (color 255 255 255 255 255 255)
                  (let ((offset (truncate (* mida 0.15))))
                    (move cx (+ cy offset))
                    (draw cx (+ cy offset))))))

             (color 0 0 0 0 0 0)
             (pinta-fila (cdr fila) (+ i 1) mida y)))))

(defun extraer-color-casella (casella)
  (let* ((tipo (car casella)))
    (cond ((eq tipo 'aigua) 'aigua)
          ((eq tipo 'terra)
           (let* ((color-info (cadr casella)))
             (cond ((eq color-info 'r) 'r)
                   ((eq color-info 'g) 'g)
                   ((eq color-info 'b) 'b)
                   (t 'terra))))
          (t 'terra))))

(defun pinta-matriu (matriu mida)
  (pinta-matriu-rec (reverse matriu) mida 0))

(defun pinta-matriu-rec (matriu mida y)
  (cond ((null matriu) (color 0 0 0 255 255 255))
        (t (move xi (+ yi (* y mida)))
           (pinta-fila (car matriu) 0 mida y)
           (pinta-matriu-rec (cdr matriu) mida (+ y 1)))))

;; ------------------------------------------------------------------
;;  ------------------- HUD Y AJUSTES -------------------
;; ------------------------------------------------------------------

(defun hud-contar-bolas (unitats equip)
  (hud-contar-bolas-rec unitats equip 0))

(defun hud-contar-bolas-rec (unitats equip contador)
  (cond ((null unitats) contador)
        ((and (eq (nth 2 (car unitats)) 'bolla)
              (eq (nth 3 (car unitats)) equip))
         (hud-contar-bolas-rec (cdr unitats) equip (+ contador 1)))
        (t (hud-contar-bolas-rec (cdr unitats) equip contador))))

(defun hud-contar-labs-de-equipo (mapa equip)
  (hud-contar-labs-mapa-rec mapa equip))

(defun hud-contar-labs-mapa-rec (mapa equip)
  (cond ((null mapa) 0)
        (t (+ (hud-contar-labs-fila (car mapa) equip)
              (hud-contar-labs-mapa-rec (cdr mapa) equip)))))

(defun hud-contar-labs-fila (fila equip)
  (cond ((null fila) 0)
        (t (let* ((celda (car fila))
                  (lab-info (member 'lab celda)))
             (+ (if (and (eq (car celda) 'terra) lab-info (eq (cadr lab-info) equip)) 1 0)
                (hud-contar-labs-fila (cdr fila) equip))))))

(defun actualiza-hud (estat)
  (let* ((ronda (cadr (assoc 'ronda estat)))
         (torn  (cadr (assoc 'torn estat)))
         (p1    (cadr (assoc 'pintura-e1 estat)))
         (p2    (cadr (assoc 'pintura-e2 estat)))
         (unitats (cadr (assoc 'unitats estat)))
         (mapa (cadr (assoc 'mapa estat)))
         (bolas-e1 (hud-contar-bolas unitats 'e1))
         (bolas-e2 (hud-contar-bolas unitats 'e2))
         (labs-e1 (hud-contar-labs-de-equipo mapa 'e1))
         (labs-e2 (hud-contar-labs-de-equipo mapa 'e2)))
    (goto-xy 0 0)
    (princ "Ronda: ") (princ ronda) (princ " | Torn: ") (princ torn)
    (princ " | P-E1: ") (princ p1) (princ " | P-E2: ") (princ p2)
    (terpri)
    (princ "E1-Bolas: ") (princ bolas-e1) (princ " | E2-Bolas: ") (princ bolas-e2)
    (princ " | E1-Labs: ") (princ labs-e1) (princ " | E2-Labs: ") (princ labs-e2)
    t))

(defun pinta (estat)
  (let* ((mapa (cadr (assoc 'mapa estat))))
    (ajusta-mida-mapa mapa)
    (pinta-matriu mapa m)
    (actualiza-hud estat)
    estat))
    
(defun ajusta-mida-mapa (mapa)
  (let* ((files (length mapa))
         (cols (length (car mapa))))
    (cond ;; Mapas pequeños (20x20 o menos)
          ((and (<= files 20) (<= cols 20))
           (setq xi 100 yi 20 m 15 g 1))
          
          ;; Mapas grandes (50x50 o mayores)
          ((or (>= files 50) (>= cols 50))
           (setq xi 100 yi 30 m 5 g 0)) ; g=0 evita que el tablero se vea negro
          
          ;; Mapas medianos o cualquier otro caso
          (t (setq xi 100 yi 20 m 10 g 1)))))

(defun rellena-cercle (cx cy radi)
  (rellena-cercle-rec cx cy radi (- radi)))

(defun rellena-cercle-rec (cx cy radi y-offset)
  (cond ((> y-offset radi) t)
        (t (let* ((amplada (truncate (sqrt (- (* radi radi) (* y-offset y-offset)))))
                  (x-inici (- cx amplada))
                  (y-actual (+ cy y-offset)))
             (move x-inici y-actual)
             (drawrel (* 2 amplada) 0)
             (rellena-cercle-rec cx cy radi (+ y-offset 1))))))