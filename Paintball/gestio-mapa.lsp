
;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA (general) ------------------- REVISAR!!!!!!!!!!!!!!!!!
;; ------------------------------------------------------------------
(defun get-mapa-dades (estat)
  "Extreu la matriu de caselles del mapa des de l'estat."
  (cadr (assoc 'dades (cadr (assoc 'mapa estat)))))

(defun get-bases (mapa) ; devuelve las coord  --> REVISAR SI ESTE MÉTODO ES UTIL
  "recorre el mapa i retorna coordenades i dades de totes les bases."
  (letrec ((aux (lambda (mapa y)
                  (if (null mapa)
                      nil
                      (append
                       (aux-fila (car mapa) y 0)
                       (aux (cdr mapa) (+ y 1))))))
           (aux-fila (lambda (fila y x)
                       (if (null fila)
                           nil
                           (let ((c (car fila)))
                             (append
                              (if (and (listp c)
                                       (>= (length c) 3)
                                       (eq (caddr c) 'base))
                                  (list (list x y c))
                                  nil)
                              (aux-fila (cdr fila) y (+ x 1))))))))
    (aux mapa 0)))


(defun get-labs (mapa) ;; agente saber labs más cercanos!
  "recorre el mapa i retorna coordenades i dades de tots els laboratoris."
  (letrec ((aux (lambda (mapa y)
                  (if (null mapa)
                      nil
                      (append
                       (aux-fila (car mapa) y 0)
                       (aux (cdr mapa) (+ y 1))))))

           (aux-fila (lambda (fila y x)
                       (if (null fila)
                           nil
                           (let ((c (car fila)))
                             (append
                              (if (and (listp c)
                                       (>= (length c) 3)
                                       (eq (caddr c) 'lab))
                                  (list (list x y c))
                                  nil)
                              (aux-fila (cdr fila) y (+ x 1))))))))
    (aux mapa 0)))


;; ------------------------------------------------------------------
;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA (caselles) ------------------- REVISAR!!!!!!!!!!!!!!!!!
;; ------------------------------------------------------------------
;; ------------------------------------------------------------------

(defun get-casella (mapa pos)
  "retorna la casella del mapa en una posició [x y]."
  (let ((x (car pos))
        (y (cadr pos)))
    (nth x (nth y mapa))))

;; REVISA LÍMITS MAPA
(defun posicio-valida (pos estat)
  "comprova que una posició estigui dins els límits del mapa."
  (let* ((mapa (get-mapa-dades estat))
         (x (car pos))
         (y (cadr pos))
         (alt (length mapa))
         (ample (length (car mapa))))
    (and (>= x 0) (< x ample)
         (>= y 0) (< y alt))))


(defun es-posicio-lliure (pos estat)
  (let ((c (get-casella (get-mapa-dades estat) pos)))
    (or (null c)
        (not (or (es-base-casella c)
                 (es-lab-casella c)
                 (es-bolla-casella c))))))


(defun es-aigua (casella)
  "retorna t si la casella és d'aigua."
  (and (listp casella) (eq (car casella) 'agua)))




(defun es-base (c)
  "indica si una casella correspon a una base."
  (and (listp c) (eq (caddr c) 'base)))


(defun es-lab (c)
  "indica si una casella correspon a un laboratori."
  (and (listp c) (eq (caddr c) 'lab)))

(defun es-bolla (c)
  "indica si una casella correspon a una bolla."
  (and (listp c) (eq (caddr c) 'bolla)))




;(defun get-color-terra-casella (casella) ; ESTE MÈTODO NOSE SI LO USAREMOS (maybe es util para repintar pero yo diría que no, que pintamos encima y ya)!!!!!!!!!!!!!!!!!!
;  "retorna el color de la casella de terra."
;  (if (es-terra-casella casella)
;      (cadr casella)
;      nil))

;(defun get-tipus-element (casella) ; NOSE si es util pq devuelve nil y claro al pedirlo podria comprarar con nil directamente y los otros metodos que tenemos serian inutiles, entonces de momento lo comento
;  "retorna el tipus d'element en la casella: 'lab, 'base, 'bolla, o nil."
;  (if (es-terra-casella casella)
;      (if (>= (length casella) 3)
;          (caddr casella)
;          nil)
;      nil))


(defun es-adjacent (origen dest)
; r^moure = 2u^2 (8 caselles adjacents)
  (member dest (adjacents origen) :test #'equal))


(defun adjacents (pos)
  (let ((x (car pos))
        (y (cadr pos)))
    (list
     (list (+ x 1) y)
     (list (- x 1) y)
     (list x (+ y 1))
     (list x (- y 1))
     (list (+ x 1) (+ y 1))
     (list (+ x 1) (- y 1))
     (list (- x 1) (+ y 1))
     (list (- x 1) (- y 1)))))

;; ------------------------------------------------------------------
;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA (bases) -------------------
;; ------------------------------------------------------------------
;; ------------------------------------------------------------------

;; Mètode trobar-base: cerca la base d'un equip dins el mapa.
(defun trobar-base (mapa equip)
  "cerca la base d'un equip dins el mapa."
  (trobar-base-rec mapa equip 0))

;; Mètode trobar-base-rec: recorregut recursiu per files per localitzar una base.
(defun trobar-base-rec (mapa equip y)
  "recorregut recursiu per files per localitzar una base."
  (if (null mapa)
      nil
      (or (trobar-base-fila (car mapa) equip y 0)
          (trobar-base-rec (cdr mapa) equip (+ y 1)))))

;; Mètode trobar-base-fila: recorregut recursiu d'una fila per trobar la base de l'equip.
(defun trobar-base-fila (fila equip y x)
  "recorregut recursiu d'una fila per trobar la base de l'equip."
  (if (null fila)
      nil
      (let ((c (car fila)))
        (if (and (listp c)
                 (>= (length c) 4)
                 (eq (caddr c) 'base)
                 (eq (cadddr c) equip))
            (list x y c)
            (trobar-base-fila (cdr fila) equip y (+ x 1))))))



;; ------------------------------------------------------------------
;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA (laboratoris) -------------------
;; ------------------------------------------------------------------
;; ------------------------------------------------------------------


;; ÚTIL per incrementar la pintura per torn.
(defun comptar-laboratoris-equip (estat equip)
  "compta laboratoris controlats per un equip."
  (let ((mapa (get-mapa-dades estat)))
    (comptar-labs-rec mapa equip)))

(defun comptar-labs-rec (mapa equip)
  "recorregut recursiu per sumar laboratoris d'un equip."
  (if (null mapa)
      0
      (+ (comptar-labs-fila (car mapa) equip)
         (comptar-labs-rec (cdr mapa) equip))))

(defun comptar-labs-fila (fila equip)
  "compta laboratoris d'un equip dins una sola fila."
  (if (null fila)
      0
      (+ (if (lab-del-equip (car fila) equip) 1 0)
         (comptar-labs-fila (cdr fila) equip))))

(defun lab-del-equip (casella equip)
  "comprova si la casella és un laboratori del equip indicat."
  (and (listp casella)
       (>= (length casella) 4)
       (eq (caddr casella) 'lab)
       (eq (cadddr casella) equip)))


;; ------------------------------------------------------------------
;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA (bolles) -------------------
;; ------------------------------------------------------------------
;; ------------------------------------------------------------------


(defun trobar-bolla (id estat)
  "cerca una bolla pel seu id dins el mapa i retorna (x y casella)."
  (let ((mapa (get-mapa-dades estat)))
    (trobar-bolla-rec id mapa 0)))

;; Mètode trobar-bolla-rec: recorre el mapa buscant la bolla pel id.
(defun trobar-bolla-rec (id mapa y)
  "recorregut recursiu per files per localitzar una bolla per id."
  (if (null mapa)
      nil
      (or (trobar-bolla-fila id (car mapa) y 0)
          (trobar-bolla-rec id (cdr mapa) (+ y 1)))))

;; Mètode trobar-bolla-fila: recorregut recursiu d'una fila per trobar la bolla.
(defun trobar-bolla-fila (id fila y x)
  "recorregut recursiu d'una fila per trobar la bolla per id."
  (if (null fila)
      nil
      (let ((c (car fila)))
        (if (and (es-bolla-casella-p c)
                 (= (get-bolla-id-casella c) id))
            (list x y c)
            (trobar-bolla-fila id (cdr fila) y (+ x 1))))))

;; ÚTIL PER DECIDIR QUI HA GUANYAT
(defun comptar-bolles-equip (estat equip)
  "compta bolles pertanyents a un equip."
  (let ((mapa (get-mapa-dades estat)))
    (comptar-bolles-rec mapa equip)))

;; Mètode comptar-bolles-rec: recorregut recursiu per sumar bolles d'un equip.
(defun comptar-bolles-rec (mapa equip)
  "recorregut recursiu per sumar bolles d'un equip."
  (if (null mapa)
      0
      (+ (comptar-bolles-fila (car mapa) equip)
         (comptar-bolles-rec (cdr mapa) equip))))

(defun comptar-bolles-fila (fila equip)
  "compta bolles d'un equip dins una fila."
  (if (null fila)
      0
      (+ (if (bolla-del-equip (car fila) equip) 1 0)
         (comptar-bolles-fila (cdr fila) equip))))

(defun bolla-del-equip (casella equip)
  "comprova si la casella és una bolla del equip indicat."
  (and (listp casella)
       (>= (length casella) 4)
       (eq (caddr casella) 'bolla)
       (eq (cadddr casella) equip)))


; -----------------------------------------

;; ACCESSORS PER A BOLLES
;; Retorna l'id d'una bolla en una casella
(defun get-bolla-id-casella (casella) ; UTIL para cuando pintemos una bola, saber cual hemos pintado y cambiarle el color
  "retorna l'id d'una bolla, o nil si no és una bolla."
  (if (es-bolla-casella-p casella)
      (cadddr casella)
      nil))

;; Retorna l'equip d'una bolla en una casella
;(defun get-bolla-equip-casella (casella)
;  "retorna l'equip d'una bolla (e1 o e2)."
;  (if (es-bolla-casella-p casella)
;      (car (cddddr casella))
;      nil))

;; Retorna els colors pintats d'una bolla
(defun get-bolla-colors-pintat (casella)
  "retorna la llista de colors amb els quals la bolla està pintada."
  (if (es-bolla-casella-p casella)
      (cadr (cddddr casella))
      nil))

;; Retorna el temps de recuperació de pintar
(defun get-bolla-tr-pintar (casella)
  "retorna el temps de recuperació de pintar de la bolla."
  (if (es-bolla-casella-p casella)
      (caddr (cddddr casella))
      nil))

;; Retorna el temps de recuperació de moure
(defun get-tr-moure (casella)
  "retorna el temps de recuperació de moure de la bolla."
  (if (es-bolla-casella-p casella)
      (cadddr (cddddr casella))
      nil))

