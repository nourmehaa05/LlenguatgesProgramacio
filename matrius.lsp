; Troba l'índex de la fila i columna d'una matriu on es troba un valor
; Fer-ho sense recursivitat de coa és relativament complicat.
; Amb recursivitat de coa és un poc més senzill, però fa falta:
; a) paràmetres opcionals inicialitzats a 0
; b) una funció auxiliar per trobar l'índex de la columna dins d'una fila

; VERSIÓ AMB MEMBER
(defun troba-dins-fila (fila valor &optional (x 0))
  "Retorna l'índex de la llista on un element conté el valor."
  (cond ((null fila) nil) ; Hem acabat la llista: no existeix.
        ((member valor (car fila)) x) ; Pertany a l'element: retornam l'índex.
        (t (troba-dins-fila (cdr fila) valor (+ x 1))))) ; Seguim mirant a la resta de la llista.

(defun troba-matriu (matriu valor &optional (y 0))
  "Retorna una llista (x y) on x és l'índex de la fila i y el de la columna on es troba el valor."
  (cond ((null matriu) nil) ; Hem acabat la matriu: no existeix.
        (t (let ((x (troba-dins-fila (car matriu) valor))) ; x = resultat de cercar-lo en aquesta fila.
                (cond (x (list x y)) ; Si l'havíem trobat, el resultat és (x y).
                      (t (troba-matriu (cdr matriu) valor (+ y 1)))))))); Seguim mirant amb la resta de la matriu.

; VERSIÓ AMB FUNCALL
;; Deixat com a exercici.

; Donada una matriu, una posició x y i un valor, volem la matriu on a la posició x y hi ha el nou valor.

(defun posa-dins-fila (fila x valor)
  "Retorna una nova fila on a la posició x hi ha el valor, i la resta és igual."
  (cond ((null fila) nil) ; Hem acabat la fila: no hem de fer res.
        ((= x 0) (cons valor (cdr fila))) ; Hem arribat a la posició: el resultat és el valor seguit de la resta de la fila.
        (t (cons (car fila) (posa-dins-fila (cdr fila) (- x 1) valor))))) ; Seguim mirant a la resta de la fila.

(defun posa-matriu (matriu x y valor)
  "Retorna una nova matriu on a la posició x y hi ha el valor, i la resta és igual."
  (cond ((null matriu) nil)
        ((= y 0) (cons (posa-dins-fila (car matriu) x valor) (cdr matriu)))
        (t (cons (car matriu) (posa-matriu (cdr matriu) x (- y 1) valor)))))

; Indexa una matriu: retorna el valor que hi ha a la posició x y.

(defun indexa-fila (fila x)
  (cond ((null fila) nil)
        ((= x 0) (car fila))
        (t (indexa-fila (cdr fila) (- x 1)))))

(defun indexa-matriu (matriu x y)
  (cond ((null matriu) nil)
        ((= y 0) (indexa-fila (car matriu) x))
        (t (indexa-matriu (cdr matriu) x (- y 1)))))

; Amb funcions predefinides és més senzill:

(defun nth-matriu (matriu x y)
  (nth y (nth x matriu)))

; Exemple:

(defun exemple ()
  (let* ((matriu '(((a 1) (b 2) (c 3))
                   ((d 4) (e 5) (f 6))
                   ((g 7) (h 8) (i 9)))) ; Definim la matriu
         (pos (troba-matriu matriu 'c))  ; Cercam la pos d'un element
         (el (indexa-matriu matriu (car pos) (cadr pos))) ; Obtenim l'element
         (el2 (list (car el) (* 10 (cadr el)))) ; Cream un altre element
         (matriu2 (posa-matriu matriu (car pos) (cadr pos) el2))) ; El posam a pos
        (break "mira els valors de matriu, pos, el, el2 i matriu2")
        matriu2)) ; Avalua a la nova matriu
