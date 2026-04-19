;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer de l'agent intel·ligent XYZ999.
;; <Descripció de les funcions d'aquest fitxer>

;; Documentació d'això...
(defun agent-xyz999 (dades)
    "Retorna la jugada que realitza l'agent XYZ999 en un torn donades les dades de la partida."
    ; (car dades) = ronda
    ; (cadr dades) = equip
    ; etc.
    ; Exemple: retornam les següents accions:
    ; 1. Crear una bolla a la posició (2, 3).
    ; 2. Moure a la posició (4, 5).
    ; 3. Pintar a la posició (6, 7).
    ; 4. Escriure a la posició 1 de la memòria compartida el valor 42.
    ; 5. Escriure a la posició 2 de la memòria compartida el valor (2 3).
    '((crea-bolla (2 3))
      (mou (4 5)) ; --> (mou id-bolla (4 5)) PREGUNTAR PQ NO ENTIENDO
      (pinta (6 7))
      (escriu-memoria 1 42)
      (escriu-memoria 2 (2 3))))


;; Recordau que qualsevol funció no predefinida necessària per a l'agent, ha d'estar definida en aquest fitxer
;; i ha d'estar prefixada amb "agent-xyz999-" per evitar conflictes amb altres agents o mòduls.
;; Per exemple:
(defun agent-xyz999-longitud (llista)
    "Retorna la longitud d'una llista."
    (cond ((null llista) 0)
          (t (+ 1 (agent-xyz999-longitud (cdr llista))))))

(defun agent-xyz999-actualitzar-memoria (memoria visio)
  "Afegeix la visió nova a la memòria (sense duplicats)."
  (cond
    ((null visio) memoria)
    ((member (car visio) memoria :test #'equal)
     (agent-xyz999-actualitzar-memoria memoria (cdr visio)))
    (t
     (cons (car visio)
           (agent-xyz999-actualitzar-memoria memoria (cdr visio))))))


(defun get-memoria (estat)
  (cadr (assoc 'memoria-compartida estat)))

(defun agent-xyz999-get-labs-memoria (memoria)
  (remove-if-not
   (lambda (c)
     (and (listp (third c))
          (>= (length (third c)) 3)
          (eq (caddr (third c)) 'lab)))
   memoria))


(defun agent-xyz999-distancia (p1 p2)
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))))


(defun agent-xyz999-lab-mes-proper (pos labs)
  (if (null labs)
      nil
      (car
       (sort labs
             (lambda (a b)
               (< (agent-xyz999-distancia pos a)
                  (agent-xyz999-distancia pos b)))))))


(defun agent-xyz999 (dades)
  (let* (
         (visio (get-visio dades)) ;; lo que ve
         (memoria (get-memoria dades))
         
         ;; actualizar memoria
         (memoria-nova (agent-xyz999-actualitzar-memoria memoria visio))
         
         ;; buscar labs conocidos
         (labs (agent-xyz999-get-labs-memoria memoria-nova))
         
         ;; posición actual (ejemplo)
         (pos '(5 5))
         
         ;; objetivo
         (lab (agent-xyz999-lab-mes-proper pos labs))
        )
    
    ;; devolver acciones
    (list
     (list 'escriu-memoria 0 memoria-nova)
     
     ;; moverse hacia lab si existe
     (if lab
         (list 'mou (list (first lab) (second lab)))
         (list 'mou '(0 0))))))