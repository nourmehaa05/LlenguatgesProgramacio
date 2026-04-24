
;; Agent de prova alineat amb el contracte que li passa el controlador.
(defun get-ronda (dades)
  (car dades))

(defun get-equip (dades)
  (cadr dades))

(defun get-pintura (dades)
  (caddr dades))

(defun get-id-unitat (dades)
  (cadddr dades))

(defun get-tipus-unitat (dades)
  (nth 4 dades))

(defun get-coord-unitat (dades)
  (nth 5 dades))

(defun get-visio (dades)
  (nth 10 dades))

(defun get-memoria (dades)
  (nth 11 dades))

(defun agent-prova (dades)
  "Retorna una llista d'accions mínima i legal per una unitat."
  (let* ((equip (get-equip dades))
         (pintura (get-pintura dades))
         (tipus (get-tipus-unitat dades))
         (coord (get-coord-unitat dades))
         (visio (get-visio dades))
         (memoria (get-memoria dades)))
    (cond
      ((eq tipus 'base)
       (if (and (>= pintura 50)
                (agent-xyz999-first-free-base-cell coord visio))
           (list (list 'crea-bolla
                       (list 'r
                             (agent-xyz999-first-free-base-cell coord visio))))
         nil))
      ((eq tipus 'bolla)
       (cond
         ((and (< (agent-xyz999-temps-pintar dades) 1)
               (agent-xyz999-first-paint-target equip coord visio))
          (list (list 'pinta
                      (list (agent-xyz999-first-paint-target equip coord visio)))))
         ((and (< (agent-xyz999-temps-moure dades) 1)
               (agent-xyz999-first-move-target equip coord visio))
          (list (list 'mou
                      (list (agent-xyz999-first-move-target equip coord visio)))))
         (t nil)))
      (t nil))))

(defun agent-xyz999-temps-pintar (dades)
  (nth 8 dades))

(defun agent-xyz999-temps-moure (dades)
  (nth 9 dades))

(defun agent-xyz999-first-free-base-cell (coord visio)
  (agent-xyz999-first-free-base-cell-rec coord visio))

(defun agent-xyz999-first-free-base-cell-rec (coord visio)
  (cond
    ((null visio) nil)
    ((and (> (agent-xyz999-dist2 coord (car (car visio))) 0)
          (<= (agent-xyz999-dist2 coord (car (car visio))) 2)
          (null (agent-xyz999-cell-element (car visio))))
     (car (car visio)))
    (t
     (agent-xyz999-first-free-base-cell-rec coord (cdr visio)))))

(defun agent-xyz999-first-paint-target (equip coord visio)
  (agent-xyz999-first-paint-target-rec equip coord visio))

(defun agent-xyz999-first-paint-target-rec (equip coord visio)
  (cond
    ((null visio) nil)
    ((and (<= (agent-xyz999-dist2 coord (car (car visio))) 5)
          (> (agent-xyz999-dist2 coord (car (car visio))) 0)
          (agent-xyz999-pintable-cell-p equip (car visio)))
     (car (car visio)))
    (t
     (agent-xyz999-first-paint-target-rec equip coord (cdr visio)))))

(defun agent-xyz999-first-move-target (equip coord visio)
  (agent-xyz999-first-move-target-rec equip coord visio))

(defun agent-xyz999-first-move-target-rec (equip coord visio)
  (cond
    ((null visio) nil)
    ((and (<= (agent-xyz999-dist2 coord (car (car visio))) 5)
          (> (agent-xyz999-dist2 coord (car (car visio))) 0)
          (agent-xyz999-free-cell-p equip (car visio)))
     (car (car visio)))
    (t
     (agent-xyz999-first-move-target-rec equip coord (cdr visio)))))

(defun agent-xyz999-cell-element (celda-visio)
  (nth 3 celda-visio))

(defun agent-xyz999-cell-equip (celda-visio)
  (nth 4 celda-visio))

(defun agent-xyz999-free-cell-p (equip celda-visio)
  (and (eq (nth 1 celda-visio) 'terra)
       (null (agent-xyz999-cell-element celda-visio))))

(defun agent-xyz999-pintable-cell-p (equip celda-visio)
  (and (eq (nth 1 celda-visio) 'terra)
       (or (null (agent-xyz999-cell-element celda-visio))
           (not (eq (agent-xyz999-cell-equip celda-visio) equip)))))

(defun agent-xyz999-dist2 (a b)
  (let* ((dx (- (car a) (car b)))
         (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))


;; Recordau que qualsevol funció no predefinida necessària per a l'agent, ha d'estar definida en aquest fitxer
;; i ha d'estar prefixada amb "agent-xyz999-" per evitar conflictes amb altres agents o mòduls.
;; Per exemple:
(defun agent-xyz999-longitud (llista)
    "Retorna la longitud d'una llista."
    (cond ((null llista) 0)
          (t (+ 1 (agent-xyz999-longitud (cdr llista))))))