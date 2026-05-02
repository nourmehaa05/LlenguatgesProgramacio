;; ============================================================
;; Pràctica final de Llenguatges de Programació - LISP
;; Paintball - Agent E1
;; ============================================================
;; Estudiants:
;;   - Marín Sánchez, Carolina
;;   - Mehannek Samah, Nour Iman
;; Data: 03/05/2026
;; Assignatura: Llenguatges de Programació
;; Grup: 101
;; ============================================================
;; Estratègia E1 - atac + defensa lleugera
;;
;; ROLS per (mod id 5):
;;   0,1,2 → ATACANT  : va directe a la base enemiga amb 3 colors
;;   3     → DEFENSOR : patrulla prop de la base pròpia
;;   4     → LAB      : captura laboratoris
;;
;; Clau: els atacants PRIORITZEN pintar la base enemiga amb
;; els 3 colors necessaris per destruir-la. La base crea bolles
;; ràpidament per tenir massa crítica.
;; Memòria: base-enemiga, base-propia.
;; ============================================================

(defun agent-cms213-ronda     (d) (nth 0 d))
(defun agent-cms213-equip     (d) (nth 1 d))
(defun agent-cms213-pintura   (d) (nth 2 d))
(defun agent-cms213-id        (d) (nth 3 d))
(defun agent-cms213-tipus     (d) (nth 4 d))
(defun agent-cms213-coord     (d) (nth 5 d))
(defun agent-cms213-tr-pintar (d) (nth 8 d))
(defun agent-cms213-tr-moure  (d) (nth 9 d))
(defun agent-cms213-visio     (d) (nth 10 d))
(defun agent-cms213-memoria   (d) (nth 11 d))

(defun agent-cms213-dist2 (a b)
  (let ((dx (- (car a) (car b))) (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

(defun agent-cms213-lliure-p (c)
  (and (eq (nth 1 c) 'terra) (null (nth 3 c))))

(defun agent-cms213-terra-p (c) (eq (nth 1 c) 'terra))

(defun agent-cms213-llargada (l)
  (cond ((null l) 0) (t (+ 1 (agent-cms213-llargada (cdr l))))))

(defun agent-cms213-nth (n l)
  (cond ((null l) nil) ((= n 0) (car l))
        (t (agent-cms213-nth (- n 1) (cdr l)))))

;; ── Caselles adjacents lliures ─────────────────────────────

(defun agent-cms213-adj-lliures (coord visio)
  (agent-cms213-adj-rec coord visio nil))

(defun agent-cms213-adj-rec (coord visio acc)
  (cond
    ((null visio) acc)
    ((and (agent-cms213-lliure-p (car visio))
          (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
            (and (> d 0) (<= d 2))))
     (agent-cms213-adj-rec coord (cdr visio) (cons (nth 0 (car visio)) acc)))
    (t (agent-cms213-adj-rec coord (cdr visio) acc))))

(defun agent-cms213-encaixonada-p (coord visio)
  (<= (agent-cms213-llargada (agent-cms213-adj-lliures coord visio)) 2))

(defun agent-cms213-aleatoria (coord visio ronda id)
  (let* ((cands (agent-cms213-adj-lliures coord visio))
         (n     (agent-cms213-llargada cands)))
    (cond ((= n 0) nil)
          (t (agent-cms213-nth
              (mod (abs (+ (* ronda 31) (* id 17)
                           (* (car coord) 13) (* (cadr coord) 7)))
                   n)
              cands)))))

;; ── Casella per crear bolles ────────────────────────────────

(defun agent-cms213-primera-lliure (coord visio)
  (agent-cms213-primera-rec coord visio nil -1))

(defun agent-cms213-primera-rec (coord visio millor md)
  (cond
    ((null visio) millor)
    ((and (agent-cms213-lliure-p (car visio))
          (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
            (and (> d 0) (<= d 2) (> d md))))
     (agent-cms213-primera-rec coord (cdr visio) (nth 0 (car visio))
                                (agent-cms213-dist2 coord (nth 0 (car visio)))))
    (t (agent-cms213-primera-rec coord (cdr visio) millor md))))

(defun agent-cms213-primera-excl (coord visio excl)
  (agent-cms213-primera-excl-rec coord visio excl nil -1))

(defun agent-cms213-primera-excl-rec (coord visio excl millor md)
  (cond
    ((null visio) millor)
    ((and (agent-cms213-lliure-p (car visio))
          (not (equal (nth 0 (car visio)) excl))
          (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
            (and (> d 0) (<= d 2) (> d md))))
     (agent-cms213-primera-excl-rec coord (cdr visio) excl (nth 0 (car visio))
                                     (agent-cms213-dist2 coord (nth 0 (car visio)))))
    (t (agent-cms213-primera-excl-rec coord (cdr visio) excl millor md))))

;; ── Navegació ───────────────────────────────────────────────

(defun agent-cms213-cap-a (coord obj visio)
  (agent-cms213-cap-a-rec coord obj visio nil 999999))

(defun agent-cms213-cap-a-rec (coord obj visio millor md)
  (cond
    ((null visio) millor)
    ((and (agent-cms213-lliure-p (car visio))
          (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
            (and (> d 0) (<= d 2))))
     (let ((d (agent-cms213-dist2 (nth 0 (car visio)) obj)))
       (cond ((< d md)
              (agent-cms213-cap-a-rec coord obj (cdr visio) (nth 0 (car visio)) d))
             (t (agent-cms213-cap-a-rec coord obj (cdr visio) millor md)))))
    (t (agent-cms213-cap-a-rec coord obj (cdr visio) millor md))))

(defun agent-cms213-moure (coord obj visio ronda id)
  (let ((r (agent-cms213-cap-a coord obj visio)))
    (cond (r r) (t (agent-cms213-aleatoria coord visio ronda id)))))

;; ── Pintura en rang ─────────────────────────────────────────

(defun agent-cms213-base-rang (equip coord visio)
  (cond ((null visio) nil)
        ((and (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
                (and (> d 0) (<= d 5)))
              (eq (nth 3 (car visio)) 'base)
              (not (eq (nth 4 (car visio)) equip)))
         (nth 0 (car visio)))
        (t (agent-cms213-base-rang equip coord (cdr visio)))))

(defun agent-cms213-lab-rang (equip coord visio)
  (cond ((null visio) nil)
        ((and (agent-cms213-terra-p (car visio))
              (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
                (and (> d 0) (<= d 5)))
              (eq (nth 3 (car visio)) 'lab)
              (or (null (nth 4 (car visio)))
                  (not (eq (nth 4 (car visio)) equip))))
         (nth 0 (car visio)))
        (t (agent-cms213-lab-rang equip coord (cdr visio)))))

(defun agent-cms213-bolla-rang (equip coord visio)
  (cond ((null visio) nil)
        ((and (agent-cms213-terra-p (car visio))
              (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
                (and (> d 0) (<= d 5)))
              (eq (nth 3 (car visio)) 'bolla)
              (not (eq (nth 4 (car visio)) equip)))
         (nth 0 (car visio)))
        (t (agent-cms213-bolla-rang equip coord (cdr visio)))))

;; ── Cerca a la visió ────────────────────────────────────────

(defun agent-cms213-base-enemiga-vis (equip coord visio)
  (agent-cms213-benv-rec equip coord visio nil 999999))

(defun agent-cms213-benv-rec (equip coord visio millor md)
  (cond ((null visio) millor)
        ((and (agent-cms213-terra-p (car visio))
              (eq (nth 3 (car visio)) 'base)
              (not (eq (nth 4 (car visio)) equip)))
         (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
           (cond ((< d md)
                  (agent-cms213-benv-rec equip coord (cdr visio) (nth 0 (car visio)) d))
                 (t (agent-cms213-benv-rec equip coord (cdr visio) millor md)))))
        (t (agent-cms213-benv-rec equip coord (cdr visio) millor md))))

(defun agent-cms213-lab-enemic-vis (equip coord visio)
  (agent-cms213-lenv-rec equip coord visio nil 999999))

(defun agent-cms213-lenv-rec (equip coord visio millor md)
  (cond ((null visio) millor)
        ((and (agent-cms213-terra-p (car visio))
              (eq (nth 3 (car visio)) 'lab)
              (or (null (nth 4 (car visio)))
                  (not (eq (nth 4 (car visio)) equip))))
         (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
           (cond ((< d md)
                  (agent-cms213-lenv-rec equip coord (cdr visio) (nth 0 (car visio)) d))
                 (t (agent-cms213-lenv-rec equip coord (cdr visio) millor md)))))
        (t (agent-cms213-lenv-rec equip coord (cdr visio) millor md))))

(defun agent-cms213-base-propia-vis (equip visio)
  (cond ((null visio) nil)
        ((and (agent-cms213-terra-p (car visio))
              (eq (nth 3 (car visio)) 'base)
              (eq (nth 4 (car visio)) equip))
         (nth 0 (car visio)))
        (t (agent-cms213-base-propia-vis equip (cdr visio)))))

;; ── Actualitzar memòria ─────────────────────────────────────

(defun agent-cms213-act-mem (memoria equip coord visio ronda)
  "Actualitza la memòria: guarda base-enemiga (si visible) i base-propia.
   Usa coord per trobar la base enemiga més propera, no (0 0)."
  (let* ((base-e (agent-cms213-base-enemiga-vis equip coord visio))
         (base-p (agent-cms213-base-propia-vis equip visio))
         (m1 (cond (base-e
                    (let* ((m (mem-escriure memoria 'base-enemiga base-e)))
                      (mem-escriure m 'darrera-vista ronda)))
                   (t memoria)))
         (m2 (cond (base-p (mem-escriure m1 'base-propia base-p))
                   (t m1))))
    m2))

;; ── ROL per id ──────────────────────────────────────────────
;; mod 5: 0,1,2=atacant  3=defensor  4=lab

(defun agent-cms213-rol (id)
  (let ((m (mod id 5)))
    (cond ((or (= m 0) (= m 1) (= m 2)) 'atacant)
          ((= m 3) 'defensor)
          (t 'lab))))

;; ── Zona de patrulla defensors ──────────────────────────────

(defun agent-cms213-zona-defensa (id base ronda)
  (cond
    (base
     (let* ((s   (mod (+ id (mod (truncate (/ ronda 20)) 4)) 4))
            (bx  (car base)) (by (cadr base)))
       (cond ((= s 0) (list (+ bx 7) by))
             ((= s 1) (list bx (+ by 7)))
             ((= s 2) (list (- bx 7) by))
             (t       (list bx (- by 7))))))
    (t '(5 5))))

;; ── Zona d'exploració labs ──────────────────────────────────

(defun agent-cms213-zona-lab (id ronda)
  (let* ((f (mod (truncate (/ ronda 40)) 4))
         (z (mod (+ (mod id 4) f) 4)))
    (cond ((= z 0) '(12 12)) ((= z 1) '(48 12))
          ((= z 2) '(12 48)) (t       '(48 48)))))

;; ── Lògica ATACANT ──────────────────────────────────────────

(defun agent-cms213-torn-atacant (equip coord visio ronda memoria id tr-p tr-m)
  (let* ((base-vis (agent-cms213-base-enemiga-vis equip coord visio))
         (base-mem (mem-llegir memoria 'base-enemiga))
         (obj      (cond (base-vis base-vis) (base-mem base-mem) (t '(30 30))))
         (encaix   (agent-cms213-encaixonada-p coord visio))
         ;; Prioritat: base > bolla > lab (per destruir la base amb 3 colors)
         (pint     (cond ((agent-cms213-base-rang equip coord visio))
                         ((agent-cms213-bolla-rang equip coord visio))
                         ((agent-cms213-lab-rang equip coord visio))
                         (t nil)))
         (mou      (cond (encaix (agent-cms213-aleatoria coord visio ronda id))
                         (t (agent-cms213-moure coord obj visio ronda id))))
         (ap       (cond ((and pint (< tr-p 1)) (list (list 'pinta (list pint)))) (t nil)))
         (am       (cond ((and mou  (< tr-m 1)) (list (list 'mou   (list mou))))  (t nil))))
    (append ap am)))

;; ── Lògica DEFENSOR ─────────────────────────────────────────

(defun agent-cms213-torn-defensor (equip coord visio ronda memoria id tr-p tr-m)
  (let* ((base-p  (mem-llegir memoria 'base-propia))
         (enemic  (agent-cms213-base-enemiga-vis equip coord visio))
         (zona    (agent-cms213-zona-defensa id base-p ronda))
         (encaix  (agent-cms213-encaixonada-p coord visio))
         (pint    (cond ((agent-cms213-bolla-rang equip coord visio))
                        ((agent-cms213-base-rang equip coord visio))
                        ((agent-cms213-lab-rang equip coord visio))
                        (t nil)))
         (desti   (cond (enemic enemic) (t zona)))
         (mou     (cond (encaix (agent-cms213-aleatoria coord visio ronda id))
                        (t (agent-cms213-moure coord desti visio ronda id))))
         (ap      (cond ((and pint (< tr-p 1)) (list (list 'pinta (list pint)))) (t nil)))
         (am      (cond ((and mou  (< tr-m 1)) (list (list 'mou   (list mou))))  (t nil))))
    (append ap am)))

;; ── Lògica LAB ──────────────────────────────────────────────

(defun agent-cms213-torn-lab (equip coord visio ronda memoria id tr-p tr-m)
  (let* ((lab-vis (agent-cms213-lab-enemic-vis equip coord visio))
         (base-vis (agent-cms213-base-enemiga-vis equip coord visio))
         (base-mem (mem-llegir memoria 'base-enemiga))
         (zona    (agent-cms213-zona-lab id ronda))
         (desti   (cond (lab-vis lab-vis) (base-vis base-vis)
                        (base-mem base-mem) (t zona)))
         (encaix  (agent-cms213-encaixonada-p coord visio))
         (pint    (cond ((agent-cms213-lab-rang equip coord visio))
                        ((agent-cms213-base-rang equip coord visio))
                        ((agent-cms213-bolla-rang equip coord visio))
                        (t nil)))
         (mou     (cond (encaix (agent-cms213-aleatoria coord visio ronda id))
                        (t (agent-cms213-moure coord desti visio ronda id))))
         (ap      (cond ((and pint (< tr-p 1)) (list (list 'pinta (list pint)))) (t nil)))
         (am      (cond ((and mou  (< tr-m 1)) (list (list 'mou   (list mou))))  (t nil))))
    (append ap am)))

;; ── Funció principal ────────────────────────────────────────

(defun agent-cms213 (dades)
  (let* ((ronda   (agent-cms213-ronda   dades))
         (equip   (agent-cms213-equip   dades))
         (pintura (agent-cms213-pintura dades))
         (id      (agent-cms213-id      dades))
         (tipus   (agent-cms213-tipus   dades))
         (coord   (agent-cms213-coord   dades))
         (tr-p    (agent-cms213-tr-pintar dades))
         (tr-m    (agent-cms213-tr-moure  dades))
         (visio   (agent-cms213-visio   dades))
         (memoria (agent-cms213-memoria dades)))
    (cond
      ((eq tipus 'base)
       (let* ((color  (nth (mod ronda 3) '(r g b)))
              (cell1  (agent-cms213-primera-lliure coord visio))
              (cell2  (cond (cell1 (agent-cms213-primera-excl coord visio cell1)) (t nil)))
              (mem-n  (agent-cms213-act-mem memoria equip coord visio ronda))
              (mem-n2 (mem-escriure mem-n 'base-propia coord))
              (acc-m  (list 'escriu-memoria (list mem-n2))))
         (cond
           ((and (>= pintura 100) cell1 cell2)
            (list (list 'crea-bolla (list color cell1))
                  (list 'crea-bolla (list (nth (mod (+ ronda 1) 3) '(r g b)) cell2))
                  acc-m))
           ((and (>= pintura 50) cell1)
            (list (list 'crea-bolla (list color cell1)) acc-m))
           (t (list acc-m)))))
      ((eq tipus 'bolla)
       (let* ((mem-n  (agent-cms213-act-mem memoria equip coord visio ronda))
              (rol    (agent-cms213-rol id))
              (acrol  (cond
                        ((eq rol 'atacant)
                         (agent-cms213-torn-atacant equip coord visio ronda mem-n id tr-p tr-m))
                        ((eq rol 'defensor)
                         (agent-cms213-torn-defensor equip coord visio ronda mem-n id tr-p tr-m))
                        (t
                         (agent-cms213-torn-lab equip coord visio ronda mem-n id tr-p tr-m))))
              (acc-m  (list (list 'escriu-memoria (list mem-n)))))
         (append acrol acc-m)))
      (t nil))))