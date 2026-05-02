;; ============================================================
;; Pràctica final de Llenguatges de Programació - LISP
;; Paintball - Agent E2
;; ============================================================
;; Estudiants:
;;   - Marín Sánchez, Carolina
;;   - Mehannek Samah, Nour Iman
;; Data: 03/05/2026
;; Assignatura: Llenguatges de Programació
;; Grup: 101
;; ============================================================
;; Estratègia E2 - atac + labs, sense defensa
;;
;; ROLS per (mod id 5):
;;   0,1,2 → ATACANT : va directe a la base enemiga
;;   3,4   → LAB     : captura laboratoris (i ataca de pas)
;;
;; Igual de agressiu que E1 però sense defensors:
;; compensa amb triangulació de la base enemiga i
;; bolles lab que també ataquen si veuen la base.
;; Memòria: base-enemiga, base-enemiga-2.
;; ============================================================

(defun agent-nms864-ronda     (d) (nth 0 d))
(defun agent-nms864-equip     (d) (nth 1 d))
(defun agent-nms864-pintura   (d) (nth 2 d))
(defun agent-nms864-id        (d) (nth 3 d))
(defun agent-nms864-tipus     (d) (nth 4 d))
(defun agent-nms864-coord     (d) (nth 5 d))
(defun agent-nms864-tr-pintar (d) (nth 8 d))
(defun agent-nms864-tr-moure  (d) (nth 9 d))
(defun agent-nms864-visio     (d) (nth 10 d))
(defun agent-nms864-memoria   (d) (nth 11 d))

(defun agent-nms864-cas-coord (c) (nth 0 c))
(defun agent-nms864-cas-tipus (c) (nth 1 c))
(defun agent-nms864-cas-elem  (c) (nth 3 c))
(defun agent-nms864-cas-equip (c) (nth 4 c))

(defun agent-nms864-dist2 (a b)
  (let ((dx (- (car a) (car b))) (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

(defun agent-nms864-terra-p (c) (eq (agent-nms864-cas-tipus c) 'terra))
(defun agent-nms864-lliure-p (c)
  (and (agent-nms864-terra-p c) (null (agent-nms864-cas-elem c))))
(defun agent-nms864-adj-p (coord c)
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord c))))
    (and (> d 0) (<= d 2))))
(defun agent-nms864-rang-p (coord c)
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord c))))
    (and (> d 0) (<= d 5))))

(defun agent-nms864-llargada (l)
  (cond ((null l) 0) (t (+ 1 (agent-nms864-llargada (cdr l))))))

(defun agent-nms864-nth (n l)
  (cond ((null l) nil) ((= n 0) (car l))
        (t (agent-nms864-nth (- n 1) (cdr l)))))

;; ── Caselles adjacents lliures ─────────────────────────────

(defun agent-nms864-adj-lliures (coord visio)
  (agent-nms864-adj-rec coord visio nil))

(defun agent-nms864-adj-rec (coord visio acc)
  (cond
    ((null visio) acc)
    ((and (agent-nms864-adj-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (agent-nms864-adj-rec coord (cdr visio)
                            (cons (agent-nms864-cas-coord (car visio)) acc)))
    (t (agent-nms864-adj-rec coord (cdr visio) acc))))

(defun agent-nms864-encaixonada-p (coord visio)
  (<= (agent-nms864-llargada (agent-nms864-adj-lliures coord visio)) 2))

(defun agent-nms864-aleatoria (coord visio ronda id)
  (let* ((cands (agent-nms864-adj-lliures coord visio))
         (n     (agent-nms864-llargada cands)))
    (cond ((= n 0) nil)
          (t (agent-nms864-nth
              (mod (abs (+ (* ronda 31) (* id 17)
                           (* (car coord) 13) (* (cadr coord) 7)))
                   n)
              cands)))))

;; ── Primera lliure per la base ──────────────────────────────

(defun agent-nms864-primera-lliure (coord visio)
  (cond ((null visio) nil)
        ((and (agent-nms864-adj-p coord (car visio))
              (agent-nms864-lliure-p (car visio)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-primera-lliure coord (cdr visio)))))

;; ── Navegació ───────────────────────────────────────────────

(defun agent-nms864-cap-a (coord obj visio)
  (agent-nms864-cap-a-rec coord obj visio nil 999999))

(defun agent-nms864-cap-a-rec (coord obj visio millor md)
  (cond
    ((null visio) millor)
    ((and (agent-nms864-adj-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (let ((d (agent-nms864-dist2 (agent-nms864-cas-coord (car visio)) obj)))
       (cond ((< d md)
              (agent-nms864-cap-a-rec coord obj (cdr visio)
                                      (agent-nms864-cas-coord (car visio)) d))
             (t (agent-nms864-cap-a-rec coord obj (cdr visio) millor md)))))
    (t (agent-nms864-cap-a-rec coord obj (cdr visio) millor md))))

(defun agent-nms864-moure (coord obj visio ronda id)
  (let ((r (agent-nms864-cap-a coord obj visio)))
    (cond (r r) (t (agent-nms864-aleatoria coord visio ronda id)))))

;; ── Pintura en rang ─────────────────────────────────────────

(defun agent-nms864-base-rang (equip coord visio)
  (cond ((null visio) nil)
        ((and (agent-nms864-rang-p coord (car visio))
              (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'base)
              (not (eq (agent-nms864-cas-equip (car visio)) equip)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-base-rang equip coord (cdr visio)))))

(defun agent-nms864-bolla-rang (equip coord visio)
  (cond ((null visio) nil)
        ((and (agent-nms864-rang-p coord (car visio))
              (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'bolla)
              (not (eq (agent-nms864-cas-equip (car visio)) equip)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-bolla-rang equip coord (cdr visio)))))

(defun agent-nms864-lab-rang (equip coord visio)
  (cond ((null visio) nil)
        ((and (agent-nms864-rang-p coord (car visio))
              (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'lab)
              (not (eq (agent-nms864-cas-equip (car visio)) equip)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-lab-rang equip coord (cdr visio)))))

;; ── Cerca a la visió ────────────────────────────────────────

(defun agent-nms864-base-enemiga-vis (equip coord visio)
  (agent-nms864-benv-rec equip coord visio nil 999999))

(defun agent-nms864-benv-rec (equip coord visio millor md)
  (cond ((null visio) millor)
        ((and (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'base)
              (not (eq (agent-nms864-cas-equip (car visio)) equip)))
         (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
           (cond ((< d md)
                  (agent-nms864-benv-rec equip coord (cdr visio)
                                         (agent-nms864-cas-coord (car visio)) d))
                 (t (agent-nms864-benv-rec equip coord (cdr visio) millor md)))))
        (t (agent-nms864-benv-rec equip coord (cdr visio) millor md))))

(defun agent-nms864-lab-enemic-vis (equip coord visio)
  (agent-nms864-lenv-rec equip coord visio nil 999999))

(defun agent-nms864-lenv-rec (equip coord visio millor md)
  (cond ((null visio) millor)
        ((and (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'lab)
              (or (null (agent-nms864-cas-equip (car visio)))
                  (not (eq (agent-nms864-cas-equip (car visio)) equip))))
         (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
           (cond ((< d md)
                  (agent-nms864-lenv-rec equip coord (cdr visio)
                                         (agent-nms864-cas-coord (car visio)) d))
                 (t (agent-nms864-lenv-rec equip coord (cdr visio) millor md)))))
        (t (agent-nms864-lenv-rec equip coord (cdr visio) millor md))))

;; ── Actualitzar memòria ─────────────────────────────────────

(defun agent-nms864-act-mem (memoria equip coord visio ronda)
  "Actualitza la memòria: guarda base-enemiga amb triangulació.
   Usa coord per trobar la base enemiga més propera, no (0 0)."
  (let ((base-e (agent-nms864-base-enemiga-vis equip coord visio)))
    (cond (base-e
           (let* ((ant (mem-llegir memoria 'base-enemiga))
                  (m1  (cond (ant (mem-escriure memoria 'base-enemiga-2 ant))
                             (t memoria)))
                  (m2  (mem-escriure m1 'base-enemiga base-e)))
             (mem-escriure m2 'darrera-vista ronda)))
          (t memoria))))

;; ── Triangulació ────────────────────────────────────────────

(defun agent-nms864-triangulat (memoria)
  (let ((p1 (mem-llegir memoria 'base-enemiga))
        (p2 (mem-llegir memoria 'base-enemiga-2)))
    (cond
      ((and p1 p2)
       (list (truncate (/ (+ (car p1) (car p2)) 2))
             (truncate (/ (+ (cadr p1) (cadr p2)) 2))))
      (p1 p1)
      (t nil))))

;; ── ROL per id ──────────────────────────────────────────────
;; mod 5: 0,1,2=atacant  3,4=lab

(defun agent-nms864-rol (id)
  (let ((m (mod id 5)))
    (cond ((or (= m 0) (= m 1) (= m 2)) 'atacant)
          (t 'lab))))

;; ── Zona d'exploració labs ──────────────────────────────────

(defun agent-nms864-zona-lab (id ronda)
  (let* ((f (mod (truncate (/ ronda 40)) 6))
         (z (mod (+ (mod id 6) f) 6)))
    (cond ((= z 0) '(10 10)) ((= z 1) '(50 10))
          ((= z 2) '(10 50)) ((= z 3) '(50 50))
          ((= z 4) '(30 10)) (t       '(10 30)))))

;; ── Lògica ATACANT ──────────────────────────────────────────

(defun agent-nms864-torn-atacant (equip coord visio ronda memoria id tr-p tr-m)
  (let* ((base-vis (agent-nms864-base-enemiga-vis equip coord visio))
         (base-mem (agent-nms864-triangulat memoria))
         (obj      (cond (base-vis base-vis) (base-mem base-mem) (t '(30 30))))
         (encaix   (agent-nms864-encaixonada-p coord visio))
         (pint     (cond ((agent-nms864-base-rang equip coord visio))
                         ((agent-nms864-bolla-rang equip coord visio))
                         ((agent-nms864-lab-rang equip coord visio))
                         (t nil)))
         (mou      (cond (encaix (agent-nms864-aleatoria coord visio ronda id))
                         (t (agent-nms864-moure coord obj visio ronda id))))
         (ap       (cond ((and pint (< tr-p 1)) (list (list 'pinta (list pint)))) (t nil)))
         (am       (cond ((and mou  (< tr-m 1)) (list (list 'mou   (list mou))))  (t nil))))
    (append ap am)))

;; ── Lògica LAB ──────────────────────────────────────────────

(defun agent-nms864-torn-lab (equip coord visio ronda memoria id tr-p tr-m)
  (let* ((lab-vis  (agent-nms864-lab-enemic-vis equip coord visio))
         (base-vis (agent-nms864-base-enemiga-vis equip coord visio))
         (base-mem (mem-llegir memoria 'base-enemiga))
         (zona     (agent-nms864-zona-lab id ronda))
         (desti    (cond (lab-vis lab-vis) (base-vis base-vis)
                         (base-mem base-mem) (t zona)))
         (encaix   (agent-nms864-encaixonada-p coord visio))
         (pint     (cond ((agent-nms864-lab-rang equip coord visio))
                         ((agent-nms864-base-rang equip coord visio))
                         ((agent-nms864-bolla-rang equip coord visio))
                         (t nil)))
         (mou      (cond (encaix (agent-nms864-aleatoria coord visio ronda id))
                         (t (agent-nms864-moure coord desti visio ronda id))))
         (ap       (cond ((and pint (< tr-p 1)) (list (list 'pinta (list pint)))) (t nil)))
         (am       (cond ((and mou  (< tr-m 1)) (list (list 'mou   (list mou))))  (t nil))))
    (append ap am)))

;; ── Color per ronda ─────────────────────────────────────────

(defun agent-nms864-color-per-ronda (ronda)
  (nth (mod ronda 3) '(r g b)))

;; ── Funció principal ────────────────────────────────────────

(defun agent-nms864 (dades)
  (let* ((ronda   (agent-nms864-ronda   dades))
         (equip   (agent-nms864-equip   dades))
         (pintura (agent-nms864-pintura dades))
         (id      (agent-nms864-id      dades))
         (tipus   (agent-nms864-tipus   dades))
         (coord   (agent-nms864-coord   dades))
         (tr-p    (agent-nms864-tr-pintar dades))
         (tr-m    (agent-nms864-tr-moure  dades))
         (visio   (agent-nms864-visio   dades))
         (memoria (agent-nms864-memoria dades)))
    (cond
      ((eq tipus 'base)
       (let* ((cell   (agent-nms864-primera-lliure coord visio))
              (mem-n  (agent-nms864-act-mem memoria equip coord visio ronda))
              (acc-m  (list 'escriu-memoria (list mem-n))))
         (cond
           ((and cell (>= pintura 50))
            (list (list 'crea-bolla (list (agent-nms864-color-per-ronda ronda) cell))
                  acc-m))
           (t (list acc-m)))))
      ((eq tipus 'bolla)
       (let* ((mem-n  (agent-nms864-act-mem memoria equip coord visio ronda))
              (rol    (agent-nms864-rol id))
              (acrol  (cond
                        ((eq rol 'atacant)
                         (agent-nms864-torn-atacant equip coord visio ronda mem-n id tr-p tr-m))
                        (t
                         (agent-nms864-torn-lab equip coord visio ronda mem-n id tr-p tr-m))))
              (acc-m  (list (list 'escriu-memoria (list mem-n)))))
         (append acrol acc-m)))
      (t nil))))