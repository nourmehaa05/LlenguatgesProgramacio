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
;; Professors:
;;   - Cabot Nadal, Miquel Àngel
;;   - Oliver Tomàs, Antoni
;; Lliurament: primera convocatòria.
;; ============================================================
;; Estratègia E1 - atac generalitzat amb prioritats diferenciades per rol
;;
;; ROLS per (mod id 5):
;;   0,1,2 → ATACANT  : prioritza base enemiga > bolles > labs
;;                      es mou sempre cap a la base enemiga
;;   3     → SEMI-DEF : prioritza bolles > base > labs
;;                      es mou cap a la base enemiga si la veu,
;;                      si no patrulla al voltant de la base pròpia
;;   4     → LAB      : prioritza labs > base > bolles
;;                      es mou cap al lab més proper, la base enemiga
;;                      si no hi ha labs, o una zona fixa si no sap res
;;
;; La base crea fins a dues bolles per torn (si té ≥100 pintura).
;; Memòria compartida: base-enemiga, base-propia.
;; ============================================================
;; Descripció de les funcions d'aquest fitxer:
;;   - Accessors de les dades de la unitat
;;   - Funcions auxiliars: distància, predicats, longitud, índex
;;   - Gestió de caselles adjacents lliures i detecció d'encaixonament
;;   - Navegació cap a un objectiu
;;   - Cerca d'objectius per pintar dins el rang
;;   - Cerca d'objectius a la visió
;;   - Actualització de la memòria compartida
;;   - Assignació de rol per id i càlcul de zones
;;   - Lògica de torn per a cada rol (atacant, semi-defensor, lab)
;;   - Funció principal de l'agent
;; ============================================================


;; ============================================================
;; ACCESSORS DE LES DADES DE LA UNITAT
;; Format: (ronda equip pintura id tipus coord colors-pintat
;;          color-propi tr-pintar tr-moure visio memoria)
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


;; ============================================================
;; FUNCIONS AUXILIARS
;; ============================================================

(defun agent-cms213-dist2 (a b)
  "Retorna la distància euclidiana al quadrat entre a i b.
   a: primera coordenada (x y)
   b: segona coordenada (x y)"
  (let ((dx (- (car a) (car b))) (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

(defun agent-cms213-lliure-p (c)
  "Retorna T si la casella de la visió és de terra i no té cap element.
   c: entrada de la llista de visió"
  (and (eq (nth 1 c) 'terra) (null (nth 3 c))))

(defun agent-cms213-terra-p (c)
  "Retorna T si la casella de la visió és de terra.
   c: entrada de la llista de visió"
  (eq (nth 1 c) 'terra))

(defun agent-cms213-llargada (l)
  "Retorna el nombre d'elements d'una llista.
   l: llista a comptar"
  (cond ((null l) 0) (t (+ 1 (agent-cms213-llargada (cdr l))))))

(defun agent-cms213-nth (n l)
  "Retorna l'element a la posició n de la llista l, o nil si n és fora de rang.
   n: índex de l'element a obtenir
   l: llista on cercar"
  (cond ((null l) nil) ((= n 0) (car l))
        (t (agent-cms213-nth (- n 1) (cdr l)))))


;; ============================================================
;; CASELLES ADJACENTS LLIURES I ENCAIXONAMENT
;; ============================================================

(defun agent-cms213-adj-lliures (coord visio)
  "Retorna la llista de coordenades adjacents lliures a coord dins la visió.
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (agent-cms213-adj-rec coord visio nil))

(defun agent-cms213-adj-rec (coord visio acc)
  "Acumula recursivament les coordenades adjacents lliures a coord.
   coord: coordenada (x y) de la unitat
   visio: llista de caselles restants a recórrer
   acc:   acumulador de coordenades lliures trobades"
  (cond
    ((null visio) acc)
    ((and (agent-cms213-lliure-p (car visio))
          (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
            (and (> d 0) (<= d 2))))
     (agent-cms213-adj-rec coord (cdr visio) (cons (nth 0 (car visio)) acc)))
    (t (agent-cms213-adj-rec coord (cdr visio) acc))))

(defun agent-cms213-encaixonada-p (coord visio)
  "Retorna T si la unitat té 2 o menys caselles adjacents lliures (encaixonada).
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (<= (agent-cms213-llargada (agent-cms213-adj-lliures coord visio)) 2))

(defun agent-cms213-aleatoria (coord visio ronda id)
  "Retorna una casella adjacent lliure de forma pseudo-aleatòria basada en ronda i id.
   Usada per escapar quan la unitat està encaixonada.
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles
   ronda: número de ronda actual
   id:    identificador de la unitat"
  (let* ((cands (agent-cms213-adj-lliures coord visio))
         (n     (agent-cms213-llargada cands)))
    (cond ((= n 0) nil)
          (t (agent-cms213-nth
              (mod (abs (+ (* ronda 31) (* id 17)
                           (* (car coord) 13) (* (cadr coord) 7)))
                   n)
              cands)))))


;; ============================================================
;; CASELLA PER CREAR BOLLES
;; ============================================================

(defun agent-cms213-primera-lliure (coord visio)
  "Retorna la casella adjacent lliure més allunyada de coord per crear una bolla.
   coord: coordenada (x y) de la base
   visio: llista de caselles visibles"
  (agent-cms213-primera-rec coord visio nil -1))

(defun agent-cms213-primera-rec (coord visio millor md)
  "Recorre la visió cercant la casella adjacent lliure de màxima distància a coord.
   coord:  coordenada (x y) de la base
   visio:  llista de caselles restants a recórrer
   millor: millor coordenada trobada fins ara
   md:     distància màxima trobada fins ara"
  (cond
    ((null visio) millor)
    ((and (agent-cms213-lliure-p (car visio))
          (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
            (and (> d 0) (<= d 2) (> d md))))
     (agent-cms213-primera-rec coord (cdr visio) (nth 0 (car visio))
                                (agent-cms213-dist2 coord (nth 0 (car visio)))))
    (t (agent-cms213-primera-rec coord (cdr visio) millor md))))

(defun agent-cms213-primera-excl (coord visio excl)
  "Retorna la casella adjacent lliure més allunyada de coord, excloent 'excl'.
   Usada per crear una segona bolla en una casella diferent a la primera.
   coord: coordenada (x y) de la base
   visio: llista de caselles visibles
   excl:  coordenada a excloure (ja usada per la primera bolla)"
  (agent-cms213-primera-excl-rec coord visio excl nil -1))

(defun agent-cms213-primera-excl-rec (coord visio excl millor md)
  "Recorre la visió cercant la casella adjacent lliure de màxima distància, excloent excl.
   coord:  coordenada (x y) de la base
   visio:  llista de caselles restants a recórrer
   excl:   coordenada a excloure
   millor: millor coordenada trobada fins ara
   md:     distància màxima trobada fins ara"
  (cond
    ((null visio) millor)
    ((and (agent-cms213-lliure-p (car visio))
          (not (equal (nth 0 (car visio)) excl))
          (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
            (and (> d 0) (<= d 2) (> d md))))
     (agent-cms213-primera-excl-rec coord (cdr visio) excl (nth 0 (car visio))
                                     (agent-cms213-dist2 coord (nth 0 (car visio)))))
    (t (agent-cms213-primera-excl-rec coord (cdr visio) excl millor md))))


;; ============================================================
;; NAVEGACIÓ CAP A OBJECTIU
;; ============================================================

(defun agent-cms213-cap-a (coord obj visio)
  "Retorna la casella adjacent lliure que minimitza la distància a obj.
   coord: coordenada (x y) de la unitat
   obj:   coordenada (x y) de l'objectiu
   visio: llista de caselles visibles"
  (agent-cms213-cap-a-rec coord obj visio nil 999999))

(defun agent-cms213-cap-a-rec (coord obj visio millor md)
  "Recorre la visió cercant la casella adjacent lliure més propera a obj.
   coord:  coordenada (x y) de la unitat
   obj:    coordenada (x y) de l'objectiu
   visio:  llista de caselles restants a recórrer
   millor: millor coordenada trobada fins ara
   md:     distància mínima a obj trobada fins ara"
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
  "Retorna la casella destí del moviment: cap a obj si és possible, aleatòria si no.
   coord: coordenada (x y) de la unitat
   obj:   coordenada (x y) de l'objectiu
   visio: llista de caselles visibles
   ronda: número de ronda actual
   id:    identificador de la unitat"
  (let ((r (agent-cms213-cap-a coord obj visio)))
    (cond (r r) (t (agent-cms213-aleatoria coord visio ronda id)))))


;; ============================================================
;; CERCA D'OBJECTIUS PER PINTAR (RANG d²≤5)
;; ============================================================

(defun agent-cms213-base-rang (equip coord visio)
  "Retorna la coordenada de la base enemiga dins rang de pintura (d²≤5), o nil.
   equip: equip de la bolla que pinta
   coord: coordenada (x y) de la bolla
   visio: llista de caselles visibles"
  (cond ((null visio) nil)
        ((and (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
                (and (> d 0) (<= d 5)))
              (eq (nth 3 (car visio)) 'base)
              (not (eq (nth 4 (car visio)) equip)))
         (nth 0 (car visio)))
        (t (agent-cms213-base-rang equip coord (cdr visio)))))

(defun agent-cms213-lab-rang (equip coord visio)
  "Retorna la coordenada del lab enemic o no capturat dins rang de pintura (d²≤5), o nil.
   equip: equip de la bolla que pinta
   coord: coordenada (x y) de la bolla
   visio: llista de caselles visibles"
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
  "Retorna la coordenada de la bolla enemiga dins rang de pintura (d²≤5), o nil.
   equip: equip de la bolla que pinta
   coord: coordenada (x y) de la bolla
   visio: llista de caselles visibles"
  (cond ((null visio) nil)
        ((and (agent-cms213-terra-p (car visio))
              (let ((d (agent-cms213-dist2 coord (nth 0 (car visio)))))
                (and (> d 0) (<= d 5)))
              (eq (nth 3 (car visio)) 'bolla)
              (not (eq (nth 4 (car visio)) equip)))
         (nth 0 (car visio)))
        (t (agent-cms213-bolla-rang equip coord (cdr visio)))))


;; ============================================================
;; CERCA D'OBJECTIUS A LA VISIÓ
;; ============================================================

(defun agent-cms213-base-enemiga-vis (equip coord visio)
  "Retorna la coordenada de la base enemiga més propera dins la visió, o nil.
   equip: equip de la unitat que cerca
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (agent-cms213-benv-rec equip coord visio nil 999999))

(defun agent-cms213-benv-rec (equip coord visio millor md)
  "Recorre la visió cercant la base enemiga de mínima distància.
   equip:  equip de la unitat que cerca
   coord:  coordenada (x y) de la unitat
   visio:  llista de caselles restants a recórrer
   millor: millor coordenada trobada fins ara
   md:     distància mínima trobada fins ara"
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
  "Retorna la coordenada del lab enemic o no capturat més proper dins la visió, o nil.
   equip: equip de la unitat que cerca
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (agent-cms213-lenv-rec equip coord visio nil 999999))

(defun agent-cms213-lenv-rec (equip coord visio millor md)
  "Recorre la visió cercant el lab enemic o no capturat de mínima distància.
   equip:  equip de la unitat que cerca
   coord:  coordenada (x y) de la unitat
   visio:  llista de caselles restants a recórrer
   millor: millor coordenada trobada fins ara
   md:     distància mínima trobada fins ara"
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
  "Retorna la coordenada de la base pròpia si és visible, o nil.
   equip: equip de la unitat que cerca
   visio: llista de caselles visibles"
  (cond ((null visio) nil)
        ((and (agent-cms213-terra-p (car visio))
              (eq (nth 3 (car visio)) 'base)
              (eq (nth 4 (car visio)) equip))
         (nth 0 (car visio)))
        (t (agent-cms213-base-propia-vis equip (cdr visio)))))


;; ============================================================
;; ACTUALITZACIÓ DE LA MEMÒRIA COMPARTIDA
;; ============================================================

(defun agent-cms213-act-mem (memoria equip coord visio ronda)
  "Retorna la memòria actualitzada amb la base enemiga i la base pròpia si són visibles.
   memoria: alist de memòria compartida de l'equip
   equip:   equip de la unitat
   coord:   coordenada (x y) de la unitat (per trobar la base enemiga més propera)
   visio:   llista de caselles visibles
   ronda:   número de ronda actual"
  (let* ((base-e (agent-cms213-base-enemiga-vis equip coord visio))
         (base-p (agent-cms213-base-propia-vis equip visio))
         (m1 (cond (base-e
                    (let* ((m (mem-escriure memoria 'base-enemiga base-e)))
                      (mem-escriure m 'darrera-vista ronda)))
                   (t memoria)))
         (m2 (cond (base-p (mem-escriure m1 'base-propia base-p))
                   (t m1))))
    m2))


;; ============================================================
;; ROL PER ID I ZONES
;; ============================================================

(defun agent-cms213-rol (id)
  "Retorna el rol de la bolla segons (mod id 5): 'atacant, 'semi-def o 'lab.
   id: identificador únic de la unitat"
  (let ((m (mod id 5)))
    (cond ((or (= m 0) (= m 1) (= m 2)) 'atacant)
          ((= m 3) 'semi-def)
          (t 'lab))))

(defun agent-cms213-zona-defensa (id base ronda)
  "Retorna la coordenada de patrulla del semi-defensor, rotant al voltant de la base pròpia.
   Si no es coneix la base pròpia, va a (5 5) com a fallback.
   id:    identificador de la unitat
   base:  coordenada (x y) de la base pròpia (o nil si no es coneix)
   ronda: número de ronda actual"
  (cond
    (base
     (let* ((s   (mod (+ id (mod (truncate (/ ronda 20)) 4)) 4))
            (bx  (car base)) (by (cadr base)))
       (cond ((= s 0) (list (+ bx 7) by))
             ((= s 1) (list bx (+ by 7)))
             ((= s 2) (list (- bx 7) by))
             (t       (list bx (- by 7))))))
    (t '(5 5))))

(defun agent-cms213-zona-lab (id ronda)
  "Retorna la coordenada d'exploració de labs, rotant entre 4 zones del mapa.
   id:    identificador de la unitat
   ronda: número de ronda actual"
  (let* ((f (mod (truncate (/ ronda 40)) 4))
         (z (mod (+ (mod id 4) f) 4)))
    (cond ((= z 0) '(12 12)) ((= z 1) '(48 12))
          ((= z 2) '(12 48)) (t       '(48 48)))))


;; ============================================================
;; LÒGICA DE TORN PER ROL
;; ============================================================

(defun agent-cms213-torn-atacant (equip coord visio ronda memoria id tr-p tr-m)
  "Retorna les accions de la bolla atacant.
   Prioritat de pintura: base enemiga > bolles enemigues > labs.
   Moviment: cap a la base enemiga (via memòria si cal); aleatori si encaixonada.
   equip:   equip de la bolla
   coord:   coordenada (x y) de la bolla
   visio:   llista de caselles visibles
   ronda:   número de ronda actual
   memoria: alist de memòria compartida
   id:      identificador de la unitat
   tr-p:    temps de recuperació de pintar
   tr-m:    temps de recuperació de moure"
  (let* ((base-vis (agent-cms213-base-enemiga-vis equip coord visio))
         (base-mem (mem-llegir memoria 'base-enemiga))
         (obj      (cond (base-vis base-vis) (base-mem base-mem) (t '(30 30))))
         (encaix   (agent-cms213-encaixonada-p coord visio))
         (pint     (cond ((agent-cms213-base-rang equip coord visio))
                         ((agent-cms213-bolla-rang equip coord visio))
                         ((agent-cms213-lab-rang equip coord visio))
                         (t nil)))
         (mou      (cond (encaix (agent-cms213-aleatoria coord visio ronda id))
                         (t (agent-cms213-moure coord obj visio ronda id))))
         (ap       (cond ((and pint (< tr-p 1)) (list (list 'pinta (list pint)))) (t nil)))
         (am       (cond ((and mou  (< tr-m 1)) (list (list 'mou   (list mou))))  (t nil))))
    (append ap am)))

(defun agent-cms213-torn-defensor (equip coord visio ronda memoria id tr-p tr-m)
  "Retorna les accions de la bolla semi-defensora.
   Prioritat de pintura: bolles enemigues > base enemiga > labs.
   Moviment: cap a la base enemiga si la veu; si no, patrulla al voltant
   de la base pròpia. Si no sap on és la base pròpia, va a (5 5).
   equip:   equip de la bolla
   coord:   coordenada (x y) de la bolla
   visio:   llista de caselles visibles
   ronda:   número de ronda actual
   memoria: alist de memòria compartida
   id:      identificador de la unitat
   tr-p:    temps de recuperació de pintar
   tr-m:    temps de recuperació de moure"
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

(defun agent-cms213-torn-lab (equip coord visio ronda memoria id tr-p tr-m)
  "Retorna les accions de la bolla de labs.
   Prioritat de pintura: labs > base enemiga > bolles enemigues.
   Moviment: lab enemic visible > base enemiga visible > base de memòria > zona fixa.
   equip:   equip de la bolla
   coord:   coordenada (x y) de la bolla
   visio:   llista de caselles visibles
   ronda:   número de ronda actual
   memoria: alist de memòria compartida
   id:      identificador de la unitat
   tr-p:    temps de recuperació de pintar
   tr-m:    temps de recuperació de moure"
  (let* ((lab-vis  (agent-cms213-lab-enemic-vis equip coord visio))
         (base-vis (agent-cms213-base-enemiga-vis equip coord visio))
         (base-mem (mem-llegir memoria 'base-enemiga))
         (zona     (agent-cms213-zona-lab id ronda))
         (desti    (cond (lab-vis lab-vis) (base-vis base-vis)
                         (base-mem base-mem) (t zona)))
         (encaix   (agent-cms213-encaixonada-p coord visio))
         (pint     (cond ((agent-cms213-lab-rang equip coord visio))
                         ((agent-cms213-base-rang equip coord visio))
                         ((agent-cms213-bolla-rang equip coord visio))
                         (t nil)))
         (mou      (cond (encaix (agent-cms213-aleatoria coord visio ronda id))
                         (t (agent-cms213-moure coord desti visio ronda id))))
         (ap       (cond ((and pint (< tr-p 1)) (list (list 'pinta (list pint)))) (t nil)))
         (am       (cond ((and mou  (< tr-m 1)) (list (list 'mou   (list mou))))  (t nil))))
    (append ap am)))


;; ============================================================
;; FUNCIÓ PRINCIPAL DE L'AGENT
;; ============================================================

(defun agent-cms213 (dades)
  "Agent per E1. Assigna rols per id i executa la lògica corresponent.
   La base crea fins a dues bolles per torn si té prou pintura (≥100 per dues, ≥50 per una).
   dades: llista d'informació de la unitat proporcionada pel controlador"
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
      ;; BASE: crea fins a dues bolles per torn i actualitza memòria
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
      ;; BOLLA: executa la lògica del rol assignat per id
      ((eq tipus 'bolla)
       (let* ((mem-n  (agent-cms213-act-mem memoria equip coord visio ronda))
              (rol    (agent-cms213-rol id))
              (acrol  (cond
                        ((eq rol 'atacant)
                         (agent-cms213-torn-atacant equip coord visio ronda mem-n id tr-p tr-m))
                        ((eq rol 'semi-def)
                         (agent-cms213-torn-defensor equip coord visio ronda mem-n id tr-p tr-m))
                        (t
                         (agent-cms213-torn-lab equip coord visio ronda mem-n id tr-p tr-m))))
              (acc-m  (list (list 'escriu-memoria (list mem-n)))))
         (append acrol acc-m)))
      (t nil))))