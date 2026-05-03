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
;; Professors:
;;   - Cabot Nadal, Miquel Àngel
;;   - Oliver Tomàs, Antoni
;; Lliurament: primera convocatòria.
;; ============================================================
;; Estratègia E2 - atac i captura de labs, sense defensa
;;
;; ROLS per (mod id 5):
;;   0,1,2 → ATACANT : prioritza base enemiga > bolles > labs
;;                     es mou sempre cap a la base enemiga
;;                     usa triangulació (dues posicions) per afinar l'objectiu
;;   3,4   → LAB     : prioritza labs > base enemiga > bolles
;;                     es mou cap al lab visible, la base enemiga si no hi ha labs,
;;                     o explora el centre del mapa si no sap res
;;
;; Totes les bolles ataquen quan veuen la base enemiga.
;; Les bolles lab no defensen: si no veuen labs ni la base, van al centre.
;; Memòria compartida: base-enemiga, base-enemiga-2 (per triangulació).
;; ============================================================
;; Descripció de les funcions d'aquest fitxer:
;;   - Accessors de les dades de la unitat i de caselles
;;   - Funcions auxiliars: distància, predicats, longitud, índex
;;   - Gestió de caselles adjacents lliures i detecció d'encaixonament
;;   - Navegació cap a un objectiu
;;   - Cerca d'objectius per pintar dins el rang
;;   - Cerca d'objectius a la visió
;;   - Actualització de la memòria amb triangulació
;;   - Assignació de rol per id i càlcul de zones
;;   - Lògica de torn per a cada rol (atacant, lab)
;;   - Funció principal de l'agent
;; ============================================================


;; ============================================================
;; ACCESSORS DE LES DADES DE LA UNITAT
;; Format: (ronda equip pintura id tipus coord colors-pintat
;;          color-propi tr-pintar tr-moure visio memoria)
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


;; ============================================================
;; ACCESSORS D'UNA CASELLA DE LA VISIÓ
;; Format casella: (coord tipus color elem equip colors-pintat
;;                  color-propi tr-pintar tr-moure)
;; ============================================================

(defun agent-nms864-cas-coord (c) (nth 0 c))
(defun agent-nms864-cas-tipus (c) (nth 1 c))
(defun agent-nms864-cas-elem  (c) (nth 3 c))
(defun agent-nms864-cas-equip (c) (nth 4 c))


;; ============================================================
;; FUNCIONS AUXILIARS
;; ============================================================

(defun agent-nms864-dist2 (a b)
  "Retorna la distància euclidiana al quadrat entre a i b.
   a: primera coordenada (x y)
   b: segona coordenada (x y)"
  (let ((dx (- (car a) (car b))) (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

(defun agent-nms864-terra-p (c)
  "Retorna T si la casella de la visió és de terra.
   c: entrada de la llista de visió"
  (eq (agent-nms864-cas-tipus c) 'terra))

(defun agent-nms864-lliure-p (c)
  "Retorna T si la casella de la visió és de terra i no té cap element.
   c: entrada de la llista de visió"
  (and (agent-nms864-terra-p c) (null (agent-nms864-cas-elem c))))

(defun agent-nms864-adj-p (coord c)
  "Retorna T si la casella és adjacent a coord (d²≤2, d²>0).
   coord: coordenada (x y) de la unitat
   c:     entrada de la llista de visió"
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord c))))
    (and (> d 0) (<= d 2))))

(defun agent-nms864-rang-p (coord c)
  "Retorna T si la casella és dins rang de pintura (d²≤5, d²>0).
   coord: coordenada (x y) de la unitat
   c:     entrada de la llista de visió"
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord c))))
    (and (> d 0) (<= d 5))))

(defun agent-nms864-llargada (l)
  "Retorna el nombre d'elements d'una llista.
   l: llista a comptar"
  (cond ((null l) 0) (t (+ 1 (agent-nms864-llargada (cdr l))))))

(defun agent-nms864-nth (n l)
  "Retorna l'element a la posició n de la llista l, o nil si n és fora de rang.
   n: índex de l'element a obtenir
   l: llista on cercar"
  (cond ((null l) nil) ((= n 0) (car l))
        (t (agent-nms864-nth (- n 1) (cdr l)))))


;; ============================================================
;; CASELLES ADJACENTS LLIURES I ENCAIXONAMENT
;; ============================================================

(defun agent-nms864-adj-lliures (coord visio)
  "Retorna la llista de coordenades adjacents lliures a coord dins la visió.
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (agent-nms864-adj-rec coord visio nil))

(defun agent-nms864-adj-rec (coord visio acc)
  "Acumula recursivament les coordenades adjacents lliures a coord.
   coord: coordenada (x y) de la unitat
   visio: llista de caselles restants a recórrer
   acc:   acumulador de coordenades lliures trobades"
  (cond
    ((null visio) acc)
    ((and (agent-nms864-adj-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (agent-nms864-adj-rec coord (cdr visio)
                            (cons (agent-nms864-cas-coord (car visio)) acc)))
    (t (agent-nms864-adj-rec coord (cdr visio) acc))))

(defun agent-nms864-encaixonada-p (coord visio)
  "Retorna T si la unitat té 2 o menys caselles adjacents lliures (encaixonada).
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (<= (agent-nms864-llargada (agent-nms864-adj-lliures coord visio)) 2))

(defun agent-nms864-aleatoria (coord visio ronda id)
  "Retorna una casella adjacent lliure de forma pseudo-aleatòria basada en ronda i id.
   Usada per escapar quan la unitat està encaixonada.
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles
   ronda: número de ronda actual
   id:    identificador de la unitat"
  (let* ((cands (agent-nms864-adj-lliures coord visio))
         (n     (agent-nms864-llargada cands)))
    (cond ((= n 0) nil)
          (t (agent-nms864-nth
              (mod (abs (+ (* ronda 31) (* id 17)
                           (* (car coord) 13) (* (cadr coord) 7)))
                   n)
              cands)))))


;; ============================================================
;; CASELLA PER CREAR BOLLES
;; ============================================================

(defun agent-nms864-primera-lliure (coord visio)
  "Retorna la primera casella adjacent lliure per crear una bolla, o nil.
   coord: coordenada (x y) de la base
   visio: llista de caselles visibles"
  (cond ((null visio) nil)
        ((and (agent-nms864-adj-p coord (car visio))
              (agent-nms864-lliure-p (car visio)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-primera-lliure coord (cdr visio)))))


;; ============================================================
;; NAVEGACIÓ CAP A OBJECTIU
;; ============================================================

(defun agent-nms864-cap-a (coord obj visio)
  "Retorna la casella adjacent lliure que minimitza la distància a obj.
   coord: coordenada (x y) de la unitat
   obj:   coordenada (x y) de l'objectiu
   visio: llista de caselles visibles"
  (agent-nms864-cap-a-rec coord obj visio nil 999999))

(defun agent-nms864-cap-a-rec (coord obj visio millor md)
  "Recorre la visió cercant la casella adjacent lliure més propera a obj.
   coord:  coordenada (x y) de la unitat
   obj:    coordenada (x y) de l'objectiu
   visio:  llista de caselles restants a recórrer
   millor: millor coordenada trobada fins ara
   md:     distància mínima a obj trobada fins ara"
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
  "Retorna la casella destí del moviment: cap a obj si és possible, aleatòria si no.
   coord: coordenada (x y) de la unitat
   obj:   coordenada (x y) de l'objectiu
   visio: llista de caselles visibles
   ronda: número de ronda actual
   id:    identificador de la unitat"
  (let ((r (agent-nms864-cap-a coord obj visio)))
    (cond (r r) (t (agent-nms864-aleatoria coord visio ronda id)))))


;; ============================================================
;; CERCA D'OBJECTIUS PER PINTAR (RANG d²≤5)
;; ============================================================

(defun agent-nms864-base-rang (equip coord visio)
  "Retorna la coordenada de la base enemiga dins rang de pintura (d²≤5), o nil.
   equip: equip de la bolla que pinta
   coord: coordenada (x y) de la bolla
   visio: llista de caselles visibles"
  (cond ((null visio) nil)
        ((and (agent-nms864-rang-p coord (car visio))
              (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'base)
              (not (eq (agent-nms864-cas-equip (car visio)) equip)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-base-rang equip coord (cdr visio)))))

(defun agent-nms864-bolla-rang (equip coord visio)
  "Retorna la coordenada de la bolla enemiga dins rang de pintura (d²≤5), o nil.
   equip: equip de la bolla que pinta
   coord: coordenada (x y) de la bolla
   visio: llista de caselles visibles"
  (cond ((null visio) nil)
        ((and (agent-nms864-rang-p coord (car visio))
              (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'bolla)
              (not (eq (agent-nms864-cas-equip (car visio)) equip)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-bolla-rang equip coord (cdr visio)))))

(defun agent-nms864-lab-rang (equip coord visio)
  "Retorna la coordenada del lab enemic o no capturat dins rang de pintura (d²≤5), o nil.
   equip: equip de la bolla que pinta
   coord: coordenada (x y) de la bolla
   visio: llista de caselles visibles"
  (cond ((null visio) nil)
        ((and (agent-nms864-rang-p coord (car visio))
              (agent-nms864-terra-p (car visio))
              (eq (agent-nms864-cas-elem (car visio)) 'lab)
              (not (eq (agent-nms864-cas-equip (car visio)) equip)))
         (agent-nms864-cas-coord (car visio)))
        (t (agent-nms864-lab-rang equip coord (cdr visio)))))


;; ============================================================
;; CERCA D'OBJECTIUS A LA VISIÓ
;; ============================================================

(defun agent-nms864-base-enemiga-vis (equip coord visio)
  "Retorna la coordenada de la base enemiga més propera dins la visió, o nil.
   equip: equip de la unitat que cerca
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (agent-nms864-benv-rec equip coord visio nil 999999))

(defun agent-nms864-benv-rec (equip coord visio millor md)
  "Recorre la visió cercant la base enemiga de mínima distància.
   equip:  equip de la unitat que cerca
   coord:  coordenada (x y) de la unitat
   visio:  llista de caselles restants a recórrer
   millor: millor coordenada trobada fins ara
   md:     distància mínima trobada fins ara"
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
  "Retorna la coordenada del lab enemic o no capturat més proper dins la visió, o nil.
   equip: equip de la unitat que cerca
   coord: coordenada (x y) de la unitat
   visio: llista de caselles visibles"
  (agent-nms864-lenv-rec equip coord visio nil 999999))

(defun agent-nms864-lenv-rec (equip coord visio millor md)
  "Recorre la visió cercant el lab enemic o no capturat de mínima distància.
   equip:  equip de la unitat que cerca
   coord:  coordenada (x y) de la unitat
   visio:  llista de caselles restants a recórrer
   millor: millor coordenada trobada fins ara
   md:     distància mínima trobada fins ara"
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


;; ============================================================
;; ACTUALITZACIÓ DE LA MEMÒRIA AMB TRIANGULACIÓ
;; ============================================================

(defun agent-nms864-act-mem (memoria equip coord visio ronda)
  "Retorna la memòria actualitzada. Guarda la base enemiga nova i conserva
   l'anterior com a 'base-enemiga-2' per permetre triangulació.
   memoria: alist de memòria compartida de l'equip
   equip:   equip de la unitat
   coord:   coordenada (x y) de la unitat (per trobar la base enemiga més propera)
   visio:   llista de caselles visibles
   ronda:   número de ronda actual"
  (let ((base-e (agent-nms864-base-enemiga-vis equip coord visio)))
    (cond (base-e
           (let* ((ant (mem-llegir memoria 'base-enemiga))
                  (m1  (cond (ant (mem-escriure memoria 'base-enemiga-2 ant))
                             (t memoria)))
                  (m2  (mem-escriure m1 'base-enemiga base-e)))
             (mem-escriure m2 'darrera-vista ronda)))
          (t memoria))))

(defun agent-nms864-triangulat (memoria)
  "Retorna el punt mig entre les dues últimes posicions vistes de la base enemiga.
   Si només se n'ha vist una, retorna aquella. Si no se n'ha vist cap, retorna nil.
   memoria: alist de memòria compartida de l'equip"
  (let ((p1 (mem-llegir memoria 'base-enemiga))
        (p2 (mem-llegir memoria 'base-enemiga-2)))
    (cond
      ((and p1 p2)
       (list (floor (/ (+ (car p1) (car p2)) 2))
             (floor (/ (+ (cadr p1) (cadr p2)) 2))))
      (p1 p1)
      (t nil))))


;; ============================================================
;; ROL PER ID I ZONES
;; ============================================================

(defun agent-nms864-rol (id)
  "Retorna el rol de la bolla segons (mod id 5): 'atacant o 'lab.
   id: identificador únic de la unitat"
  (let ((m (mod id 5)))
    (cond ((or (= m 0) (= m 1) (= m 2)) 'atacant)
          (t 'lab))))

(defun agent-nms864-zona-lab (id ronda memoria)
  "Retorna la coordenada d'exploració quan la bolla lab no té objectiu visible.
   Si hi ha base enemiga a la memòria, va cap allà.
   Si no, explora al voltant del centre del mapa amb variació per id i ronda
   per evitar que totes les bolles s'aglomerin al mateix punt.
   id:      identificador de la unitat
   ronda:   número de ronda actual
   memoria: alist de memòria compartida de l'equip"
  (let ((base-mem (mem-llegir memoria 'base-enemiga)))
    (cond
      (base-mem base-mem)
      (t
       (let* ((variacio (mod (+ (* id 7) (* ronda 3)) 20))
              (cx (+ 25 (- variacio 10)))
              (cy (+ 25 (- (mod (+ variacio 5) 20) 10))))
         (list cx cy))))))


;; ============================================================
;; LÒGICA DE TORN PER ROL
;; ============================================================

(defun agent-nms864-torn-atacant (equip coord visio ronda memoria id tr-p tr-m)
  "Retorna les accions de la bolla atacant.
   Prioritat de pintura: base enemiga > bolles enemigues > labs.
   Moviment: cap a la base enemiga usant triangulació; aleatori si encaixonada.
   Si no sap on és la base, va al centre del mapa (30 30).
   equip:   equip de la bolla
   coord:   coordenada (x y) de la bolla
   visio:   llista de caselles visibles
   ronda:   número de ronda actual
   memoria: alist de memòria compartida
   id:      identificador de la unitat
   tr-p:    temps de recuperació de pintar
   tr-m:    temps de recuperació de moure"
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

(defun agent-nms864-torn-lab (equip coord visio ronda memoria id tr-p tr-m)
  "Retorna les accions de la bolla de labs.
   Prioritat de pintura: labs > base enemiga > bolles enemigues.
   Moviment: lab visible > base enemiga visible > base de memòria > zona central.
   equip:   equip de la bolla
   coord:   coordenada (x y) de la bolla
   visio:   llista de caselles visibles
   ronda:   número de ronda actual
   memoria: alist de memòria compartida
   id:      identificador de la unitat
   tr-p:    temps de recuperació de pintar
   tr-m:    temps de recuperació de moure"
  (let* ((lab-vis  (agent-nms864-lab-enemic-vis equip coord visio))
         (base-vis (agent-nms864-base-enemiga-vis equip coord visio))
         (base-mem (mem-llegir memoria 'base-enemiga))
         (zona     (agent-nms864-zona-lab id ronda memoria))
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


;; ============================================================
;; COLOR PER RONDA
;; ============================================================

(defun agent-nms864-color-per-ronda (ronda)
  "Retorna el color de bolla corresponent a la ronda (rotació r, g, b).
   ronda: número de ronda actual"
  (nth (mod ronda 3) '(r g b)))


;; ============================================================
;; FUNCIÓ PRINCIPAL DE L'AGENT
;; ============================================================

(defun agent-nms864 (dades)
  "Agent per E2. Assigna rols per id i executa la lògica corresponent.
   La base crea una bolla per torn (≥50 pintura).
   Les bolles atacants usen triangulació per trobar la base enemiga.
   Les bolles lab capturen labs i ataquen la base si la veuen de camí.
   dades: llista d'informació de la unitat proporcionada pel controlador"
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
      ;; BASE: crea una bolla per torn i actualitza memòria
      ((eq tipus 'base)
       (let* ((cell   (agent-nms864-primera-lliure coord visio))
              (mem-n  (agent-nms864-act-mem memoria equip coord visio ronda))
              (acc-m  (list 'escriu-memoria (list mem-n))))
         (cond
           ((and cell (>= pintura 50))
            (list (list 'crea-bolla (list (agent-nms864-color-per-ronda ronda) cell))
                  acc-m))
           (t (list acc-m)))))
      ;; BOLLA: executa la lògica del rol assignat per id
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