;; ============================================================
;; Pràctica final de Llenguatges de Programació - LISP
;; Paintball - Agent E1
;; ============================================================
;; Estudiants:
;;   - Marín Sánchez, Carolina
;;   - Mehannek Samah, Nour Iman
;; Data: 2026
;; Assignatura: Llenguatges de Programació
;; Grup: 101
;; Professors:
;;   - Cabot Nadal, Miquel Àngel
;;   - Oliver Tomàs, Antoni
;; Lliurament: primera convocatòria.
;; ============================================================
;; Agent Intel·ligent DEFENSIU per E1
;; Estratègia: Protegir la base, capturar labs, atacar la base enemiga.
;;   - La base crea bolles rotant els tres colors (r, g, b).
;;   - Les bolles prioritzen pintar la base enemiga, després bolles i labs.
;;   - Si no veuen objectiu, exploren el mapa sistemàticament.
;;   - Usen la memòria compartida per recordar la posició de la base enemiga.
;; ============================================================

;; ============================================================
;; ACCESSORS DE LES DADES DE LA UNITAT
;; ============================================================

(defun agent-cms213-ronda      (d) (nth 0 d))
(defun agent-cms213-equip      (d) (nth 1 d))
(defun agent-cms213-pintura    (d) (nth 2 d))
(defun agent-cms213-id         (d) (nth 3 d))
(defun agent-cms213-tipus      (d) (nth 4 d))
(defun agent-cms213-coord      (d) (nth 5 d))
(defun agent-cms213-colors     (d) (nth 6 d))
(defun agent-cms213-color-propi(d) (nth 7 d))
(defun agent-cms213-tr-pintar  (d) (nth 8 d))
(defun agent-cms213-tr-moure   (d) (nth 9 d))
(defun agent-cms213-visio      (d) (nth 10 d))
(defun agent-cms213-memoria    (d) (nth 11 d))

;; ============================================================
;; DISTÀNCIA I PREDICATS GEOMÈTRICS
;; ============================================================

(defun agent-cms213-dist2 (a b)
  "Retorna la distància euclidiana al quadrat entre a i b."
  (let ((dx (- (car a) (car b)))
        (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

;; ============================================================
;; CERCA DE CASELLES LLIURES ADJACENTS
;; ============================================================

(defun agent-cms213-primera-casella-lliure (coord visio)
  "Retorna la casella adjacent lliure més allunyada de coord per evitar aglomeracions."
  (agent-cms213-primera-casella-lliure-rec coord visio nil -1))

(defun agent-cms213-primera-casella-lliure-rec (coord visio millor millor-dist)
  "Recorre la visió cercant la casella adjacent lliure amb major distància a coord."
  (cond
    ((null visio) millor)
    ((and (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 2)
          (null (nth 3 (car visio)))
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) millor-dist))
     (agent-cms213-primera-casella-lliure-rec coord (cdr visio)
                                               (nth 0 (car visio))
                                               (agent-cms213-dist2 coord (nth 0 (car visio)))))
    (t (agent-cms213-primera-casella-lliure-rec coord (cdr visio) millor millor-dist))))

;; ============================================================
;; CERCA D'OBJECTIUS PER PINTAR
;; ============================================================

(defun agent-cms213-primer-objectiu-pintura (equip coord visio)
  "Retorna la coordenada de l'objectiu prioritari per pintar (base > bolla > lab)."
  (let ((base (agent-cms213-cercar-base-en-rang equip coord visio)))
    (cond
      (base base)
      (t (agent-cms213-primer-objectiu-pintura-rec equip coord visio)))))

(defun agent-cms213-cercar-base-en-rang (equip coord visio)
  "Retorna la coordenada de la base enemiga dins rang de pintura (d²≤5), o nil."
  (cond
    ((null visio) nil)
    ((and (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 5)
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (eq (nth 3 (car visio)) 'base)
          (not (eq (nth 4 (car visio)) equip)))
     (nth 0 (car visio)))
    (t (agent-cms213-cercar-base-en-rang equip coord (cdr visio)))))

(defun agent-cms213-primer-objectiu-pintura-rec (equip coord visio)
  "Retorna la coordenada de la bolla o lab enemic més proper dins rang de pintura."
  (cond
    ((null visio) nil)
    ((and (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 5)
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'bolla)
          (not (eq (nth 4 (car visio)) equip)))
     (nth 0 (car visio)))
    ((and (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 5)
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'lab)
          (or (null (nth 4 (car visio)))
              (not (eq (nth 4 (car visio)) equip))))
     (nth 0 (car visio)))
    (t (agent-cms213-primer-objectiu-pintura-rec equip coord (cdr visio)))))

;; ============================================================
;; CERCA D'OBJECTIUS PER MOURE'S
;; ============================================================

(defun agent-cms213-primer-objectiu-moviment (equip coord visio ronda memoria)
  "Retorna la coordenada destí de moviment cap a l'objectiu estratègic o d'exploració."
  (let ((base-visible (agent-cms213-objectiu-des-de-memoria memoria coord visio)))
    (cond
      (base-visible
       (agent-cms213-casella-cap-a-objectiu coord base-visible visio))
      (t
       (agent-cms213-explorar-zona-cerca coord visio ronda)))))

(defun agent-cms213-objectiu-des-de-memoria (memoria coord visio)
  "Retorna l'objectiu estratègic: base visible o posició guardada a la memòria."
  (let ((base-visio (agent-cms213-trobar-objectiu-estrategic 'e1 coord visio)))
    (cond
      (base-visio base-visio)
      ((mem-llegir memoria 'base-enemiga)
       (mem-llegir memoria 'base-enemiga))
      (t nil))))

(defun agent-cms213-trobar-objectiu-estrategic (equip coord visio)
  "Retorna la coordenada de la base enemiga més propera dins la visió."
  (agent-cms213-cercar-base-enemiga-rec equip coord visio nil 100000))

(defun agent-cms213-cercar-base-enemiga-rec (equip coord visio millor millor-dist)
  "Recorre la visió cercant la base enemiga de mínima distància."
  (cond
    ((null visio) millor)
    ((and (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'base)
          (not (eq (nth 4 (car visio)) equip)))
     (let ((dist (agent-cms213-dist2 coord (nth 0 (car visio)))))
       (cond
         ((< dist millor-dist)
          (agent-cms213-cercar-base-enemiga-rec equip coord (cdr visio)
                                                (nth 0 (car visio)) dist))
         (t
          (agent-cms213-cercar-base-enemiga-rec equip coord (cdr visio)
                                                millor millor-dist)))))
    (t (agent-cms213-cercar-base-enemiga-rec equip coord (cdr visio) millor millor-dist))))

;; ============================================================
;; EXPLORACIÓ DEL MAPA
;; ============================================================

(defun agent-cms213-explorar-zona-cerca (coord visio ronda)
  "Retorna la cel·la de moviment per explorar: escapa de cantonades o cerca la més llunyana."
  (let ((es-cantonada (agent-cms213-detectar-cantonada coord visio)))
    (cond
      (es-cantonada
       (agent-cms213-escapar-cantonada coord visio ronda))
      (t
       (agent-cms213-cercar-casella-maxima-distancia coord visio nil 0)))))

(defun agent-cms213-detectar-cantonada (coord visio)
  "Retorna T si la unitat sembla estar en una cantonada del mapa (poca visió)."
  (< (agent-cms213-comptar-caselles visio) 13))

(defun agent-cms213-comptar-caselles (visio)
  "Compta el nombre de caselles a la llista de visió."
  (cond
    ((null visio) 0)
    (t (+ 1 (agent-cms213-comptar-caselles (cdr visio))))))

(defun agent-cms213-escapar-cantonada (coord visio ronda)
  "Retorna una cel·la adjacent lliure per escapar d'una cantonada, usant variació per ronda."
  (let* ((desfasament-aleatori (+ (car coord) (cadr coord) (* ronda 7)))
         (angle-variat         (* desfasament-aleatori 0.3141592))
         (objectiu-aleatori    (list (+ (car coord) (truncate (* 100 (cos angle-variat))))
                                     (+ (cadr coord) (truncate (* 100 (sin angle-variat))))))
         (casella-aleatoria    (agent-cms213-casella-cap-a-objectiu-rec
                                coord objectiu-aleatori visio nil 1000000)))
    (cond
      (casella-aleatoria casella-aleatoria)
      (t
       (let* ((desfasament       (+ (car coord) (cadr coord)))
              (angle             (* (+ ronda desfasament) 0.7853))
              (objectiu-imaginari (list (+ (car coord) (truncate (* 100 (cos angle))))
                                        (+ (cadr coord) (truncate (* 100 (sin angle))))))
              (casella           (agent-cms213-casella-cap-a-objectiu-rec
                                  coord objectiu-imaginari visio nil 1000000)))
         (cond
           (casella casella)
           (t (agent-cms213-cercar-casella-maxima-distancia coord visio nil 0))))))))

(defun agent-cms213-cercar-casella-maxima-distancia (coord visio millor millor-dist)
  "Retorna la cel·la adjacent lliure de major distància a coord per explorar."
  (cond
    ((null visio) millor)
    ((and (agent-cms213-casella-lliure-p 'dummy (car visio))
          (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 2))
     (let ((dist (agent-cms213-dist2 coord (nth 0 (car visio)))))
       (cond
         ((> dist millor-dist)
          (agent-cms213-cercar-casella-maxima-distancia coord (cdr visio)
                                                        (nth 0 (car visio)) dist))
         (t
          (agent-cms213-cercar-casella-maxima-distancia coord (cdr visio)
                                                        millor millor-dist)))))
    (t (agent-cms213-cercar-casella-maxima-distancia coord (cdr visio) millor millor-dist))))

;; ============================================================
;; NAVEGACIÓ CAP A OBJECTIU
;; ============================================================

(defun agent-cms213-casella-cap-a-objectiu (coord-bolla coord-objectiu visio)
  "Retorna la cel·la adjacent lliure que minimitza la distància a coord-objectiu."
  (let ((casella-propera (agent-cms213-casella-cap-a-objectiu-rec
                          coord-bolla coord-objectiu visio nil 100000)))
    (cond
      (casella-propera casella-propera)
      (t (agent-cms213-cercar-casella-maxima-distancia coord-bolla visio nil 0)))))

(defun agent-cms213-casella-cap-a-objectiu-rec (coord-bolla coord-objectiu visio millor millor-dist)
  "Recorre la visió cercant la cel·la adjacent lliure més propera a coord-objectiu."
  (cond
    ((null visio) millor)
    ((and (agent-cms213-casella-lliure-p 'dummy (car visio))
          (<= (agent-cms213-dist2 coord-bolla (nth 0 (car visio))) 2))
     (let* ((coord-casella (nth 0 (car visio)))
            (dist-nova     (agent-cms213-dist2 coord-casella coord-objectiu)))
       (cond
         ((< dist-nova millor-dist)
          (agent-cms213-casella-cap-a-objectiu-rec coord-bolla coord-objectiu
                                                    (cdr visio) coord-casella dist-nova))
         (t
          (agent-cms213-casella-cap-a-objectiu-rec coord-bolla coord-objectiu
                                                    (cdr visio) millor millor-dist)))))
    (t (agent-cms213-casella-cap-a-objectiu-rec coord-bolla coord-objectiu
                                                 (cdr visio) millor millor-dist))))

;; ============================================================
;; PREDICATS AUXILIARS
;; ============================================================

(defun agent-cms213-pintable-p (equip casella)
  "Retorna T si la casella és de terra i pintable per equip."
  (and (eq (nth 1 casella) 'terra)
       (or (null (nth 3 casella))
           (not (eq (nth 4 casella) equip)))))

(defun agent-cms213-casella-lliure-p (equip casella)
  "Retorna T si la casella és de terra i no té cap element."
  (and (eq (nth 1 casella) 'terra)
       (null (nth 3 casella))))

;; ============================================================
;; GESTIÓ DE LA MEMÒRIA COMPARTIDA
;; ============================================================

(defun agent-cms213-actualitzar-memoria (memoria coord visio ronda)
  "Retorna la memòria actualitzada amb la posició de la base enemiga si és visible."
  (let ((base-enemiga (agent-cms213-trobar-base-enemiga-visio visio)))
    (cond
      (base-enemiga
       (let* ((mem1 (mem-escriure memoria 'base-enemiga base-enemiga))
              (mem2 (mem-escriure mem1 'darrera-vista ronda))
              (mem3 (mem-escriure mem2 'posicio-propia coord)))
         mem3))
      ((mem-llegir memoria 'base-enemiga)
       (mem-escriure memoria 'posicio-propia coord))
      (t (mem-escriure memoria 'posicio-propia coord)))))

(defun agent-cms213-trobar-base-enemiga-visio (visio)
  "Retorna la coordenada de la primera base vista a la visió, o nil."
  (cond
    ((null visio) nil)
    ((and (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'base))
     (nth 0 (car visio)))
    (t (agent-cms213-trobar-base-enemiga-visio (cdr visio)))))

;; ============================================================
;; FUNCIÓ PRINCIPAL DE L'AGENT
;; ============================================================

(defun agent-cms213 (dades)
  "Agent defensiu per E1. Crea bolles de colors variats i ataca la base enemiga."
  (let* ((equip          (nth 1 dades))
         (pintura        (nth 2 dades))
         (ronda          (nth 0 dades))
         (tipus          (nth 4 dades))
         (coord          (nth 5 dades))
         (tr-pintar      (nth 8 dades))
         (tr-moure       (nth 9 dades))
         (visio          (nth 10 dades))
         (memoria        (nth 11 dades))
         (nova-memoria   (agent-cms213-actualitzar-memoria memoria coord visio ronda))
         (obj-pintura    (agent-cms213-primer-objectiu-pintura equip coord visio))
         (obj-moviment   (agent-cms213-primer-objectiu-moviment equip coord visio ronda memoria))
         (casella-lliure (agent-cms213-primera-casella-lliure coord visio)))
    (cond
      ;; BASE: crear bolla rotant colors i actualitzar memòria
      ((eq tipus 'base)
       (let ((color-elegit (nth (mod ronda 3) '(r g b))))
         (cond
           ((and (>= pintura 50) casella-lliure)
            (list (list 'crea-bolla (list color-elegit casella-lliure))
                  (list 'escriu-memoria (list nova-memoria))))
           (t (list (list 'escriu-memoria (list nova-memoria)))))))
      ;; BOLLA: pintar objectiu, moure's o actualitzar memòria
      ((eq tipus 'bolla)
       (let ((accions-combat
              (cond
                ((and obj-pintura (< tr-pintar 1))
                 (list (list 'pinta (list obj-pintura))))
                ((and obj-moviment (< tr-moure 1))
                 (list (list 'mou (list obj-moviment))))
                (t nil))))
         (let ((base-vista (agent-cms213-trobar-objectiu-estrategic equip coord visio)))
           (cond
             (base-vista
              (append accions-combat
                      (list (list 'escriu-memoria
                                  (list (agent-cms213-actualitzar-memoria
                                         memoria coord visio ronda))))))
             (t accions-combat)))))
      (t nil))))