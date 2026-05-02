;; ============================================================
;; Pràctica final de Llenguatges de Programació - LISP
;; Paintball - Agent E2
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
;; Agent Intel·ligent OFENSIU per E2
;; Estratègia: Atacar la base enemiga, crear bolles dels tres colors,
;;             capturar laboratoris, destruir unitats enemigues.
;;   - La base crea bolles rotant els tres colors (r, g, b) per torn.
;;   - Les bolles prioritzen pintar la base enemiga, després bolles i labs.
;;   - Si no veuen objectiu, exploren el mapa sistemàticament.
;;   - Usen la memòria compartida per recordar la posició de la base enemiga.
;; ============================================================

;; ============================================================
;; ACCESSORS DE LES DADES DE LA UNITAT
;; ============================================================

(defun agent-nms864-ronda      (d) (nth 0 d))
(defun agent-nms864-equip      (d) (nth 1 d))
(defun agent-nms864-pintura    (d) (nth 2 d))
(defun agent-nms864-id         (d) (nth 3 d))
(defun agent-nms864-tipus      (d) (nth 4 d))
(defun agent-nms864-coord      (d) (nth 5 d))
(defun agent-nms864-colors     (d) (nth 6 d))
(defun agent-nms864-color-propi(d) (nth 7 d))
(defun agent-nms864-tr-pintar  (d) (nth 8 d))
(defun agent-nms864-tr-moure   (d) (nth 9 d))
(defun agent-nms864-visio      (d) (nth 10 d))
(defun agent-nms864-memoria    (d) (nth 11 d))

;; ============================================================
;; ACCESSORS D'UNA CASELLA DE LA VISIÓ
;; Format: (coord tipus color elem equip colors-pintat color-propi tr-pintar tr-moure)
;; ============================================================

(defun agent-nms864-cas-coord        (c) (nth 0 c))
(defun agent-nms864-cas-tipus        (c) (nth 1 c))
(defun agent-nms864-cas-color        (c) (nth 2 c))
(defun agent-nms864-cas-elem         (c) (nth 3 c))
(defun agent-nms864-cas-equip        (c) (nth 4 c))
(defun agent-nms864-cas-colors-pintat(c) (nth 5 c))
(defun agent-nms864-cas-color-propi  (c) (nth 6 c))
(defun agent-nms864-cas-tr-pintar    (c) (nth 7 c))
(defun agent-nms864-cas-tr-moure     (c) (nth 8 c))

;; ============================================================
;; DISTÀNCIA I PREDICATS GEOMÈTRICS
;; ============================================================

(defun agent-nms864-dist2 (a b)
  "Retorna la distància euclidiana al quadrat entre a i b."
  (let* ((dx (- (car a) (car b)))
         (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

(defun agent-nms864-terra-p (casella)
  "Retorna T si la casella és de terra."
  (eq (agent-nms864-cas-tipus casella) 'terra))

(defun agent-nms864-lliure-p (casella)
  "Retorna T si la casella és de terra i no té cap element."
  (and (agent-nms864-terra-p casella)
       (null (agent-nms864-cas-elem casella))))

(defun agent-nms864-adjacent-p (coord casella)
  "Retorna T si la casella és adjacent a coord (d²≤2, d²>0)."
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord casella))))
    (and (> d 0) (<= d 2))))

(defun agent-nms864-en-rang-pintar-p (coord casella)
  "Retorna T si la casella és dins rang de pintura (d²≤5, d²>0)."
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord casella))))
    (and (> d 0) (<= d 5))))

;; ============================================================
;; CERCA DE CASELLA LLIURE ADJACENT (per crear bolles)
;; ============================================================

(defun agent-nms864-casella-lliure-adjacent (coord visio)
  "Retorna la coordenada d'una casella adjacent de terra lliure, o nil."
  (agent-nms864-casella-lliure-adjacent-rec coord visio))

(defun agent-nms864-casella-lliure-adjacent-rec (coord visio)
  "Recorre la visió cercant la primera casella adjacent lliure."
  (cond
    ((null visio) nil)
    ((and (agent-nms864-adjacent-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-casella-lliure-adjacent-rec coord (cdr visio)))))

;; ============================================================
;; CERCA D'OBJECTIUS PER PINTAR
;; ============================================================

(defun agent-nms864-objectiu-pintar (equip coord visio)
  "Retorna la coord de l'objectiu prioritari de pintura (base > bolla > lab)."
  (let ((base  (agent-nms864-cercar-base-enemiga-rang equip coord visio))
        (bolla (agent-nms864-cercar-bolla-enemiga-rang equip coord visio))
        (lab   (agent-nms864-cercar-lab-rang equip coord visio)))
    (cond
      (base  base)
      (bolla bolla)
      (lab   lab)
      (t nil))))

(defun agent-nms864-cercar-base-enemiga-rang (equip coord visio)
  "Retorna la coordenada de la base enemiga dins rang de pintura (d²≤5), o nil."
  (cond
    ((null visio) nil)
    ((and (agent-nms864-en-rang-pintar-p coord (car visio))
          (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'base)
          (not (eq (agent-nms864-cas-equip (car visio)) equip)))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-cercar-base-enemiga-rang equip coord (cdr visio)))))

(defun agent-nms864-cercar-bolla-enemiga-rang (equip coord visio)
  "Retorna la coordenada de la bolla enemiga dins rang de pintura (d²≤5), o nil."
  (cond
    ((null visio) nil)
    ((and (agent-nms864-en-rang-pintar-p coord (car visio))
          (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'bolla)
          (not (eq (agent-nms864-cas-equip (car visio)) equip)))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-cercar-bolla-enemiga-rang equip coord (cdr visio)))))

(defun agent-nms864-cercar-lab-rang (equip coord visio)
  "Retorna la coordenada del lab enemic o no capturat dins rang de pintura, o nil."
  (cond
    ((null visio) nil)
    ((and (agent-nms864-en-rang-pintar-p coord (car visio))
          (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'lab)
          (or (null (agent-nms864-cas-equip (car visio)))
              (not (eq (agent-nms864-cas-equip (car visio)) equip))))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-cercar-lab-rang equip coord (cdr visio)))))

;; ============================================================
;; CERCA D'OBJECTIUS PER MOURE'S
;; ============================================================

(defun agent-nms864-objectiu-moure (equip coord visio ronda)
  "Retorna la cel·la adjacent lliure destí de moviment, o nil si no n'hi ha."
  (let ((obj          (agent-nms864-objectiu-estrategic equip coord visio))
        (es-cantonada (< (length visio) 13)))
    (cond
      (es-cantonada
       (agent-nms864-escapar-cantonada coord visio ronda))
      (obj
       (agent-nms864-cel·la-cap-a coord obj visio))
      (t
       (agent-nms864-cel·la-explorar coord visio)))))

(defun agent-nms864-escapar-cantonada (coord visio ronda)
  "Retorna una cel·la adjacent lliure per escapar d'una cantonada, amb variació per ronda."
  (let* ((desfasament-aleatori (+ (car coord) (cadr coord) (* ronda 7)))
         (angle-variat         (* desfasament-aleatori 0.157))
         (objectiu-aleatori    (list (+ (car coord) (truncate (* 100 (cos angle-variat))))
                                     (+ (cadr coord) (truncate (* 100 (sin angle-variat))))))
         (casella-aleatoria    (agent-nms864-cel·la-cap-a-rec
                                coord objectiu-aleatori visio nil 999999)))
    (cond
      (casella-aleatoria casella-aleatoria)
      (t
       (let* ((desfasament        (+ (car coord) (cadr coord)))
              (angle              (* (+ ronda desfasament) 0.7853))
              (objectiu-imaginari (list (+ (car coord) (truncate (* 100 (cos angle))))
                                        (+ (cadr coord) (truncate (* 100 (sin angle))))))
              (casella            (agent-nms864-cel·la-cap-a-rec
                                   coord objectiu-imaginari visio nil 999999)))
         (cond
           (casella casella)
           (t (agent-nms864-cel·la-explorar coord visio))))))))

(defun agent-nms864-objectiu-estrategic (equip coord visio)
  "Retorna la coord de l'objectiu estratègic visible (base enemiga o lab enemic)."
  (let ((base (agent-nms864-cercar-base-enemiga-visio equip coord visio))
        (lab  (agent-nms864-cercar-lab-enemic-visio equip coord visio)))
    (cond
      (base base)
      (lab  lab)
      (t nil))))

(defun agent-nms864-cercar-base-enemiga-visio (equip coord visio)
  "Retorna la coordenada de la base enemiga més propera dins la visió, o nil."
  (agent-nms864-cercar-base-enemiga-visio-rec equip coord visio nil 999999))

(defun agent-nms864-cercar-base-enemiga-visio-rec (equip coord visio millor millor-dist)
  "Recorre la visió cercant la base enemiga de mínima distància."
  (cond
    ((null visio) millor)
    ((and (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'base)
          (not (eq (agent-nms864-cas-equip (car visio)) equip)))
     (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
       (cond
         ((< d millor-dist)
          (agent-nms864-cercar-base-enemiga-visio-rec equip coord (cdr visio)
                                                      (agent-nms864-cas-coord (car visio)) d))
         (t
          (agent-nms864-cercar-base-enemiga-visio-rec equip coord (cdr visio)
                                                      millor millor-dist)))))
    (t (agent-nms864-cercar-base-enemiga-visio-rec equip coord (cdr visio)
                                                   millor millor-dist))))

(defun agent-nms864-cercar-lab-enemic-visio (equip coord visio)
  "Retorna la coordenada del lab enemic o no capturat més proper dins la visió, o nil."
  (agent-nms864-cercar-lab-enemic-visio-rec equip coord visio nil 999999))

(defun agent-nms864-cercar-lab-enemic-visio-rec (equip coord visio millor millor-dist)
  "Recorre la visió cercant el lab enemic o no capturat de mínima distància."
  (cond
    ((null visio) millor)
    ((and (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'lab)
          (or (null (agent-nms864-cas-equip (car visio)))
              (not (eq (agent-nms864-cas-equip (car visio)) equip))))
     (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
       (cond
         ((< d millor-dist)
          (agent-nms864-cercar-lab-enemic-visio-rec equip coord (cdr visio)
                                                    (agent-nms864-cas-coord (car visio)) d))
         (t
          (agent-nms864-cercar-lab-enemic-visio-rec equip coord (cdr visio)
                                                    millor millor-dist)))))
    (t (agent-nms864-cercar-lab-enemic-visio-rec equip coord (cdr visio)
                                                 millor millor-dist))))

;; ============================================================
;; NAVEGACIÓ CAP A OBJECTIU I EXPLORACIÓ
;; ============================================================

(defun agent-nms864-cel·la-cap-a (coord objectiu visio)
  "Retorna la cel·la adjacent lliure que minimitza la distància a objectiu."
  (agent-nms864-cel·la-cap-a-rec coord objectiu visio nil 999999))

(defun agent-nms864-cel·la-cap-a-rec (coord objectiu visio millor millor-dist)
  "Recorre la visió cercant la cel·la adjacent lliure més propera a objectiu."
  (cond
    ((null visio) millor)
    ((and (agent-nms864-adjacent-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (let ((d (agent-nms864-dist2 (agent-nms864-cas-coord (car visio)) objectiu)))
       (cond
         ((< d millor-dist)
          (agent-nms864-cel·la-cap-a-rec coord objectiu (cdr visio)
                                         (agent-nms864-cas-coord (car visio)) d))
         (t
          (agent-nms864-cel·la-cap-a-rec coord objectiu (cdr visio)
                                         millor millor-dist)))))
    (t (agent-nms864-cel·la-cap-a-rec coord objectiu (cdr visio) millor millor-dist))))

(defun agent-nms864-cel·la-explorar (coord visio)
  "Retorna la cel·la adjacent lliure més allunyada de coord per explorar el mapa."
  (agent-nms864-cel·la-explorar-rec coord visio nil 0))

(defun agent-nms864-cel·la-explorar-rec (coord visio millor millor-dist)
  "Recorre la visió cercant la cel·la adjacent lliure de màxima distància."
  (cond
    ((null visio) millor)
    ((and (agent-nms864-adjacent-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
       (cond
         ((> d millor-dist)
          (agent-nms864-cel·la-explorar-rec coord (cdr visio)
                                            (agent-nms864-cas-coord (car visio)) d))
         (t
          (agent-nms864-cel·la-explorar-rec coord (cdr visio) millor millor-dist)))))
    (t (agent-nms864-cel·la-explorar-rec coord (cdr visio) millor millor-dist))))

;; ============================================================
;; GESTIÓ DE LA MEMÒRIA COMPARTIDA
;; ============================================================

(defun agent-nms864-actualitzar-memoria (memoria coord visio ronda)
  "Retorna la memòria actualitzada amb la posició de la base enemiga si és visible."
  (let ((base-vista (agent-nms864-cercar-base-enemiga-visio-simple visio)))
    (cond
      (base-vista
       (let* ((mem1 (mem-escriure memoria 'base-enemiga base-vista))
              (mem2 (mem-escriure mem1 'darrera-vista ronda))
              (mem3 (mem-escriure mem2 'posicio-propia coord)))
         mem3))
      ((and memoria (assoc 'base-enemiga memoria))
       (mem-escriure memoria 'posicio-propia coord))
      (t nil))))

(defun agent-nms864-cercar-base-enemiga-visio-simple (visio)
  "Retorna la coordenada de la primera base vista a la visió, o nil."
  (cond
    ((null visio) nil)
    ((and (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'base))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-cercar-base-enemiga-visio-simple (cdr visio)))))

(defun agent-nms864-objectiu-des-de-memoria (memoria coord visio)
  "Retorna l'objectiu estratègic: base visible o posició guardada a la memòria."
  (let ((base-visio (agent-nms864-cercar-base-enemiga-visio 'dummy coord visio)))
    (cond
      (base-visio base-visio)
      ((mem-llegir memoria 'base-enemiga)
       (mem-llegir memoria 'base-enemiga))
      (t nil))))

;; ============================================================
;; COLOR PER RONDA
;; ============================================================

(defun agent-nms864-color-per-ronda (ronda)
  "Retorna el color de bolla corresponent a la ronda (rotació r, g, b)."
  (cond
    ((= (mod ronda 3) 0) 'r)
    ((= (mod ronda 3) 1) 'g)
    (t 'b)))

;; ============================================================
;; FUNCIÓ PRINCIPAL DE L'AGENT
;; ============================================================

(defun agent-nms864 (dades)
  "Agent ofensiu per E2. Crea bolles dels tres colors i ataca la base enemiga."
  (let* ((ronda     (agent-nms864-ronda dades))
         (equip     (agent-nms864-equip dades))
         (pintura   (agent-nms864-pintura dades))
         (tipus     (agent-nms864-tipus dades))
         (coord     (agent-nms864-coord dades))
         (tr-pintar (agent-nms864-tr-pintar dades))
         (tr-moure  (agent-nms864-tr-moure dades))
         (visio     (agent-nms864-visio dades)))
    (cond
      ;; BASE: crear bolla rotant colors, actualitzar memòria si cal
      ((eq tipus 'base)
       (let* ((casella-lliure (agent-nms864-casella-lliure-adjacent coord visio))
              (nova-memoria   (agent-nms864-actualitzar-memoria
                               (agent-nms864-memoria dades) coord visio ronda)))
         (cond
           ((and casella-lliure (>= pintura 50))
            (let ((accions (list (list 'crea-bolla
                                       (list (agent-nms864-color-per-ronda ronda)
                                             casella-lliure)))))
              (cond
                (nova-memoria
                 (append accions (list (list 'escriu-memoria (list nova-memoria)))))
                (t accions))))
           (t
            (cond
              (nova-memoria (list (list 'escriu-memoria (list nova-memoria))))
              (t nil))))))
      ;; BOLLA: pintar objectiu, moure's cap a objectiu, actualitzar memòria
      ((eq tipus 'bolla)
       (let* ((obj-pintar   (agent-nms864-objectiu-pintar equip coord visio))
              (obj-moure    (agent-nms864-objectiu-moure equip coord visio ronda))
              (nova-memoria (agent-nms864-actualitzar-memoria
                             (agent-nms864-memoria dades) coord visio ronda))
              (accio-pintar (cond
                              ((and obj-pintar (< tr-pintar 1))
                               (list (list 'pinta (list obj-pintar))))
                              (t nil)))
              (accio-moure  (cond
                              ((and obj-moure (< tr-moure 1))
                               (list (list 'mou (list obj-moure))))
                              (t nil)))
              (accio-memoria (cond
                               (nova-memoria
                                (list (list 'escriu-memoria (list nova-memoria))))
                               (t nil))))
         (append accio-pintar accio-moure accio-memoria)))
      (t nil))))