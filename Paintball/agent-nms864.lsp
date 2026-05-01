;; ============================================================
;; Pràctica final de Llenguatges de Programació - Paintball
;; Agent: agent-nms864 (Equip E2 - Ofensiu)
;; Estratègia: Atacar base enemiga, crear bolles de colors variats,
;;             capturar laboratoris, destruir unitats enemigues.
;; ============================================================

;; ============================================================
;; ACCESSORS DE LES DADES DE LA UNITAT
;; Format: (ronda equip pintura id tipus coord colors-pintat
;;          color-propi tr-pintar tr-moure visio memoria)
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
;; Format casella: (coord tipus-casella color-casella
;;                  tipus-element equip colors-pintat
;;                  color-propi tr-pintar tr-moure)
;; Si tipus-casella és 'aigua, no hi ha més camps.
;; ============================================================

(defun agent-nms864-cas-coord        (c) (nth 0 c))
(defun agent-nms864-cas-tipus        (c) (nth 1 c))  ; 'terra o 'agua
(defun agent-nms864-cas-color        (c) (nth 2 c))  ; color de la casella
(defun agent-nms864-cas-elem         (c) (nth 3 c))  ; 'base 'bolla 'lab o nil
(defun agent-nms864-cas-equip        (c) (nth 4 c))  ; equip de l'element
(defun agent-nms864-cas-colors-pintat(c) (nth 5 c))  ; colors pintats de l'element
(defun agent-nms864-cas-color-propi  (c) (nth 6 c))  ; color propi bolla
(defun agent-nms864-cas-tr-pintar    (c) (nth 7 c))  ; tr-pintar bolla
(defun agent-nms864-cas-tr-moure     (c) (nth 8 c))  ; tr-moure bolla

;; ============================================================
;; FUNCIÓ DE DISTÀNCIA EUCLIDIANA AL QUADRAT
;; d²(a,b) = (ax-bx)² + (ay-by)²
;; ============================================================

(defun agent-nms864-dist2 (a b)
  ;; Retorna la distància euclidiana al quadrat entre a i b
  (let* ((dx (- (car a) (car b)))
         (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

;; ============================================================
;; PREDICATS AUXILIARS SOBRE CASELLES
;; ============================================================

(defun agent-nms864-terra-p (casella)
  ;; Cert si la casella és de terra
  (eq (agent-nms864-cas-tipus casella) 'terra))

(defun agent-nms864-lliure-p (casella)
  ;; Cert si la casella és de terra i no té cap element
  (and (agent-nms864-terra-p casella)
       (null (agent-nms864-cas-elem casella))))

(defun agent-nms864-adjacent-p (coord casella)
  ;; Cert si la casella és adjacent a coord (d² <= 2, però no la mateixa)
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord casella))))
    (and (> d 0) (<= d 2))))

(defun agent-nms864-en-rang-pintar-p (coord casella)
  ;; Cert si la casella és dins rang de pintura (d² <= 5, però no la mateixa)
  (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord casella))))
    (and (> d 0) (<= d 5))))

;; ============================================================
;; CERCA DE CASELLA LLIURE ADJACENT (per crear bolles)
;; Retorna la coordenada d'una casella de terra lliure adjacent.
;; ============================================================

(defun agent-nms864-casella-lliure-adjacent (coord visio)
  ;; Cerca la primera casella adjacent (d²<=2) de terra i sense element
  (agent-nms864-casella-lliure-adjacent-rec coord visio))

(defun agent-nms864-casella-lliure-adjacent-rec (coord visio)
  (cond
    ((null visio) nil)
    ((and (agent-nms864-adjacent-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-casella-lliure-adjacent-rec coord (cdr visio)))))

;; ============================================================
;; CERCA D'OBJECTIU PER PINTAR
;; Prioritats (per rang d²<=5):
;;   1. Base enemiga
;;   2. Bolla enemiga
;;   3. Laboratori enemic o no capturat
;; NO pintem unitats pròpies.
;; ============================================================

(defun agent-nms864-objectiu-pintar (equip coord visio)
  ;; Retorna la coord de l'objectiu prioritari de pintura, o nil
  (let ((base (agent-nms864-buscar-base-enemiga-rang equip coord visio))
        (bolla (agent-nms864-buscar-bolla-enemiga-rang equip coord visio))
        (lab   (agent-nms864-buscar-lab-rang equip coord visio)))
    (cond
      (base  base)
      (bolla bolla)
      (lab   lab)
      (t nil))))

(defun agent-nms864-buscar-base-enemiga-rang (equip coord visio)
  ;; Cerca base enemiga dins rang de pintura
  (cond
    ((null visio) nil)
    ((and (agent-nms864-en-rang-pintar-p coord (car visio))
          (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'base)
          (not (eq (agent-nms864-cas-equip (car visio)) equip)))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-buscar-base-enemiga-rang equip coord (cdr visio)))))

(defun agent-nms864-buscar-bolla-enemiga-rang (equip coord visio)
  ;; Cerca bolla enemiga dins rang de pintura
  (cond
    ((null visio) nil)
    ((and (agent-nms864-en-rang-pintar-p coord (car visio))
          (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'bolla)
          (not (eq (agent-nms864-cas-equip (car visio)) equip)))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-buscar-bolla-enemiga-rang equip coord (cdr visio)))))

(defun agent-nms864-buscar-lab-rang (equip coord visio)
  ;; Cerca laboratori enemic o no capturat dins rang de pintura
  (cond
    ((null visio) nil)
    ((and (agent-nms864-en-rang-pintar-p coord (car visio))
          (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'lab)
          (or (null (agent-nms864-cas-equip (car visio)))
              (not (eq (agent-nms864-cas-equip (car visio)) equip))))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-buscar-lab-rang equip coord (cdr visio)))))

;; ============================================================
;; CERCA D'OBJECTIU PER MOURE'S
;; Retorna la casella adjacent (d²<=2) lliure que minimitza
;; la distància a l'objectiu estratègic visible.
;; Si no hi ha objectiu visible, explora cap a cel·les allunyades.
;; ============================================================

(defun agent-nms864-objectiu-moure (equip coord visio ronda)
  ;; Retorna la coordenada destí de moviment, o nil
  (let ((obj (agent-nms864-objectiu-estrategic equip coord visio))
        (es-esquina (< (length visio) 13)))
    (cond
      ;; SI ESTEM EN ESQUINA (poca visió): escapar cap a una direcció aleatòria
      (es-esquina
       (let* ((offset-random (+ (car coord) (cadr coord) (* ronda 7)))
              (angulo-variadov (* offset-random 0.157)) ; Escapar con variabilidad
              (target-random (list (+ (car coord) (truncate (* 100 (cos angulo-variadov))))
                                   (+ (cadr coord) (truncate (* 100 (sin angulo-variadov))))))
              (celda-random (agent-nms864-cel·la-cap-a-rec coord target-random visio nil 999999)))
         ;; Si no hay celda hacia el objetivo aleatorio, intentar el determinista anterior
         (if celda-random
             celda-random
           (let* ((offset (+ (car coord) (cadr coord)))
                  (angulo (* (+ ronda offset) 0.7853))
                  (target-imag (list (+ (car coord) (truncate (* 100 (cos angulo))))
                                     (+ (cadr coord) (truncate (* 100 (sin angulo))))))
                  (celda (agent-nms864-cel·la-cap-a-rec coord target-imag visio nil 999999)))
             (if celda
                 celda
               (agent-nms864-cel·la-explorar coord visio))))))

      (obj
       ;; Si hi ha objectiu visible, anar cap a ell
       (agent-nms864-cel·la-cap-a coord obj visio))
      (t
       ;; Explorar: moure's cap a la casella adjacent més allunyada
       (agent-nms864-cel·la-explorar coord visio)))))

(defun agent-nms864-objectiu-estrategic (equip coord visio)
  ;; Retorna la coord de l'objectiu estratègic (base o bolla enemiga visible)
  (let ((base (agent-nms864-buscar-base-enemiga-visio equip coord visio))
        (lab  (agent-nms864-buscar-lab-enemic-visio equip coord visio)))
    (cond
      (base base)
      (lab  lab)
      (t nil))))

(defun agent-nms864-buscar-base-enemiga-visio (equip coord visio)
  ;; Cerca la base enemiga més propera dins la visió (d² <= 64 per bases, <= 20 bolles)
  (agent-nms864-buscar-base-enemiga-visio-rec equip coord visio nil 999999))

(defun agent-nms864-buscar-base-enemiga-visio-rec (equip coord visio millor millor-dist)
  (cond
    ((null visio) millor)
    ((and (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'base)
          (not (eq (agent-nms864-cas-equip (car visio)) equip)))
     (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
       (if (< d millor-dist)
           (agent-nms864-buscar-base-enemiga-visio-rec equip coord (cdr visio)
                                                       (agent-nms864-cas-coord (car visio)) d)
         (agent-nms864-buscar-base-enemiga-visio-rec equip coord (cdr visio) millor millor-dist))))
    (t (agent-nms864-buscar-base-enemiga-visio-rec equip coord (cdr visio) millor millor-dist))))

(defun agent-nms864-buscar-lab-enemic-visio (equip coord visio)
  ;; Cerca el laboratori enemic o no capturat més proper dins la visió
  (agent-nms864-buscar-lab-enemic-visio-rec equip coord visio nil 999999))

(defun agent-nms864-buscar-lab-enemic-visio-rec (equip coord visio millor millor-dist)
  (cond
    ((null visio) millor)
    ((and (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'lab)
          (or (null (agent-nms864-cas-equip (car visio)))
              (not (eq (agent-nms864-cas-equip (car visio)) equip))))
     (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
       (if (< d millor-dist)
           (agent-nms864-buscar-lab-enemic-visio-rec equip coord (cdr visio)
                                                     (agent-nms864-cas-coord (car visio)) d)
         (agent-nms864-buscar-lab-enemic-visio-rec equip coord (cdr visio) millor millor-dist))))
    (t (agent-nms864-buscar-lab-enemic-visio-rec equip coord (cdr visio) millor millor-dist))))

(defun agent-nms864-cel·la-cap-a (coord objectiu visio)
  ;; Retorna la casella adjacent lliure (d²<=2) que minimitza la distància a objectiu
  (agent-nms864-cel·la-cap-a-rec coord objectiu visio nil 999999))

(defun agent-nms864-cel·la-cap-a-rec (coord objectiu visio millor millor-dist)
  (cond
    ((null visio) millor)
    ((and (agent-nms864-adjacent-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (let ((d (agent-nms864-dist2 (agent-nms864-cas-coord (car visio)) objectiu)))
       (if (< d millor-dist)
           (agent-nms864-cel·la-cap-a-rec coord objectiu (cdr visio)
                                          (agent-nms864-cas-coord (car visio)) d)
         (agent-nms864-cel·la-cap-a-rec coord objectiu (cdr visio) millor millor-dist))))
    (t (agent-nms864-cel·la-cap-a-rec coord objectiu (cdr visio) millor millor-dist))))

(defun agent-nms864-cel·la-explorar (coord visio)
  ;; Retorna la casella adjacent lliure més allunyada (per explorar)
  (agent-nms864-cel·la-explorar-rec coord visio nil 0))

(defun agent-nms864-cel·la-explorar-rec (coord visio millor millor-dist)
  (cond
    ((null visio) millor)
    ((and (agent-nms864-adjacent-p coord (car visio))
          (agent-nms864-lliure-p (car visio)))
     (let ((d (agent-nms864-dist2 coord (agent-nms864-cas-coord (car visio)))))
       (if (> d millor-dist)
           (agent-nms864-cel·la-explorar-rec coord (cdr visio)
                                             (agent-nms864-cas-coord (car visio)) d)
         (agent-nms864-cel·la-explorar-rec coord (cdr visio) millor millor-dist))))
    (t (agent-nms864-cel·la-explorar-rec coord (cdr visio) millor millor-dist))))

;; ============================================================
;; GESTIÓ DE MEMÒRIA COMPARTIDA
;; La memòria és una alist: ((base-enemiga coord) (darrera-vista ronda) ...)
;; ============================================================

(defun agent-nms864-actualitzar-memoria (memoria coord visio ronda)
  "Retorna nova-memoria si hi ha nova informació rellevant, nil si no cal escriure.
   USA LES FUNCIONS GENÈRIQUES DE memoria.lsp"
  (let ((base-vista (agent-nms864-cercar-base-enemiga-visio visio)))
    (cond
      ;; Si veiem la base: guardar posició i ronda
      (base-vista
       (let* ((mem1 (mem-escriure memoria 'base-enemiga base-vista))
              (mem2 (mem-escriure mem1 'darrera-vista ronda))
              (mem3 (mem-escriure mem2 'posicio-propia coord)))
         mem3))
      ;; Si no la veiem però tenim posició antiga: conservar-la i actualitzar posició pròpia
      ((and memoria (assoc 'base-enemiga memoria))
       (mem-escriure memoria 'posicio-propia coord))
      ;; Sense informació nova: nil (no escrivim res)
      (t nil))))

(defun agent-nms864-cercar-base-enemiga-visio (visio)
  "Retorna la coord de la primera base vista a la visió, o nil."
  (cond
    ((null visio) nil)
    ((and (agent-nms864-terra-p (car visio))
          (eq (agent-nms864-cas-elem (car visio)) 'base))
     (agent-nms864-cas-coord (car visio)))
    (t (agent-nms864-cercar-base-enemiga-visio (cdr visio)))))

(defun agent-nms864-objectiu-desde-memoria (memoria coord visio)
  "Si la visió no mostra la base enemiga, usa la memòria per orientar-se.
   USA LES FUNCIONS GENÈRIQUES DE memoria.lsp"
  (let ((base-visio (agent-nms864-buscar-base-enemiga-visio 'dummy coord visio)))
    (cond
      ;; Si la veiem directament, la usam
      (base-visio base-visio)
      ;; Si no la veiem però la tenim a la memòria, anem cap allà
      ((mem-llegir memoria 'base-enemiga)
       (mem-llegir memoria 'base-enemiga))
      (t nil))))

;; ============================================================
;; SELECCIÓ DE COLOR DE LA BOLLA (rotació per ronda)
;; Fem bolles dels tres colors per poder destruir la base enemiga.
;; ============================================================

(defun agent-nms864-color-per-ronda (ronda)
  ;; Rota entre els tres colors: r, g, b
  (cond
    ((= (mod ronda 3) 0) 'r)
    ((= (mod ronda 3) 1) 'g)
    (t 'b)))

;; ============================================================
;; FUNCIÓ PRINCIPAL DE L'AGENT
;; Entrada: llista de dades de la unitat (format de l'enunciat)
;; Sortida: llista d'accions: ((nom-accio (arg1 arg2 ...)) ...)
;; ============================================================

(defun agent-nms864 (dades)
  "Agent ofensiu per E2. Crea bolles dels tres colors i ataca la base enemiga."
  (let* ((ronda       (agent-nms864-ronda dades))
         (equip       (agent-nms864-equip dades))
         (pintura     (agent-nms864-pintura dades))
         (tipus       (agent-nms864-tipus dades))
         (coord       (agent-nms864-coord dades))
         (tr-pintar   (agent-nms864-tr-pintar dades))
         (tr-moure    (agent-nms864-tr-moure dades))
         (visio       (agent-nms864-visio dades)))

    (cond

      ;; ---- BASE ------------------------------------------------
      ;; La base crea una bolla per torn si té pintura suficient (50)
      ;; i hi ha una casella adjacent lliure.
      ((eq tipus 'base)
       (let* ((cel·la-lliure (agent-nms864-casella-lliure-adjacent coord visio))
              (nova-memoria (agent-nms864-actualitzar-memoria
                             (agent-nms864-memoria dades) coord visio ronda)))
         (cond
           ((and cel·la-lliure (>= pintura 50))
            ;; Crear bolla rotant colors per tenir els tres colors actius
            (let ((accions (list (list 'crea-bolla
                                       (list (agent-nms864-color-per-ronda ronda)
                                             cel·la-lliure)))))
              (if nova-memoria
                  (append accions (list (list 'escriu-memoria (list nova-memoria))))
                accions)))
           ;; Sense pintura o sense espai: actualitzar memòria si cal
           (t (if nova-memoria
                  (list (list 'escriu-memoria (list nova-memoria)))
                nil)))))

      ;; ---- BOLLA -----------------------------------------------
      ;; Una bolla pot fer UNA o MÉS accions per torn si tr < 1.
      ;; Prioritats:
      ;;   1. Pintar objectiu (base enemiga > bolla enemiga > lab)
      ;;   2. Moure's cap a l'objectiu estratègic
      ;;   3. Escriure memòria si hem vist alguna cosa rellevant
      ((eq tipus 'bolla)
       (let* ((obj-pintar (agent-nms864-objectiu-pintar equip coord visio))
              (obj-moure  (agent-nms864-objectiu-moure equip coord visio ronda))
              ;; Construïm la llista d'accions possibles en aquest torn
              (accio-pintar
               (if (and obj-pintar (< tr-pintar 1))
                   (list (list 'pinta (list obj-pintar)))
                 nil))
              (accio-moure
               (if (and obj-moure (< tr-moure 1))
                   (list (list 'mou (list obj-moure)))
                 nil))
              ;; Actualitzar memòria si veiem la base enemiga
              (nova-memoria (agent-nms864-actualitzar-memoria
                             (agent-nms864-memoria dades) coord visio ronda))
              (accio-memoria
               (if nova-memoria
                   (list (list 'escriu-memoria (list nova-memoria)))
                 nil)))
         ;; Retornem totes les accions possibles
         (append accio-pintar accio-moure accio-memoria)))

      ;; ---- Altres tipus (no hauria de passar) ------------------
      (t nil))))