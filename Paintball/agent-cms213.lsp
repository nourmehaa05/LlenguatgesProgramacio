;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball - Agent E1
;; Agent Intel·ligent DEFENSIU per E1
;; Estratègia: Protegir la base, pintar caselles pròpies, capturar labs
;; ------------------------------------------------------------------

;; Funcions auxiliars per extreure dades
(defun agent-cms213-ronda (dades) (nth 0 dades))
(defun agent-cms213-equip (dades) (nth 1 dades))
(defun agent-cms213-pintura (dades) (nth 2 dades))
(defun agent-cms213-id (dades) (nth 3 dades))
(defun agent-cms213-tipus (dades) (nth 4 dades))
(defun agent-cms213-coord (dades) (nth 5 dades))
(defun agent-cms213-colors (dades) (nth 6 dades))
(defun agent-cms213-color-propi (dades) (nth 7 dades))
(defun agent-cms213-tr-pintar (dades) (nth 8 dades))
(defun agent-cms213-tr-moure (dades) (nth 9 dades))
(defun agent-cms213-visio (dades) (nth 10 dades))
(defun agent-cms213-memoria (dades) (nth 11 dades))

;; Distancia euclidiana quadrada
(defun agent-cms213-distancia (p1 p2)
  (let ((dx (- (car p1) (car p2)))
        (dy (- (cadr p1) (cadr p2))))
    (+ (* dx dx) (* dy dy))))

;; Buscar celdas libres adyacentes pero LEJOS de la base (para no amontonarlas)
(defun agent-cms213-first-free-cell (coord visio)
  (agent-cms213-first-free-cell-rec coord visio nil -1))

(defun agent-cms213-first-free-cell-rec (coord visio mejor mejor-dist)
  (cond
    ((null visio) mejor)
    ((and (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 2)
          (null (nth 3 (car visio)))
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) mejor-dist))
     ;; Preferir celdas más alejadas (pero adyacentes) para no amontonar bollas
     (agent-cms213-first-free-cell-rec coord (cdr visio) (nth 0 (car visio)) 
                                        (agent-cms213-dist2 coord (nth 0 (car visio)))))
    (t (agent-cms213-first-free-cell-rec coord (cdr visio) mejor mejor-dist))))

;; Buscar objetivo pintable con MÁXIMA PRIORIDAD en la base enemiga
(defun agent-cms213-first-paint-target (equip coord visio)
  (let ((base (agent-cms213-buscar-base-en-rango equip coord visio)))
    ;; Si ve la base enemiga EN RANGO DE PINTURA: ATACA SIN DUDARLO
    (if base
        base
      ;; Si no, buscar otros objetivos
      (agent-cms213-first-paint-target-rec equip coord visio))))

;; Buscar SOLO la base enemiga en rango de pintura (d² ≤ 5)
(defun agent-cms213-buscar-base-en-rango (equip coord visio)
  (cond
    ((null visio) nil)
    ((and (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 5)
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (eq (nth 3 (car visio)) 'base)
          (not (eq (nth 4 (car visio)) equip)))
     (nth 0 (car visio)))  ;; ENCONTRADA - ATACAR INMEDIATAMENTE
    (t (agent-cms213-buscar-base-en-rango equip coord (cdr visio)))))

(defun agent-cms213-first-paint-target-rec (equip coord visio)
  (cond
    ((null visio) nil)
    ;; PRIORIDAD 1: Bollas enemigas
    ((and (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 5)
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'bolla)
          (not (eq (nth 4 (car visio)) equip)))
     (nth 0 (car visio)))
    ;; PRIORIDAD 2: Labs sin dueño o enemigos
    ((and (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 5)
          (> (agent-cms213-dist2 coord (nth 0 (car visio))) 0)
          (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'lab)
          (or (null (nth 4 (car visio)))
              (not (eq (nth 4 (car visio)) equip))))
     (nth 0 (car visio)))
    (t (agent-cms213-first-paint-target-rec equip coord (cdr visio)))))

;; Búsqueda AGRESIVA de la base enemiga - exploración inteligente
;; E1 busca a E2 explorando sistemáticamente el sur del mapa
(defun agent-cms213-first-move-target (equip coord visio ronda)
  (let ((base-visible (agent-cms213-trobar-objetivo-estrategico equip coord visio)))
    (cond
      ;; CASO 1: VE LA BASE - PERSEGUIR AGRESIVAMENTE
      (base-visible
       (agent-cms213-celda-hacia-objetivo coord base-visible visio))
      
      ;; CASO 2: NO LA VE - EXPLORAR SISTEMÁTICAMENTE
      ;; Explorar hacia el sur/este del mapa buscando a E2
      (t
       (agent-cms213-explorar-zona-busqueda coord visio ronda)))))

;; Exploración inteligente: buscar moviéndose lo más lejos posible de la posición actual
;; SIN depender de coordenadas hardcodeadas que varían según el mapa
(defun agent-cms213-explorar-zona-busqueda (coord visio ronda)
  (let ((es-esquina (agent-cms213-detectar-esquina coord visio)))
    (cond
      ;; Si estamos en esquina: escapar con un objetivo "random" determinista
      (es-esquina
       (agent-cms213-escapar-esquina coord visio ronda))
      ;; Si no: buscar celdas lejanas para explorar naturalmente
      (t
       (agent-cms213-buscar-celda-maxima-distancia coord visio nil 0)))))

;; Detectar si estamos en una esquina del mapa (coordenadas extremas con pocas celdas libres)
(defun agent-cms213-detectar-esquina (coord visio)
  (let ((visio-size (agent-cms213-contar-celdas visio)))
    ;; Esquina: detección física (pocos vecinos visibles indica límite de mapa)
    (< visio-size 13))) ;; Un punto central en el mapa suele ver ~21 celdas (d² ≤ 20)

;; Contar celdas en visión (para detectar si estamos rodeados de agua)
(defun agent-cms213-contar-celdas (visio)
  (cond
    ((null visio) 0)
    (t (+ 1 (agent-cms213-contar-celdas (cdr visio))))))

;; Escapar de esquina: Primero intentar movimiento aleatorio, si no funciona usar determinista
(defun agent-cms213-escapar-esquina (coord visio ronda)
  ;; Intentar un movimiento muy variado usando ronda + random offset
  (let* ((offset-random (+ (car coord) (cadr coord) (* ronda 7)))
         (angulo-variadov (* offset-random 0.3141592))  ; Más ángulos posibles
         (target-random (list (+ (car coord) (truncate (* 100 (cos angulo-variadov))))
                              (+ (cadr coord) (truncate (* 100 (sin angulo-variadov))))))
         (celda-random (agent-cms213-celda-hacia-objetivo-rec coord target-random visio nil 1000000)))
    ;; Si encontramos movimiento aleatorio válido, usarlo
    (if celda-random
        celda-random
      ;; Si no, usar el patrón determinista alternativo
      (let* ((offset (+ (car coord) (cadr coord)))
             (angulo (* (+ ronda offset) 0.7853)) 
             (target-imag (list (+ (car coord) (truncate (* 100 (cos angulo))))
                                (+ (cadr coord) (truncate (* 100 (sin angulo))))))
             (celda (agent-cms213-celda-hacia-objetivo-rec coord target-imag visio nil 1000000)))
        ;; Si tampoco encuentra nada, buscar la más lejana disponible
        (if celda
            celda
          (agent-cms213-buscar-celda-maxima-distancia coord visio nil 0))))))

(defun agent-cms213-buscar-celda-opuesta (coord visio)
  ;; Buscar la celda más lejana de la posición actual
  (agent-cms213-buscar-celda-maxima-distancia coord visio nil 0))

(defun agent-cms213-buscar-celda-maxima-distancia (coord visio mejor mejor-dist)
  (cond
    ((null visio) mejor)
    ((and (agent-cms213-free-cell-p 'dummy (car visio))
          (<= (agent-cms213-dist2 coord (nth 0 (car visio))) 2))
     ;; Encontrar celdas adyacentes libres que estén MÁS LEJOS del punto actual
     (let ((dist (agent-cms213-dist2 coord (nth 0 (car visio)))))
       (if (> dist mejor-dist)
           (agent-cms213-buscar-celda-maxima-distancia coord (cdr visio) (nth 0 (car visio)) dist)
         (agent-cms213-buscar-celda-maxima-distancia coord (cdr visio) mejor mejor-dist))))
    (t (agent-cms213-buscar-celda-maxima-distancia coord (cdr visio) mejor mejor-dist))))

;; ESTRATEGIA SIMPLE: Solo atacar la base enemiga
;; Buscar SOLO la base enemiga más cercana
(defun agent-cms213-trobar-objetivo-estrategico (equip coord visio)
  (agent-cms213-buscar-base-enemiga-rec equip coord visio nil 100000))

;; Buscar BASES enemigas (ÚNICO OBJETIVO)
(defun agent-cms213-buscar-base-enemiga-rec (equip coord visio mejor mejor-dist)
  (cond
    ((null visio) mejor)
    ((and (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'base)
          (not (eq (nth 4 (car visio)) equip)))
     (let ((dist (agent-cms213-dist2 coord (nth 0 (car visio)))))
       (if (< dist mejor-dist)
           (agent-cms213-buscar-base-enemiga-rec equip coord (cdr visio) (nth 0 (car visio)) dist)
         (agent-cms213-buscar-base-enemiga-rec equip coord (cdr visio) mejor mejor-dist))))
    (t (agent-cms213-buscar-base-enemiga-rec equip coord (cdr visio) mejor mejor-dist))))

;; Encontrar celda libre más cercana al objetivo
;; Si hay objetivo visible, ir hacia él
;; Si no, buscar la celda más lejana del inicio para explorar todo el mapa
(defun agent-cms213-celda-hacia-objetivo (coord-bolla coord-objetivo visio)
  (let ((celda-cerca (agent-cms213-celda-hacia-objetivo-rec coord-bolla coord-objetivo visio nil 100000)))
    ;; Si encontró una celda cerca del objetivo, usarla
    (if celda-cerca
        celda-cerca
      ;; Si no hay nada cerca, moverse hacia el sur (Y mayor) para explorar
      (agent-cms213-celda-exploracion coord-bolla visio))))

(defun agent-cms213-celda-hacia-objetivo-rec (coord-bolla coord-objetivo visio mejor mejor-dist)
  (cond
    ((null visio) mejor)
    ((and (agent-cms213-free-cell-p 'dummy (car visio))
          (<= (agent-cms213-dist2 coord-bolla (nth 0 (car visio))) 2))
     (let* ((celda-coord (nth 0 (car visio)))
            (dist-nueva (agent-cms213-dist2 celda-coord coord-objetivo)))
       (if (< dist-nueva mejor-dist)
           (agent-cms213-celda-hacia-objetivo-rec coord-bolla coord-objetivo (cdr visio) celda-coord dist-nueva)
         (agent-cms213-celda-hacia-objetivo-rec coord-bolla coord-objetivo (cdr visio) mejor mejor-dist))))
    (t (agent-cms213-celda-hacia-objetivo-rec coord-bolla coord-objetivo (cdr visio) mejor mejor-dist))))

;; Estrategia de exploración: Moverse hacia el lado opuesto del mapa
;; E1 busca E2 moviéndose hacia el sur del mapa
;; Esta función ya no se usa - reemplazada por buscar-celda-maxima-distancia

;; Validar que celda es terrain y es pintable
(defun agent-cms213-pintable-p (equip celda)
  (and (eq (nth 1 celda) 'terra)
       (or (null (nth 3 celda))
           (not (eq (nth 4 celda) equip)))))

;; Validar que celda es terrain y está libre
(defun agent-cms213-free-cell-p (equip celda)
  (and (eq (nth 1 celda) 'terra)
       (null (nth 3 celda))))

;; Calcular distancia cuadrada
(defun agent-cms213-dist2 (a b)
  (let* ((dx (- (car a) (car b)))
         (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))

;; AGENT DEFENSIU per E1
(defun agent-cms213 (dades)
  "Agent defensiu: protegeix base, captura labs, ataca enemics."
  (let* ((equip (nth 1 dades))
         (pintura (nth 2 dades))
         (ronda (nth 0 dades))
         (tipus (nth 4 dades))
         (coord (nth 5 dades))
         (tr-pintar (nth 8 dades))
         (tr-moure (nth 9 dades))
         (visio (nth 10 dades))
         (paint-target (agent-cms213-first-paint-target equip coord visio))
         (move-target (agent-cms213-first-move-target equip coord visio ronda))
         (free-cell (agent-cms213-first-free-cell coord visio)))

    (cond
      ;; BASE: crear bollas CON COLORES VARIADOS (r, g, b rotados)
      ((eq tipus 'base)
       (let ((color-elegido (nth (mod ronda 3) '(r g b))))
         (cond
           ;; Crear bolla si hay pintura suficiente y celda libre
           ((and (>= pintura 50) free-cell)
            (list (list 'crea-bolla (list color-elegido free-cell))))
           ;; Si no hay celdas libres, intentar en otra posición
           (t nil))))

      ;; BOLLA: estratègia defensiva con ataque
      ((eq tipus 'bolla)
       (cond
         ;; Prioritat 1: Pintar objetivo (base enemiga > bolles > labs)
         ((and paint-target (< tr-pintar 1))
          (list (list 'pinta (list paint-target))))
         
         ;; Prioritat 2: Movarse hacia base enemiga o celda estratégica
         ((and move-target (< tr-moure 1))
          (list (list 'mou (list move-target))))
         
         ;; No pueden faire res
         (t nil)))

      (t nil))))