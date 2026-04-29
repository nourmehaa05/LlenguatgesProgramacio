;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball - Agent Ataque Agresivo
;; Agent Intel·ligent AGRESIVO - Busca y ataca la base enemiga
;; Estratègia: Buscar la base enemiga, atacarla sin parar, explorar TODO el mapa
;; ------------------------------------------------------------------

;; Funcions auxiliars per extreure dades
(defun agent-ataque-ronda (dades) (nth 0 dades))
(defun agent-ataque-equip (dades) (nth 1 dades))
(defun agent-ataque-pintura (dades) (nth 2 dades))
(defun agent-ataque-id (dades) (nth 3 dades))
(defun agent-ataque-tipus (dades) (nth 4 dades))
(defun agent-ataque-coord (dades) (nth 5 dades))
(defun agent-ataque-colors (dades) (nth 6 dades))
(defun agent-ataque-color-propi (dades) (nth 7 dades))
(defun agent-ataque-tr-pintar (dades) (nth 8 dades))
(defun agent-ataque-tr-moure (dades) (nth 9 dades))
(defun agent-ataque-visio (dades) (nth 10 dades))
(defun agent-ataque-memoria (dades) (nth 11 dades))

;; =====================================================
;; CÁLCULO DE DISTANCIAS
;; =====================================================

(defun agent-ataque-dist2 (p1 p2)
  "Calcula distancia cuadrada entre dos puntos."
  (let* ((dx (- (car p1) (car p2)))
         (dy (- (cadr p1) (cadr p2))))
    (+ (* dx dx) (* dy dy))))

;; =====================================================
;; BÚSQUEDA DE OBJETIVOS - ENFOQUE TOTAL EN BASE ENEMIGA
;; =====================================================

;; Buscar SOLO la base enemiga más cercana (OBJETIVO ÚNICO)
(defun agent-ataque-buscar-base-enemiga (equip coord visio)
  (agent-ataque-buscar-base-enemiga-rec equip coord visio nil 999999))

(defun agent-ataque-buscar-base-enemiga-rec (equip coord visio mejor mejor-dist)
  (cond
    ((null visio) mejor)
    ((and (eq (nth 1 (car visio)) 'terra)
          (eq (nth 3 (car visio)) 'base)
          (not (eq (nth 4 (car visio)) equip)))
     ;; ENCONTRADA BASE ENEMIGA
     (let ((dist (agent-ataque-dist2 coord (nth 0 (car visio)))))
       (if (< dist mejor-dist)
           (agent-ataque-buscar-base-enemiga-rec equip coord (cdr visio) (nth 0 (car visio)) dist)
         (agent-ataque-buscar-base-enemiga-rec equip coord (cdr visio) mejor mejor-dist))))
    (t (agent-ataque-buscar-base-enemiga-rec equip coord (cdr visio) mejor mejor-dist))))

;; Si ve la base enemiga dentro del rango de pintura, ATACAR INMEDIATAMENTE
(defun agent-ataque-base-en-rango-pintura (equip coord visio)
  (cond
    ((null visio) nil)
    ((and (<= (agent-ataque-dist2 coord (nth 0 (car visio))) 5)
          (> (agent-ataque-dist2 coord (nth 0 (car visio))) 0)
          (eq (nth 3 (car visio)) 'base)
          (not (eq (nth 4 (car visio)) equip)))
     (nth 0 (car visio)))
    (t (agent-ataque-base-en-rango-pintura equip coord (cdr visio)))))

;; =====================================================
;; EXPLORACIÓN - BUSCAR BASE + EVITAR ESQUINAS
;; =====================================================

;; Estrategia de movimiento: si ve la base, perseguirla; si no, explorar
(defun agent-ataque-calcular-movimiento (equip coord visio ronda)
  (let ((base-visible (agent-ataque-buscar-base-enemiga equip coord visio)))
    (cond
      ;; CASO 1: VE LA BASE ENEMIGA - PERSEGUIR AGRESIVAMENTE
      (base-visible
       (agent-ataque-moverse-hacia-objetivo coord base-visible visio))
      
      ;; CASO 2: NO LA VE - EXPLORAR SISTEMÁTICAMENTE SIN QUEDARSE EN ESQUINAS
      (t
       (agent-ataque-explorar-inteligente coord visio ronda)))))

;; Moverse hacia un objetivo (la base enemiga)
(defun agent-ataque-moverse-hacia-objetivo (coord-actual coord-objetivo visio)
  (agent-ataque-moverse-hacia-objetivo-rec coord-actual coord-objetivo visio nil 999999))

(defun agent-ataque-moverse-hacia-objetivo-rec (coord-actual coord-objetivo visio mejor mejor-dist)
  (cond
    ((null visio) mejor)
    ((and (agent-ataque-es-libre (car visio))
          (<= (agent-ataque-dist2 coord-actual (nth 0 (car visio))) 2))
     ;; Celda adyacente libre
     (let ((dist (agent-ataque-dist2 (nth 0 (car visio)) coord-objetivo)))
       (if (< dist mejor-dist)
           (agent-ataque-moverse-hacia-objetivo-rec coord-actual coord-objetivo (cdr visio) 
                                                   (nth 0 (car visio)) dist)
         (agent-ataque-moverse-hacia-objetivo-rec coord-actual coord-objetivo (cdr visio) 
                                                   mejor mejor-dist))))
    (t (agent-ataque-moverse-hacia-objetivo-rec coord-actual coord-objetivo (cdr visio) mejor mejor-dist))))

;; Exploración inteligente: detecta esquinas y evita quedarse atascado
(defun agent-ataque-explorar-inteligente (coord visio ronda)
  (let ((es-esquina (agent-ataque-detectar-esquina coord visio)))
    (cond
      ;; Si está en esquina: ESCAPAR CON URGENCIA
      (es-esquina
       (agent-ataque-escapar-esquina-urgente coord visio ronda))
      ;; Si no está en esquina: buscar celdas lejanas para explorar
      (t
       (agent-ataque-buscar-celda-exploracion coord visio)))))

;; Detectar si estamos en una esquina del mapa
(defun agent-ataque-detectar-esquina (coord visio)
  (let ((x (car coord))
        (y (cadr coord))
        (celdas-libres (agent-ataque-contar-celdas-libres visio)))
    ;; Es esquina si: coordenadas extremas Y muy pocas celdas libres (rodeados de agua)
    (and (< celdas-libres 5)  ;; Muy pocas celdas disponibles = atascado
         (or (and (< x 2) (< y 2))       ;; Esquina superior izquierda
             (and (< x 2) (> y 17))      ;; Esquina inferior izquierda
             (and (> x 17) (< y 2))      ;; Esquina superior derecha
             (and (> x 17) (> y 17)))))) ;; Esquina inferior derecha

;; Contar celdas libres en la visión
(defun agent-ataque-contar-celdas-libres (visio)
  (cond
    ((null visio) 0)
    ((agent-ataque-es-libre (car visio))
     (+ 1 (agent-ataque-contar-celdas-libres (cdr visio))))
    (t (agent-ataque-contar-celdas-libres (cdr visio)))))

;; Escapar de esquina de forma determinista basada en ronda
;; Usa un patrón que varía según la ronda para evitar quedarse atascado
(defun agent-ataque-escapar-esquina-urgente (coord visio ronda)
  (let* ((movimientos (agent-ataque-generar-movimientos-escape ronda))
         (celda-escape (agent-ataque-buscar-primera-celda-valida coord visio movimientos)))
    (if celda-escape
        celda-escape
      ;; Fallback: cualquier celda libre adyacente
      (agent-ataque-buscar-celda-adyacente-libre coord visio))))

;; Generar patrón de movimientos basado en ronda (pseudo-aleatorio determinista)
(defun agent-ataque-generar-movimientos-escape (ronda)
  (let ((patron (mod ronda 4)))
    (cond
      ((= patron 0) '((1 0) (0 1) (-1 0) (0 -1)))   ;; Derecha, abajo, izquierda, arriba
      ((= patron 1) '((0 1) (-1 0) (0 -1) (1 0)))   ;; Abajo, izquierda, arriba, derecha
      ((= patron 2) '((-1 0) (0 -1) (1 0) (0 1)))   ;; Izquierda, arriba, derecha, abajo
      (t             '((0 -1) (1 0) (0 1) (-1 0)))))) ;; Arriba, derecha, abajo, izquierda

;; Buscar celda válida en la dirección de escape
(defun agent-ataque-buscar-primera-celda-valida (coord visio movimientos)
  (cond
    ((null movimientos) nil)
    (t
     (let ((celda-en-dir (agent-ataque-celda-en-direccion coord (car movimientos) visio)))
       (if celda-en-dir
           celda-en-dir
         (agent-ataque-buscar-primera-celda-valida coord visio (cdr movimientos)))))))

;; Buscar una celda en una dirección específica
(defun agent-ataque-celda-en-direccion (coord direccion visio)
  (let ((dx (car direccion))
        (dy (cadr direccion))
        (target-x (+ (car coord) (car direccion)))
        (target-y (+ (cadr coord) (cadr direccion))))
    (agent-ataque-buscar-celda-en-visio (list target-x target-y) visio)))

;; Buscar una coordenada específica en la visión
(defun agent-ataque-buscar-celda-en-visio (target-coord visio)
  (cond
    ((null visio) nil)
    ((and (equal (nth 0 (car visio)) target-coord)
          (agent-ataque-es-libre (car visio)))
     target-coord)
    (t (agent-ataque-buscar-celda-en-visio target-coord (cdr visio)))))

;; Buscar cualquier celda adyacente libre
(defun agent-ataque-buscar-celda-adyacente-libre (coord visio)
  (cond
    ((null visio) nil)
    ((and (agent-ataque-es-libre (car visio))
          (<= (agent-ataque-dist2 coord (nth 0 (car visio))) 2))
     (nth 0 (car visio)))
    (t (agent-ataque-buscar-celda-adyacente-libre coord (cdr visio)))))

;; Buscar celda para exploración: la más lejana disponible
(defun agent-ataque-buscar-celda-exploracion (coord visio)
  (agent-ataque-buscar-celda-exploracion-rec coord visio nil -1))

(defun agent-ataque-buscar-celda-exploracion-rec (coord visio mejor mejor-dist)
  (cond
    ((null visio) mejor)
    ((and (agent-ataque-es-libre (car visio))
          (<= (agent-ataque-dist2 coord (nth 0 (car visio))) 2))
     ;; Buscar la celda adyacente que esté MÁS LEJOS de la posición actual
     ;; Para explorar hacia nuevas áreas
     (let ((dist (agent-ataque-dist2 coord (nth 0 (car visio)))))
       (if (> dist mejor-dist)
           (agent-ataque-buscar-celda-exploracion-rec coord (cdr visio) (nth 0 (car visio)) dist)
         (agent-ataque-buscar-celda-exploracion-rec coord (cdr visio) mejor mejor-dist))))
    (t (agent-ataque-buscar-celda-exploracion-rec coord (cdr visio) mejor mejor-dist))))

;; =====================================================
;; VALIDACIONES DE CELDAS
;; =====================================================

;; Verificar si una celda es terrain libre (sin nada)
(defun agent-ataque-es-libre (celda)
  (and (eq (nth 1 celda) 'terra)
       (null (nth 3 celda))))

;; Verificar si una celda es pintable
(defun agent-ataque-es-pintable (equip celda)
  (and (eq (nth 1 celda) 'terra)
       (or (null (nth 3 celda))
           (not (eq (nth 4 celda) equip)))))

;; =====================================================
;; AGENTE PRINCIPAL - ATAQUE AGRESIVO
;; =====================================================

(defun agent-ataque (dades)
  "Agent ataque: Busca y ataca SOLO la base enemiga. Explora si no la ve. Evita esquinas."
  (let* ((equip (agent-ataque-equip dades))
         (pintura (agent-ataque-pintura dades))
         (ronda (agent-ataque-ronda dades))
         (tipus (agent-ataque-tipus dades))
         (coord (agent-ataque-coord dades))
         (tr-pintar (agent-ataque-tr-pintar dades))
         (tr-moure (agent-ataque-tr-moure dades))
         (visio (agent-ataque-visio dades))
         
         ;; Buscar base enemiga en rango de pintura (MÁXIMA PRIORIDAD)
         (base-atacable (agent-ataque-base-en-rango-pintura equip coord visio))
         
         ;; Calcular movimiento (hacia base o exploración)
         (move-target (agent-ataque-calcular-movimiento equip coord visio ronda))
         
         ;; Buscar celda libre para crear bollas (solo en base)
         (free-cell (agent-ataque-buscar-celda-adyacente-libre coord visio)))

    (cond
      ;; ========== BASE ==========
      ((eq tipus 'base)
       (cond
         ;; Crear bollas agresivas si hay pintura y espacio
         ((and (>= pintura 50) free-cell)
          ;; Rotar colores para crear variedad
          (let ((color (nth (mod ronda 3) '(r g b))))
            (list (list 'crea-bolla (list color free-cell)))))
         (t nil)))

      ;; ========== BOLLA - ATAQUE AGRESIVO ==========
      ((eq tipus 'bolla)
       (cond
         ;; PRIORIDAD MÁXIMA: Atacar base enemiga si está en rango
         ((and base-atacable (< tr-pintar 1))
          (list (list 'pinta (list base-atacable))))
         
         ;; PRIORIDAD 2: Moverse hacia la base o explorar
         ((and move-target (< tr-moure 1))
          (list (list 'mou (list move-target))))
         
         ;; No puede hacer nada (cooldown)
         (t nil)))

      ;; Tipo desconocido
      (t nil))))
