;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; <Descripció de les funcions d'aquest fitxer>

;; Necessari per a l'optimització de crides recursives.
; (load 'common) ; https://almy.us/files/xl305req.zip
; (load 'tco)    ; https://github.com/antoni-oliver/defun-tco

;; Altres fitxers de la pràctica:
; (load 'grafics)
; (load 'agent-abc123)
; (load 'agent-xyz999)

;; ------------------------------------------------------------------
;;  ------------------- HOLA -------------------
;; ------------------------------------------------------------------

;; Documentació d'això...
(defun inici ()
    "Punt d'entrada del programa."
    (color 0 0 0 255 255 255) ; Compatibilitat Windows-Unix: fons blanc, línies i text negres.
    (mode 0 0 640 375)        ; Compatibilitat Windows-Unix: configura la finestra de joc per a Unix segons la de Windows.
    (move 300 167)            ; Pintam un quadrat enmig de la finestra.
    (quadrat 20)
    t)

;; Documentació d'això...
(defun quadrat (mida)
    (drawrel 0 mida)
    (drawrel mida 0)
    (drawrel 0 (- mida))
    (drawrel (- mida) 0))


;; ------------------------------------------------------------------
;;  ------------------- INICIAR PARTIDA -------------------
;; ------------------------------------------------------------------

(defun iniciar-partida (&optional (mapa-nom 'tiny) (aleatori nil))
  "Inicia la partida amb mapa petit o gran i opcionalment posicions aleatories."
  (let* ((estat0 (crear-estat-inicial))
         (estat1 (carregar-mapa estat0 mapa-nom))
         (estat2 (inicialitzar-elements estat1 (if aleatori 'aleatori 'classic))))
    (jugar-partida estat2)))

;; ------------------------------------------------------------------
;;  ------------------- CREAR ESTAT INICIAL -------------------
;; ------------------------------------------------------------------

(defun crear-estat-inicial ()
  "Crea l'estat inicial del joc."
  (list
   (list 'ronda 1)
   (list 'torn 'e1)
   (list 'mapa (list (list 'dades nil)
                     (list 'amplada 12)  ; default, s'actualitzarà en carregar-mapa
                     (list 'alt 12)))     ; default, s'actualitzarà en carregar-mapa
   (list 'bases nil)
   (list 'laboratoris nil)
   (list 'unitats '())
   (list 'next-bolla-id 0)
   (list 'pintura-e1 200)
   (list 'pintura-e2 200)))

;; ------------------------------------------------------------------
;;  ------------------- INICIALITZAR ELEMENTS -------------------
;; ------------------------------------------------------------------

(defun inicialitzar-elements (estat tipus)
  "Col·loca les bases i laboratoris segons el tipus de partida: classic o aleatori."
  (let* ((estat1 (crear-bases estat tipus))
         (estat2 (crear-laboratoris estat1 tipus)))
    estat2))

;; Bases
(defun obtenir-posicions-bases (estat tipus)
  (cond
    ((eq tipus 'classic) (posicions-bases-classic))
    ((eq tipus 'aleatori) (posicions-bases-aleatori estat))))

(defun crear-bases (estat tipus)
  (let* ((posicions (obtenir-posicions-bases estat tipus))
         (bases (list (crear-base 'e1 (first posicions))
                      (crear-base 'e2 (second posicions)))))
    (subst (list 'bases bases)
           (assoc 'bases estat)
           estat)))

(defun crear-base (equip coord)
  (list
   (list 'equip equip)
   (list 'coord coord)
   (list 'colors-pintat '())))

;; Laboratoris
(defun obtenir-posicions-labs (estat tipus)
  (if (eq tipus 'classic)
      (posicions-labs-classic)
      (posicions-labs-aleatori estat)))

(defun crear-laboratoris (estat tipus)
  (let ((labs (mapcar #'crear-lab (obtenir-posicions-labs estat tipus))))
    (subst (list 'laboratoris labs)
           (assoc 'laboratoris estat)
           estat)))

(defun crear-lab (coord)
  (list (list 'coord coord)
        (list 'equip nil)))


;; ------------------------------------------------------------------
;;  ------------------- POSICIONS FIXES I ALEATÒRIES -------------------
;; ------------------------------------------------------------------
(defun posicions-bases-classic () '((1 1) (10 10))) ; Revisar que en ambos mapas funcionen.

(defun posicions-bases-aleatori (estat)
  (list (coord-lliure-aleatoria estat)
        (coord-lliure-aleatoria estat)))

(defun posicions-labs-classic ()
  '((5 5) (7 3) (2 8) (3 4) (6 7) (8 2) (1 9) (9 1) (4 6) (10 5))) ; Revisar que en ambos mapas funcionen.

(defun posicions-labs-aleatori (estat)
  (loop repeat 10
        collect (coord-lliure-aleatoria estat)))

(defun coord-lliure-aleatoria (estat) ; POTSER en el mapa gran tardi molt la recursió, REVISAR!!!
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (amplada (cadr (assoc 'amplada mapa)))
         (alt (cadr (assoc 'alt mapa)))
         (coord (list (random amplada) (random alt))))
    (if (es-posicio-lliure coord estat)
        coord
        (coord-lliure-aleatoria estat))))

;; ------------------------------------------------------------------
;;  ------------------- CARREGAR MAPA -------------------
;; ------------------------------------------------------------------
(defun carregar-mapa (estat mapa-nom)
  "Carrega el mapa indicat ('tiny o 'big) dins l'estat."
  (let ((mapa-dades (cond
                     ((equal mapa-nom 'tiny) tiny-map)
                     ((equal mapa-nom 'big) big-map)
                     (t (error "Mapa desconegut"))))
        (amplada (length (first mapa-dades)))
        (alt (length mapa-dades)))
    (subst (list 'mapa (list (list 'dades mapa-dades)
                             (list 'amplada amplada)
                             (list 'alt alt)))
           (assoc 'mapa estat)
           estat)))


;; ------------------------------------------------------------------
;;  ------------------- JUGAR PARTIDA (BUCLE) -------------------
;; ------------------------------------------------------------------

(defun jugar-partida (estat)
  "Executa la partida fins que finalitza."
  (if (es-final estat)
      (qui-ha-guanyat estat)
      (let* ((estat1 (executar-torn estat))
             (estat2 (incrementar-pintura estat1 (get-torn-actual estat1)))
             (estat3 (canviar-equip estat2))
             (estat4 (if (eq (get-torn-actual estat3) 'e1)
                         (incrementar-ronda estat3)
                         estat3)))
        (jugar-partida estat4))))



(defun incrementar-ronda (estat)
  (let ((ronda (cadr (assoc 'ronda estat))))
    (subst (list 'ronda (+ ronda 1))
           (assoc 'ronda estat)
           estat)))



(defun es-final (estat)
  (or (>= (get-ronda-actual estat) 1500) ; hem arribat a 1500 torns
      (base-destruida estat 'e1) ; o bé, qualque base ha estat destruida
      (base-destruida estat 'e2)))



(defun qui-ha-guanyat (estat)
  (cond ((base-destruida estat 'e1) 'e2)
        ((base-destruida estat 'e2) 'e1)
        (t (comparar-puntuacio estat))))


(defun base-destruida (estat equip)
  (let ((base (find equip (cadr (assoc 'bases estat))
                    :key (lambda (b) (cadr (assoc 'equip b))))))
    (and base
         (let ((colors (cadr (assoc 'colors-pintat base))))
           (and (member 'r colors)
                (member 'g colors)
                (member 'b colors))))))


(defun comparar-puntuacio (estat) ;; MILLORAR PQ REMOVE-IF-NOT nose si està molt bé!!!
  "Compara punts per decidir l'equip guanyador."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         ;; compta bolles vives per equip
         (bolles-e1 (length (remove-if-not (lambda (b) (eq (cadr (assoc 'equip b)) 'e1))
                                           unitats)))
         (bolles-e2 (length (remove-if-not (lambda (b) (eq (cadr (assoc 'equip b)) 'e2))
                                           unitats)))
         ;; pintura restant per equip
         (pintura-e1 (cadr (assoc 'pintura-e1 estat)))
         (pintura-e2 (cadr (assoc 'pintura-e2 estat))))
    ;; decidir guanyador
    (cond
      ((> bolles-e1 bolles-e2) 'e1)
      ((> bolles-e2 bolles-e1) 'e2)
      ((> pintura-e1 pintura-e2) 'e1)
      ((> pintura-e2 pintura-e1) 'e2)
      (t (if (= (random 2) 0) 'e1 'e2)))))




;; ------------------------------------------------------------------
;;  ------------------- JUGAR PARTIDA -------------------
;; ------------------------------------------------------------------

; executar torn --> bucle per unitat I L'AGENT ENS VA DIGUENT EL QUE FEIM
(defun executar-torn()
    ; bucle de MENTRE QUEDIN UNITATS PER ACTUAR, o bé l'agent retorni fals de que no vol actuar
    ; cridam a rebre moviment agent
    ; cridam a executar accio --> si retorna fals es que no era possible executar el moviment
    ; si TODAS LAS UNIDADES YA ACTUARON --> SALIR
)

; rebre moviment agent  d'una unitat en concret
(defun rebre-accio-agent()
    

)

; esta la haria mas adelante 
(defun validar-accio()
    "Valida si la unitat ja ha actuat."
    ;; LAS SIGUIENTES COSAS NOSE SI LAS VALIDA EL AGENTE O NOSOTROS, YA VEREMOS.
    ;; si la unitat ja ha actuat
    ;; si la bolla té pintura
    ;; si la bolla es pot moure a x casella...

)

(defun executar-accio(accio)
    ;  SWITCH identificar accio i cridar al mètode.
    ; Moure bolla
    ; Crear bolla
    ; Pintar --> bolla, lab, base
)




;; ------------------------------------------------------------------
;;  ------------------- GETTERS DE L'ESTAT ACTUAL -------------------
;; ------------------------------------------------------------------

(defun get-estat-actual()
    *estat*
)

(defun get-ronda-actual()
    (cadr (assoc 'ronda *estat*))
)

(defun  get-torn-actual()
    (cadr (assoc 'torn *estat*))
)

(defun get-pintura-actual (equip)
  "Retorna la pintura actual de l'equip 'e1 o 'e2."
  (cadr
    (assoc
      (cond
        ((eq equip 'e1) 'pintura-e1)
        ((eq equip 'e2) 'pintura-e2))
      *estat*))
)

(defun get-bolla-id()

)

;; GET COLORS PINTATS (bolla, base)
(defun get-colors-pintats(unitat)

)

;; get suma bolles per equip
(defun get-suma-bolles-equip(equip)

)

;; get suma laboratoris per equip
(defun get-suma-laboratoris-equip(equip)

)


;; get temps recuperacio bolla id
(defun get-temps-recuperacio()

)


;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE TORNS ------------------- 
;; ------------------------------------------------------------------



(defun canviar-equip ()
  (setf (cadr (assoc 'torn *estat*))
        (cond
          ((eq (get-torn-actual) 'e1) 'e2)
          ((eq (get-torn-actual) 'e2) 'e1)))
)



; increment de pintura +1 per torn
; increment de pintura +2 per cada laboratori
(defun incrementar-pintura (equip)
  "Incrementa la pintura de l'equip segons torn i laboratoris capturats."
  (let* ((clau (cond ((eq equip 'e1) 'pintura-e1)
                     ((eq equip 'e2) 'pintura-e2)))
         (labs (length (remove-if-not
                        (lambda (lab) (eq (cadr (assoc 'equip lab)) equip))
                        (cadr (assoc 'laboratoris *estat*)))))
         (sum (+ 2 labs)))  ; pintura a afegir
    (setf (cadr (assoc clau *estat*))
          (+ (cadr (assoc clau *estat*)) sum)))
)


(defun decrementar-pintura(quantitat, equip)
    "Decrementa certa quantitat de pintura de l'equip"
)


;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA ------------------- NOUR!!!!!!!!!!!!
;; ------------------------------------------------------------------

;; FUNCIONS MATRICIALS
(defun es-laboratori()

)

(defun es-base-enemiga()

)

(defun es-aigua()

)

(defun es-bolla-enemiga()

)

(defun es-bolla-amiga() ; nsoe si aquest mètode és necessari pero per si de cas.

)


(defun es-posicio-lliure (coord estat) ; creo que esta bien
  (let ((x (first coord))
        (y (second coord))
        (mapa (cadr (assoc 'mapa estat))))
    (and
      ;; dentro de los límites
      (and (>= x 1) (<= x (length mapa))
           (>= y 1) (<= y (length (first mapa))))
      ;; terreno válido
      (not (es-aigua coord estat))
      ;; no hay base
      (not (member coord
                   (mapcar (lambda (b) (cadr (assoc 'coord b)))
                           (cadr (assoc 'bases estat)))
                   :test #'equal))
      ;; no hay laboratorio
      (not (member coord
                   (mapcar (lambda (l) (cadr (assoc 'coord l)))
                           (cadr (assoc 'laboratoris estat)))
                   :test #'equal))
      ;; no hay unidad
      (not (member coord
                   (mapcar (lambda (u) (cadr (assoc 'coord u)))
                           (cadr (assoc 'unitats estat)))
                   :test #'equal)))))



;; FUNCIONS DE MODIFICACIÓ DE MAPA --> potser vagin dins grafis.lsp!!!!
(defun pintar-casella(coord, color)
    "Pinta la casella enemiga."

)

;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ ACCIONS PER TORN -------------------
;; ------------------------------------------------------------------


(defun crear-bolla (estat equip) ; creo que está bien
    "Crea una bolla des de la base de l'equip en la primera posició lliure adjacent"
  (let ((id (get-next-id estat))
        (coord (buscar-posicio-lliure-base estat equip)))

    (if (null coord)
        estat

        (let* ((bolla
                (list
                 (list 'id id)
                 (list 'equip equip)
                 (list 'coord coord)
                 (list 'pintura 10)
                 (list 'cooldown 0)
                 (list 'ha-actuat nil)))

               (unitats (cadr (assoc 'unitats estat)))
               (noves-unitats (cons bolla unitats))

               (estat1
                (subst (list 'unitats noves-unitats)
                       (assoc 'unitats estat)
                       estat)))

          (incrementar-next-id estat1)))))

(defun moure-bolla()
    ; llamar a funciones --> revisar si moviment vàlid
    ; llamar a funciones: moure bolla, incrementar temps recuperacio

)

(defun pintar-bolla()

)

(defun pintar-laboratori()
)

(defun pintar-base()

)







;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE UNITATS -------------------
;; ------------------------------------------------------------------

;; LABORATORIS

(defun es-laboratori-ocupat(lab)

)

(defun es-laboratori-meu(equip, lab)
    "Revisa si el laboratori que li passam "
)

(defun canviar-equip-laboratori()

)





;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE BOLLES ------------------- NOUR!!!
;; ------------------------------------------------------------------


; incrementar temps recuperacio
(defun incrementar-temps-recuperacio()

)

; decrementar temps recuperacio
(defun decrementar-temps-recuperacio()

)

(defun validar-moviment()

)

(defun get-next-id (estat) ; creo que esta bien
  (cadr (assoc 'next-bolla-id estat)))


(defun incrementar-next-id (estat) ; creo que esta bien

  (let ((id (get-next-id estat)))

    (subst (list 'next-bolla-id (+ id 1))
           (assoc 'next-bolla-id estat)
           estat)))