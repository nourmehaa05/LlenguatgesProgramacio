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
;;  ------------------- INICIAR PARTIDA -------------------
;; ------------------------------------------------------------------

;; MAPA --> Petit o Gran
(defun iniciar-partida (&optional (mapa-nom 'tiny))
  "crea l'estat inicial, carrega el mapa (tiny o big) i arrenca el bucle principal."
  (let* ((estat0 (crear-estat-inicial))
         (estat1 (carregar-mapa estat0 mapa-nom))
         )
    (jugar-partida estat1)))


; ------------------------------------------------------------------
;;  ------------------- CREAR ESTAT INICIAL -------------------
;; ------------------------------------------------------------------

(defun crear-estat-inicial ()
  "construeix l'estructura base de l'estat de joc."
  (list
   (list 'ronda 1)
   (list 'torn 'e1)
   (list 'mapa (list (list 'dades nil)
                     (list 'amplada 0)
                     (list 'alt 0)))
   ;(list 'bolles nil) ho feim directament desde el mapa
   (list 'next-bolla-id 0)
   (list 'pintura-e1 200)
   (list 'pintura-e2 200)
   (list 'memoria-compartida nil)))


;; ------------------------------------------------------------------
;;  ------------------- CARREGAR MAPA -------------------
;; ------------------------------------------------------------------
(defun carregar-mapa (estat mapa-nom)
  "insereix al camp mapa les dades del mapa triat (tiny o big)."
  (let* ((mapa-dades (cond
                      ((eq mapa-nom 'tiny) tiny-map)
                      ((eq mapa-nom 'big) big-map)
                      (t (error "Mapa desconegut"))))
         (amplada (length (first mapa-dades)))
         (alt (length mapa-dades))
         (mapa (list (list 'dades mapa-dades)
                     (list 'amplada amplada)
                     (list 'alt alt))))
    (substituir-camp 'mapa mapa estat)))


;; ------------------------------------------------------------------
;;  ------------------- MÈTODES AUXILIARS NECESSARIS -------------------
;; ------------------------------------------------------------------

; cream estat nou amb noves dades
(defun substituir-camp (clau valor estat)
  "retorna un estat nou substituint el valor d'una clau concreta."
  (cond
    ((null estat) nil)
    ((eq (caar estat) clau)
     (cons (list clau valor)
           (cdr estat)))
    (t
     (cons (car estat)
           (substituir-camp clau valor (cdr estat))))))




;; ------------------------------------------------------------------
;;  ------------------- JUGAR PARTIDA (BUCLE) -------------------
;; ------------------------------------------------------------------

(defun jugar-partida (estat)
  "bucle principal que executa torns fins arribar a un final."
  (if (es-final estat)
      (qui-ha-guanyat estat)
      (let* (;; 1. incrementar pintura a l'INICI del torn 
             (equip-actual (get-torn-actual estat))
             (estat1 (incrementar-pintura estat equip-actual))

             ;; 2. executar el torn (totes les unitats de l'equip)
             (estat2 (executar-torn estat1))

             ;; 3. canviar d'equip (torn)
             (estat3 (canviar-torn estat2))

             ;; 4. incrementar ronda quan torna a e1 **FEIM AQUESTA COMPARACIÓ PER FER AMPLIACIONS SOBRE LA QUANTITAT D'EQUIPS JUGANT
             (estat4 (if (eq (get-torn-actual estat3) 'e1)
                         (incrementar-ronda estat3)
                         estat3)))
        (jugar-partida estat4))))


;; INCREMENTAM LA PINTURA --> CAMBIAR PQ LO HE PUESTO AL REVES JEJEJEJ !!!!!!!!!!!!!!!!!
; increment de pintura +2 per torn
; increment de pintura +1 per cada laboratori
(defun incrementar-pintura (estat equip)
  "Incrementa la pintura de l'equip: +1 per torn + 2 per cada laboratori."
  (let* ((clau (if (eq equip 'e1) 'pintura-e1 'pintura-e2))
         (labs-capturats (comptar-laboratoris-equip estat equip))
         (increment (+ 1 (* 2 labs-capturats))))
    (incrementar-pintura-rec estat clau increment)))


(defun incrementar-pintura-rec (estat clau increment)
  "Funció recursiva auxiliar que suma l'increment al camp de pintura."
  (cond
    ((null estat) nil)
    ((eq (caar estat) clau)
     (cons (list clau (+ (cadr (car estat)) increment)) (cdr estat)))
    (t
     (cons (car estat) (incrementar-pintura-rec (cdr estat) clau increment)))))


;; CANVIAR TORN DE L'EQUIP
(defun canviar-torn (estat)
  "Canvia el torn de l'equip actual."
  (let* ((torn-actual (cadr (assoc 'torn estat)))
         (nou-torn (if (eq torn-actual 'e1)
                       'e2
                       'e1)))
    
    ;; reconstruïm estat canviant només el camp 'torn
    (canviar-torn-rec estat nou-torn)))

(defun canviar-torn-rec (estat nou-torn)
  "auxiliar recursiu per substituir el camp torn."
  (if (null estat)
      nil
      (let ((element (car estat)))
        (if (eq (car element) 'torn)
            ;; substituïm el camp torn
            (cons (list 'torn nou-torn)
                  (cdr estat))
            ;; deixam igual i continuam recursivament
            (cons element
                  (canviar-torn-rec (cdr estat) nou-torn))))))



;; INCREMENTAR RONDA --> MIRAR SI SE PUEDEN UNIFICAR
(defun incrementar-ronda (estat)
  "incrementa en 1 el comptador de ronda."
  (incrementar-ronda-rec estat))

(defun incrementar-ronda-rec (estat) ; Mirar si es necesario.
  "auxiliar recursiu que modifica el camp ronda."
  (cond
    ((null estat) nil)
    ((eq (caar estat) 'ronda)
     (cons (list 'ronda (+ (cadr (car estat)) 1))
           (cdr estat)))
    (t
     (cons (car estat)
           (incrementar-ronda-rec (cdr estat))))))



;; ------------------------------------------------------------------
;;  ------------------- REVISIÓ FINAL PARTIDA -------------------
;; ------------------------------------------------------------------

(defun es-final (estat)
  "Retorna t si la partida ha acabat (base destruïda o límit de torns)."
  (or (>= (get-ronda-actual estat) 1500)
      (base-destruida estat 'e1)
      (base-destruida estat 'e2)))


;; REVISAR QUI HA GUANYAT
(defun qui-ha-guanyat (estat)
  "decideix el guanyador final de la partida."
  (cond ((base-destruida estat 'e1) 'e2)
        ((base-destruida estat 'e2) 'e1)
        (t (comparar-puntuacio estat))))


(defun base-destruida (estat equip)
  "comprova si la base de l'equip té els tres colors."
  (let* ((mapa (get-mapa-dades estat))
         (base (trobar-base mapa equip)))
    (and base
         (pintada-3-colors (caddr base)))))


(defun comparar-puntuacio (estat)
  "Compara punts per decidir l'equip guanyador."
  (let* ((bolles-e1 (comptar-bolles-equip estat 'e1))
         (bolles-e2 (comptar-bolles-equip estat 'e2))
         (pintura-e1 (cadr (assoc 'pintura-e1 estat)))
         (pintura-e2 (cadr (assoc 'pintura-e2 estat))))

    (cond
      ((> bolles-e1 bolles-e2) 'e1)
      ((> bolles-e2 bolles-e1) 'e2)
      ((> pintura-e1 pintura-e2) 'e1)
      ((> pintura-e2 pintura-e1) 'e2)
      (t (if (= (random 2) 0) 'e1 'e2)))))


;; REVISAR ESTRUCTURA SI SE PUEDE USAR PARA BOLLES
;(terra g base e1 (r g b))
;(terra g bolla e1 (r g b))
(defun te-color (color casella) ; útil para usarla en labs maybe no hace falta pero
  "comprova si una casella conté un color concret."
  (let ((colors (car (last casella))))
    (member color colors)))

(defun pintada-3-colors (casella)
  "verifica si una casella té r, g i b simultàniament."
  (and (te-color 'r casella)
       (te-color 'g casella)
       (te-color 'b casella)))


;; ------------------------------------------------------------------
;;  ------------------- EXECUTAR TORN ------------------- 
;; ------------------------------------------------------------------

(defun executar-torn (estat)
  (let* ((equip (get-torn-actual estat))
         (dades (preparar-dades estat equip))
         (accions (if (eq equip 'e1) ; REVISAM EQUIP --> ALERTA PER SI AFEGIM MÉS EQUIPS!!!!!
                      (agent-e1 dades)
                      (agent-e2 dades)))
         (estat2 (executar-accions accions estat equip)))
    (canviar-torn (actualitzar-ronda estat2))))



(defun get-torn-actual (estat)
  (cadr (assoc 'torn estat)))



(defun canviar-torn (estat)
  (let ((actual (get-torn-actual estat)))
    (substituir-camp
     'torn
     (if (eq actual 'e1) 'e2 'e1)
     estat)))



(defun actualitzar-ronda (estat)
  (if (eq (get-torn-actual estat) 'e2)
      (substituir-camp
       'ronda
       (+ 1 (cadr (assoc 'ronda estat)))
       estat)
      estat))

(defun executar-accions (accions estat equip)
  "processa seqüencialment la llista d'accions del torn."
  (if (null accions)
      estat
      (let* ((accio (car accions))
             (estat2 (if (validar-accio accio estat equip)
                         (executar-accio accio estat equip)
                         estat)))
        (executar-accions (cdr accions) estat2 equip))))



(defun executar-accio (accio estat equip)
  "executa una acció concreta segons el seu nom."
  (let ((nom (car accio))
        (args (cdr accio)))

    (cond

      ((eq nom 'mou)
       (moure-unitat estat equip (car args)))

      ((eq nom 'crea-bolla)
       (crear-bolla estat equip (car args)))

      ((eq nom 'pinta)
       (aplicar-pintura estat equip (car args)))

      ((eq nom 'escriu-memoria)
       (substituir-camp 'memoria-compartida (car args) estat))

      (t estat))))


;; Mètode preparar-dades: prepara el paquet de dades que rep l'agent.  --> REVISAR AQUEST MÈTODE
(defun preparar-dades (estat equip)
  "prepara el paquet de dades que rep l'agent."
  (list
    ;; info global
    (cadr (assoc 'ronda estat))
    equip

    ;; recursos
    (get-pintura-actual estat equip)

    ;; mapa visible (o mapa entero de momento)
    (get-mapa-dades estat)

    ;; memoria
    (cadr (assoc 'memoria-compartida estat))))

;; ------------------------------------------------------------------
;;  ------------------- VALIDAR ACCIONS ------------------- REVISAR!!!!!!!!!!!!!!!!!!!!!!!!!
;; ------------------------------------------------------------------


(defun validar-accio (accio estat equip)
  (let ((nom (car accio))
        (args (cdr accio)))

    (cond
      ((eq nom 'mou) (validar-moure args estat equip))
      ((eq nom 'crea-bolla) (validar-crear-bolla args estat equip))
      ((eq nom 'pinta) (validar-pintar args estat equip))
      ((eq nom 'escriu-memoria) t)
      (t nil))))


;; ------------------------------------------------------------------
;;  ------------------- VALIDAR MOURE -------------------
;; ------------------------------------------------------------------

; Moure:
;; posició vàlida: dins mapa
;; posició lliure: aigua, no lab, no base, no bolla
;; pot moure's: r^moure = 2u^2 (8 caselles adjacents)
;; verificar si trecuperació = 0 (pot fer coses)
(defun validar-moure (args estat equip)
  (let* ((id (car args))
         (dest (cadr args))
         (bolla (trobar-bolla id estat)))

    (and
     ;; 1. existe la bolla
     bolla

     ;; 2. es del equipo correcto --> nose si hacer un método, de momento no
     (eq (cadddr bolla) equip)

     ;; 3. destino válido (dentro mapa)
     (posicio-valida dest estat)

     ;; 4. destino NO agua
     (not (es-aigua dest estat))

     ;; 5. destino libre (no base, lab, bolla)
     (es-posicio-lliure dest estat)

     ;; 6. movimiento válido (adyacente)
     (let ((origen (get-coord-bolla bolla)))
       (member dest (adjacents origen) :test #'equal))

     ;; 7. cooldown
     (< (get-tr-moure bolla) 1))))




;; ------------------------------------------------------------------
;;  ------------------- VALIDAR CREAR BOLLA -------------------
;; ------------------------------------------------------------------

; Crear bolla: Pintura suficient, base no ocupada per costats, dins mapa (per si s'afegeixen mapes futurs en cantons)



;; ------------------------------------------------------------------
;;  ------------------- VALIDAR PINTAR -------------------
;; ------------------------------------------------------------------

; Pintar: Pintura suficient, sí lab, sí base, sí pintura, dins mapa

;; Mètode es-pintable: indica si la casella admet l'acció de pintar.
(defun es-pintable (pos estat)
  "indica si la casella admet l'acció de pintar."
  (let ((c (get-casella (get-mapa-dades estat) pos)))
    (and c
         (member (caddr c) '(base lab bolla)))))




;; ------------------------------------------------------------------
;;  ------------------- EXECUTAR ACCIONS ------------------- 
;; ------------------------------------------------------------------





;; ------------------------------------------------------------------
;;  ------------------- SETTERS: CREAR/MODIFICAR CASELLES -------------------
;; ------------------------------------------------------------------

;; Crea una casella terra buida
(defun crear-casella-terra (color)
  "crea una casella de terra buida amb el color indicat."
  (list 'terra color))

;; Crea una casella d'aigua
(defun crear-casella-agua ()
  "crea una casella d'aigua."
  (list 'agua))

;; Crea una casella amb un laboratori
(defun crear-casella-lab (color equip-capturat)
  "crea una casella amb un laboratori. equip-capturat pot ser nil si no està capturat."
  (if equip-capturat
      (list 'terra color 'lab equip-capturat)
      (list 'terra color 'lab)))

;; Crea una casella amb una base
(defun crear-casella-base (color equip)
  "crea una casella amb una base."
  (list 'terra color 'base equip))

;; Crea una casella amb una bolla
(defun crear-casella-bolla (color id equip colors-pintat tr-pintar tr-moure)
  "crea una casella amb una bolla."
  (list 'terra color 'bolla id equip colors-pintat tr-pintar tr-moure))

;; MODIFICADORS: funcions per canviar propietats

;; Afegeix o canvia la captura d'un laboratori
(defun canviar-lab-equip (casella equip-nou)
  "retorna una nova casella amb el laboratori capturat pel nou equip."
  (if (es-lab-casella-p casella)
      (list 'terra (get-color-terra-casella casella) 'lab equip-nou)
      casella))

;; Canvia els colors pintats d'una bolla
(defun canviar-bolla-colors-pintat (casella colors-nou)
  "retorna una nova casella amb els colors pintats modificats."
  (if (es-bolla-casella-p casella)
      (list 'terra
            (get-color-terra-casella casella)
            'bolla
            (get-bolla-id-casella casella)
            (get-bolla-equip-casella casella)
            colors-nou
            (get-bolla-tr-pintar casella)
            (get-bolla-tr-moure casella))
      casella))

;; Canvia els temps de recuperació d'una bolla
(defun canviar-bolla-temps-recuperacio (casella tr-pintar-nou tr-moure-nou)
  "retorna una nova casella amb els temps de recuperació modificats."
  (if (es-bolla-casella-p casella)
      (list 'terra
            (get-color-terra-casella casella)
            'bolla
            (get-bolla-id-casella casella)
            (get-bolla-equip-casella casella)
            (get-bolla-colors-pintat casella)
            tr-pintar-nou
            tr-moure-nou)
      casella))

;; Canvia el color de la terra
(defun canviar-color-terra (casella color-nou)
  "retorna una nova casella amb el color de terra modificat."
  (if (es-terra-casella casella)
      (let ((element (get-tipus-element casella)))
        (cond
          ((null element) (list 'terra color-nou))
          ((eq element 'lab) (list 'terra color-nou 'lab (get-lab-equip-capturat casella)))
          ((eq element 'base) (list 'terra color-nou 'base (get-base-equip casella)))
          ((eq element 'bolla)
           (list 'terra color-nou 'bolla
                 (get-bolla-id-casella casella)
                 (get-bolla-equip-casella casella)
                 (get-bolla-colors-pintat casella)
                 (get-bolla-tr-pintar casella)
                 (get-bolla-tr-moure casella)))))
      casella))