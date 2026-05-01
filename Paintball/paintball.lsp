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
;;  ------------------- HELPER FUNCTIONS -------------------
;; ------------------------------------------------------------------

;; Funció per substituir un camp a l'alist de l'estat
(defun substituir-camp (clau valor estat)
  "Substitueix el valor d'una clau a l'alist de l'estat."
  (cond
    ((null estat) nil)
    ((eq (caar estat) clau)
     (cons (list clau valor)
           (cdr estat)))
    (t
     (cons (car estat)
           (substituir-camp clau valor (cdr estat))))))


;; EXTRAS QUE MG

; *Anar endavant o enrere amb les accions
; límit 1500 torns (HA DE DURAR <2 MINUTS)
; mapes 60x60
; memoria-compartida ????
; bolla pinta també la casella


; ------------------------------------------------------------------
;;  ------------------- INICIAR PARTIDA -------------------
;; ------------------------------------------------------------------
(defun carrega-mapa (nom)
  "Carrega un fitxer .map de la carpeta maps/ pel seu nom."
  (let* ((ruta (concatenate 'string "maps/" nom ".map"))
         (fitxer (open ruta :direction :input)))
    (cond
      ((null fitxer)
       (princ "Error: no s'ha trobat el mapa ")
       (princ ruta)
       (terpri)
       nil)
      (t
       (let ((mapa (read fitxer)))
         (close fitxer)
         mapa)))))

(defun inici-mapa (nom-mapa)
  "Carrega el mapa pel nom i comença la partida."
  (color 0 0 0 255 255 255) ; Compatibilitat Windows-Unix: fons blanc, línies i text negres.
  (mode 0 0 640 375)        ; Compatibilitat Windows-Unix: configura la finestra de joc per a Unix segons la de Windows.
  (let* ((mapa (carrega-mapa nom-mapa))
         (estat (cond (mapa (crear-estat-inicial mapa)) (t nil))))
    (cond (estat (jugar-partida-inicial estat)) (t nil))))

(defun inici ()
  (inici-mapa "tiny"))  ;; o el que vulguis per defecte


; ------------------------------------------------------------------
;;  ------------------- GESTIÓ MAPA UNITATS -------------------
;; ------------------------------------------------------------------
; Carregam el mapa (opció de crear diferents mapes).
; terra --> están pintades d'un color i es pot canviar
; aigua --> no fer res
(defun carregar-mapa (mapa-nom)
  "Retorna la matriu del mapa seleccionat."
  (cond
    ((eq mapa-nom 'tiny) tiny-map) ; LUEGO AÑADIR MAS MAPAS.
    ((eq mapa-nom 'big) big-map)
    (t nil)))

(defun extraer-unitats (mapa x y next-id)
  (cond
    ((null mapa) (list nil next-id))
    (t
     (let* (
            (res-fila (extraer-unitats-fila (car mapa) x y next-id))
            (unitats-fila (car res-fila))
            (next-id2 (cadr res-fila))
            (res-resto (extraer-unitats (cdr mapa) 0 (+ y 1) next-id2))
            (unitats-resto (car res-resto))
            (next-id3 (cadr res-resto))
           )
       (list
        (append unitats-fila unitats-resto)
        next-id3)))))

(defun extraer-unitats-fila (fila x y next-id)
  (cond
    ((null fila) (list nil next-id))
    (t
     (let* (
            (celda (car fila))
            (res-resto (extraer-unitats-fila (cdr fila) (+ x 1) y next-id))
            (unitats-resto (car res-resto))
            (next-id2 (cadr res-resto))
           )
       (cond
         ((and (eq (car celda) 'terra)
               (member 'base celda))
          ;; CREAR BASE
          (let ((equip (cadr (member 'base celda))))
            (list
             (cons
              (crear-base next-id2 equip (list x y))
              unitats-resto)
             (+ next-id2 1))))
         ;; NO HAY BASE
         (t
          (list unitats-resto next-id2)))))))


(defun crear-base (id equip coord)
  (list
   'unitat
   id
   'base
   equip
   coord
   nil        ; color-propi
   nil        ; colors-pintat
   0          ; tr-pintar (no usado para bases)
   0          ; tr-moure (no usado para bases)
   0))        ; tr-crear (cooldown de crear bolla)


  


; ------------------------------------------------------------------
;;  ------------------- CREAR ESTAT INICIAL -------------------
;; ------------------------------------------------------------------
; 200 pintura inicial
(defun crear-estat-inicial (mapa)
  "Construeix l'estat inicial complet a partir del mapa."
  (let* (
         (res (extraer-unitats mapa 0 0 0))
         (unitats (car res))
         (next-id (cadr res))
        )
    (list
     (list 'ronda 1)
     (list 'torn 'e1)

     (list 'mapa mapa)

     (list 'unitats unitats)

     (list 'next-id next-id)

     (list 'pintura-e1 200)
     (list 'pintura-e2 200)

     (list 'memoria-e1 nil)
     (list 'memoria-e2 nil))))

; estructura unitats:
; (unitat id tipus equip coord color-propi colors tr-pintar tr-moure tr-crear)
; BOLLES:
; (unitat id tipus equip coord color-propi colors tr-pintar tr-moure nil)
; BASES:
; (unitat id tipus equip coord ----------- colors --- nil --- nil --- tr-crear)
; ------------------------------------------------------------------
;;  ------------------- BUCLE PARTIDA -------------------
;; ------------------------------------------------------------------

; - Comprobar final partida
; - (-1) temps recuperació/pintar per unitat
;       - base: temps rec
;       - bolla: temps rec (moure,p)
; - Sumar pintura --> +2 pintura, +1 pintura*lab
; - Recorrer unidades (para ir ejecutando unidad x unidad por turno)

;; Aquest mètode es fa per no incrementar la pintura ni decrementar
;; els temps a la primera ronda.
(defun jugar-partida-inicial (estat)
  "Primera ronda de la partida."
  (color 0 0 0 255 255 255)
  (cls)

  (cond
      ((final-partida-p estat)
       (finalitzar-partida estat))
      (t
       (let* ((estat1 (executar-torn estat))
              (estat2 (seguent-torn estat1)))
         (pinta estat1)
         (cond
           ((final-partida-p estat2)
            (finalitzar-partida estat2))
           (t
            (let* ((estat3 (executar-torn estat2))
                   (estat4 (seguent-torn estat3)))
              (pinta estat4)
              (jugar-partida estat4))))))))

(defun jugar-partida (estat)
  "Bucle principal de la partida."
  (cond
    ((final-partida-p estat)
     (finalitzar-partida estat))

    (t
     (let* (
            (estat1 (actualitzar-pintura estat))
            (estat2 (baixar-cooldowns estat1))
            (estat3 (executar-torn estat2))
            (estat4 (seguent-torn estat3))
           )
      (sleep 0.05) ;; Pausa para controlar la velocidad del juego
      (pinta estat4)
       (jugar-partida estat4)))))






; ------------------------------------------------------------------
;;  ------------------- CONTROL FINAL PARTIDA -------------------
;; ------------------------------------------------------------------
; 1. Guanya 1 equip (explosions) o bé
; 2. 1500 torns:
; - Equip amb més bolles vives
; - Equip amb més pintura
; - Aleatori
(defun final-partida-p (estat)
  (or
   (base-pintada-3-colors-p estat)
   (>= (cadr (assoc 'ronda estat)) 1500)))

(defun base-pintada-3-colors-p (estat)
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (base-e1 (obtenir-base-per-equip unitats 'e1))
         (base-e2 (obtenir-base-per-equip unitats 'e2)))
    (or (null base-e1)
        (null base-e2))))

(defun obtenir-base-per-equip (unitats equip)
  (cond
    ((null unitats) nil)
    ((and (eq (nth 2 (car unitats)) 'base)
          (eq (nth 3 (car unitats)) equip))
     (car unitats))
    (t
     (obtenir-base-per-equip (cdr unitats) equip))))

(defun contar-bolles-vives (unitats)
  (cond
    ((null unitats) 0)
    ((eq (nth 2 (car unitats)) 'bolla)
     (+ 1 (contar-bolles-vives (cdr unitats))))
    (t
     (contar-bolles-vives (cdr unitats)))))

(defun finalitzar-partida (estat)
  (let* (
         (unitats (cadr (assoc 'unitats estat)))
         (e1 (obtenir-unitats-per-equip unitats 'e1))
         (e2 (obtenir-unitats-per-equip unitats 'e2))
         (base-e1 (obtenir-base-per-equip e1 'e1))
         (base-e2 (obtenir-base-per-equip e2 'e2))
         (base-e1-explotada (null base-e1))
         (base-e2-explotada (null base-e2))

         (bolles-e1 (contar-bolles-vives e1))
         (bolles-e2 (contar-bolles-vives e2))

         (p1 (cadr (assoc 'pintura-e1 estat)))
         (p2 (cadr (assoc 'pintura-e2 estat)))
         (resultat
          (cond
            ((and base-e1-explotada (not base-e2-explotada)) 'guanya-e2)
            ((and base-e2-explotada (not base-e1-explotada)) 'guanya-e1)
            ((> bolles-e1 bolles-e2) 'guanya-e1)
            ((> bolles-e2 bolles-e1) 'guanya-e2)
            ((> p1 p2) 'guanya-e1)
            ((> p2 p1) 'guanya-e2)
            (t (random-empate))))
        )
    ;; Dibujamos el último estado para que el mapa se vea congelado
    (pinta estat)
    ;; Configuración para el texto final (letras blancas con contorno o fondo si es necesario, aquí negro sobre fondo invisible)
    (color 0 0 0 255 255 255)
    (goto-xy 10 340)
    (princ "================================")
    (terpri)
    (goto-xy 10 355)
    (cond
      ((eq resultat 'guanya-e1)
       (princ "GUANYA EQUIP E1 (LILA)"))
      ((eq resultat 'guanya-e2)
       (princ "GUANYA EQUIP E2 (TARONJA)")))
    (terpri)
    (goto-xy 10 370)
    (princ "================================")
    resultat))

(defun random-empate ()
  (cond
    ((= (random 2) 0) 'guanya-e1)
    (t 'guanya-e2)))
;; ------------------------------------------------------------------
;;  ------------------- ACTUALITZACIONS  -------------------
;; ------------------------------------------------------------------

(defun seguent-torn (estat)
  (let ((torn (cadr (assoc 'torn estat))))
    (cond
      ((eq torn 'e1)
       (substituir-camp 'torn 'e2 estat))
      (t
       (let ((estat1 (substituir-camp 'torn 'e1 estat)))
         (substituir-camp 'ronda
                          (+ 1 (cadr (assoc 'ronda estat1)))
                          estat1))))))

(defun actualitzar-pintura (estat)
  (let* (
         (torn (cadr (assoc 'torn estat)))
         (mapa (cadr (assoc 'mapa estat)))
         (labs (contar-labs mapa torn))
        )
    (cond
      ((eq torn 'e1)
       (substituir-camp 'pintura-e1
                        (+ (cadr (assoc 'pintura-e1 estat)) 2 labs)
                        estat))
      (t
       (substituir-camp 'pintura-e2
                        (+ (cadr (assoc 'pintura-e2 estat)) 2 labs)
                        estat)))))


  ; bases: baixar temps rec
  ; bolles: baixar temps rec i temps pintar
(defun baixar-cooldowns (estat)
  "Aplica -1 als temps de recuperació de les unitats."
  (let* (
         (unitats (cadr (assoc 'unitats estat)))
         (unitats2 (baixar-cooldowns-llista unitats))
        )
    (substituir-camp 'unitats unitats2 estat)))

(defun baixar-cooldowns-llista (unitats)
  (cond
    ((null unitats) nil)

    (t
     (cons
      (baixar-cooldown-unitat (car unitats))
      (baixar-cooldowns-llista (cdr unitats))))))

(defun baixar-cooldown-unitat (u)
  (let (
        (tipus (nth 2 u))
        (tr-pintar (nth 7 u))
        (tr-moure (nth 8 u))
        (tr-crear (nth 9 u))
       )

    (cond
      ;; BASE → només tr-crear
      ((eq tipus 'base)
       (actualitzar-unitat u
         0
         0
         (decrementar-si-numero tr-crear)))

      ;; BOLLA → tr-moure y tr-pintar
      ((eq tipus 'bolla)
       (actualitzar-unitat u
         (decrementar-si-numero tr-pintar)
         (decrementar-si-numero tr-moure)
         0))

      (t u))))


(defun decrementar-si-numero (x)
  (cond
    ((null x) 0)
    ((> x 0) (- x 1))
    (t 0)))

(defun incrementar-si-numero (x quantitat)
  (cond
    ((null x) 0)
    ((null quantitat) x)
    ((>= quantitat 0) (+ x quantitat))
    (t x)))

(defun incrementar-temps-mov (unitat quantitat)
  "Incrementa tr-moure d'una unitat en la quantitat indicada."
  (actualitzar-unitat unitat
                     nil
                     (incrementar-si-numero (nth 8 unitat) quantitat)
                     nil))

(defun incrementar-temps-pint (unitat quantitat)
  "Incrementa tr-pintar d'una unitat en la quantitat indicada."
  (actualitzar-unitat unitat
                     (incrementar-si-numero (nth 7 unitat) quantitat)
                     nil
                     nil))

; (unitat id tipus equip coord color-propi colors tr-pintar tr-moure tr-crear)
(defun actualitzar-unitat (u nou-tr-pintar nou-tr-moure nou-tr-crear)
  (list
   'unitat
   (nth 1 u) ; id
   (nth 2 u) ; tipus
   (nth 3 u) ; equip
   (nth 4 u) ; coord
   (nth 5 u) ; color-propi
   (nth 6 u) ; colors
   ; si paso 0 se pone 0, si paso nil, se deja nil:
   (cond ((null nou-tr-pintar) (nth 7 u)) (t nou-tr-pintar)) ; tr-pintar
  (cond ((null nou-tr-moure) (nth 8 u)) (t nou-tr-moure)) ; tr-moure
  (cond ((null nou-tr-crear) (nth 9 u)) (t nou-tr-crear)))) ; tr-crear
   




; ------------------------------------------------------------------
;;  ------------------- EXECUTAR TORN -------------------
;; ------------------------------------------------------------------

; unitat només pot executar acció si temps rec < 1
; unitat pot fer +1 acció
; unitat pot actuar al mateix torn en el que s'ha creat


(defun executar-torn (estat)
  "Executa el torn de l'equip actiu i retorna el nou estat."
  (let ((equip (cadr (assoc 'torn estat))))
    (executar-unitats estat equip)))

(defun executar-unitats (estat equip)
  "Recorre les unitats de l'equip i processa accions unitat a unitat."
  (executar-unitats-rec estat equip nil))

(defun executar-unitats-rec (estat equip ids-processats)
  (let ((unitat (obtenir-seguent-unitat-equip estat equip ids-processats)))
    (cond
      ((null unitat) estat)
      (t
       (let* ((info-unitat (construir-info-unitat estat unitat equip))
              (accions (cridar-agent-unitat info-unitat))
              (estat2 (processar-accions-unitat estat unitat equip accions))
              (id-unitat (nth 1 unitat)))
         (executar-unitats-rec estat2 equip (cons id-unitat ids-processats)))))))


;; HO FEIM AIXI PQ SI CREAM UNA NOVA UNITAT TAMBÉ POT ACTUAR AL TORN ON ÉS CREADA
(defun obtenir-seguent-unitat-equip (estat equip ids-processats) 
  (obtenir-seguent-unitat-equip-rec
   (obtenir-unitats-per-equip (cadr (assoc 'unitats estat)) equip)
   ids-processats))

(defun obtenir-seguent-unitat-equip-rec (unitats ids-processats)
  (cond
    ((null unitats) nil)
    ((member (nth 1 (car unitats)) ids-processats)
     (obtenir-seguent-unitat-equip-rec (cdr unitats) ids-processats))
    (t
     (car unitats))))

(defun construir-info-unitat (estat unitat equip)
  "Construeix la llista d'entrada per l'agent d'una unitat concreta."
  (let* ((ronda (cadr (assoc 'ronda estat)))
         (pintura (obtenir-pintura-equip estat equip))
         (id-unitat (nth 1 unitat))
         (tipus-unitat (nth 2 unitat))
         (coordenada (nth 4 unitat))
         (colors-pintat (nth 6 unitat))
         (color-propi (nth 5 unitat))
         (tr-pintar (nth 7 unitat))
         (tr-moure (nth 8 unitat))
         (visio (construir-visio-unitat estat unitat))
         (memoria-compartida (obtenir-memoria-equip estat equip)))
    (list ronda
          equip
          pintura
          id-unitat
          tipus-unitat
          coordenada
          colors-pintat
          color-propi
          tr-pintar
          tr-moure
          visio
          memoria-compartida)))

(defun obtenir-pintura-equip (estat equip)
  (cond
    ((eq equip 'e1) (cadr (assoc 'pintura-e1 estat)))
    (t (cadr (assoc 'pintura-e2 estat)))))

(defun obtenir-memoria-equip (estat equip)
  (cond
    ((eq equip 'e1) (cadr (assoc 'memoria-e1 estat)))
    (t (cadr (assoc 'memoria-e2 estat)))))

(defun cridar-agent-unitat (info-unitat)
  "Crida l'agent corresponent a l'equip actiu (e1/e2)."
  (let ((equip (cadr info-unitat)))
    (cond
      ((eq equip 'e1)
       (agent-cms213 info-unitat))
      ((eq equip 'e2)
       (agent-nms864 info-unitat))
      (t nil))))


(defun obtenir-unitat-per-id (unitats id-unitat)
  (cond
    ((null unitats) nil)
    ((= (nth 1 (car unitats)) id-unitat)
     (car unitats))
    (t
     (obtenir-unitat-per-id (cdr unitats) id-unitat))))

(defun processar-accions-unitat (estat unitat equip accions)
  (cond
    ((null accions) estat)
    ((not (listp accions)) estat)
    (t
     (let* ((id-unitat (nth 1 unitat))
            (unitat-actual (obtenir-unitat-per-id (cadr (assoc 'unitats estat)) id-unitat)))
       (cond
         ((null unitat-actual) estat)
         (t
          (let* ((accio (car accions))
                 (estat2 (cond
                           ((validar-accio estat unitat-actual equip accio)
                            (aplicar-accio estat unitat-actual equip accio))
                           (t estat))))
            (processar-accions-unitat estat2 unitat-actual equip (cdr accions)))))))))

(defun coord-valida-p (coord)
  (and (listp coord)
  (not (null coord))
  (not (null (cdr coord)))
  (integerp (car coord))
  (integerp (cadr coord))
  (>= (car coord) 0)
  (>= (cadr coord) 0)))

(defun accio-formada-p (accio)
  (and (listp accio)
       (symbolp (car accio))
       (listp (cadr accio))))

(defun validar-accio (estat unitat equip accio)
  (cond
    ((not (accio-formada-p accio)) nil)
    (t
     (let* ((tipus-accio (car accio))
            (arguments (cadr accio)))
       (cond
         ((eq tipus-accio 'crea-bolla)
          (validar-crea-bolla estat unitat equip
                              (car arguments)
                              (cadr arguments)))
         ((eq tipus-accio 'pinta)
          (validar-pinta estat unitat equip (car arguments)))
         ((eq tipus-accio 'mou)
          (validar-mou estat unitat equip (car arguments)))
         ((eq tipus-accio 'escriu-memoria)
          (validar-escriu-memoria estat unitat equip (car arguments)))
         (t nil))))))

(defun aplicar-accio (estat unitat equip accio)
  "Dispatch d'aplicacio d'accions segons el tipus."
  (cond
    ((not (accio-formada-p accio)) estat)
    (t
     (let* ((tipus-accio (car accio))
            (arguments (cadr accio)))
       (cond
         ((eq tipus-accio 'crea-bolla)
          (aplicar-crea-bolla estat unitat equip
                              (car arguments)
                              (cadr arguments)))
         ((eq tipus-accio 'pinta)
          (aplicar-pinta estat unitat equip (car arguments)))
         ((eq tipus-accio 'mou)
          (aplicar-mou estat unitat equip (car arguments)))
         ((eq tipus-accio 'escriu-memoria)
          (aplicar-escriu-memoria estat unitat equip (car arguments)))
         (t estat))))))


;; BUCLE D'UNITATS
; - Filtramos unidades por equipo activo ('e1 o 'e2)
; - Llamamos al agente
; - Nos devuelve una lista de acciones para la unidad en concreto.

;; ENVIAR LLISTA PER UNITAT:
; (ronda equip pintura id-unitat tipus-unitat coordenada colors-pintat color-propi
; tr-pintar tr-moure visió memòria-compartida)
; - ronda --> enter
; - equip --> ('e1 o 'e2)
; - pintura --> enter
; - id-unitat --> enter
; - tipus-unitat --> ('base o 'bolla)
; - coordenada (de la unitat) -->  llista de dos enters, x i y
; - colors-pintat --> llista buida o bé ('r, 'g o 'b)
; - color-propi --> nil (base) o bé llista buida o ('r, 'g o 'b) (bolles)
; - tr-pintar --> nil (base) o enter
; - tr-moure --> nil (base) o enter
; - visio...
; - memoria-compartida --> llista valors que comparteixen entre unitats


;; VISIÓ (llista caselles que la unitat pot veure)
;; CADA CASELLA D'AQUESTA LLISTA CONTÉ:
; - coordenada -->
; - tipus-casella -->
; - color-casella --> 
; - tipus-element -->
; - equip (que controla l'element, nil si és lab no controlat)
; - colors-pintat -->
; - color-propi -->
; - tr-pintar
; - tr-moure

;; ------------------------------------------------------------------
;;  ------------------- EXECUTAR ACCIÓ -------------------
;; ------------------------------------------------------------------
; - crea-bolla
;       1. Color bolla  ('r, 'g o 'b)
;       2. Coord on es vol crear


; - pinta
;       1. Coord on es vol pintar


; - mou
;       1. Coord on es vol moure



; - escriu-memoria
;       1. Nou valor de la memòria sencera (llista amb qualsevol estructura).


;; ------------------------------------------------------------------
;;  ------------------- VALIDAR ACCIÓ -------------------
;; ------------------------------------------------------------------

; - validar-crea-bolla (base)
;       0. Temps rec suficient (<1)
;       1. Pintura suficient (50)
;       2. Coordenada dins rang base
;       2. Coordenada dins mapa
;       3. Coordenada no aigua
;       4. Coordenada no ocupada


; - validar-pinta (bolla)
;       1. Temps pintar sufic (<1) (no necesitan pintura)
;       1. Coordenada dins rang bolla
;       2. Coordenada dins mapa
;       3. Coordenada no aigua
;       4. Coordenada ocupada
;               - base o bolla enemiga
;               - lab no ocupat
;               - lab enemic


; - validar-mou (bolla)
;       1. Temps rec suficient (<1)
;       2. Coordenada dins rang bolla
;       3. Coordenada dins mapa
;       4. Coordenada no aigua
;       5. Coordenada no ocupada

(defun validar-crea-bolla (estat unitat equip color coord)
  "Validacio base de crea-bolla."
  (and (eq (nth 2 unitat) 'base)
    (temps-unitat-disponible-p unitat 'crear)
       (pintura-suficient-crea-bolla-p estat equip)
  (coord-valida-p coord)
  (member color '(r g b))
  (coordenada-accio-dins-rang-base-p unitat coord)
  (coordenada-accio-dins-mapa-p estat coord)
  (coordenada-accio-no-aigua-p estat coord)
  (coordenada-accio-lliure-p estat coord)))

(defun validar-pinta (estat unitat equip coord)
  "Validacio base de pinta."
  (and (eq (nth 2 unitat) 'bolla)
       (temps-unitat-disponible-p unitat 'pintar)
  (coord-valida-p coord)
  (coordenada-accio-dins-rang-pinta-p unitat coord)
  (coordenada-accio-dins-mapa-p estat coord)
  (coordenada-accio-no-aigua-p estat coord)
  (coordenada-accio-pintable-p estat unitat equip coord)))

(defun validar-mou (estat unitat equip coord)
  "Validacio base de mou."
  (and (eq (nth 2 unitat) 'bolla)
       (temps-unitat-disponible-p unitat 'moure)
    (coord-valida-p coord)
    (coordenada-accio-dins-rang-mou-p unitat coord)
    (coordenada-accio-dins-mapa-p estat coord)
    (coordenada-accio-no-aigua-p estat coord)
    (coordenada-accio-lliure-p estat coord)))

(defun validar-escriu-memoria (estat unitat equip nova-memoria)
  "Permet escriure memoria compartida (afegiu restriccions si voleu)."
  t)

(defun temps-unitat-disponible-p (unitat tipus-accio)
  (let ((temps (cond
                 ((eq tipus-accio 'pintar) (nth 7 unitat))
                 ((eq tipus-accio 'moure) (nth 8 unitat))
                 ((eq tipus-accio 'crear) (nth 9 unitat))
                 (t nil))))
    (or (null temps)
        (< temps 1))))

(defun pintura-suficient-crea-bolla-p (estat equip)
  (>= (obtenir-pintura-equip estat equip) 50))

(defun dist2 (coord-a coord-b)
  "Distancia euclidiana al quadrat entre dues coordenades (x y)."
  (cond
    ((and (coord-valida-p coord-a) (coord-valida-p coord-b))
     (let* ((dx (- (car coord-a) (car coord-b)))
            (dy (- (cadr coord-a) (cadr coord-b))))
       (+ (* dx dx) (* dy dy))))
    (t 999999)))

(defun obtenir-casella-mapa (mapa coord)
  "Retorna la casella del mapa a la coordenada indicada o NIL si no existeix."
  (cond
    ((not (coord-valida-p coord)) nil)
    (t
     (let ((x (car coord))
           (y (cadr coord)))
       (cond
         ((or (< x 0) (< y 0)) nil)
         (t (obtenir-casella-fila mapa x y)))))))

(defun obtenir-casella-fila (mapa x y)
  (cond
    ((null mapa) nil)
    ((= y 0) (obtenir-casella-columna (car mapa) x))
    (t (obtenir-casella-fila (cdr mapa) x (- y 1)))))

(defun obtenir-casella-columna (fila x)
  (cond
    ((null fila) nil)
    ((= x 0) (car fila))
    (t (obtenir-casella-columna (cdr fila) (- x 1)))))

(defun unitat-a-coord-p (unitats coord)
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord) t)
    (t (unitat-a-coord-p (cdr unitats) coord))))

(defun obtenir-unitat-per-coord (unitats coord)
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord) (car unitats))
    (t (obtenir-unitat-per-coord (cdr unitats) coord))))

(defun obtenir-color-terra-casella (casella)
  (cond
    ((and casella (eq (car casella) 'terra)) (cadr casella))
    (t nil)))

(defun obtenir-equip-lab-casella (casella)
  (cond
    ((member 'lab casella) (cadr (member 'lab casella)))
    (t nil)))

(defun afegir-color-pintat (colors color)
  (cond
    ((member color colors) colors)
    (t (append colors (list color)))))

(defun canviar-lab-equip-casella (casella equip-nou)
  (cond
    ((member 'lab casella) (list 'terra (cadr casella) 'lab equip-nou))
    (t casella)))

(defun canviar-color-casella (casella color-nou)
  (cond
    ((null casella) nil)
    ((member 'lab casella)
     (list 'terra color-nou 'lab (obtenir-equip-lab-casella casella)))
    ((member 'base casella)
     (list 'terra color-nou 'base (cadr (member 'base casella))))
    ((member 'bolla casella)
     (let ((bolla (member 'bolla casella)))
       (list 'terra color-nou 'bolla
             (cadr bolla)
             (caddr bolla)
             (cadddr bolla)
             (cadr (cddddr bolla))
             (caddr (cddddr bolla)))))
    (t (list 'terra color-nou))))

(defun actualitzar-casella-mapa (mapa coord nova-casella)
  (actualitzar-casella-mapa-fila mapa coord nova-casella 0))

(defun actualitzar-casella-mapa-fila (mapa coord nova-casella y)
  (cond
    ((null mapa) nil)
    ((= y (cadr coord))
     (cons (actualitzar-casella-mapa-columna (car mapa) coord nova-casella 0)
           (cdr mapa)))
    (t
     (cons (car mapa)
           (actualitzar-casella-mapa-fila (cdr mapa) coord nova-casella (+ y 1))))))

(defun actualitzar-casella-mapa-columna (fila coord nova-casella x)
  (cond
    ((null fila) nil)
    ((= x (car coord))
     (cons nova-casella (cdr fila)))
    (t
     (cons (car fila)
           (actualitzar-casella-mapa-columna (cdr fila) coord nova-casella (+ x 1))))))

(defun actualitzar-unitat-per-coord (unitats coord nova-unitat)
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord)
     (cons nova-unitat (cdr unitats)))
    (t
     (cons (car unitats)
           (actualitzar-unitat-per-coord (cdr unitats) coord nova-unitat)))))

(defun eliminar-unitat-per-coord (unitats coord)
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord)
     (cdr unitats))
    (t
     (cons (car unitats)
           (eliminar-unitat-per-coord (cdr unitats) coord)))))

(defun recuperacio-pinta (unitat estat)
  "Recuperación de pintar: base=3, penalización origen (casella ≠ color bolla) ×3. Retorna ENTERO."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (casella-origen (obtenir-casella-mapa mapa (nth 4 unitat)))
         (color-origen (obtenir-color-terra-casella casella-origen))
         (color-bolla (nth 5 unitat))
         (base 3)
         (penalizacion-origen (cond ((eq color-origen color-bolla) 1) (t 3))))
    (* base penalizacion-origen)))

(defun recuperacio-mou (unitat estat coord)
  "Recuperación de movimiento: base=1, diagonal≈1.4, penalización destino=1 o 3. Retorna ENTERO."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (casella-desti (obtenir-casella-mapa mapa coord))
         (color-desti (obtenir-color-terra-casella casella-desti))
         (color-bolla (nth 5 unitat))
         (d2 (dist2 (nth 4 unitat) coord))
         (base 1)
         (diagonal (cond ((= d2 2) 1.4142) (t 1.0)))
         (penalizacion-destino (cond ((eq color-desti color-bolla) 1) (t 3)))
         (resultado (* base diagonal penalizacion-destino)))
    (truncate resultado)))

(defun casella-ocupada-per-element-p (casella)
  "Retorna T si la casella conté algun element de mapa (lab/base/bolla)."
  (and casella
       (or (member 'lab casella)
           (member 'base casella)
           (member 'bolla casella))))

;; Placeholders de rangs/distancies i consultes de mapa.
;; Completar quan implementeu la part geometrica i d'ocupacio real.
(defun coordenada-accio-dins-rang-base-p (unitat coord)
  (let* ((coord-base (nth 4 unitat))
         (d2 (dist2 coord-base coord)))
    (and (> d2 0)
         (<= d2 2))))

;; SEPARACIÓN: Pintura usa d² ≤ 5, Movimiento usa d² ≤ 2
(defun coordenada-accio-dins-rang-pinta-p (unitat coord)
  "Validar que la coordenada está dentro del rango de pintura (d² ≤ 5)."
  (let* ((coord-bolla (nth 4 unitat))
         (d2 (dist2 coord-bolla coord)))
    (and (<= d2 5)
         (> d2 0))))

(defun coordenada-accio-dins-rang-mou-p (unitat coord)
  "Validar que la coordenada está dentro del rango de movimiento (d² ≤ 2)."
  (let* ((coord-bolla (nth 4 unitat))
         (d2 (dist2 coord-bolla coord)))
    (and (<= d2 2)
         (> d2 0))))

;; DEPRECATED: mantener para compatibilidad pero usar las nuevas versiones
(defun coordenada-accio-dins-rang-bolla-p (unitat coord)
  (coordenada-accio-dins-rang-mou-p unitat coord))

(defun coordenada-accio-dins-mapa-p (estat coord)
  "Verifica que la coordenada existe en el mapa y está dentro de los límites."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (mapa-altura (length mapa))
         (mapa-anchura (cond ((> mapa-altura 0) (length (car mapa))) (t 0))))
    (and (coord-valida-p coord)
         (< (car coord) mapa-anchura)
         (< (cadr coord) mapa-altura)
         (not (null (obtenir-casella-mapa mapa coord))))))

(defun coordenada-accio-no-aigua-p (estat coord)
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (casella (obtenir-casella-mapa mapa coord)))
    (and casella (eq (car casella) 'terra))))

(defun coordenada-accio-lliure-p (estat coord)
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (mapa (cadr (assoc 'mapa estat)))
         (casella (obtenir-casella-mapa mapa coord)))
    (and (not (unitat-a-coord-p unitats coord))
         (not (casella-ocupada-per-element-p casella)))))

(defun coordenada-accio-pintable-p (estat unitat equip coord)
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (mapa (cadr (assoc 'mapa estat)))
         (casella (obtenir-casella-mapa mapa coord))
         (unitat-desti (obtenir-unitat-per-coord unitats coord))
         (equip-desti (and unitat-desti (nth 3 unitat-desti)))
         (equip-lab (obtenir-equip-lab-casella casella)))
    (and casella
         (eq (car casella) 'terra)
         (or (null unitat-desti)
             (not (eq equip-desti equip)))
         (or (null equip-lab)
             (not (eq equip-lab equip))))))











;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ UNITATS -------------------
;; ------------------------------------------------------------------
; Unitats: bolla, base
; Si están pintades de 3 colors --> Exploten
(defun unitat-explotada-p (u) ;; REVISAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAR
  "Retorna T si la unitat està pintada de 3 colors."
  (let ((colors (nth 6 u))) ; colors-pintat
    (and (listp colors) (= (length colors) 3))))

(defun unitat-explotada-en-llista-p (unitats tipus) ;; REVISAAAAAAAAAAAAAAAAAAAR
  "Retorna T si alguna unitat del tipus indicat està explotada (pintada 3 colors)."
  (cond
    ((null unitats) nil)
    ((eq (nth 2 (car unitats)) tipus)
     (or (unitat-explotada-p (car unitats))
         (unitat-explotada-en-llista-p (cdr unitats) tipus)))
    (t
     (unitat-explotada-en-llista-p (cdr unitats) tipus))))


(defun obtenir-unitats-per-equip (unitats equip) 
  "Retorna una llista amb les unitats que pertanyen a un equip."
  (cond
    ((null unitats) nil)
    ((eq (cadddr (car unitats)) equip) ; posición del equipo
     (cons (car unitats)
           (obtenir-unitats-per-equip (cdr unitats) equip)))
    (t
     (obtenir-unitats-per-equip (cdr unitats) equip))))
; BASE:




; BOLLES (id-unitat):
; - Comencen pintades del seu color
; - Temps recuperació (0 inici) --> per poder moure
; - Temps pintar --> per poder pintar


;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ LABORATORIS -------------------
;; ------------------------------------------------------------------
; inicialment sense color
; quan una bolla els pinta --> tornen de l'equip


(defun contar-labs (mapa equip) ; SI TENEMOS UN MAPA MUY GRANDE A LO MEJOR RENTA AÑADIR UN CONTADOR AL ESTADO.
"Contam els laboratoris d'un equip."
  (cond
    ((null mapa) 0)
    (t
     (+ (contar-labs-fila (car mapa) equip)
        (contar-labs (cdr mapa) equip)))))

;(defun contar-labs-fila (fila equip)
;  (cond
;    ((null fila) 0)
;    (t
;     (+ (if (and (eq (car (car fila)) 'terra)
;                 (member 'lab (car fila))
;                 (eq (cadr (member 'lab (car fila))) equip))
;            1
;          0)
;        (contar-labs-fila (cdr fila) equip)))))


(defun contar-labs-fila (fila equip)
  (cond
    ((null fila) 0)
    (t
     (let* (
            (celda (car fila))
            (lab-info (member 'lab celda))
           )
       (+ (cond
            ((and (eq (car celda) 'terra)
                  lab-info
                  (eq (cadr lab-info) equip))
             1)
            (t 0))
          (contar-labs-fila (cdr fila) equip))))))



;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DISTÀNCIES -------------------
;; ------------------------------------------------------------------
; Rangs distància euclidiana al quadrat (d^2)
; ESTA PARTE REVISAR!!!



;; ------------------------------------------------------------------
;;  ------------------- ACCIONS!!! -------------------
;; ------------------------------------------------------------------
; Hay penalizaciones y cosas, mirar


; pinta lab --> actualitzar equip, actualitzar mapa


(defun aplicar-crea-bolla (estat unitat equip color coord)
  "Crea una bolla nova, resta pintura i incrementa el cooldown de la base."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (next-id (cadr (assoc 'next-id estat)))
         (pintura-equip (obtenir-pintura-equip estat equip))
         (base-actual (obtenir-unitat-per-id unitats (nth 1 unitat)))
         (base-actualitzada
          (cond
            (base-actual
             (actualitzar-unitat base-actual
                                 (nth 7 base-actual)
                                 (nth 8 base-actual)
                                 (incrementar-si-numero (nth 9 base-actual) 1)))
            (t nil)))
         (nova-bolla
          (list 'unitat
                next-id
                'bolla
                equip
                coord
                color
                (list color)
                0
                0
                nil))
         (unitats1
          (cond
            (base-actualitzada
             (actualitzar-unitat-per-coord unitats
                                           (nth 4 base-actualitzada)
                                           base-actualitzada))
            (t unitats)))
         (unitats2 (cons nova-bolla unitats1))
         (mapa (cadr (assoc 'mapa estat)))
         (casella-bolla
          (list 'terra color 'bolla color next-id equip (list color) 0 0))
         (mapa1 (actualitzar-casella-mapa mapa coord casella-bolla))
         (estat1 (substituir-camp 'unitats unitats2 estat))
         (estat2 (substituir-camp 'mapa mapa1 estat1))
         (estat3 (substituir-camp 'next-id (+ next-id 1) estat2))
         (estat4 (cond
                   ((eq equip 'e1)
                    (substituir-camp 'pintura-e1 (- pintura-equip 50) estat3))
                   (t
                    (substituir-camp 'pintura-e2 (- pintura-equip 50) estat3)))))
    ;; Evitar redibujado en aplicar-crea-bolla - la función pinta() lo hará en la siguiente iteración
    estat4))

(defun aplicar-pinta (estat unitat equip coord)
  "Pinta la casella destí i actualitza l'element corresponent."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (unitats (cadr (assoc 'unitats estat)))
         (casella-desti (obtenir-casella-mapa mapa coord))
         (color-bolla (nth 5 unitat))
         (tr-extra (recuperacio-pinta unitat estat))
         ;; Solo cambiamos el color de la casilla si es diferente al color del ataque
         (color-actual-terra (obtenir-color-terra-casella casella-desti))
         (casella-pintada (cond
                            ((eq color-actual-terra color-bolla) casella-desti)
                            (t (canviar-color-casella casella-desti color-bolla))))
         (unitat-origen (obtenir-unitat-per-id unitats (nth 1 unitat)))
         (unitat-origen-actual
          (cond
            (unitat-origen
             (actualitzar-unitat unitat-origen
                     (incrementar-si-numero (nth 7 unitat-origen) tr-extra)
                                 (nth 8 unitat-origen)
                                 (nth 9 unitat-origen)))
            (t nil)))
         (unitats1
          (cond
            (unitat-origen-actual
             (actualitzar-unitat-per-coord unitats (nth 4 unitat-origen-actual) unitat-origen-actual))
            (t unitats)))
         (unitat-desti (obtenir-unitat-per-coord unitats1 coord))
         (colors-desti (and unitat-desti
                            (afegir-color-pintat (nth 6 unitat-desti) color-bolla)))
         (unitat-desti-actual
          (cond
            ((null unitat-desti) nil)
            ((eq (nth 3 unitat-desti) equip) unitat-desti)
            ((eq (nth 2 unitat-desti) 'base)
             (list 'unitat
                   (nth 1 unitat-desti)
                   (nth 2 unitat-desti)
                   (nth 3 unitat-desti)
                   (nth 4 unitat-desti)
                   (nth 5 unitat-desti)
                   colors-desti
                   (nth 7 unitat-desti)
                   (nth 8 unitat-desti)
                   (nth 9 unitat-desti)))
            ((eq (nth 2 unitat-desti) 'bolla)
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3)) nil)
               (t
                (list 'unitat
                      (nth 1 unitat-desti)
                      (nth 2 unitat-desti)
                      (nth 3 unitat-desti)
                      (nth 4 unitat-desti)
                      (nth 5 unitat-desti)
                      colors-desti
                      (nth 7 unitat-desti)
                      (nth 8 unitat-desti)
                      (nth 9 unitat-desti)))))
            (t unitat-desti)))
         (unitats2
          (cond
            ((null unitat-desti) unitats1)
            ((eq (nth 3 unitat-desti) equip) unitats1)
            ((eq (nth 2 unitat-desti) 'base)
             ;; Si la base tiene 3 colores, eliminarla; si no, actualizarla
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3))
                (eliminar-unitat-per-coord unitats1 coord))
               (t
                (actualitzar-unitat-per-coord unitats1 coord unitat-desti-actual))))
            ((eq (nth 2 unitat-desti) 'bolla)
             (cond
               (unitat-desti-actual
                (actualitzar-unitat-per-coord unitats1 coord unitat-desti-actual))
               (t
                (eliminar-unitat-per-coord unitats1 coord))))
            (t unitats1)))
         (nova-casella
          (cond
            ((member 'lab casella-desti)
             (canviar-lab-equip-casella casella-pintada equip))
            ((member 'bolla casella-desti)
             ;; Si la bolla explota (3 colores), queda solo la tierra pintada
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3))
                (list 'terra color-bolla))
               ;; Si no explota, actualizar casella de bolla con los nuevos colors-pintat
               (t
                (let ((bolla (member 'bolla casella-desti)))
                  (list 'terra color-bolla 'bolla
                        (cadr bolla)                    ; color-propi
                        (caddr bolla)                   ; id
                        (cadddr bolla)                  ; equip
                        colors-desti                    ; colors-pintat ACTUALIZADO
                        (cadr (cddddr bolla))           ; tr-pintar
                        (caddr (cddddr bolla)))))))
            ((member 'base casella-desti)
             ;; Si la base explota (3 colores), eliminarla del mapa Y ASEGURAR QUE SE VEA LA TIERRA
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3))
                (list 'terra color-bolla))
               ;; Si no explota, actualizar con los nuevos colores y cambiar color de casella
               (t
                (let ((base (member 'base casella-desti)))
                  ;; CAMBIO: Mantener color de la base pero actualizar sus colores internos
                  (list 'terra color-bolla 'base
                        (cadr base)                   ; equip
                        colors-desti)))))              ; colors-pintat ACTUALIZADO
            (t casella-pintada)))
         (mapa1 (actualitzar-casella-mapa mapa coord nova-casella))
         (estat1 (substituir-camp 'mapa mapa1 estat))
         (estat-final (substituir-camp 'unitats unitats2 estat1)))
    ;; Evitar redibujado en aplicar-pinta - la función pinta() lo hará en la siguiente iteración
    estat-final))

(defun aplicar-mou (estat unitat equip coord)
  "Mou la bolla a la coordenada destí i incrementa el seu cooldown de moviment."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (unitat-actual (obtenir-unitat-per-id unitats (nth 1 unitat)))
         (coord-origen (nth 4 unitat-actual))
         (tr-extra (recuperacio-mou unitat estat coord))
         (unitat-moguda
          (cond
            (unitat-actual
             (actualitzar-unitat unitat-actual
                                 (nth 7 unitat-actual)
                     (incrementar-si-numero (nth 8 unitat-actual) tr-extra)
                                 (nth 9 unitat-actual)))
            (t nil)))
         (unitats1
          (cond
            (unitat-moguda
             (actualitzar-unitat-per-coord unitats coord-origen
                                           (list 'unitat
                                                 (nth 1 unitat-moguda)
                                                 (nth 2 unitat-moguda)
                                                 (nth 3 unitat-moguda)
                                                 coord
                                                 (nth 5 unitat-moguda)
                                                 (nth 6 unitat-moguda)
                                                 (nth 7 unitat-moguda)
                                                 (nth 8 unitat-moguda)
                                                 (nth 9 unitat-moguda))))
            (t unitats)))
         (mapa (cadr (assoc 'mapa estat)))
         (casella-origen (obtenir-casella-mapa mapa coord-origen))
         (color-terra-origen (obtenir-color-terra-casella casella-origen))
         (casella-origen-buida
          (list 'terra color-terra-origen))
         (casella-desti-orig (obtenir-casella-mapa mapa coord))
         (color-terra-desti (obtenir-color-terra-casella casella-desti-orig))
         (color-bolla (nth 5 unitat))
         (casella-desti
          (list 'terra color-terra-desti 'bolla
                color-bolla
                (nth 1 unitat)
                equip
                (nth 6 unitat)
                (nth 7 unitat-moguda)
                (nth 8 unitat-moguda)))
         (mapa1 (actualitzar-casella-mapa mapa coord-origen casella-origen-buida))
         (mapa2 (actualitzar-casella-mapa mapa1 coord casella-desti))
         (estat1 (substituir-camp 'unitats unitats1 estat))
         (estat-final (substituir-camp 'mapa mapa2 estat1)))
    ;; Evitar redibujado en aplicar-mou - la función pinta() lo hará en la siguiente iteración
    estat-final))

(defun aplicar-escriu-memoria (estat unitat equip nova-memoria)
  "Escriu el nou valor de la memòria compartida de l'equip a l'estat."
  (cond
    ((eq equip 'e1)
     (substituir-camp 'memoria-e1 nova-memoria estat))
    (t
     (substituir-camp 'memoria-e2 nova-memoria estat))))

(defun construir-visio-unitat (estat unitat)
  "Construeix la visió de la unitat segons el seu tipus."
  (let* ((rango (cond ((eq (nth 2 unitat) 'base) 64) (t 20)))
         (coord-origen (nth 4 unitat))
         (mapa (cadr (assoc 'mapa estat)))
         (unitats (cadr (assoc 'unitats estat))))
    (construir-visio-unitat-rec mapa unitats coord-origen rango 0 0)))

(defun construir-visio-unitat-rec (mapa unitats coord-origen rango y x)
  (cond
    ((null mapa) nil)
    (t
     (append
      (construir-visio-fila (car mapa) unitats coord-origen rango y 0)
      (construir-visio-unitat-rec (cdr mapa) unitats coord-origen rango (+ y 1) 0)))))

(defun construir-visio-fila (fila unitats coord-origen rango y x)
  (cond
    ((null fila) nil)
    (t
     (let* ((coord (list x y))
            (d2 (dist2 coord-origen coord))
            (casella (car fila))
            (unitat-casella (obtenir-unitat-per-coord unitats coord)))
       (cond
         ((<= d2 rango)
          (cons (construir-entrada-visio coord casella unitat-casella)
                (construir-visio-fila (cdr fila) unitats coord-origen rango y (+ x 1))))
         (t
          (construir-visio-fila (cdr fila) unitats coord-origen rango y (+ x 1))))))))

(defun construir-entrada-visio (coord casella unitat-casella)
  ;; Si la casella es agua, retornar solo (coord 'aigua)
  (cond
    ((eq (car casella) 'aigua)
     (list coord 'aigua))
    ;; Para casillas de tierra, construir entrada completa
    (t
     (let* ((tipus-casella (car casella))
            (color-casella (obtenir-color-terra-casella casella))
            (tipus-element (cond
                             ((and unitat-casella (eq (nth 2 unitat-casella) 'base)) 'base)
                             ((and unitat-casella (eq (nth 2 unitat-casella) 'bolla)) 'bolla)
                             ((member 'lab casella) 'lab)
                             ((member 'base casella) 'base)
                             ((member 'bolla casella) 'bolla)
                             (t nil)))
            (equip (cond
                     ((and unitat-casella (eq (nth 2 unitat-casella) 'base)) (nth 3 unitat-casella))
                     ((and unitat-casella (eq (nth 2 unitat-casella) 'bolla)) (nth 3 unitat-casella))
                     ((member 'lab casella) (obtenir-equip-lab-casella casella))
                     ((member 'base casella) (cadr (member 'base casella)))
                     ((member 'bolla casella) (cadr (member 'bolla casella)))
                     (t nil)))
            (colors-pintat (cond
                             ((and unitat-casella (eq (nth 2 unitat-casella) 'base)) (nth 6 unitat-casella))
                             ((and unitat-casella (eq (nth 2 unitat-casella) 'bolla)) (nth 6 unitat-casella))
                             ((member 'lab casella) nil)
                             (t nil)))
            (color-propi (cond
                           ((and unitat-casella (eq (nth 2 unitat-casella) 'base)) (nth 5 unitat-casella))
                           ((and unitat-casella (eq (nth 2 unitat-casella) 'bolla)) (nth 5 unitat-casella))
                           (t nil)))
            (tr-pintar (cond
                         ((and unitat-casella (eq (nth 2 unitat-casella) 'base)) (nth 7 unitat-casella))
                         ((and unitat-casella (eq (nth 2 unitat-casella) 'bolla)) (nth 7 unitat-casella))
                         (t nil)))
            (tr-moure (cond
                        ((and unitat-casella (eq (nth 2 unitat-casella) 'base)) (nth 8 unitat-casella))
                        ((and unitat-casella (eq (nth 2 unitat-casella) 'bolla)) (nth 8 unitat-casella))
                        (t nil))))
       (list coord
             tipus-casella
             color-casella
             tipus-element
             equip
             colors-pintat
             color-propi
             tr-pintar
             tr-moure)))))


;; ------------------------------------------------------------------
;;  ------------------- DEBUG: IMPRIMIR VISIÓN -------------------
;; ------------------------------------------------------------------

(defun imprimir-visio (visio)
  "Imprime la visión de una unitat de forma legible."
  (princ "==== VISIÓN (")
  (princ (length visio))
  (princ " caselles) ====")
  (terpri)
  (imprimir-visio-rec visio 0))

(defun imprimir-visio-rec (visio contador)
  (cond
    ((null visio) 
     (princ "=====================================")
     (terpri))
    (t
     (let* ((entrada (car visio))
            (coord (nth 0 entrada))
            (tipus-casella (nth 1 entrada))
            (color-casella (nth 2 entrada))
            (tipus-element (nth 3 entrada))
            (equip (nth 4 entrada))
            (colors-pintat (nth 5 entrada))
            (color-propi (nth 6 entrada))
            (tr-pintar (nth 7 entrada))
            (tr-moure (nth 8 entrada)))
       
       ;; Imprimir coordenada y tipo de casella
       (princ "[")
       (princ contador)
       (princ "] Pos:")
       (princ coord)
       (princ " | Casella:")
       (princ tipus-casella)
       (princ " | Color:")
       (princ color-casella)
       (terpri)
       
       ;; Imprimir elemento si existe
       (cond
         ((not (null tipus-element))
            (princ "    Elemento:")
            (princ tipus-element)
            (princ " | Equipo:")
            (princ equip)
            (princ " | Color-propi:")
            (princ color-propi)
            (terpri)))
       
       ;; Imprimir colores pintados y tiempos
       (cond
         ((not (null colors-pintat))

            (princ "    Colors-pintat:")
            (princ colors-pintat)
            (terpri)))
       
       (cond
         ((and (not (null tr-pintar)) (> tr-pintar 0))

            (princ "    Tiempos - pintar:")
            (princ tr-pintar)))
       (cond
         ((and (not (null tr-moure)) (> tr-moure 0))

            (princ " | moure:")
            (princ tr-moure)))
       (cond
         ((or (and (not (null tr-pintar)) (> tr-pintar 0))
              (and (not (null tr-moure)) (> tr-moure 0)))
          (terpri)))
       
       (imprimir-visio-rec (cdr visio) (+ contador 1))))))
;; ------------------------------------------------------------------
;;  ------------------- GENERADORES ALEATORIOS -------------------
;; ------------------------------------------------------------------

(defun generar-coord-aleatoria-valida-en-mapa (mapa unitats equip)
  "Genera una coordenada aleatoria válida dentro del mapa (no agua, no labs, no bases de enemigos)."
  (let* ((altura (length mapa))
         (anchura (cond ((> altura 0) (length (car mapa))) (t 0))))
    (generar-coord-aleatoria-valida-rec mapa unitats equip altura anchura 0)))

(defun generar-coord-aleatoria-valida-rec (mapa unitats equip altura anchura intentos)
  "Intenta generar coordenadas aleatorias hasta encontrar una válida (máx 50 intentos)."
  (cond
    ((>= intentos 50) nil)  ; No hay más intentos, retornar nil
    (t
     (let* ((x-random (random (cond ((> anchura 0) anchura) (t 1))))
            (y-random (random (cond ((> altura 0) altura) (t 1))))
            (coord (list x-random y-random))
            (casella (obtenir-casella-mapa mapa coord))
            (unitat-en-coord (obtenir-unitat-per-coord unitats coord)))
       (cond
         ;; Si la casella es válida (tierra, no agua, no ocupada)
         ((and casella
               (eq (car casella) 'terra)
               (not (member 'agua casella))
               (not (member 'lab casella))
               (null unitat-en-coord)
               (not (member 'bolla casella))
               (not (member 'base casella)))
          coord)
         ;; Si no es válida, intentar de nuevo
         (t
          (generar-coord-aleatoria-valida-rec mapa unitats equip altura anchura (+ intentos 1))))))))

(defun contar-bolas-en-rango (unitats coord rango)
  "Cuenta cuántas bolas del mismo equipo hay en rango de la coordenada."
  (contar-bolas-en-rango-rec unitats coord rango 0))

(defun contar-bolas-en-rango-rec (unitats coord rango contador)
  (cond
    ((null unitats) contador)
    ((and (eq (nth 2 (car unitats)) 'bolla)
          (<= (dist2 (nth 4 (car unitats)) coord) rango))
     (contar-bolas-en-rango-rec (cdr unitats) coord rango (+ contador 1)))
    (t
     (contar-bolas-en-rango-rec (cdr unitats) coord rango contador))))