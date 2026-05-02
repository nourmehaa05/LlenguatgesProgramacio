;; ============================================================
;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
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
;; Com iniciar una partida:
;;   (inici)                        -> mapa "tiny", pinta cada torn
;;   (inici-mapa "nom-mapa" N)      -> mapa personalitzat, pinta cada N torns
;;   Exemples:
;;     (inici-mapa "tiny" 1)
;;     (inici-mapa "huge60" 10)
;; ============================================================
;; Aspectes opcionals implementats:
;;   - Límit de 1500 torns
;;   - Mapes de fins a 60x60
;;   - Memòria compartida entre unitats
;;   - Caselles es pinten:
;;          - Bolla pinta la casella si pinta un lab, una base o una bolla
;;          - Base crea bolles i la casella es pinta del color de la bolla
;;   - Pintar cada N torns (paràmetre pintar-cada-n)
;; ============================================================
;; Descripció de les funcions d'aquest fitxer:
;;   - Funcions auxiliars generals (substituir-camp, dist2, etc.)
;;   - Gestió del mapa: lectura, accés i modificació de caselles
;;   - Gestió d'unitats: creació, cerca i actualització
;;   - Gestió de laboratoris i comptadors de captura
;;   - Càlcul de temps de recuperació i rangs d'acció
;;   - Inicialització de la partida i de l'estat del joc
;;   - Bucle principal de la partida (jugar-partida)
;;   - Control del final de partida i determinació del guanyador
;;   - Actualitzacions d'estat: pintura, cooldowns i torn
;;   - Execució del torn: crida als agents i processament d'accions
;;   - Validació d'accions: crea-bolla, pinta, mou, escriu-memoria
;;   - Aplicació d'accions: actualització d'estat, mapa i unitats
;;   - Construcció de la visió de cada unitat
;; ============================================================
;; Fitxers necessaris:
(load 'common) ; https://almy.us/files/xl305req.zip
(load 'tco)    ; https://github.com/antoni-oliver/defun-tco
(load "grafics")
(load "agent-cms213")
(load "agent-nms864")
(load "sleep")
(load "memoria")


;; ==============================================================
;;  ------------------- HELPERS I UTILS -------------------
;; ==============================================================

(defun substituir-camp (clau valor estat)
  "Substitueix el valor associat a 'clau' dins l'alist 'estat'."
  (cond
    ((null estat) nil)
    ((eq (caar estat) clau)
     (cons (list clau valor) (cdr estat)))
    (t
     (cons (car estat)
           (substituir-camp clau valor (cdr estat))))))

(defun coord-valida-p (coord)
  "Retorna T si 'coord' és una llista de dos enters no negatius."
  (and (listp coord)
       (not (null coord))
       (not (null (cdr coord)))
       (integerp (car coord))
       (integerp (cadr coord))
       (>= (car coord) 0)
       (>= (cadr coord) 0)))

(defun accio-formada-p (accio)
  "Retorna T si 'accio' té l'estructura correcta: (nom-accio llista-arguments)."
  (and (listp accio)
       (symbolp (car accio))
       (listp (cadr accio))))

(defun dist2 (coord-a coord-b)
  "Calcula la distància euclidiana al quadrat entre dues coordenades.
   Retorna 999999 si alguna coordenada no és vàlida."
  (cond
    ((and (coord-valida-p coord-a) (coord-valida-p coord-b))
     (let* ((dx (- (car coord-a) (car coord-b)))
            (dy (- (cadr coord-a) (cadr coord-b))))
       (+ (* dx dx) (* dy dy))))
    (t 999999)))

(defun decrementar-si-numero (x)
  "Decrementa x en 1, fins a un mínim de 0. Retorna 0 si x és nil."
  (cond
    ((null x) 0)
    ((> x 0) (- x 1))
    (t 0)))

(defun incrementar-si-numero (x quantitat)
  "Incrementa x en 'quantitat'. Retorna 0 si x és nil, x si quantitat és nil."
  (cond
    ((null x) 0)
    ((null quantitat) x)
    ((>= quantitat 0) (+ x quantitat))
    (t x)))


;; ==============================================================
;;  ------------------- GESTIÓ DEL MAPA -------------------
;; ==============================================================

(defun obtenir-casella-mapa (mapa coord)
  "Retorna la casella del mapa a la coordenada indicada, o NIL si no existeix."
  (cond
    ((not (coord-valida-p coord)) nil)
    (t
     (let ((x (car coord))
           (y (cadr coord)))
       (cond
         ((or (< x 0) (< y 0)) nil)
         (t (obtenir-casella-fila mapa x y)))))))

(defun obtenir-casella-fila (mapa x y)
  "Navega per les files del mapa fins a la fila y i retorna la casella x."
  (cond
    ((null mapa) nil)
    ((= y 0) (obtenir-casella-columna (car mapa) x))
    (t (obtenir-casella-fila (cdr mapa) x (- y 1)))))

(defun obtenir-casella-columna (fila x)
  "Navega per una fila fins a la columna x i retorna l'element."
  (cond
    ((null fila) nil)
    ((= x 0) (car fila))
    (t (obtenir-casella-columna (cdr fila) (- x 1)))))

(defun obtenir-color-terra-casella (casella)
  "Retorna el color de la casella de terra, o NIL si no és terra."
  (cond
    ((and casella (eq (car casella) 'terra)) (cadr casella))
    (t nil)))

(defun obtenir-equip-lab-casella (casella)
  "Retorna l'equip que controla el laboratori de la casella, o NIL si no n'hi ha."
  (cond
    ((member 'lab casella) (cadr (member 'lab casella)))
    (t nil)))

(defun casella-ocupada-per-element-p (casella)
  "Retorna T si la casella conté algun element (lab, base o bolla)."
  (and casella
       (or (member 'lab casella)
           (member 'base casella)
           (member 'bolla casella))))

(defun canviar-color-casella (casella color-nou)
  "Retorna una nova casella amb el color actualitzat, mantenint l'element que conté."
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

(defun canviar-lab-equip-casella (casella equip-nou)
  "Retorna una nova casella de lab amb l'equip controlador actualitzat."
  (cond
    ((member 'lab casella) (list 'terra (cadr casella) 'lab equip-nou))
    (t casella)))

(defun actualitzar-casella-mapa (mapa coord nova-casella)
  "Retorna un nou mapa amb la casella a 'coord' substituïda per 'nova-casella'."
  (actualitzar-casella-mapa-fila mapa coord nova-casella 0))

(defun actualitzar-casella-mapa-fila (mapa coord nova-casella y)
  "Recorre les files del mapa fins a trobar la fila de 'coord' i l'actualitza."
  (cond
    ((null mapa) nil)
    ((= y (cadr coord))
     (cons (actualitzar-casella-mapa-columna (car mapa) coord nova-casella 0)
           (cdr mapa)))
    (t
     (cons (car mapa)
           (actualitzar-casella-mapa-fila (cdr mapa) coord nova-casella (+ y 1))))))

(defun actualitzar-casella-mapa-columna (fila coord nova-casella x)
  "Recorre una fila fins a la columna de 'coord' i substitueix la casella."
  (cond
    ((null fila) nil)
    ((= x (car coord))
     (cons nova-casella (cdr fila)))
    (t
     (cons (car fila)
           (actualitzar-casella-mapa-columna (cdr fila) coord nova-casella (+ x 1))))))

(defun coordenada-accio-dins-mapa-p (estat coord)
  "Retorna T si 'coord' existeix dins els límits del mapa de l'estat."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (mapa-altura (length mapa))
         (mapa-amplada (cond ((> mapa-altura 0) (length (car mapa))) (t 0))))
    (and (coord-valida-p coord)
         (< (car coord) mapa-amplada)
         (< (cadr coord) mapa-altura)
         (not (null (obtenir-casella-mapa mapa coord))))))

(defun coordenada-accio-no-aigua-p (estat coord)
  "Retorna T si la casella a 'coord' és de terra (no és aigua)."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (casella (obtenir-casella-mapa mapa coord)))
    (and casella (eq (car casella) 'terra))))

(defun coordenada-accio-lliure-p (estat coord)
  "Retorna T si la casella a 'coord' no té cap unitat ni element."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (mapa (cadr (assoc 'mapa estat)))
         (casella (obtenir-casella-mapa mapa coord)))
    (and (not (unitat-a-coord-p unitats coord))
         (not (casella-ocupada-per-element-p casella)))))

(defun coordenada-accio-pintable-p (estat unitat equip coord)
  "Retorna T si la casella a 'coord' pot ser pintada per 'unitat' de 'equip'.
   No es pot pintar una casella amb unitat pròpia ni un lab propi."
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


;; ==============================================================
;;  ------------------- GESTIÓ D'UNITATS -------------------
;; ==============================================================

(defun obtenir-unitat-per-id (unitats id-unitat)
  "Retorna la unitat amb l'identificador 'id-unitat', o NIL si no existeix."
  (cond
    ((null unitats) nil)
    ((= (nth 1 (car unitats)) id-unitat) (car unitats))
    (t (obtenir-unitat-per-id (cdr unitats) id-unitat))))

(defun obtenir-unitat-per-coord (unitats coord)
  "Retorna la unitat situada a 'coord', o NIL si no n'hi ha cap."
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord) (car unitats))
    (t (obtenir-unitat-per-coord (cdr unitats) coord))))

(defun unitat-a-coord-p (unitats coord)
  "Retorna T si hi ha alguna unitat a la coordenada 'coord'."
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord) t)
    (t (unitat-a-coord-p (cdr unitats) coord))))

(defun obtenir-unitats-per-equip (unitats equip)
  "Retorna la subllista d'unitats que pertanyen a 'equip'."
  (cond
    ((null unitats) nil)
    ((eq (cadddr (car unitats)) equip)
     (cons (car unitats)
           (obtenir-unitats-per-equip (cdr unitats) equip)))
    (t (obtenir-unitats-per-equip (cdr unitats) equip))))

(defun obtenir-base-per-equip (unitats equip)
  "Retorna la base de l'equip indicat, o NIL si ha estat destruïda."
  (cond
    ((null unitats) nil)
    ((and (eq (nth 2 (car unitats)) 'base)
          (eq (nth 3 (car unitats)) equip))
     (car unitats))
    (t (obtenir-base-per-equip (cdr unitats) equip))))

(defun actualitzar-unitat (u nou-tr-pintar nou-tr-moure nou-tr-crear)
  "Retorna una nova unitat amb els temps de recuperació actualitzats.
   Si un paràmetre és NIL, es conserva el valor original.
   Estructura: (unitat id tipus equip coord color-propi colors tr-pintar tr-moure tr-crear)"
  (list
   'unitat
   (nth 1 u)
   (nth 2 u)
   (nth 3 u)
   (nth 4 u)
   (nth 5 u)
   (nth 6 u)
   (cond ((null nou-tr-pintar) (nth 7 u)) (t nou-tr-pintar))
   (cond ((null nou-tr-moure) (nth 8 u)) (t nou-tr-moure))
   (cond ((null nou-tr-crear) (nth 9 u)) (t nou-tr-crear))))

(defun moure-unitat (u nova-coord)
  "Retorna la unitat 'u' amb la coordenada actualitzada a 'nova-coord'."
  (list 'unitat
        (nth 1 u) (nth 2 u) (nth 3 u)
        nova-coord
        (nth 5 u) (nth 6 u) (nth 7 u) (nth 8 u) (nth 9 u)))

(defun actualitzar-unitat-per-coord (unitats coord nova-unitat)
  "Retorna la llista d'unitats amb la unitat a 'coord' substituïda per 'nova-unitat'."
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord)
     (cons nova-unitat (cdr unitats)))
    (t
     (cons (car unitats)
           (actualitzar-unitat-per-coord (cdr unitats) coord nova-unitat)))))

(defun eliminar-unitat-per-coord (unitats coord)
  "Retorna la llista d'unitats sense la unitat situada a 'coord'."
  (cond
    ((null unitats) nil)
    ((equal (nth 4 (car unitats)) coord) (cdr unitats))
    (t
     (cons (car unitats)
           (eliminar-unitat-per-coord (cdr unitats) coord)))))

(defun afegir-color-pintat (colors color)
  "Afegeix 'color' a la llista 'colors' si no hi és ja."
  (cond
    ((member color colors) colors)
    (t (append colors (list color)))))

(defun crear-base (id equip coord)
  "Crea una nova base amb els valors inicials."
  (list 'unitat id 'base equip coord
        nil  ; color-propi (les bases no en tenen)
        nil  ; colors-pintat (buida inicialment)
        0    ; tr-pintar (no usat per bases)
        0    ; tr-moure (no usat per bases)
        0))  ; tr-crear (cooldown de creació de bolles)

(defun contar-bolles-vives (unitats)
  "Compta quantes bolles hi ha a la llista d'unitats."
  (cond
    ((null unitats) 0)
    ((eq (nth 2 (car unitats)) 'bolla)
     (+ 1 (contar-bolles-vives (cdr unitats))))
    (t (contar-bolles-vives (cdr unitats)))))


;; ==============================================================
;;  ------------------- GESTIÓ LABORATORIS -------------------
;; ==============================================================

(defun actualitzar-comptadors-lab (estat casella-desti equip)
  "Actualitza els comptadors de labs capturats quan una bolla pinta un lab.
   Si el lab era de l'equip contrari, aquest el perd. L'equip atacant el guanya."
  (cond
    ((not (member 'lab casella-desti)) estat)
    (t
     (let* ((equip-anterior (obtenir-equip-lab-casella casella-desti))
            (mateix-equip (eq equip-anterior equip))
            (equip-contrari (cond ((eq equip 'e1) 'e2) (t 'e1)))
            (clau-propi (cond ((eq equip 'e1) 'labs-e1) (t 'labs-e2)))
            (clau-contrari (cond ((eq equip 'e1) 'labs-e2) (t 'labs-e1))))
       (cond
         (mateix-equip estat)
         (t
          (let* ((labs-propi-actual (cadr (assoc clau-propi estat)))
                 (estat1 (substituir-camp clau-propi (+ labs-propi-actual 1) estat))
                 (estat2 (cond
                           ((eq equip-anterior equip-contrari)
                            (let ((labs-contrari-actual (cadr (assoc clau-contrari estat1))))
                              (substituir-camp clau-contrari
                                              (max 0 (- labs-contrari-actual 1))
                                              estat1)))
                           (t estat1))))
            estat2)))))))


;; ==============================================================
;;  ------------------- TEMPS DE RECUPERACIÓ -------------------
;; ==============================================================

(defun recuperacio-pinta (unitat estat)
  "Calcula el temps de recuperació de l'acció pintar per a una bolla.
   Base = 3. Si la casella d'origen no és del color de la bolla, es triplica."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (casella-origen (obtenir-casella-mapa mapa (nth 4 unitat)))
         (color-origen (obtenir-color-terra-casella casella-origen))
         (color-bolla (nth 5 unitat))
         (penalitzacio (cond ((eq color-origen color-bolla) 1) (t 3))))
    (* 3 penalitzacio)))

(defun recuperacio-mou (unitat estat coord)
  "Calcula el temps de recuperació de l'acció moure per a una bolla.
   Base = 1. Diagonal x1.4142. Si destí no és del color de la bolla x3."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (casella-desti (obtenir-casella-mapa mapa coord))
         (color-desti (obtenir-color-terra-casella casella-desti))
         (color-bolla (nth 5 unitat))
         (d2 (dist2 (nth 4 unitat) coord))
         (diagonal (cond ((= d2 2) 1.4142) (t 1.0)))
         (penalitzacio (cond ((eq color-desti color-bolla) 1) (t 3)))
         (resultat (* diagonal penalitzacio)))
    (truncate resultat)))


;; ==============================================================
;;  ------------------- RANGS I DISTÀNCIES -------------------
;; ==============================================================

(defun coordenada-accio-dins-rang-base-p (unitat coord)
  "Retorna T si 'coord' està dins del rang de creació de bolles d'una base (d²≤2, d²>0)."
  (let* ((d2 (dist2 (nth 4 unitat) coord)))
    (and (> d2 0) (<= d2 2))))

(defun coordenada-accio-dins-rang-pinta-p (unitat coord)
  "Retorna T si 'coord' està dins del rang de pintura d'una bolla (d²≤5, d²>0)."
  (let* ((d2 (dist2 (nth 4 unitat) coord)))
    (and (<= d2 5) (> d2 0))))

(defun coordenada-accio-dins-rang-mou-p (unitat coord)
  "Retorna T si 'coord' està dins del rang de moviment d'una bolla (d²≤2, d²>0)."
  (let* ((d2 (dist2 (nth 4 unitat) coord)))
    (and (<= d2 2) (> d2 0))))


;; ==============================================================
;;  ------------------- INICIAR PARTIDA -------------------
;; ==============================================================

(defun carrega-mapa (nom)
  "Llegeix i retorna el mapa des d'un fitxer .map dins la carpeta maps/.
   Retorna NIL si el fitxer no s'ha trobat."
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

(defun inici-mapa (nom-mapa pintar-cada-n)
  "Carrega el mapa i inicia la partida."
  (color 0 0 0 255 255 255)
  (mode 0 0 640 375)
  (let* ((mapa (carrega-mapa nom-mapa))
         (estat (cond (mapa (crear-estat-inicial mapa)) (t nil))))
    (cond (estat (jugar-partida-inicial estat pintar-cada-n)) (t nil))))

(defun inici ()
  "Inicia una partida amb el mapa 'tiny' i repintat cada torn."
  (inici-mapa "tiny" 1))

(defun extraer-unitats (mapa x y next-id)
  "Recorre el mapa i extreu totes les bases, retornant (llista-unitats next-id)."
  (cond
    ((null mapa) (list nil next-id))
    (t
     (let* ((res-fila (extraer-unitats-fila (car mapa) x y next-id))
            (unitats-fila (car res-fila))
            (next-id2 (cadr res-fila))
            (res-resto (extraer-unitats (cdr mapa) 0 (+ y 1) next-id2))
            (unitats-resto (car res-resto))
            (next-id3 (cadr res-resto)))
       (list (append unitats-fila unitats-resto) next-id3)))))

(defun extraer-unitats-fila (fila x y next-id)
  "Recorre una fila del mapa i extreu les bases que hi hagi.
   Retorna (llista-unitats next-id)."
  (cond
    ((null fila) (list nil next-id))
    (t
     (let* ((celda (car fila))
            (res-resto (extraer-unitats-fila (cdr fila) (+ x 1) y next-id))
            (unitats-resto (car res-resto))
            (next-id2 (cadr res-resto)))
       (cond
         ((and (eq (car celda) 'terra) (member 'base celda))
          (let ((equip (cadr (member 'base celda))))
            (list
             (cons (crear-base next-id2 equip (list x y)) unitats-resto)
             (+ next-id2 1))))
         (t (list unitats-resto next-id2)))))))

(defun crear-estat-inicial (mapa)
  "Construeix i retorna l'estat inicial complet del joc a partir del mapa.
   L'estat és un alist amb: ronda, torn, mapa, unitats, next-id,
   pintura de cada equip, memòria compartida i comptadors de labs."
  (let* ((res (extraer-unitats mapa 0 0 0))
         (unitats (car res))
         (next-id (cadr res)))
    (list
     (list 'ronda 1)
     (list 'torn 'e1)
     (list 'mapa mapa)
     (list 'unitats unitats)
     (list 'next-id next-id)
     (list 'pintura-e1 200)
     (list 'pintura-e2 200)
     (list 'memoria-e1 nil)
     (list 'memoria-e2 nil)
     (list 'labs-e1 0)
     (list 'labs-e2 0))))


;; ==============================================================
;;  ------------------- BUCLE PRINCIPAL DE LA PARTIDA -------------------
;; ==============================================================

(defun jugar-partida-inicial (estat pintar-cada-n)
  "Executa les dues primeres meitats de torn sense actualitzar pintura ni cooldowns,
   ja que és la ronda inicial. Després entra al bucle normal."
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
            (jugar-partida estat4 pintar-cada-n))))))))

(defun jugar-partida (estat pintar-cada-n)
  "Bucle recursiu principal de la partida. A cada iteració:
   actualitza pintura, baixa cooldowns, executa el torn i avança al següent.
   Pinta el mapa cada 'pintar-cada-n' rondes."
  (cond
    ((final-partida-p estat)
     (finalitzar-partida estat))
    (t
     (let* ((estat1 (actualitzar-pintura estat))
            (estat2 (baixar-cooldowns estat1))
            (estat3 (executar-torn estat2))
            (estat4 (seguent-torn estat3))
            (ronda (cadr (assoc 'ronda estat4))))
       (cond
         ((= (mod ronda pintar-cada-n) 0)
          (sleep 0.05)
          (pinta estat4))
         (t nil))
       (jugar-partida estat4 pintar-cada-n)))))


;; ==============================================================
;;  ------------------- CONTROL FINAL PARTIDA -------------------
;; ==============================================================

(defun final-partida-p (estat)
  "Retorna T si la partida ha acabat: una base ha estat destruïda o s'han superat 1500 rondes."
  (or (base-pintada-3-colors-p estat)
      (>= (cadr (assoc 'ronda estat)) 1500)))

(defun base-pintada-3-colors-p (estat)
  "Retorna T si alguna de les dues bases ha estat destruïda (no apareix a la llista d'unitats)."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (base-e1 (obtenir-base-per-equip unitats 'e1))
         (base-e2 (obtenir-base-per-equip unitats 'e2)))
    (or (null base-e1) (null base-e2))))

(defun finalitzar-partida (estat)
  "Determina el guanyador i mostra el resultat final per pantalla.
   Criteris de desempat (per ordre): base destruïda > bolles vives > pintura > aleatori."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (e1 (obtenir-unitats-per-equip unitats 'e1))
         (e2 (obtenir-unitats-per-equip unitats 'e2))
         (base-e1-explotada (null (obtenir-base-per-equip e1 'e1)))
         (base-e2-explotada (null (obtenir-base-per-equip e2 'e2)))
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
            (t (random-empate)))))
    (pinta estat)
    (color 0 0 0 255 255 255)
    (goto-xy 10 340)
    (princ "================================")
    (terpri)
    (goto-xy 10 355)
    (cond
      ((eq resultat 'guanya-e1) (princ "GUANYA EQUIP E1 (LILA)"))
      ((eq resultat 'guanya-e2) (princ "GUANYA EQUIP E2 (TARONJA)")))
    (terpri)
    (goto-xy 10 370)
    (princ "================================")
    resultat))

(defun random-empate ()
  "Retorna un guanyador aleatori en cas d'empat total."
  (cond
    ((= (random 2) 0) 'guanya-e1)
    (t 'guanya-e2)))


;; ==============================================================
;;  ------------------- ACTUALITZACIONS D'ESTAT -------------------
;; ==============================================================

(defun seguent-torn (estat)
  "Canvia l'equip actiu al torn següent. Si es passa de e2 a e1, incrementa la ronda."
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
  "Afegeix pintura a l'equip actiu: +2 base, +1 per cada lab capturat."
  (let* ((torn (cadr (assoc 'torn estat)))
         (labs (cond
                 ((eq torn 'e1) (cadr (assoc 'labs-e1 estat)))
                 (t (cadr (assoc 'labs-e2 estat))))))
    (cond
      ((eq torn 'e1)
       (substituir-camp 'pintura-e1
                        (+ (cadr (assoc 'pintura-e1 estat)) 2 labs)
                        estat))
      (t
       (substituir-camp 'pintura-e2
                        (+ (cadr (assoc 'pintura-e2 estat)) 2 labs)
                        estat)))))

(defun baixar-cooldowns (estat)
  "Decrementa en 1 els temps de recuperació de les unitats de l'equip actiu."
  (let* ((torn (cadr (assoc 'torn estat)))
         (unitats (cadr (assoc 'unitats estat)))
         (unitats2 (baixar-cooldowns-llista unitats torn)))
    (substituir-camp 'unitats unitats2 estat)))

(defun baixar-cooldowns-llista (unitats equip)
  "Recorre la llista d'unitats i decrementa els cooldowns de les de 'equip'."
  (cond
    ((null unitats) nil)
    (t
     (let ((u (car unitats)))
       (cond
         ((eq (nth 3 u) equip)
          (cons (baixar-cooldown-unitat u)
                (baixar-cooldowns-llista (cdr unitats) equip)))
         (t
          (cons u
                (baixar-cooldowns-llista (cdr unitats) equip))))))))

(defun baixar-cooldown-unitat (u)
  "Decrementa els temps de recuperació d'una unitat individual.
   Les bases actualitzen tr-crear; les bolles actualitzen tr-pintar i tr-moure."
  (let ((tipus (nth 2 u))
        (tr-pintar (nth 7 u))
        (tr-moure (nth 8 u))
        (tr-crear (nth 9 u)))
    (cond
      ((eq tipus 'base)
       (actualitzar-unitat u 0 0 (decrementar-si-numero tr-crear)))
      ((eq tipus 'bolla)
       (actualitzar-unitat u
                           (decrementar-si-numero tr-pintar)
                           (decrementar-si-numero tr-moure)
                           0))
      (t u))))

(defun obtenir-pintura-equip (estat equip)
  "Retorna la quantitat de pintura disponible de l'equip indicat."
  (cond
    ((eq equip 'e1) (cadr (assoc 'pintura-e1 estat)))
    (t (cadr (assoc 'pintura-e2 estat)))))

(defun obtenir-memoria-equip (estat equip)
  "Retorna la memòria compartida de l'equip indicat."
  (cond
    ((eq equip 'e1) (cadr (assoc 'memoria-e1 estat)))
    (t (cadr (assoc 'memoria-e2 estat)))))


;; ==============================================================
;;  ------------------- EXECUCIÓ DEL TORN -------------------
;; ==============================================================

(defun executar-torn (estat)
  "Executa el torn de l'equip actiu, processant totes les seves unitats."
  (let ((equip (cadr (assoc 'torn estat))))
    (executar-unitats estat equip)))

(defun executar-unitats (estat equip)
  "Inicia el processament recursiu de les unitats de 'equip'."
  (executar-unitats-rec estat equip nil))

(defun executar-unitats-rec (estat equip ids-processats)
  "Processa les unitats de 'equip' una a una, de manera recursiva.
   Les unitats creades durant el torn també poden actuar."
  (let ((unitat (obtenir-seguent-unitat-equip estat equip ids-processats)))
    (cond
      ((null unitat) estat)
      (t
       (let* ((info-unitat (construir-info-unitat estat unitat equip))
              (accions (cridar-agent-unitat info-unitat))
              (estat2 (processar-accions-unitat estat unitat equip accions))
              (id-unitat (nth 1 unitat)))
         (executar-unitats-rec estat2 equip (cons id-unitat ids-processats)))))))

(defun obtenir-seguent-unitat-equip (estat equip ids-processats)
  "Retorna la primera unitat de 'equip' que encara no ha estat processada."
  (obtenir-seguent-unitat-equip-rec
   (obtenir-unitats-per-equip (cadr (assoc 'unitats estat)) equip)
   ids-processats))

(defun obtenir-seguent-unitat-equip-rec (unitats ids-processats)
  "Cerca la primera unitat de la llista que no estigui a 'ids-processats'."
  (cond
    ((null unitats) nil)
    ((member (nth 1 (car unitats)) ids-processats)
     (obtenir-seguent-unitat-equip-rec (cdr unitats) ids-processats))
    (t (car unitats))))

(defun construir-info-unitat (estat unitat equip)
  "Construeix la llista d'informació que es passa a l'agent intel·ligent.
   Format: (ronda equip pintura id-unitat tipus-unitat coordenada colors-pintat
            color-propi tr-pintar tr-moure visió memòria-compartida)"
  (list (cadr (assoc 'ronda estat))
        equip
        (obtenir-pintura-equip estat equip)
        (nth 1 unitat)
        (nth 2 unitat)
        (nth 4 unitat)
        (nth 6 unitat)
        (nth 5 unitat)
        (nth 7 unitat)
        (nth 8 unitat)
        (construir-visio-unitat estat unitat)
        (obtenir-memoria-equip estat equip)))

(defun cridar-agent-unitat (info-unitat)
  "Crida la funció de l'agent intel·ligent corresponent a l'equip de la unitat.
   Retorna la llista d'accions que l'agent vol executar."
  (let ((equip (cadr info-unitat)))
    (cond
      ((eq equip 'e1) (agent-cms213 info-unitat))
      ((eq equip 'e2) (agent-nms864 info-unitat))
      (t nil))))

(defun processar-accions-unitat (estat unitat equip accions)
  "Processa recursivament la llista d'accions d'una unitat, validant i aplicant cada una."
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


;; ==============================================================
;;  ------------------- VALIDACIÓ D'ACCIONS -------------------
;; ==============================================================

(defun validar-coord-base (estat coord)
  "Comprova que una coordenada és vàlida, dins del mapa i sobre terra.
   Funció auxiliar compartida per les tres validacions d'acció."
  (and (coord-valida-p coord)
       (coordenada-accio-dins-mapa-p estat coord)
       (coordenada-accio-no-aigua-p estat coord)))

(defun validar-accio (estat unitat equip accio)
  "Comprova si una acció és vàlida per a la unitat i l'estat actuals.
   Retorna T si és vàlida, NIL si no."
  (cond
    ((not (accio-formada-p accio)) nil)
    (t
     (let* ((tipus-accio (car accio))
            (arguments (cadr accio)))
       (cond
         ((eq tipus-accio 'crea-bolla)
          (validar-crea-bolla estat unitat equip (car arguments) (cadr arguments)))
         ((eq tipus-accio 'pinta)
          (validar-pinta estat unitat equip (car arguments)))
         ((eq tipus-accio 'mou)
          (validar-mou estat unitat equip (car arguments)))
         ((eq tipus-accio 'escriu-memoria) t)
         (t nil))))))

(defun validar-crea-bolla (estat unitat equip color coord)
  "Valida l'acció crea-bolla d'una base: comprova tipus, cooldown, pintura,
   color vàlid, coordenada dins rang i lliure."
  (and (eq (nth 2 unitat) 'base)
       (temps-unitat-disponible-p unitat 'crear)
       (pintura-suficient-crea-bolla-p estat equip)
       (member color '(r g b))
       (validar-coord-base estat coord)
       (coordenada-accio-dins-rang-base-p unitat coord)
       (coordenada-accio-lliure-p estat coord)))

(defun validar-pinta (estat unitat equip coord)
  "Valida l'acció pinta d'una bolla: comprova tipus, cooldown,
   coordenada dins rang i que la casella sigui pintable."
  (and (eq (nth 2 unitat) 'bolla)
       (temps-unitat-disponible-p unitat 'pintar)
       (validar-coord-base estat coord)
       (coordenada-accio-dins-rang-pinta-p unitat coord)
       (coordenada-accio-pintable-p estat unitat equip coord)))

(defun validar-mou (estat unitat equip coord)
  "Valida l'acció mou d'una bolla: comprova tipus, cooldown,
   coordenada dins rang i que la casella destí estigui lliure."
  (and (eq (nth 2 unitat) 'bolla)
       (temps-unitat-disponible-p unitat 'moure)
       (validar-coord-base estat coord)
       (coordenada-accio-dins-rang-mou-p unitat coord)
       (coordenada-accio-lliure-p estat coord)))

(defun temps-unitat-disponible-p (unitat tipus-accio)
  "Retorna T si el temps de recuperació de 'tipus-accio' és menor que 1."
  (let ((temps (cond
                 ((eq tipus-accio 'pintar) (nth 7 unitat))
                 ((eq tipus-accio 'moure) (nth 8 unitat))
                 ((eq tipus-accio 'crear) (nth 9 unitat))
                 (t nil))))
    (or (null temps) (< temps 1))))

(defun pintura-suficient-crea-bolla-p (estat equip)
  "Retorna T si l'equip té almenys 50 unitats de pintura per crear una bolla."
  (>= (obtenir-pintura-equip estat equip) 50))


;; ==============================================================
;;  ------------------- APLICACIÓ D'ACCIONS -------------------
;; ==============================================================

(defun aplicar-accio (estat unitat equip accio)
  "Aplica una acció validada i retorna el nou estat resultant."
  (cond
    ((not (accio-formada-p accio)) estat)
    (t
     (let* ((tipus-accio (car accio))
            (arguments (cadr accio)))
       (cond
         ((eq tipus-accio 'crea-bolla)
          (aplicar-crea-bolla estat unitat equip (car arguments) (cadr arguments)))
         ((eq tipus-accio 'pinta)
          (aplicar-pinta estat unitat equip (car arguments)))
         ((eq tipus-accio 'mou)
          (aplicar-mou estat unitat equip (car arguments)))
         ((eq tipus-accio 'escriu-memoria)
          (aplicar-escriu-memoria estat unitat equip (car arguments)))
         (t estat))))))

(defun aplicar-crea-bolla (estat unitat equip color coord)
  "Crea una nova bolla, incrementa el cooldown de la base, resta 50 de pintura
   i actualitza el mapa i la llista d'unitats."
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
          (list 'unitat next-id 'bolla equip coord color (list color) 0 0 nil))
         (unitats1
          (cond
            (base-actualitzada
             (actualitzar-unitat-per-coord unitats (nth 4 base-actualitzada) base-actualitzada))
            (t unitats)))
         (unitats2 (cons nova-bolla unitats1))
         (mapa (cadr (assoc 'mapa estat)))
         (casella-bolla (list 'terra color 'bolla color next-id equip (list color) 0 0))
         (mapa1 (actualitzar-casella-mapa mapa coord casella-bolla))
         (estat1 (substituir-camp 'unitats unitats2 estat))
         (estat2 (substituir-camp 'mapa mapa1 estat1))
         (estat3 (substituir-camp 'next-id (+ next-id 1) estat2)))
    (cond
      ((eq equip 'e1)
       (substituir-camp 'pintura-e1 (- pintura-equip 50) estat3))
      (t
       (substituir-camp 'pintura-e2 (- pintura-equip 50) estat3)))))

(defun aplicar-pinta (estat unitat equip coord)
  "Pinta la casella destí i actualitza l'element que hi ha (bolla, base o lab).
   Si una bolla o base queda pintada de 3 colors, s'elimina (explota).
   Si es pinta un lab, canvia de propietari."
  (let* ((mapa (cadr (assoc 'mapa estat)))
         (unitats (cadr (assoc 'unitats estat)))
         (casella-desti (obtenir-casella-mapa mapa coord))
         (color-bolla (nth 5 unitat))
         (tr-extra (recuperacio-pinta unitat estat))
         (color-actual-terra (obtenir-color-terra-casella casella-desti))
         (casella-pintada (cond
                            ((eq color-actual-terra color-bolla) casella-desti)
                            (t (canviar-color-casella casella-desti color-bolla))))
         ;; Actualitzar el cooldown de la bolla que pinta
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
         ;; Actualitzar la unitat destí (si n'hi ha)
         (unitat-desti (obtenir-unitat-per-coord unitats1 coord))
         (colors-desti (and unitat-desti
                            (afegir-color-pintat (nth 6 unitat-desti) color-bolla)))
         (unitat-desti-actual
          (cond
            ((null unitat-desti) nil)
            ((eq (nth 3 unitat-desti) equip) unitat-desti) ; mateixa unitat aliada, no canvia
            ((eq (nth 2 unitat-desti) 'base)
             (list 'unitat
                   (nth 1 unitat-desti) (nth 2 unitat-desti) (nth 3 unitat-desti)
                   (nth 4 unitat-desti) (nth 5 unitat-desti) colors-desti
                   (nth 7 unitat-desti) (nth 8 unitat-desti) (nth 9 unitat-desti)))
            ((eq (nth 2 unitat-desti) 'bolla)
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3)) nil) ; explota
               (t
                (list 'unitat
                      (nth 1 unitat-desti) (nth 2 unitat-desti) (nth 3 unitat-desti)
                      (nth 4 unitat-desti) (nth 5 unitat-desti) colors-desti
                      (nth 7 unitat-desti) (nth 8 unitat-desti) (nth 9 unitat-desti)))))
            (t unitat-desti)))
         (unitats2
          (cond
            ((null unitat-desti) unitats1)
            ((eq (nth 3 unitat-desti) equip) unitats1)
            ((eq (nth 2 unitat-desti) 'base)
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3))
                (eliminar-unitat-per-coord unitats1 coord)) ; base explota
               (t (actualitzar-unitat-per-coord unitats1 coord unitat-desti-actual))))
            ((eq (nth 2 unitat-desti) 'bolla)
             (cond
               (unitat-desti-actual
                (actualitzar-unitat-per-coord unitats1 coord unitat-desti-actual))
               (t (eliminar-unitat-per-coord unitats1 coord)))) ; bolla explota
            (t unitats1)))
         ;; Actualitzar la casella del mapa
         (nova-casella
          (cond
            ((member 'lab casella-desti)
             (canviar-lab-equip-casella casella-pintada equip))
            ((member 'bolla casella-desti)
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3))
                (list 'terra color-bolla)) ; bolla explota, queda terra
               (t
                (let ((bolla (member 'bolla casella-desti)))
                  (list 'terra color-bolla 'bolla
                        (cadr bolla) (caddr bolla) (cadddr bolla)
                        colors-desti
                        (cadr (cddddr bolla)) (caddr (cddddr bolla)))))))
            ((member 'base casella-desti)
             (cond
               ((and (listp colors-desti) (= (length colors-desti) 3))
                (list 'terra color-bolla)) ; base explota, queda terra
               (t
                (let ((base (member 'base casella-desti)))
                  (list 'terra color-bolla 'base (cadr base) colors-desti)))))
            (t casella-pintada)))
         (mapa1 (actualitzar-casella-mapa mapa coord nova-casella))
         (estat1 (substituir-camp 'mapa mapa1 estat))
         (estat2 (substituir-camp 'unitats unitats2 estat1)))
    (actualitzar-comptadors-lab estat2 casella-desti equip)))

(defun aplicar-mou (estat unitat equip coord)
  "Mou la bolla a la coordenada destí, incrementa el seu cooldown de moviment
   i actualitza el mapa (casella origen buida, casella destí amb la bolla)."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (unitat-actual (obtenir-unitat-per-id unitats (nth 1 unitat)))
         (coord-origen (nth 4 unitat-actual))
         (tr-extra (recuperacio-mou unitat estat coord))
         (unitat-moguda (actualitzar-unitat unitat-actual
                                            (nth 7 unitat-actual)
                                            (incrementar-si-numero (nth 8 unitat-actual) tr-extra)
                                            (nth 9 unitat-actual)))
         (unitat-final (moure-unitat unitat-moguda coord))
         (unitats1 (actualitzar-unitat-per-coord unitats coord-origen unitat-final))
         (mapa (cadr (assoc 'mapa estat)))
         (color-origen (obtenir-color-terra-casella (obtenir-casella-mapa mapa coord-origen)))
         (color-desti (obtenir-color-terra-casella (obtenir-casella-mapa mapa coord)))
         (color-bolla (nth 5 unitat))
         (mapa1 (actualitzar-casella-mapa mapa coord-origen (list 'terra color-origen)))
         (mapa2 (actualitzar-casella-mapa mapa1 coord
                  (list 'terra color-desti 'bolla color-bolla
                        (nth 1 unitat) equip (nth 6 unitat)
                        (nth 7 unitat-final) (nth 8 unitat-final))))
         (estat1 (substituir-camp 'unitats unitats1 estat)))
    (substituir-camp 'mapa mapa2 estat1)))

(defun aplicar-escriu-memoria (estat unitat equip nova-memoria)
  "Actualitza la memòria compartida de l'equip amb el nou valor proporcionat per la unitat."
  (cond
    ((eq equip 'e1) (substituir-camp 'memoria-e1 nova-memoria estat))
    (t (substituir-camp 'memoria-e2 nova-memoria estat))))


;; ==============================================================
;;  ------------------- VISIÓ DE LES UNITATS -------------------
;; ==============================================================

(defun construir-visio-unitat (estat unitat)
  "Construeix la llista de caselles visibles per a 'unitat' segons el seu rang.
   Rang base = 64u², rang bolla = 20u². Itera per desplaçaments dins el quadrat
   del radi i filtra les caselles dins el cercle."
  (let* ((rango (cond ((eq (nth 2 unitat) 'base) 64) (t 20)))
         (coord-origen (nth 4 unitat))
         (mapa (cadr (assoc 'mapa estat)))
         (unitats (cadr (assoc 'unitats estat)))
         (r (truncate (sqrt rango))))
    (construir-visio-dx mapa unitats coord-origen rango r (- r) (- r))))

(defun construir-visio-dx (mapa unitats coord-origen rango r dx dy)
  "Itera per tots els desplaçaments (dx, dy) dins el quadrat [-r, r]×[-r, r]
   i afegeix a la visió les caselles amb d²≤rango que existeixin al mapa."
  (cond
    ((> dx r) nil)
    ((> dy r)
     (construir-visio-dx mapa unitats coord-origen rango r (+ dx 1) (- r)))
    (t
     (let* ((x (+ (car coord-origen) dx))
            (y (+ (cadr coord-origen) dy))
            (d2 (+ (* dx dx) (* dy dy))))
       (cond
         ((and (<= d2 rango) (>= x 0) (>= y 0))
          (let* ((casella (obtenir-casella-mapa mapa (list x y))))
            (cond
              ((not (null casella))
               (let* ((coord (list x y))
                      (unitat-casella (obtenir-unitat-per-coord unitats coord)))
                 (cons (construir-entrada-visio coord casella unitat-casella)
                       (construir-visio-dx mapa unitats coord-origen rango r dx (+ dy 1)))))
              (t
               (construir-visio-dx mapa unitats coord-origen rango r dx (+ dy 1))))))
         (t
          (construir-visio-dx mapa unitats coord-origen rango r dx (+ dy 1))))))))

(defun construir-entrada-visio (coord casella unitat-casella)
  "Construeix l'entrada de la llista de visió per a una casella concreta.
   Si és aigua, retorna (coord 'aigua). Si és terra, retorna informació completa."
  (cond
    ((eq (car casella) 'aigua)
     (list coord 'aigua))
    (t
     (let* ((tipus-casella (car casella))
            (color-casella (cadr casella))
            (te (cond
                  ((and unitat-casella (eq (nth 2 unitat-casella) 'base)) 'base)
                  ((and unitat-casella (eq (nth 2 unitat-casella) 'bolla)) 'bolla)
                  ((member 'lab casella) 'lab)
                  ((member 'base casella) 'base)
                  ((member 'bolla casella) 'bolla)
                  (t nil)))
            (eq-u (and unitat-casella (nth 3 unitat-casella)))
            (col-p (and unitat-casella (nth 6 unitat-casella)))
            (col-pr (and unitat-casella (nth 5 unitat-casella)))
            (tr-p (and unitat-casella (nth 7 unitat-casella)))
            (tr-m (and unitat-casella (nth 8 unitat-casella)))
            (eq-lab (and (null unitat-casella)
                         (member 'lab casella)
                         (obtenir-equip-lab-casella casella)))
            (equip-final (cond (eq-u eq-u) (eq-lab eq-lab) (t nil))))
       (list coord tipus-casella color-casella
             te equip-final col-p col-pr tr-p tr-m)))))
