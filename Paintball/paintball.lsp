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
   (list 'pintura-e2 200)
   (list 'memoria-compartida nil)))  ; <-- AFEGIT

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
  "Genera 10 posicions aleatòries lliures per als laboratoris."
  (generar-coords-aleatories 10 estat))

(defun generar-coords-aleatories (n estat)
  "Genera recursivament n coordenades aleatòries lliures."
  (if (= n 0)
      nil
      (cons (coord-lliure-aleatoria estat)
            (generar-coords-aleatories (- n 1) estat))))

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
  "Bucle principal de la partida. Alterna torns fins que hi ha guanyador."
  (if (es-final estat)
      (qui-ha-guanyat estat)
      (let* (;; 1. incrementar pintura a l'INICI del torn 
             (equip-actual (get-torn-actual estat))
             (estat1 (incrementar-pintura estat equip-actual))

             ;; 2. executar el torn (totes les unitats de l'equip)
             (estat2 (executar-torn estat1))

             ;; 3. canviar de torn
             (estat3 (canviar-equip estat2))

             ;; 4. incrementar ronda quan torna a e1
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
  "Retorna t si la partida ha acabat (base destruïda o límit de torns)."
  (or (>= (get-ronda-actual estat) 1500)
      (base-destruida estat 'e1)
      (base-destruida estat 'e2)))



(defun qui-ha-guanyat (estat)
  (cond ((base-destruida estat 'e1) 'e2)
        ((base-destruida estat 'e2) 'e1)
        (t (comparar-puntuacio estat))))


(defun base-destruida (estat equip)
  "Retorna t si la base de l'equip indicat ha estat destruïda (pintada dels 3 colors)."
  (let ((base (buscar-base-equip (cadr (assoc 'bases estat)) equip)))
    (and base
         (let ((colors (cadr (assoc 'colors-pintat base))))
           (and (membre 'r colors)
                (membre 'g colors)
                (membre 'b colors))))))

(defun buscar-base-equip (bases equip)
  "Cerca recursivament la base de l'equip indicat."
  (cond
    ((null bases) nil)
    ((eq (cadr (assoc 'equip (car bases))) equip) (car bases))
    (t (buscar-base-equip (cdr bases) equip))))


;; Membre propi per evitar dependències de :test
(defun membre (element llista)
  "Comprova si element és a la llista (comparació amb eq)."
  (cond
    ((null llista) nil)
    ((eq element (car llista)) t)
    (t (membre element (cdr llista)))))

(defun comptar-unitats-equip (unitats equip)
  "Compta quantes unitats pertanyen a un equip."
  (if (null unitats)
      0
      (+ (if (eq (cadr (assoc 'equip (car unitats))) equip)
             1
             0)
         (comptar-unitats-equip (cdr unitats) equip))))


(defun comparar-puntuacio (estat)
  "Compara punts per decidir l'equip guanyador."
  (let* ((unitats (cadr (assoc 'unitats estat)))

         ;; comptar bolles per equip (recursiu)
         (bolles-e1 (comptar-unitats-equip unitats 'e1))
         (bolles-e2 (comptar-unitats-equip unitats 'e2))

         ;; pintura restant
         (pintura-e1 (cadr (assoc 'pintura-e1 estat)))
         (pintura-e2 (cadr (assoc 'pintura-e2 estat))))

    (cond
      ((> bolles-e1 bolles-e2) 'e1)
      ((> bolles-e2 bolles-e1) 'e2)
      ((> pintura-e1 pintura-e2) 'e1)
      ((> pintura-e2 pintura-e1) 'e2)
      (t (if (= (random 2) 0) 'e1 'e2)))))




;; ------------------------------------------------------------------
;;  ------------------- JUGAR PARTIDA -------------------
;; ------------------------------------------------------------------

(defun executar-torn (estat)
  "Executa totes les accions de les unitats de l'equip actual en aquest torn, recursivament."
  (let* ((equip-actual (get-torn-actual estat)) ; torn del equip que esta jugant
         (unitats (get-unitats-equip estat equip-actual))) ; obtenir unitats del equip
    
    ; executar unitats
    (executar-unitats unitats estat)))

;; Funció auxiliar per obtenir només les unitats de l’equip actual
(defun get-unitats-equip (estat equip)
  (filtrar-unitats (cadr (assoc 'unitats estat)) equip))

(defun filtrar-unitats (unitats equip)
  (if (null unitats)
      nil
      (let ((u (car unitats)))
        (if (eq (cadr (assoc 'equip u)) equip)
            (cons u (filtrar-unitats (cdr unitats) equip))
            (filtrar-unitats (cdr unitats) equip)))))

;; Funció auxiliar recursiva per executar accions de cada unitat
(defun executar-unitats (unitats estat)
  (if (null unitats)
      estat ; no hi ha unitats, retornem estat final
      (let* ((u (car unitats)) ; agafam primera unitat
             (accio (rebre-accio-agent u estat)) ; quins acció vol fer aquesta unitat
             (estat2 (if (validar-accio accio estat u) ; mirar si es possible fer l'acció
                         (executar-accio accio u estat) ; executan si es valida
                         estat))    ; sino retornem estat actual
             (estat3 (marcar-unitat-actuada u estat2))) ; marcam unitat actuada per no tornar-la a usar
        (executar-unitats (cdr unitats) estat3)))) ; recursivitat

;; Funció per marcar la unitat com ha-actuat
(defun marcar-unitat-actuada (unitat estat)
  "Marca la unitat dins l'estat com a ha-actuat sense mutar l'estat."
  (let* ((unitats (cadr (assoc 'unitats estat)))
         (noves-unitats (marcar-unitat-rec unitats unitat)))
    ;; Reconstruïm l'estat substituint només el camp 'unitats
    (substituir-camp 'unitats noves-unitats estat)))

;; Funció recursiva que recorre la llista de unitats i marca la unitat indicada
(defun marcar-unitat-rec (unitats unitat-a-marcar)
  "Marca la unitat amb el mateix id que unitat-a-marcar com a ha-actuat."
  (if (null unitats)
      nil
      (let ((u (car unitats)))
        (if (equal (cadr (assoc 'id u))
                   (cadr (assoc 'id unitat-a-marcar)))
            (cons (marcar-unitat-directament u)
                  (marcar-unitat-rec (cdr unitats) unitat-a-marcar))
            (cons u
                  (marcar-unitat-rec (cdr unitats) unitat-a-marcar))))))


;; Funció que retorna una unitat amb el camp 'ha-actuat t
(defun marcar-unitat-directament (unitat)
  "Retorna la unitat amb (ha-actuat t) sense usar remove-if."
  (cons (list 'ha-actuat t)
        (eliminar-ha-actuat unitat)))

(defun eliminar-ha-actuat (unitat)
  (if (null unitat)
      nil
      (let ((camp (car unitat)))
        (if (eq (car camp) 'ha-actuat)
            ;; saltam aquest element
            (eliminar-ha-actuat (cdr unitat))
            ;; el mantenim
            (cons camp
                  (eliminar-ha-actuat (cdr unitat)))))))

; rebre moviment agent  d'una unitat en concret
(defun rebre-accio-agent (unitat estat)
  "Demana a l'agent corresponent quina acció vol fer aquesta unitat."
  
  (let ((equip (cadr (assoc 'equip unitat))))
    
    (cond
      ;; Agent de l'equip e1
      ((eq equip 'e1)
       (agent-e1 estat unitat))

      ;; Agent de l'equip e2
      ((eq equip 'e2)
       (agent-e2 estat unitat))

      ;; seguretat per si passa algo estrany
      (t
       nil))))


(defun validar-accio (accio estat unitat)
  "Valida si una acció retornada per l'agent és legal.
   Les accions vàlides són: 'crea-bolla, 'pinta, 'mou, 'escriu-memoria."
  (let ((nom  (car accio))
        (args (cadr accio)))
    (cond

      ;; ACCIÓ: mou  ->  (mou (x y))
      ((eq nom 'mou)
       (validar-moviment unitat args estat))

      ;; ACCIÓ: crea-bolla  ->  (crea-bolla (color (x y)))
      ;;   Només les bases poden crear bolles.
      ;;   Cal tenir >= 50 pintura.
      ;;   La coordenada destí ha de ser adjacent i lliure.
      ((eq nom 'crea-bolla)
       (and (eq (cadr (assoc 'tipus unitat)) 'base)
            (>= (get-pintura-actual estat (cadr (assoc 'equip unitat))) 50)
            (let ((dest (cadr args)))
              (and dest
                   (es-posicio-lliure dest estat)
                   (<= (distancia-quadrat (cadr (assoc 'coord unitat)) dest) 2)))))

      ;; ACCIÓ: pinta  ->  (pinta ((x y)))
      ;;   Només les bolles poden pintar.
      ;;   La casella destí ha d'estar dins rang r_pintar = 5 u².
      ((eq nom 'pinta)
       (and (eq (cadr (assoc 'tipus unitat)) 'bolla)
            (< (cadr (assoc 'tr-pintar unitat)) 1)
            (let ((dest (car args)))
              (and dest
                   (not (es-aigua dest estat))
                   (<= (distancia-quadrat (cadr (assoc 'coord unitat)) dest) 5)))))

      ;; ACCIÓ: escriu-memoria  ->  sempre vàlida (el controlador aplicara limit d'atoms)
      ((eq nom 'escriu-memoria) t)

      (t nil))))

(defun distancia-quadrat (a b)
  "Calcula la distància euclidiana al quadrat entre dues coordenades."
  (let ((dx (- (car a) (car b)))
        (dy (- (cadr a) (cadr b))))
    (+ (* dx dx) (* dy dy))))


(defun executar-accio (accio unitat estat)
  "Executa l'acció validada de la unitat i retorna l'estat actualitzat."
  (let ((nom  (car accio))
        (args (cadr accio)))
    (cond
      ((eq nom 'mou)
       (moure-bolla unitat (car args) estat))

      ((eq nom 'crea-bolla)
       (crear-bolla estat
                    (cadr (assoc 'equip unitat))
                    (car args)))  ;; args = (color (x y))

      ((eq nom 'pinta)
       (aplicar-pintura unitat (car args) estat))

      ((eq nom 'escriu-memoria)
       (substituir-camp 'memoria-compartida (car args) estat))

      (t estat))))  ;; acció desconeguda -> no fer res





;; ------------------------------------------------------------------
;;  ------------------- GETTERS DE L'ESTAT ACTUAL -------------------
;; ------------------------------------------------------------------

;; Retorna l'estat complet (ara rep l'estat per paràmetre, funcional)
(defun get-estat-actual (estat)
  "Retorna l'estat complet del joc."
  estat)

(defun get-ronda-actual (estat)
  "Retorna el número de ronda actual."
  (cadr (assoc 'ronda estat)))

(defun get-torn-actual (estat)
  (cadr (assoc 'torn estat))
)


(defun get-pintura-actual (estat equip)
"Retorna la pintura actual de l'equip 'e1 o 'e2."
  (cadr
   (assoc
    (if (eq equip 'e1)
        'pintura-e1
        'pintura-e2)
    estat)))

;; Retorna l'id d'una unitat (bolla o base)
(defun get-bolla-id (unitat)
  "Retorna l'id únic d'una unitat."
  (cadr (assoc 'id unitat)))

;; GET COLORS PINTATS (bolla, base)
;; Retorna la llista de colors dels quals una unitat està pintada
(defun get-colors-pintats (unitat)
  "Retorna la llista de colors dels quals la unitat està pintada (pot ser buida)."
  (cadr (assoc 'colors-pintat unitat)))

;; get suma bolles per equip
(defun get-suma-bolles-equip (estat equip)
  "Retorna el nombre de bolles vives de l'equip indicat."
  (get-suma-bolles-rec (cadr (assoc 'unitats estat)) equip))


(defun get-suma-bolles-rec (unitats equip)
  "Funció recursiva auxiliar per comptar bolles d'un equip."
  (cond
    ((null unitats) 0)
    ;; Només comptem bolles (no bases) de l'equip
    ((and (eq (cadr (assoc 'equip (car unitats))) equip)
          (eq (cadr (assoc 'tipus (car unitats))) 'bolla))
     (+ 1 (get-suma-bolles-rec (cdr unitats) equip)))
    (t
     (get-suma-bolles-rec (cdr unitats) equip))))

;; Compta quants laboratoris té capturats un equip
(defun get-suma-laboratoris-equip (estat equip)
  "Retorna el nombre de laboratoris capturats per l'equip indicat."
  (get-suma-labs-rec (cadr (assoc 'laboratoris estat)) equip))

(defun get-suma-labs-rec (labs equip)
  "Funció recursiva auxiliar per comptar laboratoris capturats."
  (cond
    ((null labs) 0)
    ((eq (cadr (assoc 'equip (car labs))) equip)
     (+ 1 (get-suma-labs-rec (cdr labs) equip)))
    (t
     (get-suma-labs-rec (cdr labs) equip))))

;; Retorna el temps de recuperació d'una acció concreta d'una unitat
(defun get-temps-recuperacio (unitat tipus-accio)
  "Retorna el temps de recuperació associat a una acció d'una unitat."
  (cond
    ((eq tipus-accio 'pintar) (cadr (assoc 'tr-pintar unitat)))
    ((eq tipus-accio 'moure)  (cadr (assoc 'tr-moure  unitat)))
    (t nil)))


;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE TORNS ------------------- 
;; ------------------------------------------------------------------



;(defun canviar-equip ()
  ;(setf (cadr (assoc 'torn *estat*))
   ;     (cond
  ;        ((eq (get-torn-actual) 'e1) 'e2)
 ;         ((eq (get-torn-actual) 'e2) 'e1)))
;)
(defun canviar-equip (estat)
  "Canvia el torn de l'equip actual sense usar subst."
  (let* ((torn-actual (cadr (assoc 'torn estat)))
         (nou-torn (if (eq torn-actual 'e1)
                       'e2
                       'e1)))
    
    ;; reconstruïm estat canviant només el camp 'torn
    (canviar-torn-rec estat nou-torn)))

(defun canviar-torn-rec (estat nou-torn)
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



; increment de pintura +1 per torn
; increment de pintura +2 per cada laboratori
(defun incrementar-pintura (estat equip)
  "Incrementa la pintura de l'equip: +2 base + +1 per cada laboratori capturat."
  (let* ((clau (if (eq equip 'e1) 'pintura-e1 'pintura-e2))
         (labs-capturats (get-suma-laboratoris-equip estat equip))
         (increment (+ 2 labs-capturats)))
    (incrementar-pintura-rec estat clau increment)))

(defun incrementar-pintura-rec (estat clau increment)
  "Funció recursiva auxiliar que suma l'increment al camp de pintura."
  (cond
    ((null estat) nil)
    ((eq (caar estat) clau)
     (cons (list clau (+ (cadr (car estat)) increment)) (cdr estat)))
    (t
     (cons (car estat) (incrementar-pintura-rec (cdr estat) clau increment)))))


(defun decrementar-pintura (quantitat equip estat)
  "Decrementa certa quantitat de pintura de l'equip indicat."
  (let* ((clau (if (eq equip 'e1) 'pintura-e1 'pintura-e2))
         (actual (cadr (assoc clau estat)))
         (nou (max 0 (- actual quantitat))))
    (decrementar-pintura-rec estat clau nou)))

(defun decrementar-pintura-rec (estat clau nou-valor)
  "Funció recursiva auxiliar que substitueix el valor de pintura."
  (cond
    ((null estat) nil)
    ((eq (caar estat) clau)
     (cons (list clau nou-valor) (cdr estat)))
    (t
     (cons (car estat) (decrementar-pintura-rec (cdr estat) clau nou-valor)))))



;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA ------------------- 
;; ------------------------------------------------------------------

;; Retorna el tipus de casella ('terra o 'aigua) a partir de les dades del mapa
(defun get-tipus-casella (coord estat)
  "Retorna 'terra o 'aigua per a una coordenada donada."
  (let* ((x     (car coord))
         (y     (cadr coord))
         (mapa  (cadr (assoc 'mapa estat)))
         (dades (cadr (assoc 'dades mapa)))
         (fila  (nth y dades))       ;; fila y del mapa
         (cel   (nth x fila)))       ;; columna x de la fila
    ;; El mapa emmagatzema el tipus de casella directament
    ;; S'assumeix que cada cel·la és un símbol: 'terra o 'aigua
    cel))

;; Retorna el laboratori que hi ha a una coordenada (o nil si no n'hi ha)
(defun get-laboratori-a (coord estat)
  "Retorna el laboratori situat a coord, o nil si no n'hi ha."
  (get-lab-rec coord (cadr (assoc 'laboratoris estat))))

(defun get-lab-rec (coord labs)
  (cond
    ((null labs) nil)
    ((equal coord (cadr (assoc 'coord (car labs)))) (car labs))
    (t (get-lab-rec coord (cdr labs)))))


; VALIDAR LAS COORDENADAS
(defun validar-coordenades (coord estat)
  "Comprova si les coordenades estan dins els limits del mapa (index 0-based)."

  (let* ((x (car coord))
         (y (cadr coord))
         (mapa (cadr (assoc 'mapa estat)))
         (amplada (cadr (assoc 'amplada mapa)))
         (alt (cadr (assoc 'alt mapa))))

    (and (>= x 0)
         (< x amplada)
         (>= y 0)
         (< y alt))))


;; FUNCIONS MATRICIALS
;; Comprova si hi ha un laboratori a una coordenada
(defun es-laboratori (coord estat)
  "Retorna t si hi ha un laboratori a la coordenada indicada, nil altrament."
  (es-laboratori-rec coord (cadr (assoc 'laboratoris estat))))

(defun es-laboratori-rec (coord labs)
  "Funció recursiva auxiliar per cercar un laboratori a una coordenada."
  (cond
    ((null labs) nil)
    ((equal coord (cadr (assoc 'coord (car labs)))) t)
    (t (es-laboratori-rec coord (cdr labs)))))


;; Comprova si hi ha una base enemiga a una coordenada
(defun es-base-enemiga (coord equip-propi estat)
  "Retorna t si hi ha una base enemiga a la coordenada indicada."
  (es-base-enemiga-rec coord equip-propi (cadr (assoc 'bases estat))))

(defun es-base-enemiga-rec (coord equip-propi bases)
  "Funció recursiva auxiliar per cercar una base enemiga."
  (cond
    ((null bases) nil)
    ((and (equal coord (cadr (assoc 'coord (car bases))))
          (not (eq (cadr (assoc 'equip (car bases))) equip-propi)))
     t)
    (t (es-base-enemiga-rec coord equip-propi (cdr bases)))))



;; Comprova si una casella és d'aigua
(defun es-aigua (coord estat)
  "Retorna t si la casella de la coordenada indicada és d'aigua, nil altrament."
  (eq (get-tipus-casella coord estat) 'aigua))

;; Comprova si hi ha una bolla enemiga a una coordenada
(defun es-bolla-enemiga (coord equip-propi estat)
  "Retorna t si hi ha una bolla de l'equip enemic a la coordenada indicada."
  (es-bolla-equip-rec coord equip-propi estat nil))

;; Comprova si hi ha una bolla amiga a una coordenada
(defun es-bolla-amiga (coord equip-propi estat) ; se quita si no es necesario
  "Retorna t si hi ha una bolla del propi equip a la coordenada indicada."
  (es-bolla-equip-rec coord equip-propi estat t))


(defun es-bolla-equip-rec (coord equip-propi estat mateixa-equip)
  "Funció recursiva auxiliar. Si mateixa-equip=t cerca bolla amiga, si nil cerca enemiga."
  (es-bolla-rec coord equip-propi (cadr (assoc 'unitats estat)) mateixa-equip))

(defun es-bolla-rec (coord equip-propi unitats mateixa-equip)
  (cond
    ((null unitats) nil)
    (t
     (let* ((u            (car unitats))
            (u-coord      (cadr (assoc 'coord u)))
            (u-equip      (cadr (assoc 'equip u)))
            (u-tipus      (cadr (assoc 'tipus u)))
            ;; és bolla del tipus que busquem?
            (equip-ok     (if mateixa-equip
                              (eq u-equip equip-propi)
                              (not (eq u-equip equip-propi)))))
       (if (and (equal coord u-coord)
                (eq u-tipus 'bolla)
                equip-ok)
           t
           (es-bolla-rec coord equip-propi (cdr unitats) mateixa-equip))))))

(defun es-posicio-lliure (coord estat)
  "Retorna t si la coordenada és una posició vàlida i lliure al mapa."
  (and (validar-coordenades coord estat)
       (not (es-aigua coord estat))
       (not (coord-a-llista coord (mapcar (lambda (b) (cadr (assoc 'coord b)))
                                          (cadr (assoc 'bases estat)))))
       (not (coord-a-llista coord (mapcar (lambda (l) (cadr (assoc 'coord l)))
                                          (cadr (assoc 'laboratoris estat)))))
       (not (coord-a-llista coord (mapcar (lambda (u) (cadr (assoc 'coord u)))
                                          (cadr (assoc 'unitats estat)))))))


(defun coord-a-llista (coord llista)
  "Retorna t si coord és present a la llista de coordenades (comparació amb equal)."
  (cond
    ((null llista) nil)
    ((equal coord (car llista)) t)
    (t (coord-a-llista coord (cdr llista)))))

;; FUNCIONS DE MODIFICACIÓ DE MAPA --> potser vagin dins grafis.lsp!!!!
;; Pinta una casella del color indicat (retorna estat actualitzat, no mutat)
(defun pintar-casella (coord color estat)
  "Pinta la casella de la coordenada indicada del color donat.
   Actualitza el color de la casella al mapa."
  (let* ((mapa      (cadr (assoc 'mapa estat)))
         (dades     (cadr (assoc 'dades mapa)))
         (amplada   (cadr (assoc 'amplada mapa)))
         (alt       (cadr (assoc 'alt mapa)))
         ;; reconstruïm el mapa amb la casella pintada
         (dades-noves (pintar-casella-dades coord color dades))
         (mapa-nou  (list (list 'dades    dades-noves)
                          (list 'amplada  amplada)
                          (list 'alt      alt))))
    ;; reconstruïm l'estat amb el nou mapa
    (substituir-camp 'mapa mapa-nou estat)))

(defun pintar-casella-dades (coord color dades)
  "Reconstrueix la matriu del mapa pintant la casella (x,y) del color indicat.
   Cada cel·la del mapa és una llista: (tipus-casella color-casella)"
  (let ((x (car coord))
        (y (cadr coord)))
    (pintar-fila-rec dades x y color 0)))

(defun pintar-fila-rec (files x y color fila-actual)
  "Recorre les files del mapa recursivament fins trobar la fila y."
  (cond
    ((null files) nil)
    ((= fila-actual y)
     (cons (pintar-columna-rec (car files) x color 0)
           (pintar-fila-rec (cdr files) x y color (+ fila-actual 1))))
    (t
     (cons (car files)
           (pintar-fila-rec (cdr files) x y color (+ fila-actual 1))))))

(defun pintar-columna-rec (fila x color col-actual)
  "Recorre les columnes d'una fila recursivament fins trobar la columna x."
  (cond
    ((null fila) nil)
    ((= col-actual x)
     ;; La cel·la és (tipus-casella color-casella); actualitzem el color
     (cons (list (car (car fila))   ;; mantenim el tipus ('terra o 'aigua)
                 color)             ;; actualitzem el color
           (pintar-columna-rec (cdr fila) x color (+ col-actual 1))))
    (t
     (cons (car fila)
           (pintar-columna-rec (cdr fila) x color (+ col-actual 1))))))

;; =================================================================
;;  UTILITAT: substituir-camp 
;; =================================================================

(defun substituir-camp (clau valor estat)
  "Retorna un nou estat on el camp identificat per clau té el nou valor.
   No muta l'estat original."
  (substituir-camp-rec clau valor estat))

(defun substituir-camp-rec (clau valor llista)
  "Substitueix el primer camp amb clau 'clau' pel nou valor. Para en la primera coincidència."
  (cond
    ((null llista) nil)
    ((eq (caar llista) clau)
     (cons (list clau valor)
           (cdr llista)))        
    (t
     (cons (car llista)
           (substituir-camp-rec clau valor (cdr llista))))))


;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ ACCIONS PER TORN -------------------
;; ------------------------------------------------------------------


(defun crear-bolla (estat equip color)
  "Crea una bolla del color indicat des de la base de l'equip.
   Descuenta 50 unitats de pintura. Retorna l'estat actualitzat."
  (if (< (get-pintura-actual estat equip) 50)
      estat  ;; no hi ha prou pintura, no fem res
      (let ((id    (get-next-id estat))
            (coord (buscar-posicio-lliure-base estat equip)))
        (if (null coord)
            estat  ;; no hi ha casella lliure adjacent, no fem res
            (let* ((bolla
                    (list
                     (list 'id          id)
                     (list 'tipus       'bolla)      ;; AFEGIT: necessari per get-suma-bolles-rec
                     (list 'equip       equip)
                     (list 'coord       coord)
                     (list 'color-propi color)       ;; color de la bolla (no canvia mai)
                     (list 'colors-pintat (list color)) ;; comença pintada del seu propi color
                     (list 'tr-pintar   0)
                     (list 'tr-moure    0)
                     (list 'ha-actuat   nil)))
                   (unitats      (cadr (assoc 'unitats estat)))
                   (noves-unitats (cons bolla unitats))
                   (estat1 (substituir-camp 'unitats noves-unitats estat))
                   (estat2 (decrementar-pintura 50 equip estat1)))  ;; AFEGIT: cost 50 pintura
              (incrementar-next-id estat2))))))

(defun buscar-posicio-lliure-base (estat equip)
  "Cerca la primera casella lliure adjacent (d2 <= 2) a la base de l'equip.
   Retorna la coordenada lliure o nil si no n'hi ha cap."
  (let* ((base  (buscar-base-equip (cadr (assoc 'bases estat)) equip))
         (coord (cadr (assoc 'coord base)))
         (bx    (car coord))
         (by    (cadr coord)))
    ;; Les 8 caselles adjacents (d2 <= 2)
    (buscar-en-candidats
     (list (list (- bx 1) (- by 1))
           (list bx       (- by 1))
           (list (+ bx 1) (- by 1))
           (list (- bx 1) by)
           (list (+ bx 1) by)
           (list (- bx 1) (+ by 1))
           (list bx       (+ by 1))
           (list (+ bx 1) (+ by 1)))
     estat)))

(defun buscar-en-candidats (candidats estat)
  "Retorna la primera coordenada candidata que sigui lliure, o nil."
  (cond
    ((null candidats) nil)
    ((es-posicio-lliure (car candidats) estat) (car candidats))
    (t (buscar-en-candidats (cdr candidats) estat))))



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

(defun es-laboratori-ocupat (lab)
  "Retorna t si el laboratori està capturat per algun equip."
  (not (null (cadr (assoc 'equip lab)))))

(defun es-laboratori-meu (equip lab)
  "Retorna t si el laboratori pertany a l'equip indicat."
  (eq (cadr (assoc 'equip lab)) equip))

(defun canviar-equip-laboratori()

)





;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE BOLLES ------------------- 
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