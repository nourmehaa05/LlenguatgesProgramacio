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
;;  ------------------- INICIALITZAR JOC -------------------
;; ------------------------------------------------------------------
(defun crear-estat-inicial()
    (list
        (list 'ronda 1)
        (list 'torn 'e1)
        (list 'mapa nil)
        (list 'bases nil)
        (list 'laboratoris nil)
        (list 'unitats nil)
        (list 'pintura-e1 200)
        (list 'pintura-e2 200)
    )
)

(defun iniciar-partida()
    (setq *estat* (crear-estat-inicial)) ; estat variable global.
    ; carregar mapa
    ; crear elements inicials
    ; bucle partida! --> mentre no arribem al FINAL, executar partida
)

(defun jugar-partida()
    ; bucle --> mentre nigú hagi matat base O BÉ no hem arribat a 1500 torns
    ; executar-torn
    ; si no, --> REVISAR QUI HA GUANYAT

)

(defun es-final()
    ; revisar matar base
    ; revisar 1500 torns
)

(defun qui-ha-guanyat()

)



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
;;  ------------------- CARREGAR MAPA ------------------- NOUR!!!!!!!!!!!!
;; ------------------------------------------------------------------

(defun carregar-mapa()

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

(defun es-posicio-lliure()
    "Retorna true si la casella indicada no està ocupada."
)

;; FUNCIONS DE MODIFICACIÓ DE MAPA --> potser vagin dins grafis.lsp

(defun pintar-casella(coord, color)
    "Pinta la casella enemiga."

)

;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ ACCIONS PER TORN -------------------
;; ------------------------------------------------------------------

; Crear bolla
(defun crear-bolla(equip)
    "Crea una bolla des de la base de l'equip en la primera posició lliure adjacent"
)

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