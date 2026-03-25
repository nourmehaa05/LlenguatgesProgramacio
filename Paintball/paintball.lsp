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

;; get bolles per equip


;; ...

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

;; BASE CREA BOLLES 


;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE MAPA ------------------- NOUR!!!!!!!!!!!!
;; ------------------------------------------------------------------

;; afegir funcions de matrius
; devolver true/fals si la bola puede ir
; devolver true/fals si la bola puede pintar
; pq habría que mirar --> es agua, es lab, es base, hi ha bolla?
(defun es-laboratori()
; si es equipo e1 --> si es laboratorio enemigo, PINTARLO
; sino, OMITIRLO

)






;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE BASES -------------------
;; ------------------------------------------------------------------

;; mirar si está pintada (pasar x parámetro)

;; crear bolas

;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE LABORATORIS -------------------
;; ------------------------------------------------------------------

;

;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE BOLLES ------------------- NOUR!!!!!!!!!!!!
;; ------------------------------------------------------------------

;; mirar de quants colors està pintada

; mirar su tiempo de recuperación



;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE L'ABAST ------------------- --> nose si va aquí o en otro sitio
;; ------------------------------------------------------------------

; mirar alcance base
;