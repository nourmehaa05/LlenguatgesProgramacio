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
        (list 'unitats nil)
        (list 'pintura-e1 200)
        (list 'pintura-e2 200)
    )
)

(defun iniciar-partida()

)

;; ------------------------------------------------------------------
;;  ------------------- CARREGAR MAPA -------------------
;; ------------------------------------------------------------------

(defun carregar-mapa()

)





;; ------------------------------------------------------------------
;;  ------------------- GETTERS DE L'ESTAT ACTUAL -------------------
;; ------------------------------------------------------------------

(defun get-estat-actual()

)

(defun  get-torn-actual()

)

(defun  get-equip-actual()

)

;; ...

;; ------------------------------------------------------------------
;;  ------------------- GESTIÓ DE TORNS -------------------
;; ------------------------------------------------------------------

(defun canviar-equip()

)

(defun incrementar-pintura()

)



