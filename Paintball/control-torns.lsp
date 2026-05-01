;; ==================================================================
;;  EXTRA: MILLOR CONTROL DE TORNS
;;  Fitxer: control-torns.lsp
;; ==================================================================


;; ------------------------------------------------------------------
;;  PARÀMETRES GLOBALS DE CONTROL
;; ------------------------------------------------------------------

(defvar *historial-passat* nil
  "Llista d'estats anteriors (el primer és el més recent). Permet fer undo.")

(defvar *historial-futur* nil
  "Llista d'estats cap endavant desfets (permet fer redo).")

(defvar *pas-n* 1
  "Quants torns s'executen per cada pulsació d'ENTER (o cicle en mode auto).")

(defvar *max-historial* 30
  "Nombre màxim d'estats guardats per a l'undo. Limita el consum de memòria.")


;; ------------------------------------------------------------------
;;  INICIALITZACIÓ DE L'HISTORIAL
;; ------------------------------------------------------------------

(defun inicialitzar-historial (estat-inicial)
  "Reseteja l'historial complet i hi posa l'estat inicial."
  (setf *historial-passat* (list estat-inicial)
        *historial-futur*  nil))


;; ------------------------------------------------------------------
;;  GUARDAR I RECUPERAR ESTATS
;; ------------------------------------------------------------------

(defun guardar-estat (estat)
  "Afegeix l'estat actual a l'historial passat i esborra el futur."
  (let* ((nou-passat (cons estat *historial-passat*))
         (passat-retallat (cond
                            ((> (length nou-passat) *max-historial*)
                             (butlast nou-passat))
                            (t nou-passat))))
    (setf *historial-futur*  nil
          *historial-passat* passat-retallat)))

(defun estat-actual ()
  "Retorna l'estat actual (cap de l'historial passat)."
  (car *historial-passat*))


;; ------------------------------------------------------------------
;;  ANAR ENDARRERE (UNDO)
;; ------------------------------------------------------------------

(defun anar-endarrere ()
  "Desfà un pas: torna a l'estat anterior guardat.
   Retorna el nou estat actual, o NIL si ja som al principi."
  (cond
    ((null (cdr *historial-passat*))
     (mostrar-missatge-hud "Ja sou al principi de la partida!")
     nil)
    (t
     (let ((estat-desfet (car *historial-passat*)))
       (setf *historial-futur*  (cons estat-desfet *historial-futur*)
             *historial-passat* (cdr *historial-passat*))
       (estat-actual)))))


;; ------------------------------------------------------------------
;;  ANAR ENDAVANT (REDO)
;; ------------------------------------------------------------------

(defun anar-endavant ()
  "Refà un pas: avança a l'estat que s'havia desfet.
   Retorna el nou estat actual, o NIL si no hi ha res per refer."
  (cond
    ((null *historial-futur*)
     (mostrar-missatge-hud "No hi ha torns per refer.")
     nil)
    (t
     (let ((estat-refet (car *historial-futur*)))
       (setf *historial-futur*  (cdr *historial-futur*)
             *historial-passat* (cons estat-refet *historial-passat*))
       (estat-actual)))))


;; ------------------------------------------------------------------
;;  AVANÇAR N TORNS
;; ------------------------------------------------------------------

(defun avançar-n-torns (estat n)
  "Executa N torns i retorna el nou estat.
   Para si la partida finalitza abans d'esgotar els N torns."
  (cond
    ((or (<= n 0) (final-partida-p estat))
     estat)
    (t
     (let* ((e1 (actualitzar-pintura estat))
            (e2 (baixar-cooldowns    e1))
            (e3 (executar-torn       e2))
            (e4 (seguent-torn        e3)))
       (avançar-n-torns e4 (- n 1))))))


;; ------------------------------------------------------------------
;;  HUD AMPLIAT AMB INFORMACIÓ DE CONTROL
;; ------------------------------------------------------------------

(defun mostrar-hud-control ()
  "Mostra la barra inferior amb instruccions i estat del control."
  (let* ((n-passat      (length *historial-passat*))
         (n-futur       (length *historial-futur*))
         (ronda-actual  (cond
                          ((estat-actual)
                           (cadr (assoc 'ronda (estat-actual))))
                          (t 0))))
    (color 0 0 0 255 255 255)
    (goto-xy 0 350)
    (princ "-------------------------------------------------------")
    (terpri)
    (princ "PAS: ") (princ *pas-n*) (princ " torn(s)  |  ")
    (princ "RONDA: ") (princ ronda-actual) (princ "  |  ")
    (princ "UNDO: ") (princ (- n-passat 1)) (princ "  |  ")
    (princ "REDO: ") (princ n-futur)
    (terpri)
    (princ "[ENTER]=avança  [b]=endarrere  [f]=endavant  [+/-]=pas  [q]=sortir")))

(defun mostrar-missatge-hud (msg)
  "Mostra un missatge temporal a la zona de control del HUD."
  (color 0 0 0 255 255 255)
  (goto-xy 0 350)
  (princ "-------------------------------------------------------")
  (terpri)
  (princ msg)
  (terpri)
  (princ "[ENTER]=avança  [b]=endarrere  [f]=endavant  [+/-]=pas  [q]=sortir"))


;; ------------------------------------------------------------------
;;  DIBUIXAR ESTAT ACTUAL
;; ------------------------------------------------------------------

(defun dibuixar-estat-actual ()
  "Pinta el mapa de l'estat actual i actualitza els HUDs."
  (let ((e (estat-actual)))
    (when e
      (pinta e)
      (mostrar-hud-control))))


;; ------------------------------------------------------------------
;;  MODE INTERACTIU
;; ------------------------------------------------------------------

(defun jugar-interactiu (nom-mapa)
  "Inicia una partida en mode interactiu (pas a pas)."
  (let* ((mapa  (carrega-mapa nom-mapa))
         (estat (cond (mapa (crear-estat-inicial mapa))
                      (t nil))))
    (cond
      ((null estat)
       (princ "Error: no s'ha pogut carregar el mapa.") (terpri))
      (t
       (ajusta-mida-mapa mapa)
       (color 0 0 0 255 255 255)
       (mode 0 0 640 375)
       (cls)
       (pinta-matriu mapa m)
       (inicialitzar-historial estat)
       (dibuixar-estat-actual)
       (bucle-interactiu)))))

(defun bucle-interactiu ()
  "Bucle principal del mode interactiu. Llegeix tecles i actua."
  (let ((tecla (inkey)))
    (cond

      ;; ---- ENTER → avançar N torns ----
      ((= tecla 13)
       (let* ((estat-vell (estat-actual))
              (estat-nou  (avançar-n-torns estat-vell *pas-n*)))
         (cond
           ((equal estat-vell estat-nou)
            (finalitzar-partida estat-nou))
           (t
            (guardar-estat estat-nou)
            (cls)
            (dibuixar-estat-actual)
            (when (final-partida-p estat-nou)
              (finalitzar-partida estat-nou)))))
       (unless (final-partida-p (estat-actual))
         (bucle-interactiu)))

      ;; ---- b → undo ----
      ((= tecla (char-code #\b))
       (let ((estat-recuperat (anar-endarrere)))
         (when estat-recuperat
           (cls)
           (dibuixar-estat-actual)))
       (bucle-interactiu))

      ;; ---- f → redo ----
      ((= tecla (char-code #\f))
       (let ((estat-recuperat (anar-endavant)))
         (when estat-recuperat
           (cls)
           (dibuixar-estat-actual)))
       (bucle-interactiu))

      ;; ---- + → augmentar el pas ----
      ((= tecla (char-code #\+))
       (setf *pas-n* (+ *pas-n* 1))
       (mostrar-hud-control)
       (bucle-interactiu))

      ;; ---- - → disminuir el pas (mínim 1) ----
      ((= tecla (char-code #\-))
       (setf *pas-n* (max 1 (- *pas-n* 1)))
       (mostrar-hud-control)
       (bucle-interactiu))

      ;; ---- q → sortir ----
      ((= tecla (char-code #\q))
       (princ "Sortint de la partida.")
       (terpri))

      ;; ---- Qualsevol altra tecla: ignorem ----
      (t
       (bucle-interactiu)))))


;; ------------------------------------------------------------------
;;  MODE AUTO CADA N TORNS
;; ------------------------------------------------------------------

(defun jugar-cada-n (nom-mapa n)
  "Executa la partida automàticament pintant l'estat cada N torns."
  (let* ((mapa  (carrega-mapa nom-mapa))
         (estat (cond (mapa (crear-estat-inicial mapa))
                      (t nil))))
    (cond
      ((null estat)
       (princ "Error: no s'ha pogut carregar el mapa.") (terpri))
      (t
       (ajusta-mida-mapa mapa)
       (color 0 0 0 255 255 255)
       (mode 0 0 640 375)
       (cls)
       (pinta-matriu mapa m)
       (inicialitzar-historial estat)
       (bucle-auto estat n 0)))))

(defun bucle-auto (estat n comptador)
  "Bucle automàtic: executa 1 torn, i pinta quan el comptador arriba a N."
  (cond
    ((final-partida-p estat)
     (pinta estat)
     (finalitzar-partida estat))
    (t
     (let* ((e1             (actualitzar-pintura estat))
            (e2             (baixar-cooldowns    e1))
            (e3             (executar-torn       e2))
            (e4             (seguent-torn        e3))
            (nou-comptador  (+ comptador 1))
            (ha-de-pintar   (>= nou-comptador n)))
       (when ha-de-pintar
         (cls)
         (pinta e4)
         (guardar-estat e4))
       (bucle-auto e4 n (cond (ha-de-pintar 0)
                              (t nou-comptador)))))))