;; ==================================================================
;;  EXTRA: MILLOR CONTROL DE TORNS
;;  Fitxer: control-torns.lsp
;;  Carregar DESPRÉS de paintball.lsp i grafics.lsp:
;;    (load 'paintball)
;;    (load 'grafics)
;;    (load 'control-torns)
;;
;;  MODES D'ÚS:
;;    (jugar-interactiu 'nom-mapa)      ; pas a pas amb ENTER
;;    (jugar-cada-n     'nom-mapa N)    ; pinta automàticament cada N torns
;;
;;  TECLES en mode interactiu:
;;    ENTER  → avança N torns (per defecte N=1)
;;    b      → enrere  (undo, torna a l'estat anterior guardat)
;;    f      → endavant (redo, recupera un estat desfet)
;;    +      → augmenta el pas en 1 (prem + per avançar de 2 en 2, etc.)
;;    -      → disminueix el pas en 1 (mínim 1)
;;    q      → sortir
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
  (setq *historial-passat* (list estat-inicial))
  (setq *historial-futur*  nil))


;; ------------------------------------------------------------------
;;  GUARDAR I RECUPERAR ESTATS
;; ------------------------------------------------------------------

(defun guardar-estat (estat)
  "Afegeix l'estat actual a l'historial passat i esborra el futur."
  ;; Quan avançem, ja no té sentit el redo
  (setq *historial-futur* nil)
  (setq *historial-passat* (cons estat *historial-passat*))
  ;; Tallem si supera el màxim per no gastar memòria sense límit
  (if (> (length *historial-passat*) *max-historial*)
      (setq *historial-passat* (butlast *historial-passat*))))

(defun estat-actual ()
  "Retorna l'estat actual (cap de l'historial passat)."
  (car *historial-passat*))


;; ------------------------------------------------------------------
;;  ANAR ENDARRERE (UNDO)
;;  Recupera l'estat anterior de l'historial passat.
;;  L'estat actual passa a l'historial futur per poder fer redo.
;; ------------------------------------------------------------------

(defun anar-endarrere ()
  "Desfà un pas: torna a l'estat anterior guardat.
   Retorna el nou estat actual, o NIL si ja som al principi."
  (if (null (cdr *historial-passat*))
      ;; No hi ha res per desfer (ja som al primer estat)
      (progn
        (mostrar-missatge-hud "Ja sou al principi de la partida!")
        nil)
      ;; Hi ha estats anteriors: mogem el cap al futur
      (let ((estat-desfet (car *historial-passat*)))
        (setq *historial-futur*  (cons estat-desfet *historial-futur*))
        (setq *historial-passat* (cdr *historial-passat*))
        (estat-actual))))


;; ------------------------------------------------------------------
;;  ANAR ENDAVANT (REDO)
;;  Recupera l'estat que es va desfer amb undo.
;; ------------------------------------------------------------------

(defun anar-endavant ()
  "Refà un pas: avança a l'estat que s'havia desfet.
   Retorna el nou estat actual, o NIL si no hi ha res per refer."
  (if (null *historial-futur*)
      ;; No hi ha res per refer
      (progn
        (mostrar-missatge-hud "No hi ha torns per refer.")
        nil)
      ;; Movem el cap del futur al passat
      (let ((estat-refet (car *historial-futur*)))
        (setq *historial-futur*  (cdr *historial-futur*))
        (setq *historial-passat* (cons estat-refet *historial-passat*))
        (estat-actual))))


;; ------------------------------------------------------------------
;;  AVANÇAR N TORNS
;;  Executa exactament N torns complets sobre un estat i retorna
;;  el nou estat. Para automàticament si la partida acaba.
;; ------------------------------------------------------------------

(defun avançar-n-torns (estat n)
  "Executa N torns (cada torn = actualitzar-pintura + baixar-cooldowns
   + executar-torn + seguent-torn) i retorna el nou estat.
   Para si la partida finalitza abans d'esgotar els N torns."
  (cond
    ;; Base: ja hem fet els N torns o la partida ha acabat
    ((or (<= n 0) (final-partida-p estat))
     estat)
    ;; Pas recursiu: fem un torn i continuem
    (t
     (let* ((e1 (actualitzar-pintura estat))
            (e2 (baixar-cooldowns    e1))
            (e3 (executar-torn       e2))
            (e4 (seguent-torn        e3)))
       (avançar-n-torns e4 (- n 1))))))


;; ------------------------------------------------------------------
;;  HUD AMPLIAT AMB INFORMACIÓ DE CONTROL
;;  Mostra a la part inferior de la finestra el mode actiu,
;;  el pas actual i l'estat de l'historial (undo/redo disponibles).
;; ------------------------------------------------------------------

(defun mostrar-hud-control ()
  "Mostra la barra inferior amb instruccions i estat del control."
  (let* ((n-passat (length *historial-passat*))
         (n-futur  (length *historial-futur*))
         ;; Calculem la ronda real de l'estat actual
         (ronda-actual (if (estat-actual)
                           (cadr (assoc 'ronda (estat-actual)))
                           0)))
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
;;  Pinta el mapa i tots els HUDs.
;; ------------------------------------------------------------------

(defun dibuixar-estat-actual ()
  "Pinta el mapa de l'estat actual i actualitza els HUDs."
  (let ((e (estat-actual)))
    (when e
      (pinta e)
      (mostrar-hud-control))))


;; ------------------------------------------------------------------
;;  MODE INTERACTIU: pas a pas amb teclat
;; ------------------------------------------------------------------

(defun jugar-interactiu (nom-mapa)
  "Inicia una partida en mode interactiu (pas a pas).
   L'usuari controla l'avenç amb el teclat:
     ENTER → avança *pas-n* torns
     b     → undo (endarrere)
     f     → redo (endavant)
     +     → augmenta el pas
     -     → disminueix el pas
     q     → sortir"
  ;; Preparació
  (let* ((mapa  (carrega-mapa nom-mapa))
         (estat (if mapa (crear-estat-inicial mapa) nil)))
    (if (null estat)
        (progn (princ "Error: no s'ha pogut carregar el mapa.") (terpri))
        (progn
          ;; Inicialitzam la finestra i pintem el primer estat
          (ajusta-mida-mapa mapa)
          (color 0 0 0 255 255 255)
          (mode 0 0 640 375)
          (cls)
          (pinta-matriu mapa m)
          ;; Inicialitzam l'historial amb l'estat inicial
          (inicialitzar-historial estat)
          (dibuixar-estat-actual)
          ;; Entrem al bucle interactiu
          (bucle-interactiu)))))

(defun bucle-interactiu ()
  "Bucle principal del mode interactiu. Llegeix tecles i actua."
  (let ((tecla (inkey)))              ; espera fins que l'usuari prem una tecla
    (cond

      ;; ---- ENTER (codi 13) → avançar N torns ----
      ((= tecla 13)
       (let* ((estat-vell (estat-actual))
              (estat-nou  (avançar-n-torns estat-vell *pas-n*)))
         (if (equal estat-vell estat-nou)
             ;; La partida ja havia acabat: mostrem resultat final
             (finalitzar-partida estat-nou)
             (progn
               (guardar-estat estat-nou)
               (cls)
               (dibuixar-estat-actual)
               ;; Si just ara ha acabat, mostrem el resultat
               (when (final-partida-p estat-nou)
                 (finalitzar-partida estat-nou)))))
       ;; Continuem el bucle (excepte si la partida ha acabat)
       (unless (final-partida-p (estat-actual))
         (bucle-interactiu)))

      ;; ---- b → undo (endarrere) ----
      ((= tecla (char-code #\b))
       (let ((estat-recuperat (anar-endarrere)))
         (when estat-recuperat
           (cls)
           (dibuixar-estat-actual)))
       (bucle-interactiu))

      ;; ---- f → redo (endavant) ----
      ((= tecla (char-code #\f))
       (let ((estat-recuperat (anar-endavant)))
         (when estat-recuperat
           (cls)
           (dibuixar-estat-actual)))
       (bucle-interactiu))

      ;; ---- + → augmentar el pas ----
      ((= tecla (char-code #\+))
       (setq *pas-n* (+ *pas-n* 1))
       (mostrar-hud-control)
       (bucle-interactiu))

      ;; ---- - → disminuir el pas (mínim 1) ----
      ((= tecla (char-code #\-))
       (setq *pas-n* (max 1 (- *pas-n* 1)))
       (mostrar-hud-control)
       (bucle-interactiu))

      ;; ---- q → sortir ----
      ((= tecla (char-code #\q))
       (princ "Sortint de la partida.")
       (terpri))

      ;; ---- Qualsevol altra tecla: ignorem i seguim ----
      (t
       (bucle-interactiu)))))


;; ------------------------------------------------------------------
;;  MODE AUTO CADA N TORNS
;;  Executa la partida automàticament, però només pinta cada N torns.
;;  Equival al sleep del bucle original però amb control de freqüència.
;; ------------------------------------------------------------------

(defun jugar-cada-n (nom-mapa n)
  "Executa la partida automàticament pintant l'estat cada N torns.
   N ha de ser un enter positiu (ex: 1, 5, 10...).
   Corre en mode automàtic sense interacció de teclat."
  (let* ((mapa  (carrega-mapa nom-mapa))
         (estat (if mapa (crear-estat-inicial mapa) nil)))
    (if (null estat)
        (progn (princ "Error: no s'ha pogut carregar el mapa.") (terpri))
        (progn
          (ajusta-mida-mapa mapa)
          (color 0 0 0 255 255 255)
          (mode 0 0 640 375)
          (cls)
          (pinta-matriu mapa m)
          ;; Inicialitzam l'historial (per si volem consultar-lo)
          (inicialitzar-historial estat)
          ;; Bucle automàtic
          (bucle-auto estat n 0)))))

(defun bucle-auto (estat n comptador)
  "Bucle automàtic: executa 1 torn, i pinta quan el comptador arriba a N."
  (cond
    ((final-partida-p estat)
     (pinta estat)
     (finalitzar-partida estat))
    (t
     (let* ((e1 (actualitzar-pintura estat))
            (e2 (baixar-cooldowns    e1))
            (e3 (executar-torn       e2))
            (e4 (seguent-torn        e3))
            ;; Incrementam el comptador: quan arriba a N, pintem i resetegem
            (nou-comptador (+ comptador 1))
            (ha-de-pintar  (>= nou-comptador n)))
       (when ha-de-pintar
         (cls)
         (pinta e4)
         ;; Guardem a l'historial per poder consultar-lo si cal
         (guardar-estat e4))
       (bucle-auto e4 n (if ha-de-pintar 0 nou-comptador))))))


;; ------------------------------------------------------------------
;;  EXEMPLE D'ÚS RÀPID (comenta/descomenta segons necessitis)
;; ------------------------------------------------------------------

;; Mode interactiu (pas a pas amb teclat):
;;   (jugar-interactiu 'tiny)

;; Mode automàtic, pinta cada 5 torns:
;;   (jugar-cada-n 'tiny 5)

;; Canviar el pas en mode interactiu (des de la REPL):
;;   (setq *pas-n* 10)