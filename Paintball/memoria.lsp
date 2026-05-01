;; ==================================================================
;; GESTIÓ DE MEMÒRIA COMPARTIDA - Paintball
;; ==================================================================
;; Sistema completo de memòria compartida entre unitats del mateix equip.
;; Inclou funcions genèriques i documentació de com s'usa.
;;
;; CARREGA'T PRIMER: (load "memoria.lsp")
;; ==================================================================

;; ==================================================================
;; FUNCIONS GENÈRIQUES DE LECTURA / ESCRIPTURA
;; ==================================================================

;; Llegir un valor de la memòria per clau
(defun mem-llegir (memoria clau)
  "Retorna el valor associat a clau, o nil si no existeix."
  (cond
    ((null memoria) nil)
    ((eq (caar memoria) clau)
     (cadar memoria))
    (t (mem-llegir (cdr memoria) clau))))

;; Escriure / actualitzar un valor a la memòria
(defun mem-escriure (memoria clau valor)
  "Retorna nova memòria amb clau actualitzada a valor.
   Si clau no existeix, l'afegeix."
  (cond
    ((null memoria)
     (list (list clau valor)))
    ((eq (caar memoria) clau)
     (cons (list clau valor) (cdr memoria)))
    (t
     (cons (car memoria)
           (mem-escriure (cdr memoria) clau valor)))))

;; Afegir un element a una llista dins la memòria
(defun mem-afegir (memoria clau element)
  "Afegeix element a la llista associada a clau (o la crea si no existeix)."
  (let ((llista-actual (mem-llegir memoria clau)))
    (cond
      ((null llista-actual)
       (mem-escriure memoria clau (list element)))
      (t
       (mem-escriure memoria clau (append llista-actual (list element)))))))

;; Eliminar un element d'una llista dins la memòria
(defun mem-treure (memoria clau element)
  "Elimina element de la llista associada a clau."
  (let ((llista-actual (mem-llegir memoria clau)))
    (cond
      ((null llista-actual) memoria)
      (t
       (mem-escriure memoria clau
                     (cond ((null llista-actual) nil)
                           ((equal (car llista-actual) element)
                            (cdr llista-actual))
                           (t
                            (cons (car llista-actual)
                                  (mem-treure (list (list clau (cdr llista-actual)))
                                             clau element)))))))))

;; Eliminar tots els valors d'una clau
(defun mem-eliminar (memoria clau)
  "Retorna nova memòria sense la clau."
  (cond
    ((null memoria) nil)
    ((eq (caar memoria) clau)
     (cdr memoria))
    (t
     (cons (car memoria)
           (mem-eliminar (cdr memoria) clau)))))

;; Fusionar dues memòries (la segona sobreescriu la primera)
(defun mem-fusionar (mem-1 mem-2)
  "Retorna nova memòria amb els valors de mem-2 sobreescrivint mem-1."
  (cond
    ((null mem-2) mem-1)
    (t
     (let ((clau (caar mem-2))
           (valor (cadar mem-2)))
       (mem-fusionar (mem-escriure mem-1 clau valor) (cdr mem-2))))))

;; Limpiar entrades antigues de la memòria (> N rondes)
(defun mem-limpiar-antiga (memoria ronda-actual max-antiguitat)
  "Elimina entrades de tipus (ronda VALOR) si ronda-actual - VALOR > max-antiguitat."
  (cond
    ((null memoria) nil)
    ((and (eq (caar memoria) 'darrera-vista)
          (> (- ronda-actual (cadar memoria)) max-antiguitat))
     ;; Eliminar aquesta entrada
     (mem-limpiar-antiga (cdr memoria) ronda-actual max-antiguitat))
    (t
     (cons (car memoria)
           (mem-limpiar-antiga (cdr memoria) ronda-actual max-antiguitat)))))

;; Mostrar la memòria de forma llegible (útil per debugging)
(defun mem-mostrar (memoria etiqueta)
  "Imprimeix la memòria de forma neta."
  (cond
    ((null memoria)
     (princ etiqueta)
     (princ ": BUIDA")
     (terpri))
    (t
     (princ etiqueta)
     (princ ": ")
     (print memoria))))


;; ==================================================================
;; DOCUMENTACIÓ: ESTRUCTURA I CASOS D'ÚS
;; ==================================================================

;;  ESTRUCTURA DE LA MEMÒRIA:
;;  --------------------------
;;  La memòria és una alist (association list) amb parells clau-valor:
;;    ( (clau1 valor1) (clau2 valor2) ... )
;;
;;  Exemple inicial (buida): nil
;;  Exemple poblada:
;;    ( (base-enemiga (30 25))
;;      (darrera-vista 145)
;;      (posicio-propia (20 20))
;;      (targets-prioritaris ((10 15) (12 18)))
;;      (zones-explorades ((0 0) (10 10))) )


;;  COM FUNCIONA EL FLUX A paintball.lsp:
;;  ======================================
;;  1. CREAR (línies 169-170):
;;     (list 'memoria-e1 nil)
;;     (list 'memoria-e2 nil)
;;
;;  2. PASSAR A L'AGENT (línies 500, 512):
;;     construir-info-unitat → obtenir-memoria-equip estat equip
;;     L'agent rep memòria a posició 11 de la llista de dades
;;
;;  3. AGENTS ACTUALITZEN (agent-cms213.lsp, agent-nms864.lsp):
;;     (nova-memoria (agent-xxx-actualitzar-memoria memoria ...))
;;     (if nova-memoria
;;         (list (list 'escriu-memoria (list nova-memoria))))
;;
;;  4. APLICAR A L'ESTAT (línies 1290-1298):
;;     aplicar-escriu-memoria → substituir-camp 'memoria-e1/e2 estat


;;  AGENT E2 (nms864) - OFENSIU:
;;  =============================
;;  Estructura típica de memòria que manté:
;;    (base-enemiga COORD)      → Última posició de la base enemiga vista
;;    (darrera-vista RONDA)     → Ronda en que es va veure l'última base
;;    (posicio-propia COORD)    → Posició actual del que escriu
;;
;;  Exemple d'ús:
;;    (let* ((mem (agent-nms864-memoria dades))
;;           (base (mem-llegir mem 'base-enemiga))
;;           (visio (agent-nms864-visio dades)))
;;      ;; Si té memòria de base i no la veu, va cap a la posició recordada
;;      (if base
;;          (agent-nms864-cel·la-cap-a coord base visio)
;;        (agent-nms864-cel·la-explorar coord visio)))


;;  AGENT E1 (cms213) - DEFENSIU:
;;  ==============================
;;  Estructura típica que podria mantenir:
;;    (base-enemiga COORD)      → Última posició de la base enemiga
;;    (darrera-vista RONDA)     → Quan es va veure
;;    (posicio-propia COORD)    → Posició del que escriu
;;    (bolles-amigas (ID1 ID2 ID3))  → IDs de bolles aliat (FUTUR)
;;    (ultima-explorada COORD)  → Última zona explorada (FUTUR)
;;
;;  Exemple d'ús:
;;    (let* ((mem (agent-cms213-memoria dades))
;;           (base (mem-llegir mem 'base-enemiga))
;;           (ultima-zona (mem-llegir mem 'ultima-explorada)))
;;      ;; Dividir tasques: si algú ja va explorar una zona, aquesta unitat n'agafa una altra
;;      (if (and ultima-zona (< (agent-cms213-dist2 coord ultima-zona) 100))
;;          (agent-cms213-explorar-zona-alternativa coord visio)
;;        (agent-cms213-explorar-zona-busqueda coord visio ronda)))


;;  CASOS D'ÚS PRÀCTICS:
;;  ====================

;;  CASÓ 1: EXPLORACIÓ COORDINADA
;;  ==============================
;;  E1 vol que cap agent explori dues vegades la mateixa zona.
;;
;;    Unitat 1 (base): (mem-escriure memoria 'ultima-zona-e1 '(30 25))
;;    Unitat 2 (bolla): Si veu que 'ultima-zona-e1 és a prop seu, explora altre lloc
;;
;;    (let ((ultima-zona (mem-llegir memoria 'ultima-zona-e1)))
;;      (if (and ultima-zona (< (agent-dist2 coord ultima-zona) 50))
;;          (agent-explorar-zona-alternativa ...)
;;        (agent-explorar-zona-normal ...)))


;;  CASÓ 2: RETARD EN EL COMBAT / ATACS SINCRONITZATS
;;  ==================================================
;;  E2 vol que totes les bolles atacades esperin per fer un atac coordinat.
;;
;;    Bolla 1 veu base enemiga: (mem-escriure memoria 'ordre-atac 'ATACAR-JA)
;;    Bolla 2, 3, 4 llegeixen: (mem-llegir memoria 'ordre-atac)
;;    Si reben 'ATACAR-JA: totes pinten simultàniament
;;
;;    (let ((ordre (mem-llegir memoria 'ordre-atac)))
;;      (if (eq ordre 'ATACAR-JA)
;;          (list (list 'pinta (list target)))
;;        (list (list 'mou (list nova-pos)))))


;;  CASÓ 3: REGISTRE D'AVISTAMENTS
;;  ================================
;;  Cada agent afegeix enemics avistats a una llista compartida.
;;
;;    Bolla 1 veu bolla enemiga en (30 20):
;;      (mem-afegir memoria 'enemics-avistats '(30 20))
;;
;;    Bolla 2 veu bolla enemiga en (32 22):
;;      (mem-afegir memoria 'enemics-avistats '(32 22))
;;
;;    Altres bolles llegeixen:
;;      (mem-llegir memoria 'enemics-avistats)
;;      → ((30 20) (32 22))


;;  NOTES D'IMPLEMENTACIÓ:
;;  ======================
;;
;;  1. ATOMICITAT:
;;     Cada accio 'escriu-memoria reemplaça TOTA la memòria anterior.
;;     Si vols afegir info, sempre has de llegir-la primer i fusionar.
;;     Exemple:
;;       (let ((mem-antiga (mem-llegir memoria 'base-enemiga)))
;;         (mem-escriure memoria 'base-enemiga nova-base))
;;
;;  2. SINCRONITZACIÓ:
;;     Totes les unitats del mateix equip llegeixen la memòria ACTUAL cada torn.
;;     Les escriptures es fan al final de l'accio de cada unitat.
;;     L'ordre d'execucio de les unitats pot variar, així que és millor que
;;     les updates NO entrin en conflicte - és millor que siguin aditives.
;;
;;  3. EFICIÈNCIA:
;;     La memòria creix indefinidament si no la neteges.
;;     Considera afegir lògica per eliminar info antiga (ex: >50 rondes).
;;     Exemple:
;;       (mem-limpiar-antiga memoria ronda-actual 50)
;;
;;  4. TESTING:
;;     Per provar si funciona, afegeix prints:
;;       (princ "Memòria actual: ")
;;       (print (agent-nms864-memoria dades))
;;       (mem-mostrar memoria "ESTAT E1")

;; ==================================================================
