;; ============================================================
;; Pràctica final de Llenguatges de Programació - LISP
;; Paintball - Mòdul de Memòria Compartida
;; ============================================================
;; Estudiants:
;;   - Marín Sánchez, Carolina
;;   - Mehannek Samah, Nour Iman
;; Data: 03/05/2026
;; Assignatura: Llenguatges de Programació
;; Grup: 101
;; Professors:
;;   - Cabot Nadal, Miquel Àngel
;;   - Oliver Tomàs, Antoni
;; Lliurament: primera convocatòria.
;; ============================================================
;; Descripció de les funcions d'aquest fitxer:
;;   - mem-llegir:          llegir un valor de la memòria per clau
;;   - mem-escriure:        escriure o actualitzar un valor per clau
;;   - mem-afegir:          afegir un element a una llista dins la memòria
;;   - mem-treure:          eliminar un element d'una llista dins la memòria
;;   - mem-eliminar:        eliminar una clau sencera de la memòria
;;   - mem-fusionar:        fusionar dues memòries (mem-2 sobreescriu mem-1)
;;   - mem-limpiar-antiga:  eliminar entrades massa antigues
;;   - mem-mostrar:         imprimir la memòria per depuració
;; ============================================================
;; Com carregar aquest fitxer:
;;   (load "memoria")
;; ============================================================
;; Estructura de la memòria:
;;   La memòria és una alist (association list) de parells clau-valor:
;;     ( (clau1 valor1) (clau2 valor2) ... )
;;   Exemple:
;;     ( (base-enemiga (30 25))
;;       (darrera-vista 145)
;;       (posicio-propia (20 20)) )
;; ============================================================


;; ==============================================================
;;  ------------------- FUNCIONS GENÈRIQUES DE MEMÒRIA -------------------
;; ==============================================================

(defun mem-llegir (memoria clau)
  "Retorna el valor associat a clau dins la memòria, o nil si no existeix.
   memoria: alist de memòria compartida de l'equip
   clau:    símbol que identifica l'entrada a llegir"
  (cond
    ((null memoria) nil)
    ((eq (caar memoria) clau)
     (cadar memoria))
    (t (mem-llegir (cdr memoria) clau))))

(defun mem-escriure (memoria clau valor)
  "Retorna una nova memòria amb clau actualitzada a valor.
   Si clau no existeix, l'afegeix al final.
   memoria: alist de memòria compartida de l'equip
   clau:    símbol que identifica l'entrada a escriure
   valor:   nou valor a emmagatzemar"
  (cond
    ((null memoria)
     (list (list clau valor)))
    ((eq (caar memoria) clau)
     (cons (list clau valor) (cdr memoria)))
    (t
     (cons (car memoria)
           (mem-escriure (cdr memoria) clau valor)))))

(defun mem-afegir (memoria clau element)
  "Afegeix element a la llista associada a clau, o la crea si no existeix.
   memoria: alist de memòria compartida de l'equip
   clau:    símbol que identifica la llista dins la memòria
   element: element a afegir al final de la llista"
  (let ((llista-actual (mem-llegir memoria clau)))
    (cond
      ((null llista-actual)
       (mem-escriure memoria clau (list element)))
      (t
       (mem-escriure memoria clau (append llista-actual (list element)))))))

(defun mem-treure (memoria clau element)
  "Elimina la primera ocurrència d'element de la llista associada a clau.
   memoria: alist de memòria compartida de l'equip
   clau:    símbol que identifica la llista dins la memòria
   element: element a eliminar de la llista"
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

(defun mem-eliminar (memoria clau)
  "Retorna una nova memòria sense l'entrada corresponent a clau.
   memoria: alist de memòria compartida de l'equip
   clau:    símbol de l'entrada a eliminar completament"
  (cond
    ((null memoria) nil)
    ((eq (caar memoria) clau)
     (cdr memoria))
    (t
     (cons (car memoria)
           (mem-eliminar (cdr memoria) clau)))))

(defun mem-fusionar (mem-1 mem-2)
  "Retorna una nova memòria amb els valors de mem-1 i mem-2 combinats.
   Les claus de mem-2 sobreescriuen les de mem-1 en cas de conflicte.
   mem-1: memòria base (les seves claus poden ser sobreescrites)
   mem-2: memòria prioritària (les seves claus prevalen)"
  (cond
    ((null mem-2) mem-1)
    (t
     (let ((clau  (caar mem-2))
           (valor (cadar mem-2)))
       (mem-fusionar (mem-escriure mem-1 clau valor) (cdr mem-2))))))

(defun mem-limpiar-antiga (memoria ronda-actual max-antiguitat)
  "Retorna una nova memòria eliminant l'entrada 'darrera-vista' si és massa antiga.
   Es considera antiga si ronda-actual - darrera-vista > max-antiguitat.
   memoria:        alist de memòria compartida de l'equip
   ronda-actual:   número de la ronda present
   max-antiguitat: nombre màxim de rondes que es considera informació vàlida"
  (cond
    ((null memoria) nil)
    ((and (eq (caar memoria) 'darrera-vista)
          (> (- ronda-actual (cadar memoria)) max-antiguitat))
     (mem-limpiar-antiga (cdr memoria) ronda-actual max-antiguitat))
    (t
     (cons (car memoria)
           (mem-limpiar-antiga (cdr memoria) ronda-actual max-antiguitat)))))

(defun mem-mostrar (memoria etiqueta)
  "Imprimeix el contingut de la memòria amb una etiqueta. Útil per depurar.
   memoria:  alist de memòria compartida a mostrar
   etiqueta: text identificatiu que es mostra davant del contingut"
  (cond
    ((null memoria)
     (princ etiqueta)
     (princ ": BUIDA")
     (terpri))
    (t
     (princ etiqueta)
     (princ ": ")
     (print memoria))))


;; ==============================================================
;;  ------------------- DOCUMENTACIÓ D'ÚS -------------------
;; ==============================================================

;;  COM FUNCIONA EL FLUX A paintball.lsp:
;;  ======================================
;;  1. CREAR (crear-estat-inicial):
;;       (list 'memoria-e1 nil)
;;       (list 'memoria-e2 nil)
;;
;;  2. PASSAR A L'AGENT (construir-info-unitat):
;;       → obtenir-memoria-equip estat equip
;;       L'agent rep la memòria a la posició 11 de la llista de dades.
;;
;;  3. AGENTS ACTUALITZEN (agent-cms213.lsp, agent-nms864.lsp):
;;       (nova-memoria (agent-xxx-actualitzar-memoria memoria ...))
;;       (if nova-memoria
;;           (list (list 'escriu-memoria (list nova-memoria))))
;;
;;  4. APLICAR A L'ESTAT (aplicar-escriu-memoria):
;;       → substituir-camp 'memoria-e1/e2 nova-memoria estat


;;  CASOS D'ÚS PRÀCTICS:
;;  =====================

;;  CAS 1: RECORDAR LA BASE ENEMIGA
;;  ---------------------------------
;;  Una bolla la veu i la guarda; les altres la persegueixen sense veure-la.
;;
;;    (mem-escriure memoria 'base-enemiga '(30 25))
;;    → Altres bolles: (mem-llegir memoria 'base-enemiga) → (30 25)

;;  CAS 2: EXPLORACIÓ COORDINADA
;;  ------------------------------
;;  Cap agent explora dues vegades la mateixa zona.
;;
;;    (mem-escriure memoria 'ultima-zona '(30 25))
;;    → Altra bolla: si (mem-llegir memoria 'ultima-zona) és a prop, explora un altre lloc.

;;  CAS 3: ATAC SINCRONITZAT
;;  -------------------------
;;  Totes les bolles ataquen alhora quan una dóna l'ordre.
;;
;;    (mem-escriure memoria 'ordre-atac 'ATACAR-JA)
;;    → Altres bolles: si (mem-llegir memoria 'ordre-atac) és 'ATACAR-JA, pinten.

;;  CAS 4: REGISTRE D'AVISTAMENTS
;;  --------------------------------
;;  Cada bolla afegeix els enemics que veu a una llista compartida.
;;
;;    (mem-afegir memoria 'enemics-avistats '(30 20))
;;    → (mem-llegir memoria 'enemics-avistats) → ((30 20) (32 22))


;;  NOTES D'IMPLEMENTACIÓ:
;;  =======================
;;  1. ATOMICITAT: cada 'escriu-memoria reemplaça TOTA la memòria anterior.
;;     Sempre llegeix primer i fusiona si vols afegir informació.
;;
;;  2. SINCRONITZACIÓ: totes les unitats llegeixen la memòria actual cada torn.
;;     Les escriptures es fan al final de l'acció. És millor que les
;;     actualitzacions siguin additives i no entrin en conflicte.
;;
;;  3. EFICIÈNCIA: la memòria creix indefinidament si no es neteja.
;;     Usa mem-limpiar-antiga per eliminar info massa antiga.
;;     Exemple: (mem-limpiar-antiga memoria ronda-actual 50)
;;
;;  4. DEPURACIÓ: usa mem-mostrar per inspeccionar l'estat de la memòria.
;;     Exemple: (mem-mostrar (agent-nms864-memoria dades) "E2")