;; ============================================================
;; Pràctica final de Llenguatges de Programació - LISP
;; Paintball - Mòdul de Pausa Temporal
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
;; Descripció:
;;   Implementació de la funció sleep per a XLISP-PLUS.
;;   Permet pausar l'execució un nombre de segons determinat.
;;   S'usa al bucle principal per controlar la velocitat de la partida.
;;
;;   NOTA: Conté un bucle iteratiu (do) que és l'única excepció
;;   al disseny funcional de la pràctica. No s'ha d'usar enlloc més.
;; ============================================================

(defun sleep (seconds)
  "Espera la quantitat indicada de segons.
   Nota: usa un bucle iteratiu (do), excepció justificada al disseny funcional.
   seconds: nombre de segons a esperar (pot ser decimal, p.ex. 0.05)"
  ; Bucle iteratiu necessari per implementar la pausa temporal.
  ; NO FER SERVIR BUCLES ITERATIUS FORA D'AQUEST FITXER.
  (do ((endtime (+ (get-internal-real-time)
                   (* seconds internal-time-units-per-second))))
      ((> (get-internal-real-time) endtime))))