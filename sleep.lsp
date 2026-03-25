(defun sleep (seconds)
    "Espera la quantitat indicada de segons"
    ; Això és un bucle iteratiu. NO PODEU FER-LO SERVIR ENLLOC MÉS
    (do ((endtime (+ (get-internal-real-time)
                     (* seconds internal-time-units-per-second))))
        ((> (get-internal-real-time) endtime))))