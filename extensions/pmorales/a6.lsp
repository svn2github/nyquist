;;; SIMPLE SYNTHESIS
;;; Waveform + Envelope. Modulating the frequency, 2
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(setf *pmorales-path* (current-path))
(load (strcat *pmorales-path* "pjmg.lsp"))

(defun saw-table ()
  (setf *saw-table* (pwlv 1.0 1.0 0.0))
  (setf *saw-table* (list *saw-table* (hz-to-step 1) T)))

(if (not (boundp '*saw-table*)) (saw-table))

(defun cheap (frq-randi frq dur lfor lfoi)
  (mult (randi1 frq-randi dur) 
        (fmosc frq (mult (const lfoi dur)
                         (osc (hz-to-step lfor) dur *saw-table*)))))


(defun callas (dur frq vib-r vib-w)
  (mult (pwl 0.1 1.0 (- dur 0.1) 1.0 dur)
        (fmosc frq (mult (const vib-w dur)
                         (sine (hz-to-step vib-r) dur)))))

(defun callas-demo ()
  (ss (seq (sim (at 0.0 (cheap 80 a4 6.5 3 1000))
                (at 2.5 (cheap 150 a5 6.5 3 750)))
           (callas 1 a4 5 24)
           (callas 0.5 e5 5 24) (callas 0.5 f5 5 24) (callas 1 a5 5 24)
           (callas 1 c6 5 24) (callas 1 e6 5 24)
           (callas 1 g4 5 24) (callas 1 f4 5 24)
          (callas 3 e4 5 24))))
