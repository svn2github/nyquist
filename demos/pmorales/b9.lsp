;;; ADDITIVE SYNTHESIS
;;; Risset Endless
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(load "pjmg.lsp")

(defun twopi ()
  (setf *twopi* (* 2 3.141592653589793)))

(if (not (boundp '*twopi*)) (twopi))

(defun bell-table ()
  (setf *bell-table* (make-array 512))
  (dotimes (i 512)
    (setf (aref *bell-table* i)
      (exp (* -4.8283 (- 1 (cos (* *twopi* (- i 255.5) (/ 511.0))))))))
  (setf *bell-table* (snd-from-array 0.0 512 *bell-table*))
  (setf *bell-table* (list *bell-table* (hz-to-step 1.0) T)))

(if (not (boundp '*bell-table*)) (bell-table))

(defun frq-table ()
  (setf *frq-table*
    (list (sim (pwe 1.0 16e-4) (const -1.0 1.0)) (hz-to-step 1.0) T)))

(if (not (boundp '*frq-table*)) (frq-table))

(defun endless-partial ()
  (mult (osc (hz-to-step 0.025) 40 *bell-table*)
        (fmosc (hz-to-step 16000) (scale 16000
                    (osc (hz-to-step 0.025) 40 *frq-table*)))))

(setf *endless-partial* (endless-partial))

(autonorm-off)

(play (scale 0.25 (apply #'sim (mapcar #'(lambda (x) (at x (cue *endless-partial*)))
            '(0.0 2.0 4.0 6.0 8.0 10.0 12.0
             14.0 16.0 18.0 20.0)))))

