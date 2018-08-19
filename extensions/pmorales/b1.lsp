;;; ADDITIVE SYNTHESIS
;;; Gong like sounds
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(autonorm-on)
(setf *pmorales-path* (current-path))
(load (strcat *pmorales-path* "pjmg.lsp"))


(defun add-partial (dur frq scal)
  (amosc (hz-to-step frq) (pwev scal dur (* scal 1e-2))))

(defun gong-1 ()
    (sim (add-partial 4 240 3.0)
         (add-partial 4 277 2.5)
         (add-partial 4 385 2.0)
         (add-partial 4 605 3.0)
         (add-partial 4 340 1.0)
         (add-partial 4 670 1.0)
         (add-partial 4 812 1.0)))

(defun add-partial-2 (frq scal)
  (amosc (hz-to-step frq) (pwev scal (/ (* 6 240) frq) (* scal 1e-2))))

(defun gong-2 ()
  (sim (add-partial-2 240 3.0)
       (add-partial-2 277 2.5)
       (add-partial-2 385 2.0)
       (add-partial-2 605 3.0)
       (add-partial-2 340 1.0)
       (add-partial-2 670 1.0)
       (add-partial-2 812 1.0)))

(defun add-partial-3 (frq fratio dur amp)
  (amosc (hz-to-step (* frq fratio)) (pwev amp (/ dur fratio) (* amp 1e-2))))

(defun gong-3 (frq dur)
  (sim (add-partial-3 frq 1.0 dur 2.0)
       (add-partial-3 frq 2.0 dur 2.0)
       (add-partial-3 frq 2.4 dur 2.0)
       (add-partial-3 frq 3.0 dur 2.0)
       (add-partial-3 frq 4.5 dur 3.0)
       (add-partial-3 frq 5.33 dur 3.0)
       (add-partial-3 frq 6.0 dur 3.0)))


(defun gong-3-melody ()
  (sim (at 0.0 (gong-3 329 5))
       (at 0.2 (gong-3 360 6))
       (at 0.4 (gong-3 380 5))
       (at 0.6 (gong-3 300 8))
       (at 0.8 (gong-3 430 4))
       (at 2.0 (gong-3 640 4))
       (at 2.2 (gong-3 610 5))
       (at 2.4 (gong-3 580 4))
       (at 2.6 (gong-3 660 5))))

(defun gong-3-demo () (ss (gong-3-melody)))


