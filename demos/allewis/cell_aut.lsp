;; cell_aut.lsp -- cellular automata for sound generation
;;    originally by Ann Lewis
;;    revised by Roger B. Dannenberg
;;    March 2004

;; compute-update-rule -- compute the output given a rule and 3 inputs
;; 
(defun compute-update-rule (rule a b c)
  ;; this is the most interesting part of the whole program
  ;; the rule has 8 bits. Use a, b, c to index the rule bits
  ;; and determine the result. To do this, we want a bit in 
  ;; the right position, e.g. if a=0,b=1,c=1, then the index
  ;; is 011 = 3, and we want a bit pattern with "1" in position 3:
  ;; 00001000. Note that this is 2^3 = a*2^4 + b*2^2 + c*2^1. So
  ;; we can test a, b, and c and multiply by 16, 4, and 2 to get
  ;; the right bit pattern. Then we can "and" it with rule to
  ;; get either zero or a non-zero as the result.
  (let ((abc 1))
    (cond (a (setf abc 16)))
    (cond (b (setf abc (* abc 4))))
    (cond (c (setf abc (* abc 2))))
    (setf rule (logand rule abc))
    (> rule 0)))


;; the main cellular automata implementation
(defun cell-aut (sound-object-list duration update-rule iter)
  (let (prev current array-len score (now 0.0) tmp)
    ; create and initialize current and prev bit lists
    (setf array-len (length sound-object-list))
    (setf prev (make-array array-len))
    (setf current (make-array array-len))
    (setf prev (make-array array-len))
    (setf (aref prev (/ array-len 2)) t)
    ; create the score by computing iter generations of the automata
    (dotimes (i iter)
      (dotimes (j array-len)
        (princ (if (aref prev j) " X" " 0"))
        ; push a note on the list for each "true" in the prev array
        (if (aref prev j)
            (push (list now duration (nth j sound-object-list)) score)))
        (terpri)
      ; compute current from prev using update-rule
      ; first do endpoints, wrap-around style
      (setf (aref current 0) 
            (compute-update-rule update-rule 
             (aref prev (- array-len 1)) (aref prev 0) (aref prev 1)))
      (setf (aref current (- array-len 1)) 
            (compute-update-rule update-rule
             (aref prev (- array-len 2)) (aref prev (- array-len 1))
             (aref prev 0)))

      ; then loop through everything else
      ; want to cycle from 1 to array-len-2
      (dotimes (j (- array-len 2))
        (setf (aref current (+ j 1)) 
              (compute-update-rule update-rule 
               (aref prev j) (aref prev (+ j 1)) (aref prev (+ j 2))))
)
      ; set prev to current
      (setf tmp prev)
      (setf prev current)
      (setf current tmp)
      ; increment the time
      (setf now (+ now duration)))
    ;; the score is now in reverse order, so fix it
    (setf score (reverse score)) 
    ;; now we have a score, render it to sound and return it:
    (timed-seq score))
)

(defun cell-aut-major-scale ()
  ;; for testing, this creates an 8-note scale
  ;; requires (load "pianosyn") to get the piano library
  (let (scale offsets)
    (setf offsets '(0 2 4 5 7 9 11 12))
    (dolist (p offsets)
      (push (list 'piano-note-2 (+ A3 p) 100) scale))
    (reverse scale)))

(defun cell-aut-demo ()
  (require-from 'piano-note-2 "pianosyn.lsp")
  (play (scale 0.5 (cell-aut (cell-aut-major-scale) 0.2 30 80))))
