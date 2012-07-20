;; generate-defines.lsp
;;
;; Roger B. Dannenberg
;; July 2012
;;
;; 


;; GENERATE-DEFINES -- generate INTERP_xxx defines for sound.h
;;
;; n is number of parameters
;;
(defun generate-defines (n)
  (let ((count (int-power 2 n))
        sum code pos)
    (dotimes (i count)
      (format t "#define INTERP_")
      (setf sum 0)
      (dotimes (j n)
        (setf pos (- n 1 j)) ; bit position
        (cond ((testbit i pos)
               (setf code "s")
               (setf sum (+ sum (int-power 4 pos))))
              (t
               (setf code "n")))
        (format t "~A" code))
      (format t " ~A~%" sum))))



(defun int-power (i exponent) (truncate (+ 0.5 (power i exponent))))
(defun testbit (n pos) (/= 0 (logand n (int-power 2 pos))))

