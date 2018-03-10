;; xmheaptest.lsp - make-heap (heap pattern generator) test
;; 
;; Roger B. Dannenberg
;; Mar, 2018

(defun contains-all (ref actual len)
  (and (eql (length actual) len)
       (not (dolist (r ref)
              (if (not (member r actual))
                  (return t))))))

(dotimes (i 20) ;; because it is random, we'll make a lot of runs
    (setf hp (make-heap '(a b c d)))
    (setf hpp (next hp t))
    (xmassert (contains-all '(a b c d) hpp 4) "hpp")
    (setf hpp (next hp t))
    (xmassert (contains-all '(a b c d) hpp 4) "hpp2")
    (dotimes (i 10)
      (xmassert (member (next hp) '(a b c d)) "next of hp"))
    
    (setf hpf (make-heap '(a b c d) :for 3))
    (setf hpfp (next hpf t))
    (xmassert (contains-and-length '(a b c d) hpfp 3) "hpfp")
    (setf hpfp (next hpf t))
    (xmassert (contains-and-length '(a b c d) hpfp 3) "hpfp2")
    
    (setf cr (make-cycle (list (make-heap '(a b c)) (make-heap '(d e)))))
    (setf chp (next cr t))
    (xmassert (contains-all '(a b c) chp 3) "chp1")
    (setf chp (next cr t))
    (xmassert (contains-all '(d e) chp 2) "chp2")
    (setf chp (next cr t))
    (xmassert (contains-all '(a b c) chp 3) "chp3")
    
    (setf crm (make-cycle (list (make-heap '(a b c)) (make-heap '(d e))) :merge t))
    (setf crmp (next crm t))
    (xmassert (contains-all '(a b c d e) crmp 5) "crmp1")
    (setf crmp (next crm t))
    (xmassert (contains-all '(a b c d e) crmp 5) "crmp2")
    
    (setf crf (make-cycle (list (make-heap '(a b c)) (make-heap '(d e) :for 3))))
    (setf crfp (next crf t))
    (xmassert (contains-all '(a b c) crfp 3) "crfp1")
    (setf crfp (next crf t))
    (xmassert (contains-and-length '(e d) crfp 3) "crfp2")
    (setf crfp (next crf t))
    (xmassert (contains-all '(a b c) crfp 3) "crfp3")
    (setf crfp (next crf t))
    (xmassert (contains-and-length '(e d) crfp 3) "crfp4")
    
    (setf crmf (make-cycle (list (make-heap '(a b c)) (make-heap '(d e) :for 3)) :merge t))
    (setf crmfp (next crmf t))
    (xmassert (contains-and-length '(a b c d e) crmfp 6) "crmfp1")
    (setf crmfp (next crmf t))
    (xmassert (contains-and-length '(a b c d e) crmfp 6) "crmfp2")
    
    (setf crff (make-cycle (list (make-heap '(a b c) :name "randabc") 
                                 (make-heap '(d e) :for 3 :name "randde")) :for 7))
    (setf crffp (next crff t))
    (xmassert (contains-and-length '(a b c d e) crffp 7) "crffp1")
    (setf crffp (next crff t))
    (xmassert (contains-and-length '(a b c d e) crffp 7) "crffp2")
    
    ;; patterns for parameters
    (setf hpfpp (make-heap '(a b) :for (make-cycle '(1 2 3) :name "forheap")))
    (setf hpfppp (next hpfpp t))
    (xmassert (contains-and-length '(a b) hpfppp 1) "hpfppp")
    (setf hpfppp (next hpfpp t))
    (xmassert (contains-and-length '(a b) hpfppp 2) "hpfppp2")
    (setf hpfppp (next hpfpp t))
    (xmassert (contains-and-length '(a b) hpfppp 3) "hpfppp3")
    (setf hpfppp (next hpfpp t))
    (xmassert (contains-and-length '(a b) hpfppp 1) "hpfppp4")
    
    (setf crmpp (make-cycle (list (make-heap '(a b)) (make-heap '(d e)))
                            :merge (make-cycle '(nil t) :name "mergecylce")))
    (setf crmppp (next crmpp t))
    (xmassert (contains-all '(a b) crmppp 2) "crmppp1")
    (setf crmppp (next crmpp t))
    (xmassert (contains-all '(d e) crmppp 2) "crmppp2")
    (setf crmppp (next crmpp t))
    (xmassert (contains-all '(a b d e) crmppp 4) "crmppp3")
    (setf crmppp (next crmpp t))
    (xmassert (contains-all '(a b) crmppp 2) "crmppp4")
    ) ;; end of (dotimes (i 20) ...)


;; heap with max
(setf hpmax3 (make-heap '(a b (c :max 1) (d :max 1))))
(setf ccount 0 dcount 0)
(dotimes (i 100)
  (setf hpmax3p (next hpmax3))
  (cond ((eq hpmax3p 'c) (incf ccount) (setf dcount 0))
        ((eq hpmax3p 'd) (incf dcount) (setf ccount 0))
        (t (setf ccount 0 dcount 0)))
  (if (or (> ccount 1) (> dcount 1))
      (xmassert nil "hpmax3")))
(print "passed hpmax3")


;; heap with pattern for list
(setf hpat (make-heap (make-cycle (list (make-cycle '(a b c d) :for 100)
                                        (make-cycle '(e f g h) :for 100)))))
(defun make-hist ()
  (let (c j)
    (setf hist (make-array 8))
    (dotimes (i 8) (setf (aref hist i) 0))
    (setf hpatp-ok t)
    (dotimes (i 100)
      (setf hpatp (next hpat))
      (setf c (char (symbol-name hpatp) 0))
      (princ c)
      (setf j (- (char-int c) (char-int #\A)))
      (if (and (>= j 0) (< j 8))
          (setf (aref hist j) (1+ (aref hist j)))
          (setf hpatp-ok nil)))))
(setf hpatp-ok t)
;---- pattern a b c d
(make-hist)
(xmassert hpatp-ok "hpatp0")
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (zerop n))
               (and (> i 3) (> n 0)))
           (setf hpatp-ok nil)))))
(xmassert hpatp-ok "hpatp1")
;---- next pattern e f g h
(make-hist)
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (> n 0))
               (and (> i 3) (zerop n)))
           (setf hpatp-ok nil)))))
(xmassert hpatp-ok "hpatp2")
;---- next pattern back to a b c d
(make-hist)
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (zerop n))
               (and (> i 3) (> n 0)))
           (setf hpatp-ok nil)))))
(xmassert hpatp-ok "hpatp3")
(make-hist)
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (> n 0))
               (and (> i 3) (zerop n)))
           (setf hpatp-ok nil)))))
(xmassert hpatp-ok "hpatp4")


;; heap with max pattern

;; this test alternates 30 max 2 with 30 max 1
(setf hpmxpp-cnt (* 3 30))
;; use hpmxpp-cnt / 3 because each item is used for a cycle of 3:
(setf hpmxpp (make-heap '(a b c) :max
         (make-cycle (list (make-cycle '(2) :for (/ hpmxpp-cnt 3) 
                                            :name "ten")
                           (make-cycle '(1) :for (/ hpmxpp-cnt 3) 
                                             :name "one")))
                     :trace nil :name "max-pattern"))

;; check each list of hpmxpp for repeats
;;
(defun repeats-count (lis)
  (cond ((cddr lis)
         (+ (if (eql (car lis) (cadr lis)) 1 0)
            (repeats-count (cdr lis))))
        (t 0)))


(dotimes (i 10)
  (setf hpdata nil)
  (dotimes (i hpmxpp-cnt)
    (setf hpdata (cons (next hpmxpp) hpdata)))
  (setf hpdata (reverse hpdata))
  (setf hpcnt (repeats-count hpdata))
  (cond ((evenp i)
         (xmassert (> hpcnt 0) "hpmxp even"))
        (t
         (xmassert (zerop hpcnt) "hpmxp odd"))))

