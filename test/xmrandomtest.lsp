;; xmrandomtest.lsp - make-random (random pattern generator) test
;; 
;; Roger B. Dannenberg
;; Feb, 2018

;; CONTAINS - return true if each element of actual is in possible
;;            and length of actual is len
;; 
;; algorithm: dolist returns TRUE if there is an error, so
;; not dolist is the correct return value. Default return value
;; of dolist is nil, meaning every actual is in possible
;;
(defun contains-and-length (possible actual len)
  (and (eql (length actual) len)
       (not (dolist (a actual)
              (if (not (member a possible))
                  (return t))))))

(dotimes (i 20) ;; because it is random, we'll make a lot of runs
    (setf rp (make-random '(a b c d)))
    (setf rpp (next rp t))
    (xmassert (contains-and-length '(a b c d) rpp 4) "rpp")
    (setf rpp (next rp t))
    (xmassert (contains-and-length '(a b c d) rpp 4) "rpp2")
    (dotimes (i 10)
      (xmassert (member (next rp) '(a b c d)) "next of rp"))
    
    (setf rpf (make-random '(a b c d) :for 3))
    (setf rpfp (next rpf t))
    (xmassert (contains-and-length '(a b c d) rpfp 3) "rpfp")
    (setf rpfp (next rpf t))
    (xmassert (contains-and-length '(a b c d) rpfp 3) "rpfp2")
    
    (setf cr (make-cycle (list (make-random '(a b c)) (make-random '(d e)))))
    (setf crp (next cr t))
    (xmassert (contains-and-length '(a b c) crp 3) "crp1")
    (setf crp (next cr t))
    (xmassert (contains-and-length '(d e) crp 2) "crp2")
    (setf crp (next cr t))
    (xmassert (contains-and-length '(a b c) crp 3) "crp3")
    
    (setf crm (make-cycle (list (make-random '(a b c)) (make-random '(d e))) :merge t))
    (setf crmp (next crm t))
    (xmassert (contains-and-length '(a b c d e) crmp 5) "crmp1")
    (setf crmp (next crm t))
    (xmassert (contains-and-length '(a b c d e) crmp 5) "crmp2")
    
    (setf crf (make-cycle (list (make-random '(a b c)) (make-random '(d e) :for 3))))
    (setf crfp (next crf t))
    (xmassert (contains-and-length '(a b c) crfp 3) "crfp1")
    (setf crfp (next crf t))
    (xmassert (contains-and-length '(e d) crfp 3) "crfp2")
    (setf crfp (next crf t))
    (xmassert (contains-and-length '(a b c) crfp 3) "crfp3")
    (setf crfp (next crf t))
    (xmassert (contains-and-length '(e d) crfp 3) "crfp4")
    
    (setf crmf (make-cycle (list (make-random '(a b c)) (make-random '(d e) :for 3)) :merge t))
    (setf crmfp (next crmf t))
    (xmassert (contains-and-length '(a b c d e) crmfp 6) "crmfp1")
    (setf crmfp (next crmf t))
    (xmassert (contains-and-length '(a b c d e) crmfp 6) "crmfp2")
    
    (setf crff (make-cycle (list (make-random '(a b c) :name "randabc") 
                                 (make-random '(d e) :for 3 :name "randde")) :for 7))
    (setf crffp (next crff t))
    (xmassert (contains-and-length '(a b c d e) crffp 7) "crffp1")
    (setf crffp (next crff t))
    (xmassert (contains-and-length '(a b c d e) crffp 7) "crffp2")
    
    ;; patterns for parameters
    (setf rpfpp (make-random '(a b) :for (make-cycle '(1 2 3) :name "forrandom")))
    (setf rpfppp (next rpfpp t))
    (xmassert (contains-and-length '(a b) rpfppp 1) "rpfppp")
    (setf rpfppp (next rpfpp t))
    (xmassert (contains-and-length '(a b) rpfppp 2) "rpfppp2")
    (setf rpfppp (next rpfpp t))
    (xmassert (contains-and-length '(a b) rpfppp 3) "rpfppp3")
    (setf rpfppp (next rpfpp t))
    (xmassert (contains-and-length '(a b) rpfppp 1) "rpfppp4")
    
    (setf crmpp (make-cycle (list (make-random '(a b)) (make-random '(d e)))
                            :merge (make-cycle '(nil t) :name "mergecylce")))
    (setf crmppp (next crmpp t))
    (xmassert (contains-and-length '(a b) crmppp 2) "crmppp1")
    (setf crmppp (next crmpp t))
    (xmassert (contains-and-length '(d e) crmppp 2) "crmppp2")
    (setf crmppp (next crmpp t))
    (xmassert (contains-and-length '(a b d e) crmppp 4) "crmppp3")
    (setf crmppp (next crmpp t))
    (xmassert (contains-and-length '(a b) crmppp 2) "crmppp4")
    ) ;; end of (dotimes (i 20) ...)


;; random with min
(setf rpmin2 (make-random '((a :min 2) b c d) :trace nil :name "rpmin2"))
(setf rpmin2p (next rpmin2))
(dotimes (i 100) ;; try it 20 times
  (cond ((eq rpmin2p 'a)
         (setf rpmin2p (next rpmin2))
         (if (not (eq rpmin2p 'a))
             (xmassert nil "rpmin2"))
         ;; now, we could get a 3rd a and expect a 4th which
         ;; is not required, so keep getting a's until there's
         ;; a non-a
         (while (eq (setf rpmin2p (next rpmin2)) 'a) nil))
        (t
         (setf rpmin2p (next rpmin2)))))

(print "passed rpmin2")

;; more random with min
(setf rpmin3 (make-random '((a :min 2) (b :min 3) c d)))
(setf rpmin3p (next rpmin3))
(dotimes (i 100)
  (cond ((eq rpmin3p 'a)
         (setf rpmin3p (next rpmin3))
         (if (not (eq rpmin3p 'a))
             (xmassert nil "rpmin3"))
         ;; now, we could get a 3rd a and expect a 4th which
         ;; is not required, so keep getting a's until there's
         ;; a non-a
         (while (eq (setf rpmin3p (next rpmin3)) 'a) nil))
        ((eq rpmin3p 'b)
         (setf rpmin3p (next rpmin3))
         (if (not (eq rpmin3p 'b))
             (xmassert nil "rpmin3"))
         (setf rpmin3p (next rpmin3))
         (if (not (eq rpmin3p 'b))
             (xmassert nil "rpmin3"))
         (while (eq (setf rpmin3p (next rpmin3)) 'b) nil))
        (t
         (setf rpmin3p (next rpmin3)))))
(print "passed rpmin3")

;; random with max
(setf rpmax3 (make-random '(a b (c :max 2) (d :max 3))))
(setf ccount 0 dcount 0)
(dotimes (i 100)
  (setf rpmax3p (next rpmax3))
  (cond ((eq rpmax3p 'c) (incf ccount) (setf dcount 0))
        ((eq rpmax3p 'd) (incf dcount) (setf ccount 0))
        (t (setf ccount 0 dcount 0)))
  (if (or (> ccount 2) (> dcount 3))
      (xmassert nil "rpmax3")))
(print "passed rpmax3")


;; random with weight

(setf rpwt3 (make-random '(a b (c :weight 2) (d :weight 4))))
(setf acount 0 bcount 0 ccount 0 dcount 0)
(dotimes (i 400)
  (setf rpwt3p (next rpwt3))
  (cond ((eq rpwt3p 'a) (incf acount))
        ((eq rpwt3p 'b) (incf bcount))
        ((eq rpwt3p 'c) (incf ccount))
        ((eq rpwt3p 'd) (incf dcount))))
(setf abcount (+ acount bcount))
(setf abccount (+ abcount ccount))
(xmassert (and (> acount (/ bcount 2))
               (> bcount (/ acount 2))
               (> ccount (/ abcount 2))
               (> abcount (/ ccount 2))
               (> dcount (/ abccount 2))
               (> abccount (/ dcount 2)))
           "rpwt3")

;; random with pattern for list
(setf rpat (make-random (make-cycle (list (make-cycle '(a b c d) :for 100)
                                          (make-cycle '(e f g h) :for 100)))))
(defun make-hist ()
  (let (c j)
    (setf hist (make-array 8))
    (dotimes (i 8) (setf (aref hist i) 0))
    (setf rpatp-ok t)
    (dotimes (i 100)
      (setf rpatp (next rpat))
      (setf c (char (symbol-name rpatp) 0))
      (princ c)
      (setf j (- (char-int c) (char-int #\A)))
      (if (and (>= j 0) (< j 8))
          (setf (aref hist j) (1+ (aref hist j)))
          (setf rpatp-ok nil)))))
(setf rpatp-ok t)
;---- pattern a b c d
(make-hist)
(xmassert rpatp-ok "rpatp0")
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (zerop n))
               (and (> i 3) (> n 0)))
           (setf rpatp-ok nil)))))
(xmassert rpatp-ok "rpatp1")
;---- next pattern e f g h
(make-hist)
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (> n 0))
               (and (> i 3) (zerop n)))
           (setf rpatp-ok nil)))))
(xmassert rpatp-ok "rpatp2")
;---- next pattern back to a b c d
(make-hist)
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (zerop n))
               (and (> i 3) (> n 0)))
           (setf rpatp-ok nil)))))
(xmassert rpatp-ok "rpatp3")
(make-hist)
(dotimes (i 8)
  (let ((n (aref hist i)))
    (cond ((or (and (< i 4) (> n 0))
               (and (> i 3) (zerop n)))
           (setf rpatp-ok nil)))))
(xmassert rpatp-ok "rpatp4")


;; random with weight pattern
;; first test alternates 180 weight 1 with 180 weight 100
(setf rpwtpp-cnt (* 3 100)) ;; actually failed at 3 * 60
;; use rpwtpp-cnt / 3 because each item is used for a cycle of 3:
(setf rpwtpp (make-random 
  (list 'a 'b 
        (list 'c :weight 
         (make-cycle (list (make-cycle '(1) :for (/ rpwtpp-cnt 3))
                           (make-cycle '(100) :for (/ rpwtpp-cnt 3))))))))
(dotimes (i 5)
  ;; weights all equal
  (setf acount 0 bcount 0 ccount 0)
  (print "Starting equal weights")
  (dotimes (i rpwtpp-cnt)
    (setf rpwtp (next rpwtpp))
    (cond ((eq rpwtp 'a) (incf acount))
          ((eq rpwtp 'b) (incf bcount))
          ((eq rpwtp 'c) (incf ccount))))
  (xmassert (and (> acount (/ bcount 2))
                 (> bcount (/ acount 2))
                 (> bcount (/ ccount 2))
                 (> ccount (/ bcount 2))
                 (> acount (/ ccount 2))
                 (> ccount (/ acount 2)))
            "rpwtpp with equal weights")
  ;; weights favor c
  (setf acount 0 bcount 0 ccount 0)
  (print "Starting unequal weights")
  (dotimes (i rpwtpp-cnt)
    (setf rpwtp (next rpwtpp))
    (cond ((eq rpwtp 'a) (incf acount))
          ((eq rpwtp 'b) (incf bcount))
          ((eq rpwtp 'c) (incf ccount))))
  (xmassert (and (> (/ ccount 2) acount)
                 (> (/ ccount 2) acount))
            "rpwtpp with unequal weights"))


;; random with min pattern

;; this test alternates 30 min 1 with 30 min 5
(setf rpmnpp-cnt (* 3 10))
;; use rpmnpp-cnt / 3 because each item is used for a cycle of 3:
(setf rpmnpp (make-random 
  (list 'a 'b 
        (list 'c :min
         (make-cycle (list (make-cycle '(1) :for (/ rpmnpp-cnt 3) 
                                            :name "one")
                           (make-cycle '(5) :for (/ rpmnpp-cnt 3) 
                                             :name "five"))
                     :trace nil :name "min-pattern")))))
;; correct behavior:
;;    keep c-run-len == number of time C has occurred in a row
;;    on transition from C to anything else, if min=5, 
;;    c-run-len should be > 5. 
;;
(setf acount 0 bcount 0 ccount 0 allcnt 0)
(setf good-min-runs t)
(setf last-rpmnp nil)
(setf c-run-len 0)
(setf c-trans nil)
(dotimes (i 10)
  (dotimes (i rpmnpp-cnt)
    (setf rpmnp (next rpmnpp))
    (cond ((eq rpmnp 'a) (incf acount))
          ((eq rpmnp 'b) (incf bcount))
          ((eq rpmnp 'c) (incf ccount) 
                         (incf c-run-len)))
    ;; we really want to know if last-rpmnp was under a min=5 condition,
    ;;   not if rpmnp is under min=5, because we could get 'a at the
    ;;   beginning of min=5 after a run of 2 'c's
    (setf min=5 (oddp (/ (1- allcnt) 30)))
    (setf changed-from-c (and (eq last-rpmnp 'c) (not (eq rpmnp 'c))))
    (cond ((and changed-from-c min=5 (< c-run-len 5))
           (display "ERROR" allcnt changed-from-c min=5 c-run-len)
           (setf good-min-runs nil)))
    (if (not (eq rpmnp 'c)) (setf c-run-len 0))
    (setf last-rpmnp rpmnp)
    (incf allcnt)))
(display "after rpmnpp" acount bcount ccount allcnt)
(xmassert good-min-runs "rpmnp")
(xmassert (and (> acount (/ bcount 2))
               (> bcount (/ acount 2))
               (> ccount bcount)
               (> ccount acount))
          "rpmnpp final counts")


;; random with max pattern

;; this test alternates 30 max 10 with 30 max 1
(setf rpmxpp-cnt (* 3 10))
;; use rpmxpp-cnt / 3 because each item is used for a cycle of 3:
(setf rpmxpp (make-random 
  (list 'a 'b 
        (list 'c :max
         (make-cycle (list (make-cycle '(10) :for (/ rpmxpp-cnt 3) 
                                            :name "ten")
                           (make-cycle '(1) :for (/ rpmxpp-cnt 3) 
                                             :name "one"))
                     :trace nil :name "max-pattern")))))
;; correct behavior:
;;    keep c-run-len == number of time C has occurred in a row
;;    on transition from C to anything else, 
;;    c-run-len should be <= the-prev-max. 
;;
(setf acount 0 bcount 0 ccount 0 allcnt 0)
(setf good-max-runs t)
(setf last-rpmxp nil)
(setf c-run-len 0)
(setf c-trans nil)
(dotimes (i 10)
  (dotimes (i rpmxpp-cnt)
    (setf rpmxp (next rpmxpp))
    (cond ((eq rpmxp 'a) (incf acount))
          ((eq rpmxp 'b) (incf bcount))
          ((eq rpmxp 'c) (incf ccount) 
                         (incf c-run-len)))
    (setf the-prev-max (if (evenp (/ (- allcnt 2) 30)) 10 1))
    (setf changed-from-c (and (eq last-rpmxp 'c) (not (eq rpmxp 'c))))
    (cond ((and changed-from-c (> c-run-len the-prev-max))
           (display "ERROR" allcnt changed-from-c the-prev-max c-run-len)
           (setf good-max-runs nil)))
    (if (not (eq rpmxp 'c)) (setf c-run-len 0))
    (setf last-rpmxp rpmxp)
    (incf allcnt)))
(display "after rpmxpp" acount bcount ccount allcnt)
(xmassert good-max-runs "rpmxp")
(xmassert (and (> acount (/ bcount 2))
               (> bcount (/ acount 2))
               (> ccount 0)
               (< (/ ccount 2) bcount)
               (< (/ ccount 2) acount))
          "rpmxpp final counts")

