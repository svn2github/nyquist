;; xmwindowtest - make-window (window pattern generator) test
;;
;; Roger B. Dannenberg
;; Mar, 2018

;; window test

(setf wp (make-window (make-cycle '(a b c d)) 3 1))
(setf wpp (next wp t))
(xmassert (equal '(a b c) wpp) "wpp")
(setf wpp (next wp t))
(xmassert (equal '(b c d) wpp) "wpp2")
(setf wpp (next wp t))
(xmassert (equal '(c d a) wpp) "wpp3")
(setf wpp (next wp t))
(xmassert (equal '(d a b) wpp) "wpp4")

;; use length-pattern
(setf wfp (make-window (make-cycle '(a b c d) :name "abcd")
                       (make-cycle '(1 2 3) :name "c123") 1 :name "wfp"))
(setf wfpp (next wfp t))
(xmassert (equal '(a) wfpp) "wfpp")
(setf wfpp (next wfp t))
(xmassert (equal '(b c) wfpp) "wfpp2")
(setf wfpp (next wfp t))
(xmassert (equal '(c d a) wfpp) "wfpp3")
(setf wfpp (next wfp t))
(xmassert (equal '(d) wfpp) "wfpp4")
(setf wfpp (next wfp t))
(xmassert (equal '(a b) wfpp) "wfpp5")
(setf wfpp (next wfp t))
(xmassert (equal '(b c d) wfpp) "wfpp6")

