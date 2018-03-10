;; xmlinetest.lsp - make-line (line pattern) test
;;
;; Roger B. Dannenberg
;; Feb, 2018

;;; line tests

(setf lp (make-line '(a b c d)))
(setf lpp (next lp t))
(xmassert (equal '(a b c d) lpp) "lpp")
(setf lpp (next lp t))
(xmassert (equal '(d d d d) lpp) "lpp2")
(dotimes (i 10)
  (xmassert (equal (next lp) 'd) "next of lp"))

(setf lpf (make-line '(a b c d) :for 6))
(setf lpfp (next lpf t))
(xmassert (equal '(a b c d d d) lpfp) "lpfp")
(setf lpfp (next lpf t))
(xmassert (equal '(d d d d d d) lpfp) "lpfp2")

(setf ll (make-line (list (make-line '(a b c)) (make-line '(d e)))))
(setf llp (next ll t))
(xmassert (equal '(a b c) llp) "llp1")
(setf llp (next ll t))
(xmassert (equal '(d e) llp) "llp2")
(setf llp (next ll t))
(xmassert (equal '(e e) llp) "llp3")

(setf llm (make-line (list (make-line '(a b c)) (make-line '(d e))) :merge t))
(setf llmp (next llm t))
(xmassert (equal '(a b c d e) llmp) "llmp1")
(setf llmp (next llm t))
(xmassert (equal '(e e e e) llmp) "llmp2")

(setf llf (make-line (list (make-line '(a b c)) (make-line '(d e) :for 3))))
(setf llfp (next llf t))
(xmassert (equal '(a b c) llfp) "llfp1")
(setf llfp (next llf t))
(xmassert (equal '(d e e) llfp) "llfp2")
(setf llfp (next llf t))
(xmassert (equal '(e e e) llfp) "llfp3")
(setf llfp (next llf t))
(xmassert (equal '(e e e) llfp) "llfp4")

(setf llmf (make-line (list (make-line '(a b c)) (make-line '(d e) :for 3)) :merge t))
(setf llmfp (next llmf t))
(xmassert (equal '(a b c d e e) llmfp) "llmfp1")
(setf llmfp (next llmf t))
(xmassert (equal '(e e e e e e) llmfp) "llmfp2")

(setf llff (make-line (list (make-line '(a b c) :name "lineabc") 
                             (make-line '(d e) :for 3 :name "linede")) :for 7))
(setf llffp (next llff t))
(xmassert (equal '(a b c d e e e) llffp) "llffp1")
(setf llffp (next llff t))
(xmassert (equal '(e e e e e e e) llffp) "llffp2")

;; patterns for parameters
(setf lpfpp (make-line '(a b) :for (make-cycle '(1 2 3) :name "forline")))
(setf lpfppp (next lpfpp t))
(xmassert (equal '(a) lpfppp) "lpfppp")
(setf lpfppp (next lpfpp t))
(xmassert (equal '(b b) lpfppp) "lpfppp2")
(setf lpfppp (next lpfpp t))
(xmassert (equal '(b b b) lpfppp) "lpfppp3")
(setf lpfppp (next lpfpp t))
(xmassert (equal '(b) lpfppp) "lpfppp4")

(setf llmpp (make-line (list (make-line '(a b)) (make-line '(d e)))
                        :merge (make-cycle '(nil t) :name "mergeline")))
(setf llmppp (next llmpp t))
(xmassert (equal '(a b) llmppp) "llmppp1")
(setf llmppp (next llmpp t))
(xmassert (equal '(d e) llmppp) "llmppp2")
(setf llmppp (next llmpp t))
(xmassert (equal '(e e e e) llmppp) "llmppp3")
(setf llmppp (next llmpp t))
(xmassert (equal '(e e) llmppp) "llmppp4")


;; make list be a pattern

(setf lpatp (make-line (make-cycle (list (make-cycle '(a b c d))
                                         (make-cycle '(e f g)))
                                    :trace t :name "pcabcdefg")))
(setf lpatpp (next lpatp t))
(xmassert (equal '(a b c d) lpatpp) "lpatpp")
(setf lpatpp (next lpatp t))
(xmassert (equal '(e f g) lpatpp) "lpatpp2")

(setf lpatfp (make-line (make-cycle (list (make-cycle '(a b c d))
                                         (make-cycle '(e f g)))
                                    :trace t :name "lpatfpabcdefg")
                       :for 6 :name "lpatfp"))
(setf lpatfpp (next lpatfp t))
(xmassert (equal '(a b c d d d) lpatfpp) "lpatfpp")
(setf lpatfpp (next lpatfp t))
(xmassert (equal '(e f g g g g) lpatfpp) "lpatfpp2")

