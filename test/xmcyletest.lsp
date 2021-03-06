;; xmcycletest.lsp - make-cycle (cycle pattern) test
;;
;; Roger B. Dannenberg
;; Feb, 2018

;; cycle test

(setf cp (make-cycle '(a b c d)))
(setf cpp (next cp t))
(xmassert (equal '(a b c d) cpp) "cpp")
(setf cpp (next cp t))
(xmassert (equal '(a b c d) cpp) "cpp2")
(dotimes (i 10)
  (xmassert (equal (next cp) (nth (rem i 4) '(a b c d))) "next of cp"))

(setf cpf (make-cycle '(a b c d) :for 3))
(setf cpfp (next cpf t))
(xmassert (equal '(a b c) cpfp) "cpfp")
(setf cpfp (next cpf t))
(xmassert (equal '(a b c) cpfp) "cpfp2")

(setf cc (make-cycle (list (make-cycle '(a b c)) (make-cycle '(d e)))))
(setf ccp (next cc t))
(xmassert (equal '(a b c) ccp) "ccp1")
(setf ccp (next cc t))
(xmassert (equal '(d e) ccp) "ccp2")
(setf ccp (next cc t))
(xmassert (equal '(a b c) ccp) "ccp3")

(setf ccm (make-cycle (list (make-cycle '(a b c)) (make-cycle '(d e))) :merge t))
(setf ccmp (next ccm t))
(xmassert (equal '(a b c d e) ccmp) "ccmp1")
(setf ccmp (next ccm t))
(xmassert (equal '(a b c d e) ccmp) "ccmp2")

(setf ccf (make-cycle (list (make-cycle '(a b c)) (make-cycle '(d e) :for 3))))
(setf ccfp (next ccf t))
(xmassert (equal '(a b c) ccfp) "ccfp1")
(setf ccfp (next ccf t))
(xmassert (equal '(d e d) ccfp) "ccfp2")
(setf ccfp (next ccf t))
(xmassert (equal '(a b c) ccfp) "ccfp3")
(setf ccfp (next ccf t))
(xmassert (equal '(d e d) ccfp) "ccfp4")

(setf ccmf (make-cycle (list (make-cycle '(a b c)) (make-cycle '(d e) :for 3)) :merge t))
(setf ccmfp (next ccmf t))
(xmassert (equal '(a b c d e d) ccmfp) "ccmfp1")
(setf ccmfp (next ccmf t))
(xmassert (equal '(a b c d e d) ccmfp) "ccmfp2")

(setf ccff (make-cycle (list (make-cycle '(a b c) :name "cycabc") 
                             (make-cycle '(d e) :for 3 :name "cycde")) :for 7))
(setf ccffp (next ccff t))
(xmassert (equal '(a b c d e d a) ccffp) "ccffp1")
(setf ccffp (next ccff t))
(xmassert (equal '(b c d e d a b) ccffp) "ccffp2")

;; patterns for parameters
(setf cpfpp (make-cycle '(a b) :for (make-cycle '(1 2 3) :name "forcycle")))
(setf cpfppp (next cpfpp t))
(xmassert (equal '(a) cpfppp) "cpfppp")
(setf cpfppp (next cpfpp t))
(xmassert (equal '(a b) cpfppp) "cpfppp2")
(setf cpfppp (next cpfpp t))
(xmassert (equal '(a b a) cpfppp) "cpfppp3")
(setf cpfppp (next cpfpp t))
(xmassert (equal '(a) cpfppp) "cpfppp4")

(setf ccmpp (make-cycle (list (make-cycle '(a b)) (make-cycle '(d e)))
                        :merge (make-cycle '(nil t) :name "mergecylce")))
(setf ccmppp (next ccmpp t))
(xmassert (equal '(a b) ccmppp) "ccmppp1")
(setf ccmppp (next ccmpp t))
(xmassert (equal '(d e) ccmppp) "ccmppp2")
(setf ccmppp (next ccmpp t))
(xmassert (equal '(a b d e) ccmppp) "ccmppp3")
(setf ccmppp (next ccmpp t))
(xmassert (equal '(a b) ccmppp) "ccmppp4")

; cycle list as pattern
(setf cpat (make-cycle (make-cycle (list (make-cycle '(a b) :name "cpatab")
                                         (make-cycle '(c d) :name "cpatcd"))
                                   :name "cpatabcd")
                       :name "cpat"))
(setf cpatp (next cpat t))
(xmassert (equal '(a b) cpatp) "cpatp1")
(setf cpatp (next cpat t))
(xmassert (equal '(c d) cpatp) "cpatp2")
(setf cpatp (next cpat t))
(xmassert (equal '(a b) cpatp) "cpatp3")

(setf cpatf (make-cycle (make-cycle (list (make-cycle '(a b) :name "cpatfab")
                                          (make-cycle '(c d) :name "cpatfcd"))
                                    :name "cpatfabcd")
                        :name "cpatf" :for 10))
(setf cpatfp (next cpatf t))
(xmassert (equal '(a b a b a b a b a b) cpatfp) "cpatfp1")
(setf cpatfp (next cpatf t))
(xmassert (equal '(c d c d c d c d c d) cpatfp) "cpatfp2")
(setf cpatfp (next cpatf t))
(xmassert (equal '(a b a b a b a b a b) cpatfp) "cpatfp3")


(setf cpatff (make-cycle (make-cycle (list (make-cycle '(a b) :name "cpatffab")
                                           (make-cycle '(c d) :name "cpatffcd" :for 3))
                                     :name "cpatffabcd")
                         :name "cpatff" :for 10))
(setf cpatffp (next cpatff t))
(xmassert (equal '(a b a b a b a b a b) cpatffp) "cpatffp1")
(setf cpatffp (next cpatff t))
(xmassert (equal '(c d c c d c c d c c) cpatffp) "cpatffp2")
(setf cpatffp (next cpatff t))
(xmassert (equal '(a b a b a b a b a b) cpatffp) "cpatffp3")

