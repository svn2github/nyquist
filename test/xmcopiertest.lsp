;; xmcopier.lsp -- run some tests on xm.lsp patterns
;;
;; Roger B. Dannenberg, Mar 2018

;; natural sub-pattern, no merge, no for
(setf cp (make-copier (make-cycle '(a b c)) :repeat 3))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t)))
(xmassert (equal cpout '((A B C) (A B C) (A B C) (A B C))) "copy-nat-n-n")

;; for sub-pattern, no merge, no for
(setf cp (make-copier (make-cycle '(a b c) :for 2) :repeat 3))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t t)))
(xmassert (equal cpout '((A B) (A B) (A B) (A B) (A B))) "copy-for-n-n")

;; natural sub-pattern, merge, no for
(setf cp (make-copier (make-cycle '(a b c)) :repeat 3 :merge t))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t)))
(xmassert (equal cpout '((A B C A B C A B C)
                         (A B C A B C A B C)
                         (A B C A B C A B C)
                         (A B C A B C A B C))) "copy-nat-n-n")

;; for sub-pattern, merge, no for
(setf cp (make-copier (make-cycle '(a b c) :for 2) :repeat 3))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t t)))
(xmassert (equal cpout '((A B) (A B) (A B) (A B) (A B))) "copy-for-t-n")

;; natural sub-pattern, no merge, for
(setf cp (make-copier (make-cycle '(a b c)) :repeat 3 :for 5))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t)))
(xmassert (equal cpout '((A B C A B) (C A B C A) (B C A B C) (A B C A B))) 
          "copy-nat-n-t")

;; for sub-pattern, no merge, for
(setf cp (make-copier (make-cycle '(a b c) :for 2) :repeat 3 :for 5))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t t)))
(xmassert (equal cpout '((A B A B A) (A B A B A) (A B A B A) 
                         (A B A B A) (A B A B A))) "copy-for-n-t")

;; natural sub-pattern, merge, for
(setf cp (make-copier (make-cycle '(a b c)) :repeat 3 :for 5))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t t)))
(xmassert (equal cpout '((A B C A B) (C A B C A) (B C A B C) 
                         (A B C A B) (C A B C A))) "copy-for-t-t")


;; for sub-pattern, merge, for
(setf cp (make-copier (make-cycle '(a b c) :for 2) :repeat 3 :for 5))
(setf cpout (mapcar #'(lambda (x) (next cp t)) '(t t t t t)))
(xmassert (equal cpout '((A B A B A) (A B A B A) (A B A B A) 
                         (A B A B A) (A B A B A))) "copy-for-t-t")
