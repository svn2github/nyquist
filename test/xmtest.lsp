;; xmtest.lsp -- run some tests on xm.lsp patterns
;;
;; Roger B. Dannenberg, Feb 2018


(setf *xm-trace* nil)

(defun xmassert (test msg)
  (cond (test (format t "PASSED ~A~%" msg))
        (t (error msg))))

(load "xmcycletest")
(load "xmlinetest")
(load "xmrandomtest")
(load "xmpalindrometest")
(load "xmheaptest")
(load "xmaccumulationtest")
(load "xmcopiertest")
(load "xmaccumulatetest")
(load "xmsumtest")
(load "xmproducttest")
(load "xmevaltest")
(load "xmlengthtest")
(load "xmwindowtest")
(load "xmmarkovtest")

(exit)

