;; regression-test.lsp - run lots of stuff
;;
;; Most of these are not actual tests because the output is not tested
;; for correctness, but often these tests reveal bugs when Nyquist is 
;; configured for release.
;;
;; This file extends the ex-global() tests in examples.lsp and examples.sal
;; by running both and then running the typechecks.sal program which really
;; does have unit tests for many type-error checks.

(print (setdir (current-path)))
(print (setdir "../demos/src"))
(load "examples.lsp")
(ex-all)
(sal-load "examples.sal")
(ex-global)
(print (setdir "../../test"))
(load "typecheck-fns.lsp")
(load "typechecks.lsp")
(setfn error original-error)
(setfn sal-print original-sal-print)
(setfn s-plot original-plot)
(setfn ny:error original-nyerror)
