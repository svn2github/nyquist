;;; ADDITIVE SYNTHESIS
;;; Risset Bell
;;; coded by Pedro Jose Morales
;;; pmorales@iele-ab.uclm.es

(load "pjmg.lsp")

(defun bell-partial (amp dur frq)
  (amosc (hz-to-step frq) (pwev amp dur (* amp 12e-5))))

(defun risset-bell (amp dur frq)
  (sim
    (bell-partial amp dur (* frq .56))
    (bell-partial (* amp .67) (* dur .9) (+ (* frq .56) 1))
    (bell-partial (* amp 1.35) (* dur .65) (* frq .92))
    (bell-partial (* amp 1.8) (* dur .55) (+ (* frq .92) 1.7))
    (bell-partial (* amp 2.67) (* dur .325) (* frq 1.19))
    (bell-partial (* amp 1.67) (* dur .35) (* frq 1.7))
    (bell-partial (* amp 1.46) (* dur .25) (* frq 2.0))
    (bell-partial (* amp 1.33) (* dur .2) (* frq 2.74))
    (bell-partial (* amp 1.33) (* dur .15) (* frq 3.0))
    (bell-partial amp (* dur .1) (* frq 3.76))
    (bell-partial (* amp 1.33) (* dur .075) (* frq 4.07))))


(defun m ()
  (sim (at 0.0 (risset-bell 1.0 4.0 999.0))
       (at 2.0 (risset-bell 1.0 4.0 633.0))
       (at 4.0 (risset-bell 1.0 4.0 211.0))
       (at 6.0 (risset-bell 1.0 4.0 999.0))
       (at 8.0 (risset-bell 0.7 20.0 633.0))
       (at 10.0 (risset-bell 0.7 20.0 211.0))
       (at 12.0 (risset-bell 0.7 20.0 999.0))
       (at 14.0 (risset-bell 0.7 20.0 80.0))))

(ss (m))
  


