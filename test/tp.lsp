;;; Single Carriar Trumpet Algorithm based on a design by Dexter Morrill
;;; Computer Music Journal, February 1997, pgs 46-52
;;;
;;; Naming changes: (Morill's name = my name)
;;;  u.g.1 = envelope       p2 -> ignored           p13 = ***
;;;  u.g.2 = cf_deviation   p3 -> ignored           p14 = *** 
;;;  u.g.3 = vibrato        p4 = freq (pitch)       p15 = modIndex1
;;;  u.g.4 = fm_env         p5 = peak_amp (scaled)  p16 = modIndex2
;;;  u.g.5 = random         p6 -> ignored           p17 = ***
;;;  u.g.6 = fmod           p7 = amp_rise           p18 = ***
;;;  u.g.7 = main           p8 = amp_decay          p19 = fm_rise
;;;                         p9 = ***                p20 = fm_decay
;;;                         p10 = cfd_rise          p21 = ***
;;;                         p11 = cfd_decay         p22 = ***
;;;                         p12 -> ignored          p23 -> ignored

;;; NOTES:
;;;  P9 has been completely ignored!!!
;;;  Look at p13 and p14

(defun trumpet (pitch &key (peak_amp (/ 500.0 2048.0))
                      (amp_rise .02) (amp_decay .15)
                      (cfd_rise .06) (cfd_decay .06)
                      (modIndex1 3.523) (modIndex2 0.0)
                      (vib_dev .33)
                      (fm_rise .02) (fm_decay .01))
  (let* ((freq (float (step-to-hz pitch)))
         (p4 freq)
         (p13 freq)
         (p14 1.0)
         (p18 (hz-to-step 7))
         (p21 0.5)
         (p22 (/ freq 4))

         ;; main envelope
         (envelope (mult peak_amp (env amp_rise 0 amp_decay 1 1 .9)))
                                        ; 1,1,.9 need to be parameters?

         ;; center frequency deviation
         (my_dur (local-to-global 1))
         (cf_deviation (stretch-abs 1 (pwl 0 -1 cfd_rise .1
                                           (+ cfd_rise cfd_decay)
                                           0 my_dur 0)))

         (m1_f (sum p4 cf_deviation))
         
         ;; vibrato generator
         (vibrato (mult (mult m1_f (* vib_dev .01)) (osc p18)))

         (n1 (sum (* p13 p14) cf_deviation))

         ;; envelope for fmod
;;;  WARNING:  This generator needs to be scaled by 1/2048 ???
         (fm_env (mult (sum (mult modIndex1 n1) (mult modIndex2 n1))
                    (env fm_rise 0 fm_decay 1 1 .9)))

         ;; random frequency modulation
         (random (mult (mult (sum m1_f vibrato) (* p21 .01))
                    (sound-srate-abs p22 (noise))))

         ;; frequency modulation
         (fmod_pitch (hz-to-step (* p13 p14)))
         (fmod_mod (sum (sum cf_deviation vibrato) random))
         (fmod_amp (sum (sum (sum fm_env (mult n1 modIndex2)) vibrato) random))
         (fmod (mult fmod_amp (fmosc fmod_pitch fmod_mod)))

         ;; main generator
         (main_mod (sum(sum (sum cf_deviation vibrato) random) fmod))
         (main (mult envelope (fmosc pitch main_mod))))

    main))
