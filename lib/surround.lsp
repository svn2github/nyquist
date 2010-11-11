;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SURROUND.LSP -- Implements Dolby Pro-Logic encoding
;                  and a 2D Sound Positioning Scheme
;;
;;     Dave Borel (dborel) with minor changes by
;;     Roger B. Dannenberg
;;
;;     Features:
;;       -Dolby Pro-Logic panning
;;       -Doppler for moving sounds
;;       -Distance attenuation
;;       -Atmospheric damping of high frequencies
;;       -Progagation delay
;;       -Test programs

(setf config 1) ;Distance between listener and speakers

;---------------------------------------------------
;                  Math Helpers
;---------------------------------------------------

;
; Distance between two points
;
(defun dist (x0 y0 x1 y1)
  (let* ( (rx (sum x1 (mult -1.0 x0)))
          (ry (sum y1 (mult -1.0 y0))) )
    (s-sqrt (sum (mult rx rx) (mult ry ry)))))

;
; Raise x to each sample of snd
;
(defun s-expt (x snd)
  (s-exp (mult (s-log x) snd)))


;---------------------------------------------------
;                Signal Processing
;---------------------------------------------------

;;
;; DOLBY HELPERS:
;;

;
; Smooth FFT Frame Iterator
; (Inspired by Dannenberg's fft example)
;
(defun raised-cosine ()
  (scale 0.5 
    (sum (const 1) 
         (lfo (/ 1.0 (get-duration 1)) 1 *sine-table* 270))))

(defun fft-window (frame-size)
  (control-srate-abs frame-size (raised-cosine)))

(setf fft-class (send class :new '(sound length skip window)))

(send fft-class :answer :next '() '(
    (snd-fft sound length skip window)))

(send fft-class :answer :isnew '(snd len skp) '(
    (setf sound snd)
    (setf length len)
    (setf skip skp)
    (setf window (fft-window len)) ))

(defun make-fft-iterator (sound length skip)
  (send fft-class :new (snd-copy sound) length skip))


;;
;; SPATIALIZATION HELPERS:
;;

;
; Doppler effect
;
(defun doppler (snd r)
  (let* ( (v (mult -1 (slope r)))
          (ratio (recip (sum 1 (mult v (recip 344.31)))))
          (map (integrate ratio)) )

    (sound-warp map snd) ))


;
; Distance-based low-pass filter
; (see report)
;
(defun absorb (snd r_m)
  (lp snd (mult 14763.67 (s-expt 0.97895 r_m))))


;
; Distance-based attenuation
; (see report)
;
(defun atten (snd r)

;  (let* ( (log2_r (mult (s-log r) (recip (log 10.0))))
;          (db_ratio (mult 20 log2_r))
;          (ratio (db-to-linear db_ratio)) )
;
;    (mult (clip ratio 1.0) snd)))
  (mult snd (clip (recip (mult r r)) 1)))

;
; Top-level spatializer
; sound source at (x,y)
; speaker at (xs, ys)
; assumes listener at (0,0)
;
; You could use this with
; configurations other than
; pro-logic (e.g. 6.1, 7.1, etc)
;
(defun stage (snd x y xs ys)
  (let* ( (r (dist x y 0 0))
          (rs (dist xs ys 0 0))
          (x_hat (mult x (recip r)))
          (y_hat (mult y (recip r)))
          (xs_hat (mult xs (recip rs)))
          (ys_hat (mult ys (recip rs)))

          (dot (sum (mult x_hat xs_hat) (mult y_hat ys_hat)))
          (overlap (mult 0.5 (sum dot (s-abs dot)))) )
    (mult overlap snd)))


;---------------------------------------------------
;                  Speaker Mixing
;---------------------------------------------------
;
; Dolby Pro-Logic Encoder
;
(defun prologic (left center right surround)
  (let* ( (c (scale 0.5 center))
          (s (highpass2 (lowpass2 (scale 0.5 surround) 7000) 100) )
          (slfe (scale 0.25 (lp surround 100)))
          (l (sim left  center (mult -1.0 s) slfe))
          (r (sim right center            s  slfe)) )
    (vector l r)))


;
; Direct-to-speaker playback
;
(defun pl_left   (snd) (let ((s (mult 0 snd))) (prologic snd s s s)))
(defun pl_center (snd) (let ((s (mult 0 snd))) (prologic s snd s s)))
(defun pl_right  (snd) (let ((s (mult 0 snd))) (prologic s s snd s)))
(defun pl_rear   (snd) (let ((s (mult 0 snd))) (prologic s s s snd)))
            
;
; Pans a sound across the surround speakers
; (no realistic imaging or attenuation)
; Works like pan but y specifies depth
(defun pan2d (s_in x y)
  (let ((snd (scale 0.5 s_in)))
    (prologic (mult snd (sum 1.0 (mult -1.0 x)
                             (sum 1 (mult -1.0 y))) );left
              (mult snd 0.0)                         ;center(null)
              (mult snd x (sum 1.0 (mult -1.0 y) ))  ;right
              (mult snd y 0.5))))                    ;rear

;
; Position a sound in the 2D soundstage
; Includes spatialization effects
; 
; (x,y) may be (flonum, flonum) or (behavior, behavior)
;
(defun position (s_in x y config)
  (let* ( (r_m (dist x y 0 0))
          (r (mult r_m (recip config)))
          (spd_snd (/ 344.31 config))
          (offset (if (soundp r_m)
                      (/ (aref (snd-samples r 1) 0) spd_snd)
                      (/ r spd_snd)))
          (snd (seq (s-rest offset)
                    (if (soundp r_m)
                        (atten (absorb (doppler s_in r_m) r_m) r)
                        (atten (absorb s_in r_m) r)))) )
    
    ; Two Notes:
    ; 1.) The center channel is automatically imaged correctly
    ;     because sounds placed between the left-and-right channels
    ;     distribute linearly between the two channels.
    ;
    ; 2.) Although the below settings assume that all speakers are
    ;     equidistant from the listener, you can easily assume a
    ;     different layout by modifying the xs and ys values in
    ;     each channel's call to the stage function.
    ;
    (prologic (stage snd x y -.1913  -.4619)       ;left
              (scale 0.0 snd)                        ;center (null)
              (stage snd x y  .1913  -.4619)       ;right
              (stage snd x y 0.0     .5    ))))    ;rear


;---------------------------------------------------
;                   Diagnostics
;---------------------------------------------------

;
; Pro-Logic Channel Test Tones
;
(defun pl_test ()
  (play (prologic
         (                   osc-note a3 )
         (seq (s-rest 1.25) (osc-note b3))
         (seq (s-rest 2.5 ) (osc-note c4))
         (seq (s-rest 3.75) (osc-note d4)) )))


;
; Pan Test
;
(defun pan_test ()
  (play (pan2d
         (seq
          (s-rest .25) (osc a3 .75)
          (s-rest .25) (osc b3 .75)
          (s-rest .25) (osc c4 .75)
          (s-rest .25) (osc d4 .75))

         (pwl
          0    0
          0.99 0

          1    1
          2.99 1

          3    0
          4    0
          4    )

         (pwl
          0    0
          1.99 0

          2    1 
          4    1
          4    ))))


;
; Doppler test
;
(defun dop ()
  (play (doppler (osc c4 10) (pwl .25 0 .5 100 .75 100 1.0))))

;
; Attenuation test
;
(defun att ()
  (play (atten (osc-note c4 4) 
               (pwl 0 2
                    1 2 
                    2 100
                    3 100
                    4 2 
                    4 ))))


;
; Doppler positioning test (ambulance)
;
(defun ambulance ()
  (play (scale 0.2
               (position

                (stretch 16 (fmosc c4 (mult 1000 (lfo 2))))
                
                (pwl
                 0  -20
                 8   20
                 8   )
                
                (pwl
                 0    0
                 8    )

                config))))


;
; Position test
;

; Make a sound orbit the listener
(defun orbit_x (r t times)
  (let (k)
    (seqrep (k times)
            (pwl
             0.0               0.0
             (/ t 4)           r
             (/ t 2)           0.0
             (/ (* t 3) 4)     (* -1 r)
             t                 0.0
             t                 ) )))
(defun orbit_y (r t times)
  (let (k)
    (seqrep (k times)
            (pwl
             0.0               (* -1 r)
             (/ t 4.0)         0.0
             (/ t 2.0)         r
             (/ (* t 3.0)4.0)  0.0
             t                 (* -1 r)
             t                 ) )))
(defun orbit (snd r t times)
  (position snd
            (orbit_x r t times)
            (orbit_y r t times)
            config))


; Play some tones
(defun pos_1 ()
  (play (position
         (seq
          (s-rest .125) (osc a3 1.75)
          (s-rest .25)  (osc b3 1.75)
          (s-rest .25)  (osc c4 1.75)
          (s-rest .25)  (osc d4 1.75) (s-rest .125))

         (pwl
          0    -5
          1    5
          2    5
          3    -5
          4    -5
          5    5
          6    5
          7    -5
          8    -5
          8    )

         (pwl
          0    -5
          1    -5
          2    5
          3    5
          4    -5
          5    -5
          6    5
          7    5
          8    -5
          8    )

         config)))

(defun pos_2 ()
  (play (seq
         (orbit (seq
                (s-rest .125) (osc a3 1.75)
                (s-rest .25)  (osc b3 1.75)
                (s-rest .25)  (osc c4 1.75)
                (s-rest .25)  (osc d4 1.75) (s-rest .125))

               5 8 1)
         (orbit (seq
                (s-rest .125) (osc a3 1.75)
                (s-rest .25)  (osc b3 1.75)
                (s-rest .25)  (osc c4 1.75)
                (s-rest .25)  (osc d4 1.75) (s-rest .125))

               5 8 1))))
