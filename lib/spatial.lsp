; SPATIAL.LSP 
; created by Adam Hartman and Roger B. Dannenberg
; 2005
; stereo manipulation and spatialization functions

; EMPHIGH -- use four equalizer bands to emphasize
;    the higher frequencies in an input sound
;
(defun emphigh (base) 
  (eq-band 
   (eq-band 
    (eq-band 
     (eq-band base 31 -3 1)
                  62 -3 1)
                8000 3 1)
              16000 3 1))

; EMPLOW -- use four equalizer bands to emphasize
;    the lower frequencies in an input sound
;
(defun emplow (base)
  (eq-band
   (eq-band
    (eq-band
     (eq-band base 31 3 1)
                  62 3 1)
              8000 -3 1)
            16000 -3 1))

; LEFTIN -- apply low frequency emphasis to a sound
(defun leftin (inl) (emplow inl))

; RIGHTIN - apply high frequency emphasis and
;    a very slight delay to a sound
;
(defun rightin (inr) (seq (s-rest 0.02) (emphigh inr)))

; STEREOIZE -- create a stereo sound from a monaural source
;
(defun stereoize (monoin) 
  (vector (leftin monoin) (rightin monoin)))

; EXTRACTLEFT -- extract the left channel of a stereo sound
(defun extractleft (inl) (aref inl 0))

; EXTRACTRIGHT -- extract the right channel of a stereo sound
(defun extractright (inr) (aref inr 1))


; WSUM -- weighted sum of two monaural sounds
;
;   inl: first monaural sound
;   inr: second monaural sound
;   amtl: multiplier for the first monaural sound
;   amtr: multiplier for the second monaural sound
;
(defun wsum (inl inr amtl amtr)
  (sum (mult inl amtl) (mult inr amtr)))


; SMIXER -- remix a stereo signal
;
;   in: original stereo sound
;   lamtl: amount in new left channel from the original left channel
;   lamtr: amount in new left channel from the original right channel
;   ramtl: amount in new right channel from the original left channel
;   ramtr: amount in new right channel from the original right channel
;  Note: lamtl, lamtr, ramtl, ramtr should have values in the 
;  range of -1 to 1 and may be static numbers or sounds
;
(defun smixer (in lamtl lamtr ramtl ramtr)
   (let ((eleft (extractleft in)) (eright (extractright in)))
    (vector (wsum eleft eright lamtl lamtr) 
	    (wsum eleft eright ramtl ramtr))))


; WIDEN -- widen the field of a stereo sound
;
;   in: original stereo sound
;   amt: a value between 0 and 1 which represents a widening factor
;      0 will leave the sound unchanged while 1 indicates the widest
;      possible stereo field
; Note: amt may be a static number or a sound
;
(defun widen (in amt) 
   (let ((widenamt (mult -1 amt)))
    (smixer in 1 widenamt widenamt 1)))

; SPAN -- pan the virtual center channel of a stereo sound
;
;   in: original stereo sound
;   amt: a value between 0 and 1 which represents the panning location
;        0 pans the center channel all the way to the left while 1 pans
;        it all the way to the right
; Note: amt may be a static number or a sound
;
(defun span (in amt)
   (let ((leftc (sum 0.5 (mult -1 amt))) (rightc (sum -0.5 amt)))
    (smixer in 0.5 leftc rightc 0.5)))

; SWAPCHANNELS -- swap the two channels in a stereo sound
(defun swapchannels (in) (vector (aref in 1) (aref in 0)))

#| NOTE: there's nothing wrong with the code that is commented out here.
These functions were in the original library, but I have commented them
out because they are very simple and not very general. Perhaps they
can be incorporated in an expanded form in a future version of Nyquist.
For example, some general 3-D positioning with Doppler effects, etc., 
and some more elaborate HRTF code would be very interesting. Feel free
to give these a try. -RBD

; IID -- position a monaural sound source by attenuating the volume
; of the sound at each ear point based on the distance between the
; two ear points and the distance of the sound source from the listener
;
;   in: monaural source sound
;   dist: lateral distance of the sound source from the listener in meters
;   headwidth: width of the listener's head (i.e. the distance between
;     the two ears) in meters
;   rorl: a value of either 0 or 1 which represents whether the sound 
;     source is to the left or to the right of the listener 
;
(defun iid (in dist headWidth RorL)
  (let ((nearmult (/ 1.0 (mult dist dist)))
        (farmult (/ 1.0 (mult (sum dist headWidth) 
			      (sum dist headWidth)))))
  (if (eq rorl 0)
    ; sound source is to the left of listener 
    (vector (mult in nearmult) (mult in farmult))
    ; sound source is to the right of listener
    (vector (mult in farmult) (mult in nearmult)))))

; ITD -- position a monaural sound source by delaying the arrival
;        of the sound at each ear point based on the distance 
;        between the two ear points and the distance of the sound
;        source from the listener
;  in: monaural source sound
;  dist: lateral distance of the sound source from the listener in meters
;  headwidth: width of the listener's head (i.e. the distance
;    between the two ears) in meters
;  rorl: a value of either 0 or 1 which represents whether the sound
;    source is to the left or to the right of the listener
;
(defun itd (in dist headWidth RorL)
  (let ((neardel (mult 0.0029387 dist))
        (fardel (mult 0.0029387 (sum dist headWidth))))
  (if (eq rorl 0)
    ; sound source is to the left of listener
    (vector (seq (s-rest neardel) in ) (seq (s-rest fardel) in))
    ; sound source is to the right of listener
    (vector (seq (s-rest fardel) in) (seq (s-rest neardel) in))))) 


; CFSPATIALIZATION -- a spatialization effect based on a cross-feed network
;
(defun cfspatialization (in)
  (let ((shadowLeft (lp (seq (s-rest 0.0004) (aref in 0)) 265))
        (shadowRight (lp (seq (s-rest 0.0004) (aref in 1)) 265)))
  (vector (sum (aref in 0) shadowRight) (sum (aref in 1) shadowLeft))))

; CUSTBP -- a helper function that creates a custom bandpass filter
;           for use in the hrtfapprox function
(defun custbp (in) (lp (sum (hp in 4980) (mult in 0.75)) 7900))

; HRTFAPPROX -- a spatialization effect based on an approximated HRTF
;
(defun hrtfapprox (in)
  (let ((filteredLeft (seq (s-rest 0.00025) (custbp (aref in 0))))
        (filteredRight (seq (s-rest 0.00025) (custbp (aref in 1)))))
  (vector (sum (aref in 0) (lp filteredRight 10200)) 
	  (sum (aref in 0) (lp filteredLeft 1020)))))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dolby Pro-Logic encoding and a 
;; 2D Sound Positioning Scheme
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


;;
;; SPATIALIZATION HELPERS:
;;

;
; Doppler effect
;
(defun pl-doppler (snd r)
  (let* ( (v (mult -1 (slope r)))
          (ratio (recip (sum 1 (mult v (recip 344.31)))))
          (map (integrate ratio)) )

    (sound-warp map snd) ))


;
; Distance-based low-pass filter
; (see report)
;
(defun absorb (snd r-m)
  (lp snd (mult 14763.67 (s-expt 0.97895 r-m))))


;
; Distance-based attenuation
; (see report)
;
(defun atten (snd r)

;  (let* ( (log2-r (mult (s-log r) (recip (log 10.0))))
;          (db-ratio (mult 20 log2-r))
;          (ratio (db-to-linear db-ratio)) )
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
          (x-hat (mult x (recip r)))
          (y-hat (mult y (recip r)))
          (xs-hat (mult xs (recip rs)))
          (ys-hat (mult ys (recip rs)))

          (dot (sum (mult x-hat xs-hat) (mult y-hat ys-hat)))
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
(defun pl-left   (snd) (let ((s (mult 0 snd))) (prologic snd s s s)))
(defun pl-center (snd) (let ((s (mult 0 snd))) (prologic s snd s s)))
(defun pl-right  (snd) (let ((s (mult 0 snd))) (prologic s s snd s)))
(defun pl-rear   (snd) (let ((s (mult 0 snd))) (prologic s s s snd)))
            
;
; Pans a sound across the surround speakers
; (no realistic imaging or attenuation)
; Works like pan but y specifies depth
(defun pl-pan2d (s-in x y)
  (let ((snd (scale 0.5 s-in)))
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
(defun pl-position (s-in x y config)
  (let* ( (r-m (dist x y 0 0))
          (r (mult r-m (recip config)))
          (spd-snd (/ 344.31 config))
          (offset (if (soundp r-m)
                      (/ (aref (snd-samples r 1) 0) spd-snd)
                      (/ r spd-snd)))
          (snd (seq (s-rest offset)
                    (if (soundp r-m)
                        (atten (absorb (pl-doppler s-in r-m) r-m) r)
                        (atten (absorb s-in r-m) r)))) )
    
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
(defun pl-test ()
  (play (prologic
         (                   osc-note a3 )
         (seq (s-rest 1.25) (osc-note b3))
         (seq (s-rest 2.5 ) (osc-note c4))
         (seq (s-rest 3.75) (osc-note d4)) )))


;
; Pan Test
;
(defun pan-test ()
  (play (pl-pan2d
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
  (play (pl-doppler (osc c4 10) (pwl .25 0 .5 100 .75 100 1.0))))

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
               (pl-position

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
(defun orbit-x (r t times)
  (let (k)
    (seqrep (k times)
            (pwl
             0.0               0.0
             (/ t 4)           r
             (/ t 2)           0.0
             (/ (* t 3) 4)     (* -1 r)
             t                 0.0
             t                 ) )))
(defun orbit-y (r t times)
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
  (pl-position snd
            (orbit-x r t times)
            (orbit-y r t times)
            config))


; Play some tones
(defun pos-1 ()
  (play (pl-position
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

(defun pos-2 ()
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
