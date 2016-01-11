;; ================================================
;; Show Program Information
;; ================================================
(princ "\n\nPiano Synthesizer V1.2 (Feb 2004)\n")
(princ "    Original algorithm and program by Zheng (Geoffrey) Hua\n")
(princ "    and Jim Beauchamp, University of Illinois. Any publication\n")
(princ "    or notes on any composition that utilizes this software\n")
(princ "    should credit the original creators. Any software based on\n")
(princ "    this algorithm should carry a similar notice and restriction.\n")
(princ "    Ported to Nyquist from source code in M4C program by\n")
(princ "    Ning Hu and Roger Dannenberg, Carnegie Mellon University\n")
(princ "    School of Computer Science\n\n")
(princ "    Program Initializing...\n")

(setf *pianosyn-path* (current-path))

;; ================================================
;; Function definition
;; ================================================
(defun readdat (filename dim data)
  (setf filename (strcat *pianosyn-path* "piano" 
                         (string *file-separator*) filename))
  (setq fp (open-binary filename :direction :input))
  (dotimes (count 4) 
    (setf (aref dim count) (read-int fp)))
  (dotimes (count (aref dim 3))
    (setf (aref data count) (read-float fp)))
  (close fp))

(defun build-harmonic-phase (n phase size)
  (sound-srate-abs size (osc (hz-to-step n) 1 *sine-table* phase)))


;; ******************************************
;; *	Build envelope                      *
;; ******************************************

; after the initial envelope, which is stored in a table,
; envelopes are extended by splicing together final segments
; of the real envelope. The final segment is approximately
; exponential, and so each copy of the segments that is
; spliced is scaled by the amount of decay during the segment.
; The long term shape will therefore be exactly exponential, but
; we thought that a bit of variation rather than a perfectly 
; smooth exponential decay might be better.
;
; This function takes a segment, the amount of decay in the
; segment (the scale factor for the next segment) and a count
; and builds an envelope
;
(defun decay-env (segment decay count)
  (cond ((<= count 1) (cue segment))
        (t (seq (cue segment) 
                (scale decay (decay-env segment decay (1- count)))))))

; PIANO-ENVELOPE builds the amplitude envelope for a group of partials.
; igroup is the index of the group
; sc-duration is the score duration
; attack is the sampled portion of the envelope with a duration of 
;     gmagendtime
; seg-array is the repeating portion of the envelope tacked onto 
;     attack to make the envelope longer. The duration of a segment
;     in seg-array is gmagendtimemini
; the amount by which seg-array[igroup] decays is scalemag1[igroup]
;
; Algorithm:
;   figure out how many repetitions of the seq-array[igroup] to
;   add onto the attack to make the envelope long enough. Multiply
;   by an exponential decay starting at the duration -- effectively
;   the damper hits the string at sc-duration.
;
(defun piano-envelope (igroup sc-duration gmagendtime gmagendtimemini 
                       attack seg-array scalegmag1)
  (let ((decaycount (1+ (truncate (/ (- (+ sc-duration endingtime) gmagendtime)
                                     gmagendtimemini))))
        pianoenv )
  
    (setf pianoenv 
          (sim (at 0 (cue attack))
               (at gmagendtime (decay-env (aref seg-array igroup) 
                                          (aref scalegmag1 igroup) 
                                          decaycount))))
    ;; For ending time 
    (mult (scale (aref scale1 igroup) pianoenv) 
          (pwlv 1 sc-duration
		; decay to 1/1000: about 60dB
                1 (+ sc-duration endingtime) 0.001))))
  
 
;; ******************************************
;; *	Build wavetable                     *
;; ******************************************  
(defun piano-group (jgroup sc-duration freq table)
  (sound-srate-abs *piano-srate*
    (osc (hz-to-step freq) sc-duration 
         (aref table jgroup))))         

;; ******************************************
;; *	Produce single piano note           *
;; ******************************************
(defun piano-note (duration pitch dynamic)
  (let ((ioi (get-duration duration))
        (full-pitch (+ (get-transpose) pitch))
        (full-dynamic (+ (get-loud) dynamic))
        ;; note: the "loud" is nominally in dB, but
        ;; piano-note-abs uses something akin to midi velocity
        ;; we should probably work out a better conversion
        (start-time (local-to-global 0))
        on-dur)
    (setf on-dur (* ioi (get-sustain)))
    (set-logical-stop
      (abs-env (at start-time 
                   (piano-note-abs on-dur full-pitch full-dynamic)))
      ioi)))


;; PIANO-NOTE-ABS -- private function to do the work; assumes 
;;    stretch factor of 1, etc.
(defun piano-note-abs (sc-duration sc-pitch sc-dynamic)
  (let (attnamp freq key whichone whichone1 ngroup1 ngroup2 dyna smax
        dur gmagendtime gmagendtimemini k j envpoint)
    ;; ******************************************
    ;; *	Initilization for each note         *
    ;; ******************************************
    (setq attnamp 0.03) 
    
    ; key is midi pitch number 
    (setq key (truncate (+ sc-pitch 0.000001)))
    (cond ((< key 21) ;; 21 is A0, lowest pitch on this piano
	   (break "piano-note-abs pitch is too low" sc-pitch)
	   ;; continued -- transpose up to lowest octave
	   (while (< key 21)
             (setf sc-pitch (+ sc-pitch 12))
	     (setf key (truncate (+ sc-pitch 0.000001)))))
          ((> key 108) ;; 108 is c9, highest pitch on this piano
	   (break "piano-note-abs pitch is too high" sc-pitch)
	   ;; continued -- transpose down to highest octave
           (while (> key 108)
	     (setf sc-pitch (- sc-pitch 12))
	     (setf key (truncate (+ sc-pitch 0.000001))))))
    (setq freq (step-to-hz sc-pitch))
	   
    (setq whichone -2)
    (dotimes (i GROUPCON)  
        (if (and (= whichone -2)
                 (< freq (- (aref fa i) 0.001)))
            (setq whichone (- i 1))))
    ;; Have to use (- (aref fa i) 0.001) because of the calculation precision of Nyquist
           
    (setq whichone1 (1+ whichone))
    (setq ngroup2 (aref ngroup whichone1))

    (setq dyna (truncate sc-dynamic))
    (setq smax 0.25)
        
    ; (setq attnpretime (/ (+ (* 0.018 dyna dyna) (* -3.9588 dyna) 244.8139) 1000.0))
    (setq dur (aref durtab (+ (* key 128) dyna)))
    (setq ngroup1 (aref ngroup whichone))
    (setq gmagendtime (* (nth whichone hkframe) (aref dt whichone))) 
    (setq gmagendtimemini (* (aref nptsmini whichone) (aref dtmini whichone))) 

    (setq k (* (aref gmaxtabdim 1) (aref gmaxtabdim 2)))
    (setq j (+ (* whichone k) (* dyna (aref gmaxtabdim 2))))
    (dotimes (i (aref gmaxtabdim 2))
      (setf (aref gmax1 i) (aref gmaxtab j))
      (incf j))

    (dotimes (i ngroup1)
      (setq envpoint (sref (aref (aref gmagmini whichone) i) 0))
      (if (/= envpoint 0) 
          (setf (aref scalegmag1 i) 
                (/ (sref (aref (aref gmagmini  whichone) i)
                         (- gmagendtimemini (aref dtmini whichone)))
                   envpoint))        
          (setf (aref scalegmag1 i) 0.0))
      (setf (aref scale1 i) (* smax (aref gmax1 i))))
    (if (> ngroup2 ngroup1) (setf ngroup2 ngroup1))

    (if (< dur sc-duration) (setq sc-duration dur))
    ;; **********************
    ;; * now sum the groups *
    ;; **********************      
    (scale 0.5 
        (sim (at 0 (set-logical-stop (cue (scale attnamp attsound)) sc-duration))
            (at 0 (cue (simrep (i ngroup2)
                            (mult (piano-envelope i sc-duration gmagendtime 
                                   gmagendtimemini (aref (aref gmag whichone) i) 
                                   (aref gmagmini whichone) scalegmag1)
                                  (piano-group i (+ sc-duration endingtime) freq 
                                   (aref wavetab whichone1)))))))) ))
;;;;; This is for debugging -- replace synthesis with a sine tone to study envelope
;             (at 0 (cue (mult (piano-envelope 0 sc-duration gmagendtime 
;                                    gmagendtimemini (aref (aref gmag whichone) 0) 
;                                    (aref gmagmini whichone) scalegmag1)
;                              (osc c4 2.0))))))))

       
(defun piano-note-2 (sc-pitch sc-dynamic)
  (let ((dur (get-duration 1)))
    (stretch-abs 1 (piano-note dur sc-pitch sc-dynamic))))


(defun piano-midi (midiin)
  (let (midi-seq midifile)
    (setf midi-seq (seq-create))
    (setf midifile (open-binary midiin))
    (seq-read-smf midi-seq midifile)
    (close midifile)
    (seq-midi midi-seq
        (note (channel pitch velocity)
          (piano-note-2 pitch velocity)))))


;; ******************************************
;; *Produce wave file according to MIDI file*
;; ******************************************
(defun piano-midi2file (midiin out-name)
    (princ "\nBegin sound production\n")
    (princ "=============================================\n")
    (s-save (piano-midi midiin) 
            ny:all (string out-name) :play T) 
    (princ "=============================================\n")
    (princ "End sound production\n"))
 

;; ====================================
;; Main Program       
;; ====================================
(if (not (boundp '*piano-srate*)) ;; if pianosyn.lsp wasn't loaded already
    (expand 70)) ;; we'll allocate a lot of nodes for data, so expand now
(setf *pianosyn-save-gc-flag* *gc-flag*)
(setf *gc-flag* nil) ;; we'll do a lot of gc, so turn off messages
;; Definite some constant
(setq NPITCH 22 GROUPCON 23)
(setq MAXAMP 32767.0)
(setq TWOPI (+ pi pi))
(setq *piano-srate* *default-sound-srate*)
(setq bits 32)
;; 512 gives pretty good SNR for interpolated sines
;; some tables will be larger: 512 is just the minimum
(setq tabsize 512)

;; For ending time, use 30 msec. (This was originally 0.1 msec, 
;;    about 4 samples, but that's too short to avoid clicks.)
;;    This not only must avoid clicks but it simulates the damper.
;;    This is the time to decay to 0.001 of the original, so it's
;;    actually quite a rapid decay.
(setq endingtime 0.03)

(setf hkframe (list 66 73 82 90 99 108 116 123 130 135 138 140 138 
                    133 126 117 107 102 105 127 153 187 200))
(setf attsratelist (list 8000 11025 16000 22050 32000 44100 48000))                     
(setf gmax1 (make-array GROUPCON))
(setf scalegmag1 (make-array GROUPCON))
(setf scale1 (make-array GROUPCON))
(setf wavetab (make-array GROUPCON))
(setf ti (make-array GROUPCON))
(setf tstep (make-array GROUPCON))
(setf gmaxtabdim (make-array 4))
(setf gmaxtab (make-array 64768))
(setf durtabdim (make-array 4))
(setf durtab (make-array 16384))
(setf rlsratetabdim (make-array 4))
(setf rlsratetab (make-array 11392))
(setf fa (make-array GROUPCON))
(setf dt (make-array GROUPCON))
(setf ngroup (make-array GROUPCON))
(setf npts (make-array GROUPCON))
(setf gmag (make-array GROUPCON))
(setf nhar (make-array GROUPCON))

(setf gmagmini (make-array GROUPCON))
(setf dtmini (make-array GROUPCON))
(setf nptsmini (make-array GROUPCON))

(setf cw (make-array GROUPCON))
(setf phase (make-array GROUPCON))
(setf hfrom (make-array GROUPCON))
(setf hto (make-array GROUPCON))

(setf *zero-table* (scale 0 (build-harmonic 1 tabsize)))

;; =================================================
;; run-once initilization: pianoActor construction  
;; =================================================  
(princ "\nBegin Instrument-wise initialization...\n")
(princ "=======================================\n")
(princ "Reading source files:\n")


;; Read gmax.tab
(readdat "gmax.tab" gmaxtabdim gmaxtab)

;; Read dur.tab
(readdat "dur.tab" durtabdim durtab)

;; Read rlsrate.tab
(readdat "rlsrate.tab" rlsratetabdim rlsratetab)

;; Read cwxx.cwd
(dotimes (pncount GROUPCON)
  (format t "~A " pncount)
  (setq filename (strcat "pn" 
                         (string (int-char (+ (truncate (/ pncount 10)) 48)))
                         (string (int-char (+ (rem pncount 10) 48)))
                         ".cod"))
  (setq filename (strcat *pianosyn-path* "piano"
                         (string *file-separator*) filename))
  (setq fp (open-binary filename))

  ;; Read cwdHdr in cwxx.cwd
  (setq cwdHdr-ckID (read-int fp) cwdHdr-type (read-int fp))  
  ;; "CNDN" == 1129202766

  ;; That is for "FORM"==cwdHdr-ckID
  ;;(if (and (= cwdHdr-ckID 1179603533) (= cwdHdr-type 1129202766))
  ;;    ()
  ;;    (error "Error in reading chunk header."))  

  ;;That is for "SYNC"==cwdHdr-ckID
  (if (and (= cwdHdr-ckID 1398361667) (= cwdHdr-type 1129202766))
      () 
      (error "Error in reading chunk header."))

  ;; Read COMMCK in cwxx.cwd
  (setq COMMCK-ckID (read-int fp))
  (if (= COMMCK-ckID 1129270605) () (error "COMMCK chunk not found."))
  (setq COMMCK-fa (read-float fp) COMMCK-dt (read-float fp))
  (setf (aref fa pncount) COMMCK-fa)
  (setf (aref dt pncount) COMMCK-dt)
  (setf (aref dtmini pncount) (* 10 COMMCK-dt))
  (setq COMMCK-npts (read-int fp) COMMCK-ngroup (read-int fp))
  (setf (aref npts pncount) COMMCK-npts)
  (setf (aref nptsmini pncount) 
        (truncate (/ (+ 9 (- COMMCK-npts (nth pncount hkframe))) 10)))
  (setf (aref ngroup pncount) COMMCK-ngroup)

  ;; Read DATACK in cwxx.cwd
  (setq DATACK-ckID (read-int fp))  
  (if (= DATACK-ckID 1346458196) () (error "DATACK chunk not found."))
  (setf (aref nhar pncount) (read-int fp))
  (setf (aref cw pncount) (make-array (aref nhar pncount)))
  (setf (aref phase pncount) (make-array (aref nhar pncount)))
  (dotimes (count (aref nhar pncount)) 
           (setf (aref (aref cw pncount) count) (setf rffp (read-float fp)))
           (display "read cw" count rffp))
  (dotimes (count (aref nhar pncount)) 
           (setf (aref (aref phase pncount) count) (read-float fp)))

  ;; Read GRUPCK in cwxx.cwd
  (setq GRUPCK-ckID (read-int fp))  
  (if (= GRUPCK-ckID 1196578128) () (error "GRUPCK chunk not found."))
  (setf (aref hfrom pncount) (make-array (aref ngroup pncount)))
  (setf (aref hto pncount) (make-array (aref ngroup pncount)))  
  ;(display "reading grupck" (aref ngroup pncount) (aref nhar pncount) pncount)
  (dotimes (count (aref ngroup pncount))   
    (setf (aref (aref hfrom pncount) count)
	  (read-float fp)))
  (dotimes (count (aref ngroup pncount)) 
    (setf (aref (aref hto pncount) count) (read-float fp)))

  ;; Read GMAGCK in cwxx.cwd
  (setq GMAGCK-ckID (read-int fp))  
  (if (= GMAGCK-ckID 1196245319) 
      ()
      (error "GMAGCK chunk not found."))
  (setq gmaghead (read-int fp))
  (close fp)
  (setf (aref gmag pncount) (make-array (aref ngroup pncount)))
  (setq gmagrate (/ 1 (aref dt pncount)))  
  (setq gmagdur (/ (nth pncount hkframe) gmagrate))

  ; (display "gmagmini" pncount (aref ngroup pncount))
  (setf (aref gmagmini pncount) (make-array (aref ngroup pncount)))  
  (setq gmagratemini (/ 1 (aref dtmini pncount)))
  (setq gmagdurmini (/ (aref nptsmini pncount) gmagratemini))

  (dotimes (i (aref ngroup pncount))
    (let (gmaghead1 samps gmaghead1mini)
      (setf gmaghead1 (/ (float gmaghead) (* gmagrate (/ bits 8))))
      ;(display "gmag read" i gmaghead1 gmagrate filename)
      (setf samps (s-read filename :time-offset gmaghead1 :srate gmagrate 
                  :dur gmagdur :mode snd-mode-float 
                  :format snd-head-raw :bits bits :endian :big))
      (if samps (snd-length samps ny:all)) ; force read into memory
      (setf (aref (aref gmag pncount) i) samps)
      (setq gmaghead (+ gmaghead (* 4 (nth pncount hkframe))))
      (setq gmaghead1mini (/ (float gmaghead) (* gmagratemini (/ bits 8))))  
      ;(display "gmag read mini" i gmaghead1mini gmagratemini filename)
      (setf samps (s-read filename :time-offset gmaghead1mini :srate gmagratemini 
            :dur gmagdurmini :mode snd-mode-float :format snd-head-raw
            :bits bits :endian :big))
      (if samps (snd-length samps ny:all)) ; force read into memory
      ;(display "read gmagmini" filename pncount i
      ;         (if samps (snd-length samps ny:all)))
      (setf (aref (aref gmagmini pncount) i) samps)
      (setq gmaghead (+ gmaghead (* 4 (aref nptsmini pncount))))
    ))
)

(setq maxfreq (aref fa (1- GROUPCON)))
(dotimes (i GROUPCON) 
  (setq ngrouptemp -1)
  (dotimes (j (aref ngroup i))
    (if (and (= ngrouptemp -1) 
             (>= (* (aref (aref hto i) j) (aref fa i)) 
                 (/ *piano-srate* 2)))
        (setq ngrouptemp j)))  
  (if (>= ngrouptemp 0) (setf (aref ngroup i) ngrouptemp)))

(princ "\nGenerating wavetables...\n")
(setq tempi (/ (* 360 tabsize) (* TWOPI TWOPI)))
(dotimes (h GROUPCON)    
  (setf (aref wavetab h) (make-array (aref ngroup h)))
  (dotimes (i (aref ngroup h))
    ;(FORMAT T "WAVE ~A OF GROUP ~A~%" i h)
    (let ((low (aref (aref hfrom h) i))
          (high (aref (aref hto h) i))
          tempphase tempcw
          (len tabsize))
      ; table size must be more than twice greatest harmonic number
      ; use a factor of three so we have a wider margin of oversampling
      (setf len (max len (* 3 high)))
      (setf sumwave *zero-table*)  
      (do ((k (truncate low) (incf k)))
           ((> k high))
         (cond ((< k (aref nhar h))
                (setq tempphase (aref (aref phase h) k))
                (setq tempcw (aref (aref cw h) k)))
               (t
                (setq tempphase 0)
                (setq tempcw 0)))
         (setf sumwave (sum sumwave (scale tempcw (build-harmonic-phase k
                                                   (+ (* tempphase tempi) 90.0) 
                                                   len))))))
    ;(PRINT "FORCE SUMMATION OF WAVE")
    (snd-length sumwave ny:all) ; force summation
    ;( "END SUMMATION OF WAVE")
    (setf (aref (aref wavetab h) i) (list sumwave (hz-to-step 1) T))))

;; Read in attack sound    
(princ "\nRead in attack sound...\n")
(setq attndur 0.5)
(setq attnth -1)
(dotimes (count (length attsratelist))
    (if (and (= attnth -1) (<= *piano-srate* (nth count attsratelist))) (setq attnth count)))
(if (or (= attnth -1) (/= (nth attnth attsratelist) *piano-srate*))
    (princ "No attack sound rate corresponds to current sound rate, use the nearest one\n"))   
(if (> attnth 0)
    (if (<= (- (nth attnth attsratelist) *piano-srate*) (- *piano-srate* (nth (1- attnth) attsratelist)))
        (setq attsrate (nth attnth attsratelist)) (setq attsrate (nth (1- attnth) attsratelist)))    	
    (case attnth
        (-1 (setq attsrate (last attsratelist)))
        (0 (setq attsrate (nth 0 attsratelist)))))
(setq filename (format nil "att~A.pcm" (truncate attsrate)))
(setf filename (strcat *pianosyn-path* "piano" 
                        (string *file-separator*) filename))
(setf attsound 
    (s-read filename :srate attsrate :dur attndur :format snd-head-raw
            :mode snd-mode-pcm :bits 16 :endian :big))                         

(princ "=============================================\n")
(princ "End instrument-wise initialization\n")
(princ "\n\n=============================================\n")
(princ "Piano Synthesizer function definition:\n")
(princ "(piano-note-2 step dynamic)\n")
(princ "(piano-note duration step dynamic)\n")
(princ "(piano-midi midi-file-name)\n")
(princ "(piano-midi2file midi-file-name sound-file-name)\n\n")
(princ "=============================================\n")
(setf *gc-flag* *pianosyn-save-gc-flag*) ;; restore original value


#|

;;================= DEBUGGING CODE =========================
;;
;; run (show-cn-file n) to dump some data from pn??.cod
;; 
;;==========================================================


;; INT-HEX -- convert integer to hex string
;;
(defun int-hex (int)
  (let ((result "") ch)
    (while (/= int 0)
      (setf ch (char "0123456789ABCDEF" (logand int 15)))
      (setf result (strcat (string ch) result))
      (setf int (/ int 16)))
    (if (equal result "") "0" result)))

(defun int-4char (int)
  (strcat (string (int-char (logand 255 (/ int (* 256 256 256)))))
	  (string (int-char (logand 255 (/ int (* 256 256)))))
	  (string (int-char (logand 255 (/ int 256))))
	  (string (int-char (logand 255 int)))))

(defun show-cn-file (pncount)
  (let (filename fp cwdhdr-ckid cwdhdr-type)
    (setq filename (strcat "pn" 
			   (string (int-char (+ (truncate (/ pncount 10)) 48)))
			   (string (int-char (+ (rem pncount 10) 48)))
			   ".cod"))
    (setf filename (strcat *pianosyn-path* "piano" 
			   (string *file-separator*) filename))
    (format t "SHOW-CN-FILE ~A (~A)~%" pncount filename)
    (setf fp (open-binary filename))
    ;; Read cwdHdr in cwxx.cwd
    (setq cwdHdr-ckID (read-int fp) cwdHdr-type (read-int fp))
    (format t "header ckID: ~A (~A)~%" (int-hex cwdhdr-ckid)
	                             (int-4char cwdhdr-ckid))
    (format t "header type: ~A (~A)~%" (int-hex cwdhdr-type)
	                             (int-4char cwdhdr-type))
    (setq COMMCK-ckID (read-int fp))
    (format t "header ckID: ~A (~A)~%" (int-hex commck-ckid)
	                               (int-4char commck-ckid))
    (setq COMMCK-fa (read-float fp) COMMCK-dt (read-float fp))
    (format t "commck-fa ~A commck-dt ~A~%" commck-fa commck-dt)
    (setq COMMCK-npts (read-int fp) COMMCK-ngroup (read-int fp))
    (format t "commck-npts ~A commck-ngroup ~A~%" commck-npts commck-ngroup)
    (setq DATACK-ckID (read-int fp))  
    (format t "header ckID: ~A (~A)~%" (int-hex datack-ckid)
	                               (int-4char datack-ckid))
    (setf datack-nhar (read-int fp))
    (format t "datack-nhar ~A~%cw data:" datack-nhar)
    (dotimes (i datack-nhar) 
      (if (and (zerop (rem i 10)) (or (< i 10) (> i (- datack-nhar 10))))
	  (format t "~%  ~A:" i))
      (setf data-cw (read-float fp))
      (if (or (< i 10) (>= i (* (/ datack-nhar 10) 10)))
          (format t " ~A" data-cw)))
    (format t "~%phase data:")
    (dotimes (i datack-nhar)
      (if (and (zerop (rem i 10)) (or (< i 10) (> i (- datack-nhar 10))))
	  (format t "~%  ~A:" i))
      (setf data-phase (read-float fp))
      (if (or (< i 10) (> i (- datack-nhar 10))) (format t " ~A" data-cw)))
    (format t "~%")
    (setf grupck-ckid (read-int fp))
    (format t "header ckID: ~A (~A)~%hfrom data:" 
	    (int-hex grupck-ckid) (int-4char grupck-ckid))
    (dotimes (count commck-ngroup)
      (setf data-hfrom (read-float fp))
      (if (zerop (rem count 10))
	  (format t "~%  ~A:" count))
      (format t " ~A" data-hfrom))
    (format t "~%hto data:")
    (dotimes (count commck-ngroup)
      (setf data-hto (read-float fp))
      (if (zerop (rem count 10))
	  (format t "~%  ~A:" count))
      (format t " ~A" data-hto))
    (setf gmagck-ckid (read-int fp))
    (format t "~%header ckID: ~A (~A)~%"
	    (int-hex gmagck-ckid) (int-4char gmagck-ckid))
    (setf gmaghead (read-int fp))
    (format t "gmaghead ~A" gmaghead)
    (format t "~%")
    ;; compute range of data to be read
    (setf offset gmaghead)
    (dotimes (i commck-ngroup)
      (format t "gmag: group ~A offset ~A length ~A end ~A~%"
	      i offset (* 4 (nth pncount hkframe))
	      (+ offset (* 4 (nth pncount hkframe))))
      (setf offset (+ offset (* 4 (nth pncount hkframe))))
      (format t "gmagmini: group ~A offset ~A length ~A end ~A~%"
	      i offset (* 4 (aref nptsmini pncount))
	      (+ offset (* 4 (aref nptsmini pncount))))
      (setf offset (+ offset (* 4 (aref nptsmini pncount)))))
      
    (close fp)

    (setf gmag-and-gmagmini 
     (s-read filename 
	     :time-offset (* (float gmaghead) 0.25 commck-dt)
	     :srate (/ 1.0 commck-dt)
	     :mode snd-mode-float :format snd-head-raw
	     :bits 32 :endian :big))))
|#
