;; s-save -- saves a file
(setf NY:ALL 1000000000)	; 1GIG constant for maxlen
(defmacro s-save (expression &optional (maxlen NY:ALL) filename 
                  &key (format '*default-sf-format*)
                  (mode '*default-sf-mode*) (bits '*default-sf-bits*)
                  (endian NIL) ; nil, :big, or :little -- specifies file format
                  (play nil))
  `(let ((ny:fname ,filename)
         (ny:maxlen ,maxlen)
         (ny:endian ,endian)
         (ny:swap 0))
     ; allow caller to omit maxlen, in which case the filename will
     ; be a string in the maxlen parameter position and filename will be null
     (cond ((null ny:fname)
                 (cond ((stringp ny:maxlen)
                            (setf ny:fname ny:maxlen)
                            (setf ny:maxlen NY:ALL))
                           (t
                            (setf ny:fname *default-sound-file*)))))
     
     (cond ((equal ny:fname "")
                 (cond ((not ,play)
                       (format t "s-save: no file to write! play option is off!\n"))))
           (t
            (setf ny:fname (soundfilename ny:fname))
            (format t "Saving sound file to ~A~%" ny:fname)))
     (cond ((eq ny:endian :big)
            (setf ny:swap (if ny:bigendianp 0 1)))
           ((eq ny:endian :little)
            (setf ny:swap (if ny:bigendianp 1 0))))
     (snd-save ',expression ny:maxlen ny:fname ,format ,mode ,bits ny:swap ,play)))

(defmacro s-save-autonorm (expression &rest arglist)
  `(let ((peak (s-save (scale *autonorm* ,expression) ,@arglist)))
     (autonorm-update peak)))

;; The "AutoNorm" facility: when you play something, the Nyquist play
;; command will automatically compute what normalization factor you
;; should have used. If you play the same thing again, the normalization
;; factor is automatically applied.
;;
;; Call AUTONORM-OFF to turn off this feature, and AUTONORM-ON to turn
;; it back on.
;;
;; *autonorm-target* is the peak value we're aiming for (it's set below 1
;; so allow the next signal to get slightly louder without clipping)
;;
(setf *autonorm-target* 0.9)

(defun autonorm-on ()
  (setf *autonorm* 1.0)
  (setf *autonorm-previous-peak* 1.0)
  (setf *autonormflag* t)
  (format t "AutoNorm feature is on.~%"))

(if (not (boundp '*autonormflag*)) (autonorm-on))

(defun autonorm-off ()
  (setf *autonormflag* nil)
  (setf *autonorm* 1.0)
  (format t "AutoNorm feature is off.~%"))

(defun autonorm-update (peak)
  (cond ((and *autonormflag* (> peak 0.0))
           (setf *autonorm-previous-peak* (/ peak *autonorm*))
         (setf *autonorm* (/ *autonorm-target* *autonorm-previous-peak*))
         (format t "AutoNorm: peak was ~A,~%" *autonorm-previous-peak*)
         (format t "     peak after normalization was ~A,~%" peak)
         (format t "     new normalization factor is ~A~%" *autonorm*)
         *autonorm-previous-peak*
        )
        (t peak)
  ))

;; s-read -- reads a file
(defun s-read (filename &key (time-offset 0) (srate *sound-srate*)
        (dur 10000.0) (nchans 1) (format *default-sf-format*)
        (mode *default-sf-mode*) (bits *default-sf-bits*) (endian NIL))
  (let ((swap 0))
    (cond ((eq endian :big)
           (setf swap (if ny:bigendianp 0 1)))
          ((eq endian :little)
           (setf swap (if ny:bigendianp 1 0))))
    (snd-read (soundfilename filename) time-offset
            (local-to-global 0) format nchans mode bits swap srate
            dur)))

;; SF-INFO -- print sound file info
;;
(defun sf-info (filename)
  (let (s format channels mode bits swap srate dur flags)
    (format t "~A:~%" (soundfilename filename))
    (setf s (s-read filename))
    (setf format (car *rslt*))
    (setf channels (cadr *rslt*))
    (setf mode (caddr *rslt*))
    (setf bits (cadddr *rslt*))
    (setf *rslt* (cddddr *rslt*))
    (setf swap (car *rslt*))
    (setf srate (cadr *rslt*))
    (setf dur (caddr *rslt*))
    (setf flags (cadddr *rslt*))
    (format t "Format: ~A~%" 
            (nth format '("none" "AIFF" "IRCAM" "NeXT" "Wave")))
    (cond ((setp (logand flags snd-head-channels))
           (format t "Channels: ~A~%" channels)))
    (cond ((setp (logand flags snd-head-mode))
           (format t "Mode: ~A~%"
                   (nth mode '("ADPCM" "PCM" "uLaw" "aLaw" "Float" "UPCM")))))
    (cond ((setp (logand flags snd-head-bits))
           (format t "Bits/Sample: ~A~%" bits)))
    (cond ((setp (logand flags snd-head-srate))
           (format t "SampleRate: ~A~%" srate)))
    (cond ((setp (logand flags snd-head-dur))
           (format t "Duration: ~A~%" dur)))
    ))

;; SETP -- tests whether a bit is set (non-zero)
;
(defun setp (bits) (not (zerop bits)))

;; SOUNDFILENAME -- add default directory to name to get filename
;;
(defun soundfilename (filename)
  (cond ((= 0 (length filename))
         (break "filename must be at least one character long" filename))
        ((full-name-p filename))
        (t
         ; if sf-dir nonempty and does not end with filename separator,
         ; append one
         (cond ((and (< 0 (length *default-sf-dir*))
                     (not (eq (char *default-sf-dir* 
                                    (1- (length *default-sf-dir*)))
                              *file-separator*)))
                (setf *default-sf-dir* (strcat *default-sf-dir* (string *file-separator*)))
                (format t "Warning: appending \"~A\" to *default-sf-dir*~%"
                        *file-separator*)))
         (setf filename (strcat *default-sf-dir* (string filename)))))
  filename)


(setfn s-read-format car)
(setfn s-read-channels cadr)
(setfn s-read-mode caddr)
(setfn s-read-bits cadddr)
(defun s-read-swap (rslt) (car (cddddr rslt)))
(defun s-read-srate (rslt) (cadr (cddddr rslt)))
(defun s-read-dur (rslt) (caddr (cddddr rslt)))
(defun s-read-byte-offset (rslt) (car (cddddr (cddddr rslt))))
(defun round (x) (truncate (+ 0.5 x)))

;; change defaults for PLAY macro:
(setf *soundenable* t)
(defun sound-on () (setf *soundenable* t))
(defun sound-off () (setf *soundenable* nil))

(defmacro s-add-to (expr maxlen filename &optional time-offset)
  `(let ((ny:fname (soundfilename ,filename))
         ny:input ny:rslt ny:offset
         )
     (cond ((setf ny:input (s-read ny:fname :time-offset ,time-offset))
            (setf ny:rslt *rslt*)
            (format t "Adding sound to ~A at offset ~A~%" 
                    ny:fname ,time-offset)
            (setf ny:offset (s-read-byte-offset ny:rslt))

            (snd-overwrite '(let ((ny:addend ,expr))
                              (sum (snd-coterm
                                    (s-read ny:fname :time-offset ,time-offset)
                                    ny:addend)
                                 ny:addend))
                           ,maxlen ny:fname ny:offset 
                           (s-read-mode ny:rslt) (s-read-bits ny:rslt)
                           (s-read-srate ny:rslt) (s-read-channels ny:rslt))
            (format t "Duration written: ~A~%" (car *rslt*)))
           ((setf ny:input (s-read ny:fname :time-offset 0))
            (format t "Could not open ~A at time offset ~A~%" 
                    ny:fname ,time-offset))
           (t
            (format t "Could not open ~A~%" ny:fname)))))


(defmacro s-overwrite (expr maxlen filename &optional time-offset)
  `(let ((ny:fname (soundfilename ,filename))
         ny:input ny:rslt ny:offset)
         (setf ny:offset ,time-offset)
         (cond ((null ny:offset) (setf ny:offset 0)))
     (cond ((setf ny:input (s-read ny:fname :time-offset ny:offset))
            (setf ny:rslt *rslt*)
            (format t "Overwriting ~A at offset ~A~%" ny:fname ny:offset)
            (setf ny:offset (s-read-byte-offset ny:rslt))
                (display "s-overwrite" ny:offset)
            (snd-overwrite `,expr ,maxlen ny:fname ny:offset
                           (s-read-format ny:rslt)
                           (s-read-mode ny:rslt) (s-read-bits ny:rslt)
                           (s-read-swap ny:rslt)
                           (s-read-srate ny:rslt) (s-read-channels ny:rslt))
            (format t "Duration written: ~A~%" (car *rslt*)))
           ((s-read ny:fname :time-offset 0)
            (format t "Could not open ~A at time offset ~A~%" 
                    ny:fname ,time-offset))
           (t
            (format t "Could not open ~A~%" ny:fname)))))


