;; drum.lsp -- Phil Light - 2006
;; modified by Roger B. Dannenberg

; set to NIL to turn off debugging, or *standard-output* to see the messages.
(setf *drum-debug* NIL)

(setf *plight-drum-path* (current-path))

; this function demonstrates how to use this drum machine
(defun play-drum-example ()
  (play (plight-drum-example)))

(defun plight-drum-example ()
  ; read the properties file 
  (load-props-file (strcat *plight-drum-path* "beats.props"))
  (create-drum-patches) ; create the drum kit
  (create-patterns) ; load the beats
  (setf kick  (drum 5 5 480)) ; a single measure of the kicker
  (setf snare (drum 7 7 480)) ; a single measure of the snare
  (setf hat   (drum 4 4 480)) ; a single measure of the hihat
  ; sim them together and play 3 measures
  (drum-loop (sim hat kick snare) (* 16 (length-of-beat 480)) 3))

; A drum machine pattern generator for a single instrument.  
; Before you call this function, you must call 3 functions to load things into 
; memory:
; 
; 1. (load-props-file "beats.props" )
;    this function reads in the properties file and creates a dictionary list
;    of its key/value pairs.  the properties are available in the property list
;    called 'drum-data
; 2. (create-drum-patches)
;    this function looks through 'drum-data and creates drum sample sets
;    from it. These drums are stored in the new property list 'drums
; 3. (create-patterns)
;    this function does the same things with patterns, stores them in 'beats
;
; This function takes 3 arguments:
;   tracknum: the track number of the drum you want to use.  There must be a
;    corresponding set of drums at that number.
;   patternnum: the number of the pattern you want that drum to play on.  there
;    must be a corresponding pattern with that index number.
;   bpm: the speed at which you want this drum to be played.  NOTE THAT THIS 
;    DRUM MACHINE HAS NO UNDERSTANDING OF THE LENGTH OF YOUR MEASURE.  It is 
;    just as happy to play through a sequence of 17 steps as it is 16.  This 
;    means that if you want to have a 4/4 sequence of sixteenth notes play at 
;    120 real beats per minute, you need to pass this function 480, which is
;    how many SIXTEENTHS you want per minute, not quarter notes.
;
; The output is a sound.  The sound is a single pass of this drum playing the
; requested pattern.  Use the provided loop function to assemble multiple
; measures together correctly.
;
(defun drum (tracknum patternnum bpm)
  (let (velocity  ; how hard to hit each stroke
       (result (const 0 0))  ; a placeholder "sound" for the result
       ; figure out when we're playing the sound
       (strikes (get 'beats (intern (format NIL "beats.~A" patternnum)))))
    (dotimes (note-index (length strikes))                                      
      (format *drum-debug* "appending strike #~A~%" note-index)
      (setf velocity (nth note-index strikes)) ; how hard do we hit this drum?
      (if (not (eq velocity 0))  ; if we're not resting right now:
          ; add another drum stroke to whatever we've accumulated so far
          (setf result (sum result 
                            (at (* note-index (length-of-beat bpm))
                                (cue (get-sample tracknum velocity)))))))
    (set-logical-stop result (* (length-of-beat bpm) (length strikes)))))



; DRUM-LOOP -- return a sound which loops the drum measure numtimes times.
;
(defun drum-loop (snd duration numtimes)
  (seqrep (index numtimes)
          (set-logical-stop (cue snd) duration)))


; LENGTH-OF-BEAT -- how long is a beat (in seconds), given beats per minute?
;
(defun length-of-beat (bpm)
  (/ 1.0 (/ bpm 60.0)))

; GET-SAMPLE -- retrieve sample given tracknum and velocity
;
(defun get-sample (tracknum velocity)
  (let ((rslt
         (get 'drums (intern (format NIL "track.~A.~A" tracknum velocity)))))
    (format *drum-debug* "sample for #~A,~A: ~A~%" tracknum velocity rslt)
    rslt))


; LOAD-PROPS-FILE -- loads a file of properties, which must be in the form:
;    propertyName = propertyValue.
;
; in addition, it supports commenting out lines with the # character.
; spaces (only the space character, not all whitespace) are trimmed from both 
; keys and values.
; blank lines are ignored. (actually, any line without an = is ignored.)
;
(defun load-props-file (filename)
  (let ((file (open filename)) commentdex equaldex key value result)
     (do ((line (read-line file) (read-line file)))
         ((eq line NIL))  ; read until there are no more lines
       (setf commentdex (string-search "#" line))
       (if (not (equal NIL commentdex)) ; are there any comments?
           (setf line (subseq line 0 commentdex))) ; chop off everything after #
       (setf equaldex (string-search "=" line))     ; find the =
       (if (not (equal NIL equaldex))
           ; if there is an "=", set the key/value pair in 'drum-data
           (parse-for-property line equaldex)))
     (close file)))


; PARSE-FOR-PROPERTY -- store key/value property from string
;    input: "key = value"
;    result: a mapping from "key" to "value" in 'drum-data
;
(defun parse-for-property (line equaldex)
  (let ((key (string-trim " \r" (subseq line 0 equaldex))) ; parse key name
       (value (string-trim " \r" (subseq line (+ 1 equaldex))))) ; parse value
     (putprop 'druM-DATA VALUE (INTERN KEY))))


; CREATE-DRUM-PATCHES -- load drum sounds into property list
;
; takes a series of mappings like this
;      "track.3.2 = mysound.wav"
; and creates a second mapping, stored in 'drums, of the keys to samples made
; by loading the listed file.
;
; the first number following the track represents which track the sample
; belongs to.  the second, the velocity of a particular sample.
;
(defun create-drum-patches ()
  (let ((patch-made t) key)
     ; loop until we are unable to make a patch
     (do ((i 1 (+ 1 i))) ((not patch-made))
       ; look for at least one sound at velocity 1
       (setf key (format NIL "track.~A.1" i))
       (setf patch-made
             ; if there's no value for key, then don't try to make patch
             (and (get 'drum-data (intern key))
                  (setf patch-made (create-one-patch i)))))))


; CREATE-ONE-PATCH -- load set of drum samples at different velocities
;
; samples are loaded and stored as properties of 'drums
;
(defun create-one-patch (patchnum)   
   (format *drum-debug* "making patch #~A~%" patchnum) ; debugging
   (let (key sound-dir sound-file spacedex patch-made)
     (do ((i 1 (+ 1 i)))
         ((= i 10)) ; (played) velocities range from 1-9.
       ; make the key for this velocity stroke
       (setf key (format NIL "track.~A.~A" patchnum i))
       ; where are these files located?
       (setf sound-dir (get 'drum-data '|sound-directory|))
       ; sound-dir is specified relative to this file's location
       (setf sound-dir (strcat *plight-drum-path* sound-dir))
       ; make the filename of the sound
       (setf sound-file (get 'drum-data (intern key)))
       (format *drum-debug* "sound-file: ~A~%" sound-file)
       (and sound-file
            (read-sample-with-dir sound-dir sound-file key)
            (setf patch-made t))) ;; patch-made if at least one success
     (format *drum-debug* "made patch? ~A it is ~A~%"
                          patch-made (get 'drums (intern key))) 
     patch-made)) ; return our success.


(defun read-sample-with-dir (sound-dir sound-file key)
  (if sound-dir
      (setf sound-file (strcat sound-dir sound-file)))
  (read-sample key sound-file))


; READ-SAMPLE -- read in a sample from the argument soundpath.
;
;    add a mapping in the property list 'drums from the string key
;    to the resulting sound.
;    returns: sound if the sound was created and mapped successfully
;
(defun read-sample (key soundpath)
  (format *drum-debug* "key: ~A path: ~A~%" key soundpath) 
  (putprop 'drums (s-read soundpath) (intern key)))


; CREATE-PATTERNS -- load set of drum sequences
; 
; input looks like: "beats.1 = 1---1---1---5---"
; where the digits 1-9 represent a drum stroke and any other character
; is a rest.  The digits are meant to represent the velocity of each
; drumstroke.
;
(defun create-patterns ()
  (let ((pattern-made 'true ) key) 
    (do ((i 1 (+ 1 i)))
        ((not pattern-made)) ; loop until we didn't make a pattern
      (setf key (format NIL "beats.~A" i)) ; the key for this pattern
      (setf pattern-made
            (and (get 'drum-data (intern key))
                 (create-one-pattern i))))))


; CREATE-ONE-PATTERN -- parse beat string and save result in 'beats
;
; get string using key "beats.<N>". It should look like:
;     "1---3---2---8---"
; add an integer to a result list for each char.
; the beat velocities are stored as integers.  the result of this function is 
; t iff the pattern was added to the property list 'beats
;
(defun create-one-pattern (tracknum)                                            
  (format *drum-debug* "making pattern #~A~%" tracknum)
  (let ((key (format NIL "beats.~A" tracknum)) pattern-str pattern-list)
    (setf pattern-str (get 'drum-data (intern key)))
    (format *drum-debug* "pattern #~A at key ~A is ~A~%"
            tracknum key pattern-str)
    (do ((i 0 (+ 1 i)))
        ((eq i (length pattern-str)))
      (setf cc (char-code (char pattern-str i)))
      (cond ((and (>= cc (char-code #\1))
                  (<= cc (char-code #\9)))
             (push (- cc (char-code #\0)) pattern-list))
            (t (push 0 pattern-list))))
    (setf pattern-list (reverse pattern-list))
    (format *drum-debug* "beats: ~A~%" pattern-list)
    (putprop 'beats pattern-list (intern key)))) ; save this pattern to 'beats
