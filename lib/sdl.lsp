;;; Score Description Library. v 1.0 
;;; pmorales. Junio, 2007


; NOTAS:
;  - es obligatorio definir un instrumento al menos y asignarlo desde el principio
;  - en su lugar hay que utilizar TF (time factor) que tiene un efecto similar al de Cakewalk
;  - los atributos ATTR solo tienen efecto sobre el instrumento que estan definidos.
;    los atributos estan asociados a un instrumento en particular

; a helper function ------------------------------------------

(defun floor (x)
  (round (- x 0.5)))


; this code is imported from pmorales lambda music 

(defun sdl:pitch-lex (pitch)
  "ARGS: pitch
DEVUELVE: Cadena con el valor del argumento convertido a pitch-midi"
  (case (type-of pitch)
	(fixnum pitch)
	(flonum pitch (round pitch))
	(symbol (sdl:pitch-name->step (let ((str (symbol-name pitch))) (if (equal (char str 0) #\:) (subseq str 1) str))))
	(string (sdl:pitch-name->step pitch))
	(t (error "PITCH-LEX: Error lexico en especificacion de pitch"))))


(defun sdl:digit-char-p (chr)
  (char>= #\9 chr #\0))

(defun sdl:code-pitch-p-1 (chr)
  (or (char>= #\g chr #\a) (char>= #\G chr #\A)))

(defun sdl:code-pitch-p-2 (chr)
  (or (eq chr #\#) (eq chr #\b)(eq chr #\B)(eq chr #\s)(eq chr #\f) (sdl:digit-char-p chr)))

(defun sdl:pitch-p (str)
  "Detecta si el argumento es un simbolo que representa un pitch"
  (case (length str)
     (1 (sdl:code-pitch-p-1 (aref str 0)))
     (2 (and (sdl:code-pitch-p-1 (char str 0)) (sdl:code-pitch-p-2 (char str 1))))
     (3 (and (sdl:code-pitch-p-1 (char str 0)) (sdl:code-pitch-p-2 (char str 1))
             (sdl:digit-char-p (char str 2))))
     (4 (and (sdl:code-pitch-p-1 (char str 0)) (sdl:code-pitch-p-2 (char str 1))
             (sdl:digit-char-p (char str 2)) (sdl:digit-char-p (char str 3))))))


(defun sdl:b-or-# (pname)
  (let ((chrom (char pname 1)))
    (case chrom
      ((#\b #\B #\f) -1)
      ((#\# #\s) 1)
      (t 0))))

(defun sdl:pitch-name-category (pname)
  (let ((first-char (char pname 0)))
    (case first-char
      ((#\C #\c) 0)
      ((#\D #\d) 2)
      ((#\E #\e) 4)
      ((#\F #\f) 5)
      ((#\G #\g) 7)
      ((#\A #\a) 9)
      ((#\B #\b) 11)
      (t (error (strcat "Improper pitch name " pname))))))

(defun sdl:char-to-val (char)
  (- (char-code char) 48))

(defun sdl:string-to-val (string)
  (let ((len (1- (length string))))
    (do* ((i -1 (1+ i))
          (suma 0 (+ suma (* (sdl:char-to-val (char string i)) (expt 10 (float (- len i)))))))
         ((= i len) suma))))

(defun sdl:pitch-name->step (pname)
  (when (symbolp pname) (setf pname (string-trim ":" (symbol-name pname))))
  (let ((chrom (sdl:b-or-# pname))
        (category (sdl:pitch-name-category pname))
        (octave (sdl:string-to-val (string-left-trim "AaBbCcDdEeFfGg#s" pname))))
    (+ chrom category (* 12 (- octave 4)) 60)))


(defun sdl:one-of-twelve-to-string (number)
  (case number
    (0 "C")
    (1 "C#")
    (2 "D")
    (3 "D#")
    (4 "E")
    (5 "F")
    (6 "F#")
    (7 "G")
    (8 "G#")
    (9 "A")
    (10 "A#")
    (11 "B")))

(defun step->pitch-name (midi-number)
  (let ((one-of-twelve (rem midi-number 12))
        (octave (1- (floor (/ midi-number 12)))))
    (format nil "~A~A" (sdl:one-of-twelve-to-string one-of-twelve) octave)))

(defun step->hz (midi)
  (* 440.0 (expt 2.0 (/ (- midi 69.0) 12.0))))

(defun pitch-name->hz (name)
  (step->hz (pitch-name->step name)))

(defun pitch-name->step (pn) 
   (if (numberp pn)
       pn
     (sdl:pitch-name->step pn)))





;;; functions for SYMBOL PROPERTY LIST processing 


(defun sdl:iterate-on-symbol-plist (fun plist &optional result)
  (if plist
      (sdl:iterate-on-symbol-plist fun (cddr plist) (cons (funcall fun (car plist) (second plist)) result))
      result))
 
      
(defun sdl:sort-pwl (plist)
  (sdl:iterate-on-symbol-plist
   #'(lambda (sym pwl-list)
       (list sym (sort pwl-list  #'(lambda (x y) (< (car x)(car y))))))
   plist))      
 

               
; ATENCION: los calculos se hacen sobre pulsos, no sobre segundos               
(defun sdl:calcule-pwl-val (tm plist)
  (apply #'append 
	(sdl:iterate-on-symbol-plist
	 #'(lambda (prop-sym prop-val) 
	     (list prop-sym (sdl:pwl-val tm prop-val)))
	 plist)))                             
                    

   
; this function compute variable attributes           
          
(defun sdl:pwl-val (x points)
  (labels
   ((pwl-interpolate (x x1 y1 x2 y2)
		     (let* ((a (/ (- y1 y2) (- x1 (float x2))))
			    (b (- y1 (* a x1))))
		       (+ b (* a x))))
    (search-points (x points &optional (result 0))
		   (if (or (null points) (< x (caar points)))
		       result
		     (search-points x (cdr points) (+ 1 result))))
    (points-xval (n points) (car (nth n points)))
    (points-yval (n points) (cadr (nth n points))))          
   (let ((len (length points))
	 (index (search-points x points)))
     (cond
      ((= 0 index) (points-yval 0 points))
      ((= len index) (points-yval (- len 1) points))
      (t (pwl-interpolate x (points-xval (- index 1) points) (points-yval (- index 1) points)
			  (points-xval index points) (points-yval index points)))))))       


; macros in SDL--------------------------------------------------------------              
      
(defun sdl:is-event-macro? (ev)
  (and (listp ev) (equal (car ev) 'MAC)))

(defun sdl:score-has-macros? (sdl-sco)
  (do ((i 0 (+ 1 i))
       result)
      ((cond 
	((sdl:is-event-macro? (nth i sdl-sco)) (setf result T) T)
	 ((= i (length sdl-sco)) T))
       result)))
     
(defun sdl:expand-macros (sdl-sco)
  (apply #'append
	 (mapcar #'(lambda (ev)
		     (cond
		      ((not (listp ev)) (list ev))
		      ((and (listp ev) (not (equal (car ev) 'MAC))) (list ev))
		      (t (apply (second ev) (cddr ev)))))
		 sdl-sco)))
                  

; main functions for SDL -------------------------------------------------------

; this is a BIG function
                            
(defun sdl:sdl->score-aux (score-data &optional time-marks)
  (let ((tf 1.0)  ; global time factor
	sc-instr
        chords
        (sc-time 0)
        (sc-dur   1))
    (unless time-marks (setf time-marks (gensym)))
    (labels ((filter-name (ky l &optional xresult)
			  (if l
			      (if (not (member (car l) ky :test #'equal))                   
				  (let () (push (car l) xresult)
				       (push (cadr l) xresult)
				       (filter-name ky (cddr l) xresult))
				(filter-name ky (cddr l) xresult))
			    (reverse xresult)))
	     (attrs-vals () (symbol-plist sc-instr))
	     (scale-score-time (event scale)
			       (list (* scale (car event)) (* scale (cadr event)) (caddr event)))
	     (make-sc-note (p) (list sc-time sc-dur
				     (append (list (get sc-instr :name) :pitch (sdl:pitch-lex p))
					     (sdl:calcule-pwl-val sc-time (get sc-instr :pwl))
					     (filter-name (list :name :pwl) (attrs-vals)))))
	     (calcula-dur (datum) (if (listp datum) (eval datum) datum))
	     (setdur (dur) (setf sc-dur (calcula-dur (car dur))))
	     (setinstr (instr) (setf sc-instr (intern (car instr))))
	     (init-instr (instr instr-name)
			 (setf sc-instr (intern instr))
			 (setf (symbol-plist sc-instr) NIL)
			 (putprop sc-instr (gensym) :pwl)
			 (putprop sc-instr instr-name :name))
	     (set-attr (prop val) (putprop sc-instr val prop))
	     (set-pwl-point (prop val)  
			    (push (list sc-time val) (get (get sc-instr :pwl) prop)))
	     (set-mrk (mrk-symbol) (if (get time-marks mrk-symbol)
				       (error "sdl->score: time mark ~A already set" mrk-symbol)
				     (putprop time-marks sc-time mrk-symbol)))
	     (set-time-mrk (mrk-symbol) 
			   (let ((mrk-time (get time-marks mrk-symbol)))
			     (if mrk-time (setf sc-time mrk-time)
			       (error (format nil "sdl->score: time mark ~A does not exists" mrk-symbol))))) 
	     
                                              
             (proc-elt-for-pwl (elt)
			       (if (not (listp elt))
				   (cond ((numberp elt) (setf sc-time (+ sc-time elt)) NIL)
					 (t (setf sc-time (+ sc-time sc-dur)) NIL))
				 (case (car elt)
				       ((KEY TS CHN PATCH LM TN MRK NTR) NIL) ; filter out all these
					; for compatibility with lambda music
				       ((TF) (setf tf (calcula-dur (cadr elt))) NIL)
				       ((LABEL SET-MRK) NIL)
				       ((AT-LABEL AT-MRK)  NIL)
				       ((TIME-IN-SECONDS) (setf tf 0.25) NIL) ; with mm = 60
				       ((DUR) (setdur (cdr elt)) NIL)
				       ((INSTRUMENT) (setinstr  (cdr elt)) NIL)
				       ((INIT-INSTR) (init-instr (second elt)(third elt)) NIL)
				       ((ATTR) NIL)
				       ((PWL-POINT) (set-pwl-point (second elt) (calcula-dur (third elt))) NIL)
				       ((FUN) (apply (eval (cadr elt)) (cddr elt)))
				       ((DELTA PAU) (setf sc-time (+ sc-time (calcula-dur (second elt)))) NIL) ; pause positive or negative
				       ((CH)  (setf sc-time (+ sc-time sc-dur)) NIL)
				       ((CH1) (setf sc-dur (calcula-dur (third elt))) ; pitch dur
					(setf sc-time (+ sc-time sc-dur)) NIL)
				       (t     (setf sc-dur (calcula-dur (cadr elt)))
					      (setf sc-time (+ sc-time sc-dur))
					      NIL))))                                              
	     
	     
	     
	     
	     (proc-elt (elt)
		       (if (not (listp elt))
			   (cond ((numberp elt) (setf sc-time (+ sc-time elt)) NIL)
				 (t     (let ((ret-note (make-sc-note elt)))
					  (setf sc-time (+ sc-time sc-dur))
					  ret-note)))
			 (case (car elt)
			       ((KEY TS CHN PATCH LM TN MRK NTR) NIL) ; filter out all these
					; for compatibility with lambda music
			       ((TF) (setf tf (calcula-dur (cadr elt))) NIL)
			       ((LABEL SET-MRK) (set-mrk (second elt)) NIL)
			       ((AT-LABEL AT-MRK)  (set-time-mrk (second elt)) NIL)
			       ((TIME-IN-SECONDS) (setf tf 0.25) NIL)
			       ((DUR) (setdur (cdr elt)) NIL)
			       ((INSTRUMENT) (setinstr  (cdr elt)) NIL)
			       ((INIT-INSTR) NIL) ;(init-instr (second elt)(third elt)) NIL)
			       ((ATTR) (set-attr (second elt) (calcula-dur (third elt))) NIL)
			       ((PWL-POINT)  NIL)
			       ((FUN) (apply (eval (cadr elt)) (cddr elt)))
			       ((DELTA PAU) (setf sc-time (+ sc-time (calcula-dur (second elt)))) NIL) ; pause positive or negative
			       ((CH) (dolist (n (cdr elt)) (push (make-sc-note n) chords)) NIL)
			       ((CH1) (setf sc-dur (calcula-dur (third elt))) ; pitch dur
				(make-sc-note (second elt)))
			       (t     (setf sc-dur (calcula-dur (cadr elt)))
				      (let ((ret-note (make-sc-note (car elt))))
					(setf sc-time (+ sc-time sc-dur))
					ret-note))))))
	    
					; first sets pwl data
	    (dolist (ev score-data NIL)
	      (proc-elt-for-pwl ev))
	    
					; sort pwl data
	    (dolist (elt score-data NIL)
	      (when
		  (and (listp elt) (equal (car elt) 'INIT-INSTR))
		
		(putprop (intern (second elt)) 
                         (apply #'append (sdl:sort-pwl (symbol-plist (get (intern (second elt)) :pwl))))
                         :pwl)))
	    
	    
					; then process score
	    
	    (setf sc-time 0.0)       
	    
	    (do ((data score-data (cdr data))
		 (result '()))
		((null data) (list (sort (mapcar #'(lambda (ev) (scale-score-time ev tf)) result)
					 #'(lambda (x y) (< (car x) (car y)))) time-marks))
		(let ((proced-elt (proc-elt (car data))))
		  (when proced-elt (push proced-elt result)))
		(setf result (append result chords))))))


(defun sdl:get-key (l k)
  (when l
    (if (equal (car l) k)
	(second l)
      (sdl:get-key (cdr l) k))))

        
(defun sdl:apply-mm-to-score (sco)
  (let (current-time current-dur end-last-note gap current-mm result)
    (setf current-mm (sdl:get-key (caddar sco) :mm)) 
    (setf current-time (* (/ 15.0 current-mm) (caar sco))) 
    (setf end-last-note (caar sco))
    (dolist (ev sco)
      (setf gap (- (car ev) end-last-note))
      (setf end-last-note (+ (car ev) (second ev)))
      (setf current-mm (sdl:get-key (caddr ev) :mm))
      (setf current-dur (* (/ 15.0 current-mm) (cadr ev)))
      (push (list  (+ current-time (* (/ 15.0 current-mm) gap))  current-dur (caddr ev)) result)
      (setf current-time (+ current-time (* (/ 15.0 current-mm) gap) current-dur)))
    (reverse result)))


(defun sdl:normalize-score-duration (sco)
 (mapcar #'(lambda (ev) (list (car ev) 1 (third ev))) sco))

          
; main functions interface -------

;(defun sdl->score (score-data &optional time-marks)
;  (when (sdl:score-has-macros? score-data)
;    (setf score-data (sdl:expand-macros score-data)))
;  (car (sdl:sdl->score-aux score-data time-marks)))

(defun sdl->score (score-data &optional time-marks)
  (when (sdl:score-has-macros? score-data)
    (setf score-data (sdl:expand-macros score-data)))
  (sdl:apply-mm-to-score
   (car (sdl:sdl->score-aux score-data time-marks))))
	
(defun sdl->timelabels (score-data &optional time-marks)
  (when (sdl:score-has-macros? score-data)
    (setf score-data (sdl:expand-macros score-data)))
  (second (sdl:sdl->score-aux score-data time-marks)))

	
#|

; PRUEBAS


(defun sdl-repeat (n quoted-event)
  (let (result)
    (dotimes (i n (apply #'append result))
      (push quoted-event result))))


(setf *score* '((TF 1.0)
		(INIT-INSTR "i1" xx)(INIT-INSTR "i2" yy)
		(INSTRUMENT "i1")(ATTR :at1 2)(ATTR :mm 60) 4 (:e4 2) 2 (:d4 4) 10 (LABEL :t1)
		(INSTRUMENT "i2") (ATTR :mm 60)(:f4 8) 
		(LABEL :vuelta) (AT-LABEL :t1) (:f5 4) 
		(AT-LABEL :vuelta) (:f6 8)
))


(setf *score2* '((TF 1.0)
		 (INIT-INSTR "i1" xx2)(INSTRUMENT "i1")(ATTR :mm 60)
		 (AT-LABEL :t1) (:e4 4) (MAC sdl-repeat 4 (:f4 :g4)) ))

;(print  (sdl:apply-mm-to-score (car (sdl:sdl->score-aux *score*))))
;(print (car (sdl:sdl->score-aux *score*)))

(setf *tlabels* (sdl->timelabels *score*))

(print (sdl->score *score*))
(print (sdl->score *score2* *tlabels*))

|#


