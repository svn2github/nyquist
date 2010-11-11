;; plugin-test -- simulate Audacity interface to Nyquist plugins
;;
;; Roger B. Dannenberg, Dec 2005
;;
;; This program runs an Audacity plugin from within Nyquist, where
;; more debugging tools are available.
;; There are two functions:
;;
;;  (PLUGIN-TEST "plugin-file-name") -- full emulation of Audacity,
;;     prompts for parameters and audio file. The ".ny" extension
;;     is optional.
;;
;;  (PLUGIN-AUTO-TEST "plugin-file-name" bindings ["audio-file-name"]) -- load
;;     and run the plugin. Bindings is a list of bindings, e.g.
;;     ((amp 1.0) (n 3)), setting the controls of the plugin.
;;     This version does not prompt for values.
;;


;; ADD-EXTENSION -- if filename does not end in ext, append ext to 
;;    filename ext should include the ".", e.g. ".ny"
;;
(defun add-extension (filename ext)
  (cond ((equal (subseq filename (- (length filename) (length ext)))
		ext)
	 filename)
	(t
	 (strcat filename ext))))

(defun string-to-number (str)
  (read (make-string-input-stream str)))


(defun parse-control-spec (line)
  (let ((stream (make-string-input-stream (subseq line 8))))
    (list (read stream)
	  (read stream)
	  (read stream)
	  (read stream)
	  (read stream)
	  (read stream)
	  (read stream))))


(defun describe-sound (snd)
  (let ((typ (type-of snd)) sr)
    (cond ((eq typ 'sound)
	   (setf typ "single-channel sound")
	   (setf sr (snd-srate snd)))
	  ((and (eq typ 'VECTOR) (eq (type-of (aref snd 0)) 'SOUND))
	   (setf typ "multi-channel sound")
	   (setf sr (snd-srate (aref snd 0)))))
    (cond ((stringp typ)
	   (format t "=== Plugin result is a ~A at sample rate ~A ===~%"
		   typ sr)
	   snd)
	  (t
	   (format t "=== Plugin result is of type ~A ===~%" typ)
	   (pprint snd) ;; print result of plugin if it's not a sound
	   (s-rest 0.1))))) ;; return silence to make play happy
	  

(defun read-file-expressions (filename)
  (let (file expr exprs)
    (setf file (open filename))
    (while (setf expr (read file))
      (push expr exprs))
    (reverse exprs)))


;; AUDIO-FILE-TO-BINDINGS -- convert audio filename to pair of bindings:
;;    ((s (s-read <filename>)) (len <length-of-audio-file>))
;; return nil if filename is invalid
;;
(defun audio-file-to-bindings (audio-file)
  (let (source)
    (if (> (length audio-file) 0)
	(setf source (s-read audio-file)))
    (cond (source 
	   (setf len (* (nth 5 *rslt*) (nth 6 *rslt*)))
	   (list `(len ,len)
		 `(s (s-read ,audio-file))))
	  (t nil))))


(defun plugin-test (filename)
  (let (file controls bindings description plug-type
	     value audio-file source exprs len)
    ;; first, check for filename extension
    (setf filename (add-extension filename ".ny"))
    ;; see if we can open the file
    (setf file (open filename))
    (if (null file)
	(error (strcat "Could not open " filename)))
    ;; parse the file
         ;sym  init             step
    (do ((line (read-line file) (read-line file)))
	((null line))
	;(display "pass 1" line)
	(cond ((eql 0 (string-search ";control" line))
	       (push (parse-control-spec line) controls))
	      ((or (eql 0 (string-search ";nyquist" line))
		   (eql 0 (string-search ";version" line))
		   (eql 0 (string-search ";name" line))
		   (eql 0 (string-search ";action" line))
		   (eql 0 (string-search ";info" line)))
	       (push line description))
	      ((eql 0 (string-search ";type" line))
	       (cond ((string-search "process" line)
		      (setf plug-type 'process))
		     ((string-search "generate")
		      (setf plug-type 'generate))
		     ((string-search "analyze")
		      (setf plug-type 'analyze))
		     (t
		      (error (strcat "unexpected specification: " line)))))))
    (close file)
    ;; print description
    (dolist (line description)
      (format t "~A~%" line))
    ;; get control values and set them as global variables
    (setf controls (reverse controls))
    (read-line) ;; read the newline after the expression that called this fn
       ;; (otherwise, we'll read in unintended new-line for first control)
    (dolist (control controls)
      ;; control is (symbol description type units default minimum maximum)
      (let ((sym (car control))
	    (desc (cadr control))
	    (ctrl-type (caddr control))
	    (units (cadddr control))
	    (default (nth 4 control))
	    (minimum (nth 5 control))
	    (maximum (nth 6 control)))
	(loop
          (format t "~A (~A) [~A]: " desc units default)
	  (setf value (read-line))
	  (if (equal value "")
	      (setf value default)
	      (setf value (string-to-number value)))
	  (if (equal ctrl-type 'int)
	      (setf value (round value))
	      (setf value (float value)))
	  (if (and (<= minimum value) (<= value maximum))
	      (return))	; break from loop
	  (format t "Try again, value must be between ~A and ~A.~%"
		  minimum maximum))
	(push (list sym value) bindings)))
    (setf bindings (reverse bindings))
    ;; determine the sound file name to process, if any, and open it
    (cond ((member plug-type '(process analyze))
	   (loop
	     (format t "Audio input file: ")
	     (setf audio-file (read-line))
	     (setf source (audio-file-to-bindings audio-file))
	     (if source (return))
	     (format t "Could not open ~A. Try again.~%" audio-file))
	   (setf bindings (append source bindings))))
    ;; now we're ready to read the plug-in as expressions
    (setf exprs (read-file-expressions filename))
    ;; turn expression list into a let and evaluate
    (run-plugin exprs bindings)))


(defun plugin-auto-test (filename bindings &optional audio-file)
  (setf filename (add-extension filename ".ny"))
  (let ((exprs (read-file-expressions filename))
	source)
    (cond (audio-file
	   (setf source (audio-file-to-bindings audio-file))))
    (cond (source
	   (setf bindings (append source bindings)))
	  (t
	   (error (strcat "audio file not valid: " audio-file))))
    (run-plugin exprs bindings)))

(defun run-plugin (exprs bindings)
  (setf exprs `(let (,@bindings) ,@exprs))
  (pprint exprs)
  (play (describe-sound (eval exprs))))


    
    
	      
