;; typecheck-gen.lsp - functions to support regression testing
;;   of examples that would normally raise errors. This redefines
;;   error handling and generates unit tests by running the code
;;   unit tests are written ot typechecks.sal

(defun ny:error (src index typ val &optional multi (val2 nil second-val))
  (display "ny:error" src index typ val)
  (setf *expect-nyerror* (list src index typ))
  (setf *error-msg* 
        (strcat "In " src "," (index-to-string index) " argument"
          (if (second typ) (strcat " (" (second typ) ")") "")
          " must be a "
          (ny:type-list-as-string (first typ) multi)
          ", got " (param-to-string val)
          (if second-val (strcat ", and" (param-to-string val2)) "")))
  (throw 'simulated-error))

(defun sal-print (&rest args)
  (if (/= (length args) 1)
      (error-original "redefined sal-print expects single argument" args))
  (setf *expect-print* (car args)))

(defun s-plot (snd &optional (dur 2) (n 1000))
  ;; these are copied from the "real" s-plot:
  (ny:typecheck (not (soundp snd))
    (ny:error "S-PLOT (or PLOT command)" 1 '((SOUND) nil) snd))
  (ny:typecheck (not (numberp dur))
    (ny:error "S-PLOT (or PLOT command)" 2 '((NUMBER) "dur") dur))
  (ny:typecheck (not (integerp n))
    (ny:error "S-PLOT (or PLOT command)" 3 '((INTEGER) nil) n))

  (setf *expect-plot* t))

(setfn error-original error)

(defun error (x &optional y)
  (setf *expect-error* x)
  (throw 'simulated-error))


(defun print-arg (arg outf)
  (cond ((arrayp arg)
         (princ "vector(" outf)
         (let (need-comma)
           (dotimes (i (length arg))
             (if need-comma (princ ", " outf) (setf need-comma t))
             (print-arg (aref arg i) outf))
           (princ ")" outf)))
        ((listp arg)
         (princ "{" outf)
         (let (need-space)
           (dolist (a arg)
             (if need-space (princ " " outf) (setf need-space t))
             (print-arg a outf)))
         (princ "}" outf))
        (t
         (prin1 arg outf))))


(defun escaped (line)
  (let ((rslt "\"") ch)
    (dotimes (i (length line))
      (setf ch (char line i))
      (setf rslt (strcat rslt (if (equal ch #\") "\\\""
                                                 (string ch)))))
    (strcat rslt "\"")))


;; this function generates the "unit test" to check for what
;; just happened when we executed the previous command
;; what can be :print :plot :error or :nyerror 
;; command looks like:
;;   (ny:expect expression what val)
;;
(defun expect-gen (line outf)
  (princ "exec ny:expect(" outf)
  (princ line outf)
  (princ ",\n               " outf)
  (cond (*expect-print*
         (princ ":print, " outf)
         (print-arg *expect-print* outf))
        (*expect-plot*
         (princ ":plot, " outf)
         (prin1 *expect-plot* outf))
        (*expect-nyerror*
         (princ ":nyerror, " outf)
         (print-arg *expect-nyerror* outf))
        (*expect-error*
         (princ ":error, " outf)
         (prin1 *expect-error* outf))
        (t
         (close outf)
         (error-original "in expect-gen, nothing happened?")))
   (princ ")\n" outf)
   (princ "exec #print(" outf)
   (princ line outf)
   (princ ")\n" outf))


(defun is-a-command (line)
  (and (> (length line) 0)
       (not (eql #\; (char line 0)))))


(defun generate-line (line outf)
  (setf *expect-print* nil *expect-plot* nil 
        *expect-error* nil *expect-nyerror* nil 
        *error-msg* nil)
  (catch 'simulated-error
    (sal-compile line t nil "<typecheck-gen>"))
  ;; after execution, see what happened and generate unit test
  (setf line (escaped line))
  (expect-gen line outf)
  (format outf "exec catch(quote(simulated-error), ~%")
  (format outf "           sal-compile(~A, #t, #f,~%" line)
  (format outf "                       \"<in typecheck.sal>\"))~%")
  (if *error-msg* (format outf "; ERROR MSG: ~A~%" *error-msg*))
  (format outf "~%"))


;; There was a stack overflow problem putting everything into
;; a 3000-line SAL file, so separate the tests into multiple files
(setf *tests-per-file* 100)

(defun generate ()
  (let ((inf (open "typechecks.txt"))
        (mainf (open "typechecks.lsp" :direction :output))
        outf
        outf-name
        (file-count 0)
        (test-count *tests-per-file*)
        line)
    ;; test for successful open file operations
    (if (not inf) (error-original "could not open typechecks.txt"))
    (if (not mainf) (error-original "could not open for output typechecks.lsp"))

    ;; write header info
    (format mainf "; typechecks.lsp -- unit tests for typechecks~%")
    (format mainf "; ~%")
    (format mainf "; This code was generated from typechecks.txt by~%")
    (format mainf "; typecheck-gen.lsp~%~%")

    ;; process every test
    (while (setf line (read-line inf))
      (cond ((is-a-command line)
            (cond ((>= test-count *tests-per-file*)
                   (if outf (close outf))
                   (setf test-count 0)
                   (setf file-count (1+ file-count))
                   (setf outf-name (format nil "typechecks~A.sal" file-count))
                   (setf outf (open outf-name :direction :output))
                   (if (not outf) (error-original "could not open" outf-name))

                   ;; add file to main file
                   (format mainf "(sal-load \"~A\")\n" outf-name)

                   ;; write the header information
                   (format outf "; ~A -- unit tests for typechecks~%" outf-name)
                   (format outf "; ~%")
                   (format outf "; This code was generated from typechecks.txt by~%")
                   (format outf "; typecheck-gen.lsp~%~%")))

             (format t "line: |~s|\n" line)
             (generate-line line outf)
             (setf test-count (1+ test-count)))))
    (close outf)
    (close inf)
    (close mainf)))

(generate)
