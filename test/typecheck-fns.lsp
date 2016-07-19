;; typecheck-fns.lsp - functions to support regression testing
;;   of examples that would normally raise errors. This redefines
;;   error handling to check for proper error reporting rather than
;;   actually raising an error

(setf *skip-command* nil)

(setfn original-error error)

(defun error (msg &optional value)
  (cond ((equal msg *expect-error*))
        (t (original-error "unexpected value to error" (list msg value))))
  (setf *expect-error* nil
        *skip-command* t)
  (throw 'simulated-error))
     
(setfn original-nyerror ny:error)

(defun ny:error (src index typ val &optional multi (val2 nil second-val))
  (cond ((and (equal src (car *expect-nyerror*)))
              (equal index (cadr *expect-nyerror*))
              (equal typ (caddr *expect-nyerror*))
              (equal typ (cadddr *expect-nyerror*)))
         (t (original-error "unexpected call to ny:error")))
  (setf *expect-nyerror* nil
        *skip-command* t)
  (throw 'simulated-error))


(setfn original-sal-print sal-print)

(defun sal-print (&rest args)
  (cond (*skip-command* (setf *skip-command* nil))
        ; use sal-equal so that when 2.0 is printed as 2 and we read it
        ; back as an integer, we get (sal-equal 2 2.0) which is different
        ; from (equal 2 2.0) because that test is false!
        ((and (= (length args) 1) (sal-equal (car args) *expect-print*)))
        (t (original-error "unexpected value to print")))
  (setf *expect-print* nil))

(setfn original-plot s-plot)

(defun s-plot (s)
  (cond (*skip-command* (setf *skip-command* nil))
        (*expect-plot*)
        (t (original-error "unexpected call to plot")))
  (setf *expect-plot* nil))


(setf *expect-print* nil *expect-plot* nil 
      *expect-error* nil *expect-nyerror* nil)

;; this function says what the "unit test" is supposed to do.
;; what can be :print :plot :nyerror or :error
;;
(defun ny:expect (expression what val)
  (cond ((or *expect-print* *expect-plot* *expect-error* *expect-nyerror*)
         (original-error "expected result did not happen")))
  (setf *expect-print* nil *expect-plot* nil 
        *expect-error* nil *expect-nyerror* nil)
  (cond ((eq what :print)
         (setf *expect-print* val))
        ((eq what :plot)
         (setf *expect-plot* val))
        ((eq what :error)
         (setf *expect-error* val))
        ((eq what :nyerror)
         (setf *expect-nyerror* val))))
