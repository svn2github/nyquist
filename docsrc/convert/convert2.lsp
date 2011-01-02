;; convert2.lsp -- insert XLISP syntax definitions to Nyquist manual AND SAL syntax
;;   definitions into xlisp.mss

(defun assert (co) (if co t (error "assertion error")))

(defun alpha-char-p (c)
  (let ((cc (char-code c)))
    (or (and (>= cc (char-code #\a))
	     (<= cc (char-code #\z)))
	(and (>= cc (char-code #\A))
	     (<= cc (char-code #\Z))))))

(defun get-begin-end-token (begin-end)
  (open-paren)
  (let ((env-name (get-token))
        (close-tok (get-token)))
    (cond ((paren-match close-tok)
           (pop paren-stack)
           (return (strcat begin-end "(" env-name ")")))
          (t
           (display "get-begin-end-token failed" begin-end env-name close-tok)))))

(defun get-token ()
  (prog ((token (read-char *inf*))
         (next-char (peek-char nil *inf*)))
    (if (not token) (return token))
    (if (and token (not (alpha-char-p token)) (not (eql token #\@)))
        (return (string token)))
    (setf token (string token))
    (while (and next-char (alpha-char-p next-char))
      (setf token (strcat token (string (read-char *inf*))))
      (setf next-char (peek-char nil *inf*)))
    (if (or (string= token "@begin") (string= token "@end"))
        (return (get-begin-end-token token))
        (return token))))


(defun convert (infile outfile)
  (setf *next-tokens* nil)
  (setf paren-stack nil)
  (let ((inf (open infile))
        (outf (open outfile :direction :output)))
    (process inf outf)
    (close inf)
    (close outf)))

;; note: "<" has been omitted here to allow parsing of "<" as an operator
;; in XLISP documentation. "<" is not commonly used as scribe bracket, but
;; that could cause problems in some cases because this is not a full
;; scribe parser.
(defun is-open-paren (tok)
  (member tok '("(" "{" "[") :test 'equal))

(defun open-paren ()
  (let ((tok (get-token)))
    (cond ((is-open-paren tok)
	   (push tok paren-stack))
          (t
           (display "open-paren got a surprise" tok)))))
;	   (push tok *next-tokens*)
;	   ;; if no open paren, then fake open and close
;	   (push #\( paren-stack)
;	   (push #\) *next-tokens*)))))

(defun close-paren-p (tok)
  (paren-match tok))

    
(defun paren-match (p2)
  (let ((p1 (car paren-stack)))
    (or (and (equal p2 ")")
	     (equal p1 "("))
	(and (equal p2 "]")
	     (equal p1 "["))
	(and (equal p2 "}")
	     (equal p1 "{"))
	(and (equal p2 ">")
	     (equal p1 "<")))))


(defun starts-with-symbol-char (tok)
  (let ((c (char tok 0)))
    (or (alpha-char-p c)
        (digit-char-p c)
        (eql c #\-)
        (eql c #\+)
        (eql c #\*)
        (eql c #\=)
        (eql c #\/)
        (eql c #\>)
        (eql c #\<))))

(defun get-fn-name ()
  (setf *token-list* (cdr *token-list*))
  (let ((fn-name ""))
    (while (and *token-list*
            (or (starts-with-symbol-char (car *token-list*))
                (equal (car *token-list*) "@i") ; allow c@i(xx)r
                (equal (car *token-list*) "(")
                (equal (car *token-list*) ")")))
      (setf fn-name (strcat fn-name (car *token-list*)))
      (setf *token-list* (cdr *token-list*)))
    fn-name))

(defun get-symbol()
  (let ((s ""))
    (while (and *token-list*
                (starts-with-symbol-char (car *token-list*)))
      (setf s (strcat s (car *token-list*)))
      (setf *token-list* (cdr *token-list*)))
    s))

;; GET-ARG - *token-list* starts with open bracket (after @i). Get the
;;           tokens between this and the close bracket.
(defun get-arg ()
  (let (arg)
    (push (car *token-list*) paren-stack)
    (setf *token-list* (cdr *token-list*)) ;; go to parameter name
    (while paren-stack
      (if (close-paren-p (car *token-list*)) (pop paren-stack))
      (push (car *token-list*) arg)
      (setf *token-list* (cdr *token-list*)))
    ;; take cdr to drop the close bracket
    (reverse (cdr arg))))

(defun get-args ()
  (prog (args arg)
loop
    (cond ((and *token-list* (cdr *token-list*) (cddr *token-list*)
                (equal (car *token-list*) "@i"))
           (setf *token-list* (cdr *token-list*))
           (push (get-arg) args))
          ((and (equal (car *token-list*) ".")
                (equal (cadr *token-list*) ".")
                (equal (caddr *token-list*) "."))
           (setf *token-list* (cdddr *token-list*))
           (push '("...") args))
          ((and *token-list* (cddr *token-list*)
                (equal (car *token-list*) "&")
                (equal (cadr *token-list*) "key")
                (equal (caddr *token-list*) " "))
           (push '("&key ") args)
           (setf *token-list* (cdddr *token-list*)))
          ((and *token-list* (cdr *token-list*)
                (equal (car *token-list*) ":"))
           (setf arg '(":"))
           (setf *token-list* (cdr *token-list*)) ; skip ":"
           (push (get-symbol) arg) ;; keyword
           (setf arg (reverse arg))
           (push arg args))
          (*token-list*
           (push (list :meta (car *token-list*)) args)
           (setf *token-list* (cdr *token-list*)))
          ((null *token-list*)
           (return (reverse args))))
    (go loop)))
       
(defun write-list-of-args (args)
  (let (need-space)
    (dolist (arg args)
      (cond ((equal arg '("..."))
             (setf need-space t)
             (format *outf* "@r(...)"))
            ((and (consp arg) (equal (car arg) :meta))
             (setf need-space nil)
             (format *outf* (cadr arg)))
            ((and (consp arg) (or (equal (car arg) ":")
                                  (equal (car arg) "&key ")))
             (setf need-space nil)
             (format *outf* "@t(")
             (write-list-of-tokens arg)
             (format *outf* ")"))
            (t
             ;; insert space between consecutive args
             (if need-space (format *outf* "@t( )"))
             (setf need-space t)
             (format *outf* "@t(@i(")
             (write-list-of-tokens arg)
             (format *outf* "))"))))))

(defun write-sal-args (args)
  (let (need-comma)
    (dolist (arg args)
      (cond ((equal arg '("..."))
             (format *outf* "@r(...)"))
            ((and (consp arg) (equal (car arg) :meta)
                  (or (equal (cadr arg) "[")
                      (equal (cadr arg) "]")))
             (format *outf* (cadr arg)))
            ((and (consp arg) (equal (car arg) :meta)) nil) ;; o.w. ignore meta
            ((and (consp arg) (equal (car arg) "&key ")))
            ((and (consp arg) (equal (car arg) ":")) ;; must be a keyword parm
             ;; assumes this is not the first parameter
             (format *outf* ", ~A: @i(~A)"
                     (cadr arg) (cadr arg)))
            (t
             (format *outf* "~A@i(" (if need-comma ", " ""))
             (setf need-comma t)
             (write-list-of-tokens arg)
             (format *outf* ")"))))))


(defun write-list-of-tokens (toks)
  (dolist (tok toks)
    (format *outf* "~A" tok)))
  

;; this is a variable if there are no args and if there is no
;; back-to-back open/close paren pair as in foo().
(defun is-variable-check (args)
  (prog ()
loop
    (cond ((null (cdr args))
           (return t))
          ((and (equal (car args) '(:meta "("))
                (equal (cadr args) '(:meta ")")))
           (return nil))
          ((= (length (car args)) 1)
           (return nil)))
    (setf args (cdr args))
    (go loop)))
               

(defun get-balanced-token-list (tok)
  (let (token-list)
    (push tok paren-stack)
    (push tok token-list)
    (while (and tok paren-stack)
      (setf tok (get-token))
      (if (is-open-paren tok) (push tok paren-stack)
          (if (close-paren-p tok) (pop paren-stack)))
      (push tok token-list))
    (setf token-list (reverse token-list))))


(defun process-codef ()
  (let (fn-name args save-tokens)
    (setf *token-list* (get-balanced-token-list (get-token)))
    ;; now we have a list of tokens including brackets
    (display "process-codef" *token-list*)
    (setf save-tokens *token-list*)
    (setf fn-name (get-fn-name))
    (setf args (get-args))
    (setf is-var (is-variable-check args))
    (display "parse" fn-name args is-var)
    (cond (is-var
           (format *outf* "@codef")
           (write-list-of-tokens save-tokens))
          (t
           (format *outf* "@codef")
           (write-list-of-tokens *token-list*)
           (format *outf* " @c{[sal]}@*\n@altdef{@code[(~A" fn-name)
           (write-list-of-args args)
           (format *outf* "] @c{[lisp]}}")))))


(defun exclude-from-sal (name)
  (or (equal name "*")
      (equal name "/")
      (equal name "+")
      (equal name "-")
      (equal name "cond")
      (equal name "case")
      (equal name "let")
      (equal name "let*")
      (equal name "prog")
      (equal name "prog*")
      (equal name "flet")
      (equal name "labels")
      (equal name "macrolet")
      (equal name "defun")
      (equal name "defmacro")
      (equal name "do")
      (equal name "do*")
      (equal name "dolist")
      (equal name "dotimes")
      (equal name "return")
      (equal name "loop")
      (equal name "progv")
      (equal name "clean-up")
      (equal name "top-level")
      (equal name "continue")
      (equal name "errset")
      (equal name "baktrace")
      (equal name "evalhook")
      (equal name "1+")
      (equal name "1-")
      (equal name "<")
      (equal name "<=")
      (equal name ">")
      (equal name ">=")
      (equal name "=")
      (equal name "/=")
      (equal name "print")
      (equal name "load")))
      

(defun process-fdescription ()
  (let (function-name args save-tokens (tok (get-token)) has-sal)
    (format *outf* "@begin(fdescription)")
    (while (and tok (not (equal tok "@end(fdescription)")))
      (cond ((equal tok "(")
             (setf *token-list* (get-balanced-token-list tok))
             (assert (equal (first *token-list*) "("))
             (setf function-name (get-fn-name))
             (setf save-tokens *token-list*)
             (setf args (get-args))
             (display "process-fdescription" save-tokens args)
             (setf has-sal (not (exclude-from-sal function-name)))
             (cond (has-sal
                    (format *outf* "@begin(fgroup)@xlcode{~A(" function-name)
                    (write-sal-args args)
                    (format *outf* ")} @c{[sal]}\n\n        ")))
             (format *outf* "@xlcode{(~A" function-name)
             ;(setf save-tokens (reverse save-tokens))
             ;(cond ((equal (car save-tokens) ")")
             ;       (setf save-tokens (cons "@xlcode{)}" (cdr save-tokens))))
             ;      (t
             ;       (display "MISSING CLOSE PAREN" save-tokens)))
             ;(setf save-tokens (reverse save-tokens))
             ;(write-list-of-tokens save-tokens)
             (write-list-of-args args)
             (format *outf* "} @c{[lisp]}")
             (setf tok (get-token))
             (format *outf* tok)
             (display "process-fdescription" function-name args)
             (while (not (equal tok "\n"))
               (setf tok (get-token))
               (format *outf* tok))
             (cond (has-sal
                    (format *outf* "@end(fgroup)\n")
                    (setf has-sal nil)))
             (setf tok (get-token)))
            ((equal tok "@begin(pdescription)")
             (format *outf* tok)
             (scan-to "@end(pdescription)")
             (setf tok (get-token)))
            ((equal (char tok 0) #\@)
             (format *outf* tok)
             (cond ((equal (setf tok (get-token)) "(")
                    (write-list-of-tokens (get-balanced-token-list tok))
                    (setf tok (get-token)))))
            (t
             (format *outf* tok)
             (setf tok (get-token)))))
    (if tok (format *outf* tok))))


(defun scan-to (stop-tok)
  (prog (tok)
 loop
    (setf tok (get-token))
    (format *outf* "~A" tok)
    ;; handle nested pdescriptions
    (if (equal tok "@begin(pdescription)")
        (scan-to "@end(pdescription)"))
    (if (equal tok stop-tok) (return))
    (go loop)))

(defun process (inf outf)
  (setf *inf* inf)
  (setf *outf* outf)
  (prog (tok)
loop
    (setf tok (get-token))
    (cond ((null tok)
           (return 'done))
          ((string= tok "@codef")
           (process-codef))
          ((string= tok "@begin(fdescription)")
           (process-fdescription))
          (t
           (format *outf* "~A" tok)))
    (go loop)))

(defun l () (load "convert2.lsp"))
;(convert "xltest.mss" "xltest.out.mss")
(convert "../../xlisp/xlisp-no-sal.mss" "xlisp-out.mss")


    
