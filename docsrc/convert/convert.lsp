;; convert.lsp -- insert XLISP syntax definitions

(defun alpha-char-p (c)
  (let ((cc (char-code c)))
    (or (and (>= cc (char-code #\a))
	     (<= cc (char-code #\z)))
	(and (>= cc (char-code #\A))
	     (<= cc (char-code #\Z))))))

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
    (return token)))


(defun convert (infile outfile)
  (setf *next-tokens* nil)
  (setf paren-stack nil)
  (let ((inf (open infile))
        (outf (open outfile :direction :output)))
    (process inf outf)
    (close inf)
    (close outf)))

(defun is-open-paren (tok)
  (member tok '("(" "{" "[" "<") :test 'equal))

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
        (eql c #\*))))

(defun get-fn-name (token-list)
  (setf token-list (cdr token-list))
  (let ((fn-name ""))
    (while (and token-list (starts-with-symbol-char (car token-list)))
      (setf fn-name (strcat fn-name (car token-list)))
      (setf token-list (cdr token-list)))
    fn-name))

(defun get-args (token-list)
  (prog (arg args)
loop
    (setf token-list (cdr token-list))
    (cond ((and token-list (cdr token-list) (cddr token-list)
                (equal (car token-list) "@i"))
           (push (cadr token-list) paren-stack)
           (setf token-list (cddr token-list)) ;; go to parameter name
           (while paren-stack
             (if (close-paren-p (car token-list)) (pop paren-stack)
                 (push (car token-list) arg))
             (setf token-list (cdr token-list)))
           (push (reverse arg) args)
           (setf arg nil))
          ((null token-list)
           (return (reverse args))))
    (go loop)))
       
(defun write-list-of-args (args)
  (dolist (arg args)
    (format *outf* " @i(")
    (write-list-of-tokens arg)
    (format *outf* ")")))

(defun write-list-of-tokens (toks)
  (dolist (tok toks)
    (format *outf* "~A" tok)))
  

;; this is a variable if there are no args and if there is no
;; back-to-back open/close paren pair as in foo().
(defun is-variable-check (tokens)
  (prog ()
loop
    (cond ((null (cdr tokens))
           (return t))
          ((and (equal (car tokens) "(")
                (equal (cadr tokens) ")"))
           (return nil)))
    (setf tokens (cdr tokens))
    (go loop)))
               

(defun process-codef ()
  (let ((tok (get-token))
        token-list fn-name args)
    (push tok paren-stack)
    (push tok token-list)
    (while (and tok paren-stack)
      (setf tok (get-token))
      (if (is-open-paren tok) (push tok paren-stack)
          (if (close-paren-p tok) (pop paren-stack)))
      (push tok token-list))
    (setf token-list (reverse token-list))
    ;; now we have a list of tokens including brackets
    (display "process-codef" token-list)
    (setf fn-name (get-fn-name token-list))
    (setf args (get-args token-list))
    (setf is-var (and (null args)
                      (is-variable-check token-list)))
    (display "parse" fn-name args)
    (cond (is-var
           (format *outf* "@codef")
           (write-list-of-tokens token-list))
          (t
           (format *outf* "@codef")
           (write-list-of-tokens token-list)
           (format *outf* " @c{[sal]}@*\n@altdef{@code[~A~A"
                          (if is-var "" "(") fn-name)
           (write-list-of-args args)
           (format *outf* "~A] @c{[lisp]}}"
                          (if is-var "" ")"))))))

    

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
          (t
           (format *outf* "~A" tok)))
    (go loop)))

(defun l () (load "convert.lsp"))
(convert "nyquistman.mss" "nyquistman-out.mss")
;(convert "short.txt" "short-out.txt")


    
