; s2h -- scribe to html

; @begin(fgroup)@xlcode{write-char(@i(ch)[, @i(stream)])} @c{[sal]}
; @xlcode{(write-char@pragma(defn)@index(write-char) @t(@i(ch))
; [@t(@i(stream))])} @c{[lisp]}  @itemsep write a character to a stream
;
; HTML: write-char(ch[, stream])
;       (write-char ch [stream])
; HINT: write-char ch[stream]


; MajorHeading = H1
; Chapter, Unnumbered, Appendix = H2
; Section = Heading = H3
; SubSection = SubHeading = H4
; Paragraph = H5

; Files:
; *dest*	-- the title and table of contents
; part1.html	-- for each unnumbered, chapter, appendix, 
;                  at end of each part, link to: next part, 
;                  index, and table of contents
;                  at beginning of each part, link to: prev part,
;                  next part, index, and table of contents
; indx.html	-- the index (not "index", because that's a special name)
; title.html    -- the title page and table of contents
; guide.html    -- the table of contents and index for left frame
; home.html     -- the top-level document with two frames, the actual name
;                     of this file is given by the dest parameter of (G ...)
; NyquistWords.txt -- completion list file


; Fancy control:
; add the following to Scribe file:
;	@textform(html = [])
;	@textform(htmltitle = [])
;	@textform(pragma = [])
;   @textform(htmlindex = index) <- untested, probably wrong
; To get a title, add this to the file:
; @html(<head><title>Your Title Here</title></head><body>)
; If you use frames, you should add this to the scribe document:
; @htmltitle(Your Title Here)
; This tells the translator the html title to use for the root 
; html file.
; anything inside @html{...} is output directly to output html file
; anything in the @pragma{...} is executed as a command to this translator
;
; If you define an index location with @index() in a scribe-only region, e.g.
;     @pragma(startscribe)something@index(foo)@pragma(endscribe), you should
; put a matching @htmlindex() within an html-only region, e.g.
;     @begin(html)<blockquote>something@end(html)@htmlindex(foo)
;     @begin(html)</blockquote>@end(html)
;
; some pragma commands are:
;   defn -- the next @index() marks a term's definition in the manual
;	scribeonly (or startscribe) -- stop outputing to html or latex starting here
;   htmlonly -- generate output only to html and scribe (in scribe syntax)
;      (use @html{...} to output literal html only to html output)
;   latexonly -- generate output only to latex and scribe (in scribe syntax)
;      (use @latex{...} to output literal latex only to latex output)
;   all (or endscribe)-- generate output for both latex and html
;	startref -- use text between here and an @ref or @pageref as link text
;      (equivalent to @latex but turned off when @ref or @pageref is 
;      encountered. But unlike @latex, text is processed by Scribe as well.)
;	doinclude -- process the next include file (default is to ignore)
;   endcodef -- end a code definition (used for completion list)
; citations are translated to links according to data in citations.lsp
;	(the actual bibliographic references must be hand-translated to HTML)
; alter *bgcolorinfo* to set background color of html documents
; set *8.3* to change from "indx.html" to "INDX.HTM" format for file names

;; *doc-title* can be set before loading s2h to give title to PanDoc:
(cond ((boundp '*doc-title*))
      (t (setf *doc-title* nil)))
(setf *need-version* nil)            ;; used to extract Version from nyquistman.mss
(setf *capture* nil)
(setf *captured* nil)
(setf skip-count 0) ;; counts down when new lines are written. A crude
      ;; way to handle \\needspace{}. In Latex, \\needspace{} cancels any
      ;; pending \\needspace{}, so if you output \\needspace{6 lines}, you
      ;; set skip-count to 6, and the next time you want to output
      ;; \\needspace{}, if skip-count has not counted down to <= 0, you
      ;; just omit writing the \\needspace{} line because a previous one
      ;; is still probably in effect.
(cond ((boundp '*doc-version*))
      (t (setf *doc-version* nil)))  ;; used to extract Version from nyquistman.mss
(cond ((boundp '*need-author*))
      (t (setf *need-author* nil)))  ;; used to extract Author from nyquistman.mss
(cond ((boundp '*doc-author*))
      (t (setf *doc-author* nil)))   ;; used to extract Author from nyquistman.mss

;; label-list shall look like this:
;;    ((label-symbol "label text" file-name number) ...)
;; where label-symbol is the scribe label,
;;   and the rest is used like this:
;;      <a href = "file-name#number">label text</a>
(cond ((boundp '*label-list*)
       (setf *previous-label-list* *label-list*)))
(setf *label-list* nil)

;; number of parts must be obtained from previous pass
(cond ((boundp '*number-of-parts*)
       (setf *previous-number-of-parts* *number-of-parts*)))
(setf *number-of-parts* nil)

(cond ((not (boundp '*citations*))
       (load (strcat (current-path) "/citations.lsp"))))

;; set this to trace parser input to help locate errors in input file
(cond ((not (boundp '*token-trace*))
       (setf *token-trace* nil)))
(cond ((not (boundp '*output-trace*))
       (setf *output-trace* nil)))
(cond ((not (boundp '*translate-trace*))
       (setf *translate-trace* nil)))
(cond ((not (boundp '*s2h-dbg*))
       (setf *s2h-dbg* nil)))

(cond ((and (boundp '*inf*) *inf*)
       (format t "closing *inf*\n")
       (setf *inf* nil)))
(cond ((and (boundp '*footnotefile*) *footnotefile*)
       (format t "closing *footnotefile*\n")
       (close *footnotefile*)
       (setf *footnotefile* nil)))
(cond ((and (boundp '*outf*) *outf*)
       (setf *outf* nil)))

;; when @pragma(defn) is seen, this flag is set to t, when
;; the following @index(...) is seen, the flag is cleared and
;; the index is entered as a definition of the term, meaning 
;; the referenced location will go to the head of the list.
;; Later, when we want to find help for a term, we know that
;; the help information is located by the first entry in the
;; index for this term.
(setf definition-flag nil)

(defun open-paren ()
  (let ((tok (get-token)))
    (cond ((member tok '(#\( #\{ #\[ #\<))
           (push tok paren-stack))
          (t
           (push tok *next-tokens*)
           ;; if no open paren, then fake open and close
           (push #\( paren-stack)
           (push #\) *next-tokens*)))))


;;    ((label-symbol "label text" file-name number) ...)
(defun write-label ()
  (let ((body (get-body "label" nil)))
    (setf body (list-to-symbol body))
    (if *lt* (lt-write-label body))
    (push (list body *name-reference* *file-name* *name-number*)
          *label-list*)))

(defun do-make ()
  (let ((what (get-body "make" nil)))
    (setf what (string-upcase
		(list-to-string what)))
    (setf what (intern what))
    (cond ((member what '(manual article))
	   (setf *document-type* what))
	  (t
	   (format t "Unknown document type: ~A, assuming ARTICLE~%" what)
	   (setf *document-type* 'ARTICLE)))))

(defun get-body (what spacesok)
  (prog (tok body)
loop
    (setf tok (get-token))
    (cond ((not (characterp tok))
	   (format t "expected characters in ~A: ~A\n" what tok))
	  ((paren-match tok)
	   (pop paren-stack)
	   (return (reverse body)))
	  ((and (not spacesok) (eql #\Space tok)))
	  ((null tok)
	   (format t "early end of file\n")
	   (break))
	  (t
	   (push tok body)))
    (go loop)))


(defun write-code ()
  (if *ht* (format *outf* "<code>"))
  (if *lt* (lt-begin-code))
  (translate)
  (if *ht* (format *outf* "</code>"))
  (if *lt* (lt-end-code)))


(defun write-codef ()
  (progv '(*codef*) '(t) (write-code) (codef-complete)))

(defun write-quotation ()
  (format *outf* "<blockquote>")
  (if *lt* (lt-begin-quotation))
  (translate)
  (format *outf* "</blockquote>")
  (if *lt* (lt-end-quotation)))

(defun write-normal ()
  (format *outf* "<span style=\"font-style:normal\">")
  (if *lt* (lt-begin-normal))
  (translate)
  (format *outf* "</span>")
  (if *lt* (lt-end-normal)))

(defun write-t ()
    (if *ht* (format *outf* "<tt>"))
    (if *lt* (lt-begin-code))
    (translate)
    (if *ht* (format *outf* "</tt>"))
    (if *lt* (lt-end-code)))

(defun list-to-string (body)
  (let ((str ""))
    (dolist (ch body)
	    (setf str (strcat str (string ch))))
    str))

(defun list-to-symbol (body)
  (setf cmd (intern (string-upcase (list-to-string body)))))

;; index-casify -- first letter upper, all others lower
;;
(defun index-casify (item)
  (nstring-downcase item)
  (nstring-upcase item :start 0 :end 1))

(defun enter-index-item (item def-flag)
  (setf item (index-casify item))
  (setf *index-number* (1+ *index-number*))
  (let ((entry (assoc item *index-list* :test #'equal))
        (ref (list *file-name* *index-number*))
        front)
    (cond ((and entry def-flag)
           ;; put new reference at beginning of list
           (rplacd entry (cons ref (cdr entry))))
          (entry
	   (nconc entry (list ref)))
	  (t
	   (push (list item ref) *index-list*)))
    *index-number*))

(setf dble nil)
(defun write-index (&optional def-flag)
  (let (body str n)
    (setf body (get-body "index" t))
    (setf str (list-to-string body))
    (if *token-trace* (format t "Index item: ~A~%" str))
    (cond (*ht*
           (setf n (enter-index-item str def-flag))
           (format *outf* "<a name=\"index~A\"></a>" n)))
    (if *lt* (lt-write-index str))))

(defun index-sort-fn (a b)
  (string< (car a) (car b)))

(defun mysort (list)
  (setf list (cons 'header list))
  (prog (first ptr result)
loop1
	(cond ((null (cdr list))
	       (return result)))
;	(display "loop1" list result)
	(setf first list)
	(setf ptr (cdr first))
loop
	(cond ((null (cdr ptr))
	       (go gotone))
	      ((index-sort-fn (cadr first) (cadr ptr))
	       (setf first ptr)))
	(setf ptr (cdr ptr))
	(go loop)
gotone
	(push (cadr first) result)
	(rplacd first (cddr first))
	(go loop1)))

(cond ((not (boundp '*bgcolorinfo*))
       (setf *bgcolorinfo* " bgcolor=\"ffffff\"")))
(cond ((not (boundp '*8.3*))
       (setf *8.3* nil)))

(defun html-file (namestring)
  (cond (*8.3*
	 (setf namestring (string-upcase namestring))
	 (cond ((> (length namestring) 8)
		(setf namestring (subseq namestring 0 8))))
	 (strcat namestring ".HTM"))
	(t
	 (strcat namestring ".html"))))

(defun generate-index-entry (entry target)
  (let (n)
    ;; if only one target for index entry, make the word be a link
    (cond ((= 1 (length (cdr entry)))
	   (format *outf* "<a href=\"~A#index~A\"~A>~A</a><br>~%"
		   (caadr entry) (cadadr entry) target (car entry)))
	  (t
	   (setf n 1)
           ; (car entry) may have formatting commands...
	   (format *outf* "~A" (car entry))
	   (dolist (ref (cdr entry))
	     (format *outf* " <a href = \"~A#index~A\"~A>~A</a> "
		     (car ref) (cadr ref) target n)
	     (setf n (1+ n)))
	   (format *outf* "<br>\n")))
    ; (if *lt* (md-generate-index-entry entry target))
  ))

;; HAS-ALPHA - trim non-alpha from beginning of key and capitalize
;;  returns: nil if no alpha chars, *rslt* = key without non-alpha prefix 
;;
(defun has-alpha (key)
  (while (and (> (length key) 0)
	      (not (both-case-p (char key 0))))
    (setf key (subseq key 1)))
  (setf *rslt* key)
  ;(display "non-alpha" key)
  (cond ((> (length key) 0)
	 (setf *rslt* (index-casify *rslt*))
	 t)
	(t nil)))


;; FIX-NON-ALPHA -- duplicate non-alpha entries that have an alpha char
;;    and make all entries have sort keys
(defun fix-non-alpha (lis)
  (let (rslt)
    (dolist (entry lis)
      (setf key (car entry))
      (push (cons key entry) rslt)
      (cond ((both-case-p (char key 0)) nil) ;; normal alpha key
	    ((has-alpha key)
	     (push (cons *rslt* entry) rslt))))
    rslt))


;; GENERATE-INDEX -- generate the index and write to outf
;;
;; if frame-flag is provided, it signals that the output goes
;; to an already open file, which is in fact the value of frame-flag
;; This would be the second time generate-index is called, so the 
;; preprocessing and sorting is not needed when frame-flag is a file
;;
(defun generate-index (&optional frame-flag)
  (let (n target initial-char)
    (setf *outf* (if frame-flag frame-flag
		 (open (strcat *dest-dir* (html-file "indx"))
		       :direction :output)))
    (cond (frame-flag t)
	  (t
	   (incf *part-number*)
	   (display "generate-index top" *part-number*)
	   (write-chapter-links t t)
	   (setf *index-list* (fix-non-alpha *index-list*))
	   (setf *index-list* (mysort *index-list*))
	   (generate-index-chars)
       (generate-top "Index" *outf*)
	   (format *outf* "<body~A>" *bgcolorinfo*)))
    (setf initial-char (car *index-chars*))
    (format *outf* "<h2>Index</h2>~%")
    (setf target (if frame-flag " target=m" ""))
    (format *outf* "<a name=\"index-~A\"><h2>~A</h2></a>~%" 
	    initial-char initial-char)
    (format *outf* "<a href=\"#top\">TOP</a><br>~%")
    ;; generate all non-alpha entries
    (dolist (entry *index-list*)
      (cond ((both-case-p (char (car entry) 0)))
	    (t
	     (generate-index-entry (cdr entry) target))))
    ;; generate A - Z
    (dolist (entry *index-list*)
      ;; put headings for every new starting character
      (let ((c (char (car entry) 0)))
	(cond ((not (both-case-p c)) nil) ; ignore non-alphas here
	      ((eql initial-char (char (car entry) 0))) ; no change
	      (t
	       (setf initial-char (char (car entry) 0))
	       (format *outf* "<a name=\"index-~A\"><h2>~A</h2></a>~%" 
		       initial-char initial-char)
	       (format *outf* "<a href=\"#top\">TOP</a><br>~%")))
	(if (both-case-p c)
	    (generate-index-entry (cdr entry) target))))
    (cond (frame-flag t)
	  (t (display "generate-index bottom" *part-number*)
	     (write-chapter-links nil t)
	     (format t "closing indx.html\n")
	     (close *outf*)))
    ; (if *lt* (md-generate-index))
    ))


;; GENERATE-TOC-2 -- generate table of contents body
;;
;; if frame-flag is t, generate html for left frame 
;; output to *outf*
;;
(defun generate-toc-2 (lis &optional frame-flag)
  (format *outf* "<ul>\n")
  (dolist (lis1 lis)
    (let (lis2 target)
      (setf lis1 (reverse lis1))
      (setf lis2 (car lis1))
;      (display "part1" lis2)
      (if frame-flag (setf target " target=\"m\""))
      (format *outf* "<li><a href = \"~A#~A\"~A>~A</a>\n"
	      ;filename  ;ref-number ;target ;name
	      (car lis2) (cadr lis2) target  (caddr lis2))
      (setf lis2 (cdr lis1))
;      (display "part2" lis2)
      (cond (lis2
	     (generate-toc-2 lis2 frame-flag)))))
  (format *outf* "</ul>\n")
  ; (if *lt* (md-generate-toc-2))
  )


(defun generate-toc (&optional frame-flag)
  (format *outf* "<a name = \"toc\">\n")
  (format *outf* "<h2>Table of Contents</h2>\n")
  (generate-toc-2 (reverse *chapter-list*) frame-flag)
  (cond (frame-flag t)
	(t 
	 (format *outf* "<ul><li><a href = \"~A\">Index</a>\n</ul>" 
		 (html-file "indx"))))
  ; (if *lt* (md-generate-toc))
  )


(defun get-primary (str)
  (get-string-after "primary" str))

(defun get-secondary (str)
  (get-string-after "secondary" str))

(defun get-string-after (key str)
  (let ((n (string-search key str))
	m)
    (cond ((null n)
	   (format t "get-string-after could not find ~A in ~A~%"
		   key str)
	   (break)))
    (setf n (+ n (length key)))
    ;(display "a" n)
    (setf n (string-search "\"" str :start n))
    ;(display "b" n)
    (cond ((null n)
	   (format t "get-string-after: no quote after ~A in ~A~%"
		   key str)
	   (break)))
    (setf n (1+ n))
    (setf m (string-search "\"" str :start n))
    ;(display "c" m)
    (cond ((null m)
	   (format t
	    "get-string-after: no close quote after ~A in ~A~%"
		   key str)
	   (break)))
    (subseq str n m)))


(defun write-indexsecondary ()
  (let (body str n)
    (setf body (get-body "secondaryindex" t))
    (setf str (list-to-string body))
    (if *token-trace* (format t "SecondaryIndex item: ~A~%" str))
    (if *lt* (lt-write-indexsecondary str n))
    (setf str (strcat (get-primary str) ", " (get-secondary str)))
    (setf n (enter-index-item str nil))
    (format *outf* "<a name=\"~A~A\">" str n)
  ))


(defun read-begin-command ()
  (let (cmd n
	(body (get-body "begin" nil)))
    (setf cmd (list-to-string body))
    (setf n (string-search "," cmd))
    (cond (n
	   (format t
	"warning: dropping parameters after comma in @begin(~A)~%"
		   cmd)
	   (setf cmd (subseq cmd 0 n))))
    (setf cmd (intern (string-upcase cmd)))
    (push cmd paren-stack)
    cmd))


(defun read-end-command ()
  (let (cmd
	(body (get-body "end" nil)))
    (setf cmd (list-to-symbol body))
    (list 'end cmd)))

(defun read-pragma-command ()
  (let ((body (get-body "pragma" nil)) cmd)
    (setf cmd (list-to-symbol body))
    cmd))

(defun remove-version-from (s)
  (let ((loc (string-search "Version" s)))
    (cond ((= loc 0)
           (string-trim " " (subseq s 7)))
          (t s))))

(defun write-style (style)
    (format *outf* "<~A>" style)
    (if *lt* (lt-begin-style style))
    (cond ((and (equal style "b") (or *need-version* *need-author*))
           (setf *capture* t)
           (setf *captured* "")))
    (translate)
    (format *outf* "</~A>" style)
    (if *lt* (lt-end-style style))
    (cond ((and (equal style "b") (or *need-version* *need-author*))
           (setf *capture* nil)
           (cond (*need-version*
                  (setf *doc-version* (remove-version-from *captured*))
                  (setf *need-version* nil)
                  (display "write-style" *need-version*)
                  (setf *need-author* t)
                  (display "write-style" *need-author*)
                  )
                 (*need-author*
                  (setf *doc-author* *captured*)
                  (setf *need-author* nil)
                  (display "write-style" *need-author*)
                  )))))


(defun write-bold-italic ()
    (format *outf* "<b><i>")
    (if *lt* (lt-begin-bold-italic))
    (translate)
    (format *outf* "</i></b>")
    (if *lt* (lt-end-bold-italic)))


(defun write-superscript ()
  (if *ht* (format *outf* "<sup ~A>" *ht*))
  (if *lt* (lt-begin-superscript))
  (translate)
  (if *ht* (format *outf* "</sup>"))
  (if *lt* (lt-end-superscript)))


(defun write-subscript ()
  (if *ht* (format *outf* "<sub>"))
  (if *lt* (lt-begin-subscript))
  (translate)
  (if *ht* (format *outf* "</sub>"))
  (if *lt* (lt-end-subscript)))


(defun write-dd ()
  (cond (*description*
         (if *ht* (format *outf* "<dd>"))
         (if *lt* (lt-write-dd)))))

(defun write-description ()
  (format *outf* "<dl>\n<dt>")
  (if *lt* (lt-begin-write-description))
  (progv '(*description*) '(t)
    (translate))
  (format *outf* "</dl>")
  (if *lt* (lt-end-write-description)))

(defun write-fdescription ()
  (format *outf* "<div style=\"padding-left: 0em;\">")
  (setf *in-div* t)
  (setf *desc-indent* 0)
  (if *lt* (lt-begin-write-fdescription))
  (progv '(*fdescription*) '(t)
    (progv '(*codef*) '(t)
      (translate)
      (codef-complete)))
  (if *in-div* (format *outf* "</div>~%"))
  (setf *in-div* nil)
  (if *lt* (lt-end-write-fdescription)))


(defun write-fgroup ()
  (progv '(*fgroup*) '(t)
;  (format *outf* " enter fgroup ")
    (translate)))
;  (format *outf* " exit fgroup ")

(defun write-pdescription ()
  ; (cond ((not *codef*) (break "not *codef* in pdescription")))
  (progv '(*pdescription* *desc-indent* *codef*)
         (list t (+ *desc-indent* 2) nil)
    ; (break "write-pdescription")
    (if *in-div* (format *outf* "</div>"))
    (format *outf* "<div style=\"padding-left: ~Aem;\">" *desc-indent*)
    (if *lt* (lt-begin-write-pdescription))
    (codef-complete) ;; finish preceding function completion string
    (translate)
    (format *outf* "</div>")
    (if *lt* (lt-end-write-pdescription))))

(defun fdescription-break ()
  (format *outf* "<br>\n")
  (if (and *lt* (not *html*)) (lt-fdescription-break)))

(defun pdescription-break ()
  (if (and *lt* (not *html*)) (lt-pdescription-break))
  (if *ht*
      (format *outf* "</div>~%<div style=\"padding-left: ~Aem;\">" *desc-indent*)))

;(defun start-pdescription-item ()
;  (if *lt* (lt-start-pdescription-item)))

(defun close-paren-p (tok)
  (or (and (listp tok)
	   (eq (car tok) 'end))
      (paren-match tok)))

    
(defun paren-match (p2)
  (let ((p1 (car paren-stack)))
    (or (and (eql p2 #\))
	     (eql p1 #\())
	(and (eql p2 #\])
	     (eql p1 #\[))
	(and (eql p2 #\})
	     (eql p1 #\{))
	(and (eql p2 #\>)
	     (eql p1 #\<))
	(and (listp p2)
	     (eq (car p2) 'end)
	     (eq p1 (cadr p2))))))

(defun skip-it ()
  (progv '(*ht* *lt*) '(nil nil)
    (translate)))

(defun write-titlepage ()
  (translate))


(defun write-titlebox ()
  (translate))

(defun write-majorheading ()
  (let ()
    (if *doc-title* (display "WARNING: *doc-title* is already defined" *doc-title*))
    (setf *doc-title* (get-body "majorheading" t))
    (setf *doc-title* (list-to-string *doc-title*))
    (setf *need-version* t)
    (display "write-majorheading" *need-version*)
    (format *outf* "<h1>~A</h1>" *doc-title*)
    (if *lt* (lt-majorheading *doc-title*))))

(defun write-h2 ()
  (format *outf* "<h2>")
  (if *lt* (lt-begin-h2))
  (translate)
  (format *outf* "</h2>")
  (if *lt* (lt-end-h2)))

(defun write-h3 ()
  (format *outf* "<h3>")
  (if *lt* (lt-begin-h3))
  (translate)
  (format *outf* "</h3>")
  (if *lt* (lt-end-h3)))

(defun write-h4 ()
  (format *outf* "<h4>")
  (if *lt* (lt-begin-h4))
  (translate)
  (format *outf* "</h4>")
  (if *lt* (lt-end-h4)))

(defun write-paragraph ()
  (let ((body (get-body "paragraph" t)))
    (setf body (list-to-string body))
    (setf *name-reference* (format nil "\"~A\"" body))
    (setf *name-number* (1+ *name-number*))
    (push (list (list *file-name* *name-number* body)) *paragraph-list*)
    (format *outf* "<a name = \"~A\"><h5>~A</h5></a>" *name-number* body)
    (if *lt* (lt-write-paragraph *name-number* body))
  ))

(defun finish-subsection ()
  (push *paragraph-list* *subsection-list*)
  (setf *paragraph-list* nil))

(defun write-subsection ()
  (let ((body (get-body "subsection" t)))
    (setf body (list-to-string body))
    (setf *name-reference* (format nil "\"~A\"" body))
    (setf *name-number* (1+ *name-number*))
    (cond (*paragraph-list*
	   (finish-subsection)))
    (setf *paragraph-list*
	  (list (list *file-name* *name-number* body)))
    (format *outf* "<a name = \"~A\"><h4>~A</h4></a>" *name-number* body)
    (if *lt* (lt-write-subsection *name-number* body)) ))


(defun finish-section ()
  (cond (*paragraph-list*
	 (finish-subsection)))
  (push *subsection-list* *section-list*)
  (setf *subsection-list* nil))

(defun write-section ()
  (let ((body (get-body "section" t)))
    (setf body (list-to-string body))
    (format t "SECTION ~A~%" body)
    (cond ((member 'fdescription paren-stack)
           (break 
            (format nil "Section ~A within fdescription environment" body))))
    (setf *name-reference* (format nil "\"~A\"" body))
    (setf *name-number* (1+ *name-number*))
    (cond (*subsection-list*
	   (finish-section)))
    (setf *subsection-list*
	  (list (list *file-name* *name-number* body)))
    (format *outf* "<a name = \"~A\"><h3>~A</h3></a>" *name-number* body)
    (if *lt* (lt-write-section *name-number* body)) ))

(defun previous-part-file-name ()
  (cond ((> *part-number* 2)
	 (html-file (format nil "part~A" (- *part-number* 2))))
	(t *dest*)))

;; called when new chapter is encountered,
;; lastflag = nil means write a Next Section link
(defun finish-chapter ()
  (cond (*subsection-list*
	 (finish-section)))
  (push *section-list* *chapter-list*)
  (setf *section-list* nil)
  ; *dest* links get added after table of contents
  (cond ((not (eq *outf* *rootf*))
         (write-chapter-links)
         (format *outf* "</body></html>\n")
         (if *lt* (lt-finish-chapter)))))



(defun write-title-page-links ()
  (format *outf* "<hr>\n")
  (let (name)
    (setf name (html-file "part1"))
    (format *outf* "<a href = \"~A\">Next Section</a> | " name))
  (format *outf* "<a href = \"~A\">Index</a> | " (html-file "indx"))
  (format *outf* "<hr>\n"))


(defun write-chapter-links (&optional top-flag index-flag title-flag)
  (display "write-chapter-links" *part-number* *previous-number-of-parts*)
  (let ((lastflag (eql *part-number* *previous-number-of-parts*)))
    (if top-flag t (format *outf* "<hr>\n"))
    (cond ((not title-flag)
	   (format *outf* "<a href = \"~A\">Previous Section</a> | "
		   (previous-part-file-name))))
    (cond (index-flag nil)
	  ((not lastflag)
	   (let (name)
	     (setf name (html-file (if title-flag "part1" 
				     (format nil "part~A" *part-number*))))
	     (format *outf* "<a href = \"~A\">Next Section</a> | " name)))
	  (t
	   (format *outf* "<a href = \"~A\">Next Section (Index)</a> | "
		   (html-file "indx"))))
    (format *outf* "<a href = \"~A#toc\">Table of Contents</a> | " *dest*)
    (cond ((and (not lastflag) (not index-flag))
	   (format *outf* "<a href = \"~A\">Index</a> | " (html-file "indx"))))
    (format *outf* "<a href = \"~A\">Title Page</a>\n" *dest*)
    (if top-flag (format *outf* "<hr>\n"))))


(defun set-html-title ()
  (setf *doc-title* (get-body "htmltitle" t))
  (setf *doc-title* (list-to-string *doc-title*)))


(defun write-chapter (numbered)
  (let ((body (get-body "chapter" t)))
    (format t "CHAPTER ~A~%" body)
    (setf body (list-to-string body))
    (setf *name-reference* (format nil "\"~A\"" body))
    (setf *name-number* (1+ *name-number*))
    (cond (*section-list*
	   (finish-chapter)))
    (cond ((eq *outf* *rootf*))
	  (t
	   (format t "Closing ~A~%" *file-name*)
	   (close *outf*)))
    (setf *file-name* (html-file (format nil "part~A" *part-number*)))
    (setf *section-list*
	  (list (list *file-name* *name-number* body)))
    (setf *outf* (open (strcat *dest-dir* *file-name*)
		       :direction :output))
    (setf *part-number* (1+ *part-number*))
    (generate-top body *outf*)
    (format *outf* "<body~A>\n" *bgcolorinfo*)
    (if *lt* (lt-write-chapter body numbered))
    (write-chapter-links t)
    (format *outf* "<a name = \"~A\"><h2>~A</h2></a>" *name-number* body)))

(defun write-detail ()
  (format *outf* "<small>")
  (if *lt* (lt-begin-detail))
  (translate)
  (format *outf* "</small>\n")
  (if *lt* (lt-end-detail)))

(defun write-appendix ()
  (let ((body0 (get-body "appendix" t)))
    (setf body0 (list-to-string body0))
    (setf *name-reference* (format nil "\"~A\"" body0))
    (setf body (format nil "Appendix ~A: ~A" *appendix-number* body0))
    (setf *appendix-number* (1+ *appendix-number*))
    (setf *name-number* (1+ *name-number*))
    (cond (*section-list*
	   (finish-chapter)))
    (cond ((eq *outf* *rootf*))
	  (t
	   (format t "Closing ~A~%" *file-name*)
	   (close *outf*)))
    (setf *file-name* (html-file (format nil "part~A" *part-number*)))
    (setf *section-list*
	  (list (list *file-name* *name-number* body)))
    (setf *outf* (open (strcat *dest-dir* *file-name*)
		       :direction :output))
    (setf *part-number* (1+ *part-number*))
    (write-chapter-links t)
    (generate-top body *outf*)
    (format *outf* "<a name = \"~A\"><h2>~A</h2></a>" *name-number* body)
    (if *lt* (lt-appendix *name-number* body0)) ))


(defun write-blankspace ()
  (cond (*fdescription*
         ; what we want is <br><br><dt> after pdescription environment
         ; to set up the next fdescription, but we may have output one <br>
         ; after the last pdescription.  By using (linebreak) we avoid a
         ; third <br> which would output too much space.
         ;;;;; with new code, it seems we just need one <br>
         (linebreak 'write-blankspace)
         ; (format *outf* "<br><dt>")
         ; (and *lt* (not *html*) (lt-blankspace-in-fdescription))
         )
        (t
         (format *outf* "<p>")
         (if *lt* (lt-paragraph))))
  (get-body "blankspace" t))


;(defun write-center ()
;  (linebreak)
;  (translate)
;  (linebreak))

(defun write-newpage ()
  (translate))

;; PARAGRAPH - take appropriate action after blank line
;;
;; ignore multiple calls in sequence
;;
(setf *paragraph* nil)
(defun paragraph ()
  (if *s2h-dbg* (display "paragraph" *fgroup* *paragraph*))
  (cond ((null *paragraph*)
         (if *s2h-dbg* (display "paragraph" *itemize* *description* *pdescription*
                                *fgroup* *fdescription* *example*))
         (cond (*itemize*
                (format *outf* "\n<li>")
                (if *lt* (lt-list-paragraph)))
               (*description*
                (format *outf* "<br><br>\n<dt>")
                (if *lt* (lt-description-paragraph)))
               (*pdescription*
                (pdescription-break))
               ((or *fdescription* *fgroup*)
                (fdescription-break))
               (*example*
                (format *outf* "~%~%"))
               (t
                (format *outf* "\n<p>\n")
                (if (and *lt* (not *html*)) (lt-paragraph))))
;         (setf *paragraph* t)
) ))


(defun linebreak (source)
  (if *s2h-dbg* (display "linebreak" source *linebreak* *line-empty* *display*))
  (cond ((and ; (null *paragraph*)
              (null *linebreak*))
         (format *outf* "<br>\n")
         (if (and *lt* (not *html*) (not *fgroup*))
             (lt-linebreak source))
         (setf *linebreak* t)))
  (setf *line-empty* t))

;; separate paragraphs in a description environment
;; TODO: needs translation for HTML too
(defun force-paragraph ()
  (cond (*lt* (lt-linebreak 'force-paragraph) (lt-linebreak 'force-paragraph))))

(defun write-smallcaps ()
  (let ((sc *smallcap*))
    (setf *smallcap* t)
    (translate)
    (setf *smallcap* sc)))


(defun write-cite ()
  (let (body link)
    (setf body (get-body "cite" nil))
    (setf body (list-to-symbol body))
    (format t "Citation: ~A ~%" body)
    (setf link (assoc body *citations*))
    (cond ((null link)
           (format t "Undefined citation: ~A~%" body)
           (break "citation problem"))
          (t
           (format *outf* " <a href = \"~A\">~A</a>"
                   (cadr link) (caddr link))
           (if *lt* (lt-write-cite link))) )))


(defun write-ref (tok)
  (let ((body (get-body "ref" nil)) ref (file ""))
    (cond (*startref*
           (setf *startref* nil)
           (setf *latex* nil))) ;; cancel pragma that omits "page" from html
    (setf body (list-to-symbol body))
    (setf ref (assoc body *previous-label-list*))
    (cond ((null ref)
           (format t "warning: undefined label ~A~%" body))
          (t
           (cond ((not (equal (caddr ref) *file-name*))
                       (setf file (caddr ref))))
           (format *outf* "<a href = \"~A#~A\">~A</a>"
                   file (cadddr ref) (cadr ref))
           (if *lt* (lt-write-ref ref tok)) ))))

(setf *example* nil)
(defun write-example ()
  (progv '(*example*) '(t)
    (if *ht* (format *outf* "<p><pre>"))
    (if *lt* (lt-begin-example))
    (translate)
    (if *ht* (format *outf* "</pre></p>\n"))
    (if *lt* (lt-end-example))))
   
(defun write-enumerate ()
  (let ((itemize *itemize*))
    (format *outf* "<ol>\n<li>")
    (if *lt* (lt-begin-enumerate))
    (setf *itemize* t)
    (translate)
    (format *outf* "</ol>")
    (if *lt* (lt-end-enumerate))
    (setf *itemize* itemize)))

(defun write-itemize ()
  (let ((itemize *itemize*))
    (format *outf* "<ul>\n<li>")
    (if *lt* (lt-begin-itemize))
    (setf *itemize* t)
    (translate)
    (format *outf* "</ul>")
    (if *lt* (lt-end-itemize))
    (setf *itemize* itemize)))



(defun write-format ()
  (progv '(*display*) '(t)
    ; hopefully, we'll get a linebreak to separate the text as does scribe
    (translate)
    (format *outf* "<p>~%") ; separate the text from what follows
    (if *lt* (lt-end-write-format))))


(defun write-display ()
  (progv '(*display*) '(t)
    (if *ht* (format *outf* "<blockquote>"))
    (if (and *lt* (not *html*)) (lt-begin-blockquote))
    (setf *linebreak* t) ; it's automatic with blockquote,
    ; so we do this to suppress an extra <br>
    (translate)
    (if *ht* (format *outf* "</blockquote>"))
    (if (and *lt* (not *html*)) (lt-end-blockquote))))


(defun write-center ()
  (progv '(*display*) '(t)
    (format *outf* "<blockquote>")
    (if (and *lt* (not *html*)) (lt-begin-center))
    (setf *linebreak* t) ; it's automatic with blockquote,
    ; so we do this to suppress an extra <br>
    (translate)
    (format *outf* "</blockquote>")
    (if (and *lt* (not *html*)) (lt-end-center))))


(defun write-figure ()
  (format *outf* "<hr>")
  (if *lt* (lt-begin-figure))
  (translate)
  (format *outf* "<hr>")
  (if *lt* (lt-end-figure)))


(defun write-dash (tok)
  (cond ((get-body "dash" nil)
         (break "non-nil returned from get-body \"dash\" nil")))
  ;; We need to output-char #\- to put the dash into HTML,
  (format *outf* (if (eq tok 'itemsep) "&ndash;" "-"))
  (and *lt* (not *html*) (lt-dash tok)))


(defun write-html ()
  (setf *html* t)
  (translate)
  (setf *html* nil))

(defun write-latex ()
  (setf *latex* t)
  (translate)
  (setf *latex* nil))
  

(defun write-foot ()
  (let ((outf *outf*)
        (name (html-file "foot")))
    (format *outf* " <a href = \"~A#foot~A\">(Footnote ~A)</a> "
            name *footnote-number*  *footnote-number*)
    (if *lt* (lt-write-footnote name *footnote-number*))
    (cond ((null *footnotefile*)
           (setf *footnotefile* (open (strcat *dest-dir* name) :direction :output))
           (generate-top "Footnotes" *outf*)
           (format *footnotefile* "<body~A>\n<h2>Footnotes</h2>\n" *bgcolorinfo*)
           ))
    (setf *outf* *footnotefile*)
    (format *outf* "<a name = \"foot~A\"> ~A. </a>" *footnote-number* *footnote-number*)
    (setf *footnote-number* (1+ *footnote-number*))
    (translate)
    (format *outf* "\n<P>\n")
    (setf *outf* outf)
    (if *lt* (lt-footnote-end))))


(defun write-fillcaption ()
  (paragraph)
  (format *outf* "<b>Figure ~A: </b>" *figure-number*)
  (if *lt* (lt-write-fillcaption *figure-number*))
  (translate)
  (if *lt* (lt-end-fillcaption))  )


(defun do-include ()
  (setf *include* t))

(defun write-include ()
  (let ((body (get-body "include" nil))
        file)
    (cond (*include*
           (setf *include* nil)
           (setf body (list-to-string body))
           (setf file *inf*)
           (setf *inf* (open (strcat *include-prefix* body)))
           (cond ((null *inf*)
                  (format t "Could not open include file ~A\n" body)
                  (break))
                 (t
                  (format t "Processing include file ~A\n" body)
                  (translate)
                  (format t "Closing include file ~A\n" body)
                  (close *inf*)
                  (setf *inf* file))) ))))

(defun do-tag ()
  (let ((body (get-body "tag" nil)) sym)
    (setf sym (list-to-symbol body))
    (push (list sym (format nil "~A" *figure-number*)
                *file-name* *name-number*)
          *label-list*)
    (if *lt* (lt-label sym))
    (setf *figure-number* (1+ *figure-number*)) ))


(defun write-math ()
  (translate))

(defun write-underline ()
  (format *outf* "<u>")
  (if *lt* (lt-begin-underline))
  (translate)
  (format *outf* "</u>\n")
  (if *lt* (lt-end-underline)))

; initiated by @startscribe()
; and ended by @endscribe()
;
(defun do-scribeonly ()
  (setf *lt* nil)
  (setf *ht* nil))

(defun do-all ()
  (setf *lt* *generate-latex*)
  (setf *ht* t))

(defun do-htmlonly ()
  (setf *lt* nil))

(defun do-latexonly ()
  (setf *ht* nil))

(defun do-all ()
  (setf *lt* *generate-latex*)
  (setf *ht* t))

(defun do-startref ()
  (setf *latex* t)
  (setf *startref* t))

(defun write-title ()
  (translate))


(defun do-comment ()
  ; skip everything until matching paren
  (prog (tok)
loop
    (setf tok (get-comment-token))
    ; (display "do-comment" tok paren-stack)
    (cond ((and (close-paren-p tok)
                (paren-match tok))
           (display "do-comment" paren-stack)
           (pop paren-stack)
           (format t "do-comment done\n")
           (return)))
    (go loop)))


(defun output-char (c)
 (let (s)
  (cond ((or *ht* *lt*)
         (cond ((and *smallcap*
                     (alpha-char-p c))
                (setf c (char-upcase c))))
         (cond (*s2h-dbg*
                (display "output-char dbg" *display* *pdescription* c *line-empty*)))
         (cond ((and (or *display* *pdescription* *fgroup*)
                     (eql c #\Newline) (not *line-empty*))
                ; (break "does this get called?") -- YES
                (linebreak 'display-pdescription))
               ((member c *white-space*))
               (t
                ; (setf *paragraph* nil)
                (setf *linebreak* nil)))
         ;; detect when there is non-whitespace on a latex output line
         ;;    so that we know when to use \\ to force a line break
         (cond ((and (not *html*) (not (member c *white-space*)))
                (if (and *display* dble)
                    (display "output-char" *display* *line-empty* c))
                (setf *line-empty* nil)))
         (cond (*html* (write-char c *outf*)) ; no translation if we're in an @html(...) section
               (*latex*) ; suppress latex-only output
               ((eq c #\<)
                (write-char #\& *outf*)
                (write-char #\l *outf*)
                (write-char #\t *outf*)
                (write-char #\; *outf*))
               ((eq c #\>)
                (write-char #\& *outf*)
                (write-char #\g *outf*)
                (write-char #\t *outf*)
                (write-char #\; *outf*))
               ((eq c #\&)
                (write-char #\& *outf*)
                (write-char #\a *outf*)
                (write-char #\m *outf*)
                (write-char #\p *outf*)
                (write-char #\; *outf*))
               ((not *latex*) ; no output if we're in an @latex(...) section
                (write-char c *outf*)))
         ; (display "output-char" c)
         (setf s (string c))
         (cond ((and *lt* *lt-started* (not *html*) (not *need-version*) 
                     (not *need-author*))
                (if *output-trace* (format t "[~A]" c))
                (cond 
                      ; latex environment outputs all text as is:
                      (*latex* (write-char c *ltoutf*))
                      ((equal s "~") (format *ltoutf* "\\textasciitilde{}"))
                      ((equal s "\\") (format *ltoutf* "\\textbackslash{}"))
                      ((equal s "^") (format *ltoutf* "\\textasciicircum{}"))
                      ((equal s "[") (format *ltoutf* "{[}"))
                      ((equal s "]") (format *ltoutf* "{]}"))
                      ((string-search s "#_%&${}")
                       (write-char #\\ *ltoutf*)
                       (write-char c *ltoutf*))
                      ((not (and *skip-white-space* (member c *white-space*)))
                       (if (equal c #\Newline) (setf *line-empty* t))
                       ;; stop skipping white-space when we get a 
                       ;;  non-white-space char
                       (if (not (member c *white-space*)) 
                           (setf *skip-white-space* nil))
                       ;; code templates get too much space if we don't 
                       ;;  revert to textrm:
                       (cond ((and (or (equal c #\Space) (equal c #\Newline))
                                   *font-is-tt* (or *fdescription-item-is-open*
                                                    *description-item-is-open*))
                              ;; output a regular roman space
                              (format *ltoutf* "\\textrm{ }"))
                             (t
                              (if (equal c #\Newline) 
                                  (setf skip-count (1- skip-count)))
                              (write-char c *ltoutf*))))
                      (t
                       ; (display "skipping output of" c)
                       )) )) )) ))


(defun dashes (n) (if (< n 1) "" (strcat "_" (dashes (1- n)))))

(setf *translate-depth* 0)

(defun translate ()
  (setf *translate-depth* (1+ *translate-depth*))
  (if *translate-trace* 
    (format t "~A~A enter ~A:~%" (dashes *translate-depth*)
                           *translate-depth* (car paren-stack)))
  (if *token-trace* (format t "(~A " *translate-depth*))
  (prog (tok (env-name (car paren-stack)))
loop
    (setf tok (get-token))
    (cond ((and tok (symbolp tok) *token-trace*)
           (format t "[~A]" tok)))
    (cond ((null tok) (go ret))
          ((close-paren-p tok)
           (cond ((paren-match tok)
                  (pop paren-stack))
                 (t
                  (format t "unmatched end: ~A~%" tok)
                  (break)))
           (go ret))
          ((eq tok 'debugon)
           (setf *s2h-dbg* t))
          ((eq tok 'debugoff)
           (setf *s2h-dbg* nil))
          ((eq tok 'traceon)
           (setf *output-trace* t))
          ((eq tok 'traceoff)
           (setf *output-trace* nil))
          ((member tok '(startscribe scribeonly))
           (do-scribeonly))
          ((member tok '(endscribe all))
           (do-all))
          ((eq tok 'htmlonly)
           (do-htmlonly))
          ((eq tok 'latexonly)
           (do-latexonly))
          ((eq tok 'altdef)
           (translate))
          ((characterp tok)
           ;; output completion hints file
           (cond (*codef* (codef-char tok)))
           (if *capture* (setf *captured* (strcat *captured* (string tok))))
           (output-char tok))
          ((eq tok 'label)
           (write-label))
          ((member tok '(code smallcode xlcode))
           (write-code))
          ((eq tok 'codef)
           (write-codef))
          ((member tok '(index htmlindex))
           (write-index definition-flag)
           (setf definition-flag nil))
          ((eq tok 'indexsecondary)
           (write-indexsecondary))
          ((eq tok 'indexdef) ;; this is obsolete now
           (write-index t))
          ((eq tok 'r)
           (write-normal))
          ((eq tok 'i)
           (write-style "i"))
          ((eq tok 'b)
           (write-style "b"))
          ((eq tok 'titlepage)
           (write-titlepage))
          ((eq tok 'titlebox)
           (write-titlebox))
          ((member tok '(majorheading chapnum))        ; chapnum is for Tomayko
           (write-majorheading))
          ((member tok '(skip blankspace)) ; skip is for Tomayko
           (write-blankspace))
          ((eq tok 'center)
           (write-center))
          ((member tok '(verse))
           (write-display))        ;; seems to be best substitute for center
          ((eq tok 'format)
           (write-format))
          ((eq tok 'newpage)
           (write-newpage))
          ((eq tok 'c)
           (write-smallcaps))
          ((member tok '(text quotation))
           (write-quotation))
          ((eq tok 't)
           (write-t))
          ((eq tok 'title)
           (write-title))
          ((eq tok 'htmltitle)
           (set-html-title))
          ((eq tok 'unnumbered)
           (write-chapter nil))
          ((eq tok 'chapter)
           (write-chapter t))
          ((eq tok 'appendix)
           (write-appendix))
          ((eq tok 'detail)
           (write-detail))
          ((eq tok 'section)
           (write-section))
          ((eq tok 'heading)
           (write-h3))
          ((eq tok 'subsection)
           (write-subsection))
          ((eq tok 'subheading)
           (write-h4))
          ((eq tok 'paragraph)
           (write-paragraph))
          ((eq tok 'forceparagraph)
           (force-paragraph))
          ((eq tok 'cite)
           (write-cite))
          ((member tok '(ref pageref))
           (write-ref tok))
          ((eq tok 'startref)
           (do-startref))
          ((eq tok 'p)
           (write-bold-italic))
          ((eq tok 'plus)
           (write-superscript))
          ((eq tok 'minus)
           (write-subscript))
          ((eq tok 'html)        ; special way to insert html
           (write-html))
          ((eq tok 'latex)
           (write-latex))        ; special way to insert latex
          ((member tok '(example programexample))
           (write-example))
          ((eq tok 'figure)
           (write-figure))
          ((eq tok 'fillcaption)
           (write-fillcaption))
          ((eq tok 'tag)
           (do-tag))
          ((eq tok 'doinclude)
           (do-include))
          ((eq tok 'include)
           (write-include))
          ((eq tok 'backslash)
           (write-dd))
          ((eq tok 'math)
           (write-math))
          ((member tok '(foot note))        ; note is for Tomayko
           (codef-complete)
           (setf *codef* nil)
           (write-foot))
          ((member tok '(description fndefs))
           (write-description))
          ((eq tok 'fdescription)
           (write-fdescription))
          ((eq tok 'pdescription)
           (write-pdescription))
          ((eq tok 'fgroup)
           (write-fgroup))
          ((eq tok 'itemize)
           (write-itemize))
          ((eq tok 'enumerate)
           (write-enumerate))
          ((eq tok 'display)
           (write-display))
          ((member tok '(subtract itemsep dash ndash))
           (cond ((eq tok 'itemsep)
                  (codef-complete)
                  (setf *codef* nil))) ;; end completion string
           (write-dash tok))
          ((eq tok 'star)
           (linebreak 'star))
          ((eq tok 'new-paragraph)
           (paragraph))
          ((member tok '(y value definefont))
           (format t "warning: omitting @~A[] text\n" tok)
           (skip-it))
          ((member tok '(one colon shout slash bar bibliography))
           (format t "ignoring ~A\n" tok))
          ((eq tok 'make)
           (do-make))
          ((member tok '(device libraryfile style commandstring modify define
                         use counter pageheading set graphic mult tabset 
                         tabclear textform part))
           (skip-it))
          ((eq tok 'comment)
           (do-comment))
          ((eq tok 'defn)
           (setf definition-flag t))
          ((eq tok 'endcodef)
           (codef-complete))
          ((eq tok 'stopcodef)
           (setf *codef* nil))
          ((eq tok 'startcodef)
           (setf *codef* t))
          ((member tok '(u ux))
           (write-underline))
          ((member tok '(group flushleft)) ; ignore it, flushleft is for Tomayko
           (translate))
          ((eq tok 'needspace3) ;; latex only -- avoids orphans
           (if *lt* (needspace 3)))
          (t
           (format t "unrecognized token: ~A~%" tok)
           (break)))
    (go loop)
ret
  (if *token-trace* (format t ")"))
  (if *translate-trace* 
    (format t "~A~A exit ~A:~%" (dashes *translate-depth*)
                           *translate-depth* env-name))
  (setf *translate-depth* (- *translate-depth* 1))
        
  ))

(defun manualp () (eq *document-type* 'manual))


(format t "To run, call:\n")
(format t "   (g \"scribe source directory\" ; omit the trailing slash\n")
(format t "      \"scribe file\"     ; omit the .mss suffix\n")
(format t "      \"html directory\"  ; omit the trailing slash\n")
(format t "      \"html root file\"  ; omit the .html suffix\n")
(format t "      [t]) ; optional flag generates html with frames\n")

(defun g (sourcedir source destdir dest &optional frame-flag latex-output)
  ;; *generate-latex* tells whether we are generating ANY latex file for this run
  ;; *lt* is a value that gets toggled on and off during the run
  (setf *generate-latex* latex-output)
  (setf *codef-capture* "")
  (setf *codef-list* nil)
  (setf *figure-number* 1)
  (setf *index-number* 1)
  (setf *appendix-number* 1)
  (setf *footnotefile* nil)
  (setf *footnote-number* 1)
  (setf *name-number* 0)
  (setf *name-reference* nil)
  (setf *omit* nil)
  (setf *html* nil)    ; says we're in section to dump html literally
  (setf *latex* nil)   ; says we're in section to dump latex literally
  (setf *ht* nil)      ; enable translations from scribe to html output
  (setf *lt* latex-output) ; enable translations from scribe to latex output
  (setf *include* nil) ; process include or not?
  (setf *next-tokens* nil)
  (setf *smallcap* nil)
  ; (setf *paragraph* t)
  (setf *linebreak* t)
  (setf *line-empty* t)
  (setf *itemize* nil)
  (setf *display* nil)
  (setf *description* nil)
  (setf *description-item-is-open* nil)  ; says \item[ in description has no ] yet
  (setf *fdescription-item-is-open* nil)
  (setf *font-is-tt* nil) ; says \begin{tt} not closed with \end{tt} yet
  (setf *fdescription* nil)
  (setf *fgroup* nil)
  (setf *pdescription* nil)
  (setf *codef* nil) ; says we're defining a function, add to completion list
  (setf *skip-white-space* nil) ; used in latex to remove space after \item[
  ;; paragraph-list is initialized to:
  ;;  ((file number sectionname))
  ;; and after each paragraph, expands, e.g.:
  ;;  ((file number paragraphname) (file number sectionname))
  (setf *paragraph-list* nil)
  ;; likewise, the subsection-list is initialized to:
  ;;  ((file number sectionname))
  ;; and after each subsection, expands, e.g.:
  ;;  (((file number paragraphname) (file number subsectionname)) 
  ;;   (file number sectionname)))
  (setf *subsection-list* nil)
  ;; and so on...
  (setf *section-list* nil)
  (setf *chapter-list* nil)
  ; *dest* is the root HTML file
  (setf *dest* (if frame-flag
                   (html-file "title")
                   (html-file dest)))
  ; *file-name* is the current HTML file
  (setf *file-name* *dest*)
  (format t "Destination HTML file: ~A~%" *file-name*)
  (setf *part-number* 1)

  ; *startref* is set when @startref() is encountered, at which
  ; point text is omitted until a reference is encountered, at
  ; which point text is resumed.
  (setf *startref* nil)

  ;; index-list shall look like this:
  ;;    (("key" "indexterm" (filename num) (filename num) ...)
  ;;     ("key" "indexterm" (filename num) (filename num) ...) ...)
  ;; where key is the sortkey (usually indexterm) and indexterm is
  ;; what gets printed. This allows non-alphabetic items to be sorted
  ;; according to their first alphabetic character, e.g. *rslt* can
  ;; be sorted as "rslt" as well as "*rslt*"
  ;;
  (setf *index-list* nil)

  (setf paren-stack nil)

  ;; run this twice to get labels right
  ;; if *label-list* is NIL, it may be because we reloaded this file,
  ;; in which case *previous-label-list* has already been copied from
  ;; *label-list*
  (cond (*label-list*
         (setf *previous-label-list* *label-list*))
        ((not (boundp '*previous-label-list*))
         (setf *previous-label-list* nil)))
  (cond (*number-of-parts*
         (setf *previous-number-of-parts* *number-of-parts*)))
  (cond ((not (boundp '*previous-number-of-parts*))
         (setf *previous-number-of-parts* 0)))

  (setf *inf* (open (strcat sourcedir "/" source ".mss")))
;               "test.mss"))
; "/afs/cs/user/rbd/doc/man/nyquist/nyquistman.mss"))
  (setf *include-prefix* (strcat sourcedir "/"))
  (setf *dest-dir* (strcat destdir "/"))
  (setf *outf* (open (strcat *dest-dir* *file-name*) :direction :output))
  (cond (*generate-latex* (setf *ltoutf* (open (strcat source ".tex") 
                                   :direction :output))
              (display "opened *ltoutf*" *ltoutf*)
              (print "******************************************************") ))
  (setf *codef-file* (open (strcat *dest-dir* "NyquistWords.txt" )
                      :direction :output))
  (setf *rootf* *outf*)
  (display "g-before translate" *outf* *rootf* *inf*)
  ; (if *lt* (lt-start))
  (translate)
  (display "g-after translate" *outf* *rootf* *inf*)
  (format t "g: closing *inf*\n")
  (close *inf*)
  (setf *inf* nil)
  (format t "g: closing *outf*\n")
  (finish-chapter)
  (cond ((not (eq *outf* *rootf*))
         (close *outf*)
         (setf *outf* nil)))
  (cond (*footnotefile*
         (format *footnotefile* "</body></html>\n")
         (close *footnotefile*)))
  (setf *footnotefile* nil)
  (setf *outf* *rootf*)
  (display "g-before toc" *outf* *rootf* *inf* (manualp))
  (setf *number-of-parts* *part-number*)
  (cond ((manualp)
         (setf *part-number* 1) ;; reset for title page
         (display "g-before-write-title-page-links")
         (write-title-page-links)
         (display "g-before-generate-toc")
         (generate-toc)
         (display "g-before-chapter-links")
         (write-chapter-links nil nil t)))
  (format *rootf* "</body></html>\n")
  (format t "g: closing *rootf*\n")
  (close *rootf*)
  (setf *outf* (setf *rootf* nil))
  (cond ((manualp)
         (setf *part-number* *number-of-parts*) ;; restore from above
         (generate-index))
        ; if this is not a manual, there are no chapters, so there are no
        ; links between chapters or table of contents, so index cannot be
        ; reached, so index is not generated.  This is bad if there really
        ; are index terms!!!!
        (*index-list*
         (format t "WARNING: NO INDEX IS BEING GENERATED, THIS IS A BUG~%")))
  (cond (frame-flag
         (if (not (manualp)) 
             (error "frame option only works with manual document types"))
         (generate-home *doc-title* (strcat *dest-dir* (html-file dest)))
         (generate-guide (strcat *dest-dir* (html-file "guide")))
         ))
  (codef-close)
  (cond ((and *lt* *ltoutf*) (lt-end-document) (close *ltoutf*)))
)


(defun generate-top (title outf)
    (format outf "<html><head><title>~A</title>~%<link rel=\"stylesheet\" " title)
    (format outf "type=\"text/css\" href=\"nyquiststyle.css\">~%</head>~%"))


(defun generate-home (title filename)
  (let ((outf (open filename :direction :output)))
    (generate-top title outf)
    (format outf "<frameset cols=\"1*, 2*\">~%")
    (format outf "<frame scrolling=auto src=guide.html frameborder=0>~%")
    (format outf 
            "<frame name=m scrolling=auto src=title.html frameborder=0>~%")
    (format outf "<noframes><body><p>This browser does not support frames.~%")
    (format outf "<p><a href=title.html>The no-frames version is here.</a>~%")
    (format outf "</body></noframes></frameset></html>~%")
    (close outf)))


;; FIND-ALPHA-AND-NON-ALPHA -- sublist with only alpha chars
;;     return the range of the rest in *rslt*, e.g. "!-~"
;;
(defun find-alpha-and-non-alpha (lis)
  (let (prev first last rslt)
    (dolist (c lis)
      (cond ((eql prev c))
            (t
             (setf prev c)
             (cond ((both-case-p prev)
                    (push prev rslt))
                   ((null first)
                    (setf first prev)
                    (setf last prev))
                   (t
                    (setf last prev))))))
    ;; nil if first and last are not defined
    (setf *rslt* (and first last (strcat (string first) "-" (string last))))
    rslt))
        

(defun generate-index-chars ()
  (let (term initial prev)
    (setf *index-chars* nil)
    ;; compute the list of characters for index
    (dolist (entry *index-list*)
      (setf term (car entry))
      (setf initial (char term 0))
      (cond ((eql prev (char term 0)))
            (t
             (setf prev initial)
             (push prev *index-chars*))))
    (setf *index-chars* (reverse *index-chars*))
    (setf *index-chars* (find-alpha-and-non-alpha *index-chars*))
    (setf *index-chars* (reverse *index-chars*))
    (if *rslt* (push *rslt* *index-chars*))))


(defun generate-guide (dest)
  (let (term initial prev index-chars non-alpha)
    (setf *outf* (open dest :direction :output))
    (generate-top "Links" *outf*)
    (format *outf* "<body>~%<h3><a name=top>~%")
    (dotimes (n (length *index-chars*))
      (setf c (nth n *index-chars*))
      (format *outf* "<a href=\"#index-~A\">~A</a>~%" c c)
      (if (zerop (rem (1+ n) 9))
          (format *outf* "<br>~%")))
    (format *outf* "</h3>~%")
    (generate-toc t)
    (generate-index *outf*)
    (format *outf* "</body></html>~%")
    (close *outf*)))


(defun alpha-char-p (c)
  (let ((cc (char-code c)))
    (or (and (>= cc (char-code #\a))
             (<= cc (char-code #\z)))
        (and (>= cc (char-code #\A))
             (<= cc (char-code #\Z))))))


(defun process-comment-at ()
  (let (c cmd)
    (read-char *inf*)        ; read the @
    (setf c (peek-char nil *inf*))
    (cond ((alpha-char-p c)
           (setf cmd (read-command))
           (cond ((eq cmd 'end)
                  (open-paren)
                  (read-end-command))
                 (t #\z))))))


(defun process-at ()
  (let (c cmd)
    (read-char *inf*)        ; read the @
    (setf c (peek-char nil *inf*))
    (cond ((eql c #\@) (read-char *inf*))
          ((eql c #\\) (read-char *inf*) 'backslash)
          ((eql c #\/) (read-char *inf*) 'slash)
          ((eql c #\1) (read-char *inf*) 'one)
          ((eql c #\+) (read-char *inf*)
                       (open-paren)
                       'plus)
          ((eql c #\*) (read-char *inf*) 'star)
          ((eql c #\-) (read-char *inf*)
                       (open-paren)
                       'minus)
          ((eql c #\:) (read-char *inf*) 'colon)
          ((eql c #\!) (read-char *inf*) 'shout)
          ((eql c #\|) (read-char *inf*) 'bar)
          ((alpha-char-p c)
           (setf cmd (read-command))
           (cond ((eq cmd 'begin)
                  (open-paren)
                  (read-begin-command))
                 ((eq cmd 'end)
                  (open-paren)
                  (read-end-command))
                 ((eq cmd 'pragma)
                  (open-paren)
                  (read-pragma-command))
                 (t
                  (open-paren)
                  cmd)))
          (t (format t "unexpected char after @: ~A~%" c)
             (break)))))


(defun process-newline ()
  (let (c)
    (read-char *inf*) ; read the newline
    (setf c (peek-char nil *inf*))
    (cond ((eql c #\Newline)
           (while (eql (peek-char nil *inf*) #\Newline)
                  (read-char *inf*))
           'new-paragraph)
          (t #\Newline))))


;; READ-COMMAND -- read command after an @
;
(defun read-command ()
  (let ((command ""))
    (while (alpha-char-p (peek-char nil *inf*))
           (setf command
            (strcat command (string (read-char *inf*)))))
    (intern (string-upcase command))))



;;           (read-char *inf*))

(defun get-token ()
  (let ((c (peek-char nil *inf*))
        result)

    ;; allow others to force next token:
    (cond (*next-tokens*
           (setf result (car *next-tokens*))
           (setf *next-tokens* (cdr *next-tokens*)))
          (t
           (setf result
                 (cond ((eql c #\@) (process-at))
                       ((eql c #\Newline) (process-newline))
                       ((eql c #\`) ;; double `` -> "
                        (read-char *inf*)
                        (setf c (peek-char nil *inf*))
                        (cond ((eql c #\`) 
                               (read-char *inf*)
                               #\")
                              (t #\`)))
                       ((eql c #\') ;; double '' -> "
                        (read-char *inf*)
                        (setf c (peek-char nil *inf*))
                        (cond ((eql c #\')
                               (read-char *inf*)
                               #\")
                              (t #\')))
                       (t (read-char *inf*))))))
    (if *token-trace* (format t "~A" result))
    result
    ))


(defun get-comment-token ()
  (let ((c (peek-char nil *inf*))
        result)
    (setf result
          (cond ((eql c #\@) (process-comment-at))
                (t (read-char *inf*))))
    (if *token-trace* (format t "->~A " result))
    result
    ))

(setf *white-space* '(#\Newline #\Tab))
(defun codef-char (c)
  (if (member c *white-space*) (setf c #\Space))
  (setf *codef-capture* (strcat *codef-capture* (string c))))

(defun codef-complete ()
;  (write-char #\) *codef-file*)
   (let (index ccd)
     ;; ccd is for debugging
     (setf ccd (or (string-search "write-char" *codef-capture*)
                   (string-search "sound-warp" *codef-capture*)))
     (if ccd (display "codef-complete" *codef-capture*))
     ;; remove [lisp] and [sal] and everything after it
     (if (setf index (string-search "[lisp]" *codef-capture*))
         (setf *codef-capture* (subseq *codef-capture* 0 index)))
     (if (setf index (string-search "[sal]" *codef-capture*))
         (setf *codef-capture* (subseq *codef-capture* 0 index)))
     ;; trim extra blanks
     (while (setf index (string-search "  " *codef-capture*))
       (setf *codef-capture* (strcat (subseq *codef-capture* 0 index)
                                     (subseq *codef-capture* (1+ index)))))
     ;; replace "expr..." with "expr ..." Want to replace all occurences,
     ;; so scan string, starting at previous spot + 2 (to get beyond the
     ;; inserted space character) until nothing is found
     (setf index 0)
     (while (and (< (+ index 2) (length *codef-capture*))
                 (setf index (string-search "..." *codef-capture* 
                                               :start (+ 2 index))))
       (cond ((and index (> index 0) 
                     (not (eq (char *codef-capture* (1- index)) #\Space)))
              (setf *codef-capture* 
                    (strcat (subseq *codef-capture* 0 index) " "
                            (subseq *codef-capture* index))))))
     (if ccd (display "codef-complete 2" *codef-capture*))
     ;; trim blanks after open bracket/comma and before close paren
     (while (setf index (string-search "[, " *codef-capture*))
       (setf index (+ 2 index))
       (setf *codef-capture* (strcat (subseq *codef-capture* 0 index)
                                     (subseq *codef-capture* (1+ index)))))
     (if ccd (display "codef-complete 3" *codef-capture*))
     (while (setf index (string-search " )" *codef-capture*))
       (setf *codef-capture* (strcat (subseq *codef-capture* 0 index)
                                     (subseq *codef-capture* (1+ index)))))
     )
    
   ;; trim blanks
   (setf *codef-capture* (string-trim " " *codef-capture*))
   ;; translate &key to example format
   (cond ((or (string-search "&key" *codef-capture*)
              (string-search "&optional" *codef-capture*))
          (setf *codef-capture* (codef-expand *codef-capture*))))
   (if (and (> (length *codef-capture*) 0) ; must be non-empty
            (not (eq (char *codef-capture* 0) #\:))) ; ignore messages
       (push (string-downcase
              (convert-sal-to-lisp *codef-capture*))
             *codef-list*))
   ;; THIS DOES NOT SEEM TO DO ANYTHING
   ;; trim leading open paren
   (if (and (> (length *codef-capture*) 0)
            (eq (char *codef-capture* 0) #\())
       (setf *codef-capture* (subseq *codef-capture* 1)))

   (setf *codef-capture* ""))


(defun convert-sal-to-lisp (codef)
  ;(format *codef-file* "sal-to-lisp |~A|~%" codef)
  ;; some of these strings are already lisp. The SAL strings have an
  ;; open paren after the function call
  (cond ((eq (char codef 0) #\()
         (setf codef (subseq codef 1)))
        ((string-search "(" codef)
         (setf codef (do-convert-sal-to-lisp codef))))
  codef)

(defun do-convert-sal-to-lisp (codef)
  ;; take out initial "(" and replace with space
  ;; delete each subsequent comma
  ;; for each colon, flip it to a keyword (key: -> :key)
  (let ((p (string-search "(" codef)))
    ;; replace "(" with " "
    (setf codef (strcat (subseq codef 0 p) " " (subseq codef (1+ p))))
    ;; delete commas:
    (setf p (string-search "," codef))
    (while p
      (setf codef (strcat (subseq codef 0 p) (subseq codef (1+ p))))
      (setf p (string-search "," codef)))
    ;; for each colon, flip it to a keyword
    (setf p (string-search ":" codef))
    (while p
      ;; back up
      (setf q (1- p))
      (while (not (member (char codef q) '(#\Space #\[)))
        (setf q (1- q)))
      (incf q)
      (setf codef (strcat (subseq codef 0 q) ":"
                          (subseq codef q p) (subseq codef (1+ p))))
      (setf p (string-search ":" codef :start (1+ p)))
      '(display "do-cstl" p codef))
    codef))
                  

(defun split (s)
  (let (rslt (token "") c)
    (dotimes (i (length s))
      (setf c (char s i))
      (cond ((eq c #\Space)
             (cond ((> (length token) 0)
                    (push token rslt)
                    (setf token ""))))
            ((member c '(#\( #\)))
             (cond ((> (length token) 0)
                    (push token rslt)
                    (setf token "")))
             (push (string c) rslt))
            (t
             (setf token (strcat token (string c))))))
    (cond ((> (length token) 0)
           (push token rslt)))
    (reverse rslt)))


(defun colonize (word)
  (if (eq (char word 0) #\:)
      word
      (strcat ":" word)))

(defun uncolonize (word)
  (if (eq (char word 0) #\:)
      (subseq word 1)
      word))


(defun codef-expand (s)
  (let (words (r "") mode (space ""))
    (setf words (split s))
    (dolist (word words)
      (cond ((equal word "&key")
             (setf mode :key))
            ((equal word "&optional")
             (setf mode :optional))
            ((equal word "(")
             (setf r (strcat r space word))
             (setf space ""))
            ((equal word ")")
             (setf r (strcat r word))
             (setf space " "))
            ((eq mode :key)
             (setf r (strcat r space "[" (colonize word) " "
                             (uncolonize word) "]"))
             (setf space " "))
            ((eq mode :optional)
             (setf r (strcat r space "[" word "]"))
             (setf space " "))
            (t
             (setf r (strcat r space word))
             (setf space " "))))
    r))

(defun find-url-for (item)
  (let (i entry)
    ;; first, extract the initial symbol from item
    (setf i (string-search " " item))
    (cond (i (setf item (subseq item 0 i))))
    ;; trim trailing (or any) close parenthesis
    (setf i (string-search ")" item))
    (cond (i (setf item (subseq item 0 i))))
    ;; fix the case/capitalization to match what's in *index-list*
    (setf item (index-casify item))
    ;; look it up
    (setf entry (assoc item *index-list* :test #'equal))
    ;; return the URL
    (cond (entry
           ;; entry has (sort-key, name, ref1, ref2, ...)
           (setf entry (third entry)) ;; get first reference
           (format nil "~A#index~A" (first entry) (second entry)))
          (t
           (format t "WARNING: ~A IS NOT IN INDEX~%" item)
           "home.htm"))))
           

(defun codef-close () ;; called to close file
  ;; nothing written yet
  (let (url)
    (setf *codef-list* (sort *codef-list* #'string<))
    (dolist (item *codef-list*)
      (setf url (find-url-for item))
      (format *codef-file* "~A~%~A~%" item url))
    (close *codef-file*)))
