;; latex.lsp -- write latex file
;;
;; Roger B. Dannenberg
;; March 2015

(setf *lt-preamble* '(
"\\documentclass[11pt,]{report}"
"\\setcounter{secnumdepth}{3}"
"\\usepackage[T1]{fontenc}"
"% \\usepackage{lmodern} -- results in texttt{AR} being too small!"
"\\usepackage{amssymb,amsmath}"
"\\usepackage{makeidx}"
"\\makeindex"
"\\usepackage{ifxetex,ifluatex}"
"\\usepackage{fixltx2e} % provides \\textsubscript"
"\\usepackage{mathptmx}% Times Roman font"
"\\usepackage[scaled=.90]{helvet}% Helvetica, served as a model for arial"
"\\usepackage[dvipsnames*,svgnames]{xcolor}"
"\\usepackage{titlesec}"
"\\usepackage{fancyvrb}"
"\\DefineVerbatimEnvironment{verbatim}{Verbatim}{xleftmargin=.3in}"
"\\usepackage{etoolbox}"
"\\usepackage{graphicx}"
"% patch for fancyvrb "
"\\makeatletter "
"\\patchcmd{\\FV@ListVSpace}{\\@topsepadd\\topsep}{}{}"
"\\makeatother "
""
"% patch for verbatim "
"% \\makeatletter"
"% \\preto{\\@verbatim}{\\topsep=0pt \\partopsep=0pt }"
"% \\makeatother"
""
"\\usepackage{alltt}"
"\\usepackage{changepage}"
"\\newenvironment{expl}{\\begin{adjustwidth}{0.4cm}{0cm}\\begin{minipage}{\\textwidth}\\begin{alltt}}"
; \\begin{adjustwidth}{0.4cm}{}
;                       \\end{adjustwidth}\\end{alltt}}"
"                      {\\end{alltt}\\end{minipage}\\end{adjustwidth}}"
"% \\newcommand{\\sectionbreak}{\\clearpage}"
"\\usepackage{placeins}"
"\\usepackage{needspace}"
"\\usepackage[tocindentauto]{tocstyle}"
"\\usepackage{enumitem}"
"\\newenvironment{description2}"
"{\\begin{description}[style=nextline,font=\\normalfont]}{\\end{description}}"
"\\newenvironment{description3}"
"{\\begin{description}[font=\\normalfont,noitemsep,nolistsep]}{\\end{description}}"
"\\makeatletter "
"%\\renewcommand{\\l@section}{\\@dottedtocline{1}{1.5em}{2.6em}}"
"%\\renewcommand{\\l@subsection}{\\@dottedtocline{2}{4.0em}{3.6em}}"
"%\\renewcommand{\\l@subsubsection}{\\@dottedtocline{3}{7.4em}{4.5em}}"
"\\makeatother "
""
"% use microtype if available"
"\\IfFileExists{microtype.sty}{\\usepackage{microtype}}{}"
"% use upquote if available, for straight quotes in verbatim environments"
"\\IfFileExists{upquote.sty}{\\usepackage{upquote}}{}"
"\\ifnum 0\\ifxetex 1\\fi\\ifluatex 1\\fi=0 % if pdftex"
"  \\usepackage[utf8]{inputenc}"
"\\else % if luatex or xelatex"
"  \\usepackage{fontspec}"
"  \\ifxetex"
"    \\usepackage{xltxtra,xunicode}"
"  \\fi"
"  \\defaultfontfeatures{Mapping=tex-text,Scale=MatchLowercase}"
"  \\newcommand{\\euro}{€}"
"    \\setmainfont{Georgia}"
"    \\setsansfont{Arial}"
"    \\setmonofont{Lucida Console}"
"\\fi"
""
"\\usepackage[includeheadfoot,top=1in, bottom=1in, left=1in, right=1in]{geometry}"
""
"\\usepackage{fancyhdr}"
"\\pagestyle{fancy}"
"\\pagenumbering{arabic}"
))

(setf *lt-preamble2* '(
"\\cfoot{}"
"\\rfoot{\\thepage}"
""
"\\ifxetex"
"  \\usepackage[setpagesize=false, % page size defined by xetex"
"              unicode=false, % unicode breaks when used with xetex"
"              xetex]{hyperref}"
"\\else"
"  \\usepackage[unicode=true]{hyperref}"
"\\fi"
"\\hypersetup{breaklinks=true,"
"            bookmarks=true,"
"            pdfauthor={},"
"            pdftitle={},"
"            colorlinks=true,"
"            urlcolor=blue,"
"            linkcolor=magenta,"
"            pdfborder={0 0 0}}"
"\\setlength{\\parindent}{0pt}"
"\\setlength{\\parskip}{6pt plus 2pt minus 1pt}"
"\\setlength{\\emergencystretch}{3em}  % prevent overfull lines"
""
))

(setf *lt-preamble3* '(
""
"%\\definecolor{sec-color}{gray}{0.4}"
"%\\definecolor{subsec-color}{gray}{0.3}"
"%\\definecolor{subsubsec-color}{gray}{0.25}"
""
""
"\\titleformat{\\chapter}"
"  {\\normalfont\\sffamily\\huge\\bfseries%\\color{MidnightBlue}"
"}"
"  {\\thechapter}{24pt}{}"
""
"\\titleformat{\\section}"
"  {\\normalfont\\sffamily\\LARGE\\bfseries%\\color{MidnightBlue}"
"}"
"  {\\thesection}{20pt}{}"
"\\titleformat{\\subsection}"
"  {\\normalfont\\sffamily\\Large\\bfseries%\\color{MidnightBlue}"
"}"
"  {\\thesubsection}{16pt}{}"
"\\titleformat{\\subsubsection}"
"  {\\normalfont\\sffamily\\large\\bfseries%\\color{MidnightBlue}"
"}"
"  {\\thesubsubsection}{14pt}{}"
""
"\\begin{document}"
"\\maketitle"
""
"\\newpage"
""
"{"
"\\hypersetup{linkcolor=black}"
"\\setcounter{tocdepth}{3}"
"\\tableofcontents"
"}"
))


(defun write-lines (lines)
  (dolist (line lines)
          (princ line *ltoutf*)
          (terpri *ltoutf*)))


;; tells when preamble has been written, no other output until then
(setf *lt-started* nil)

(defun lt-title-info(title version)
  (write-lines *lt-preamble*)
  (format *ltoutf* "\\lhead{\\itshape ~A}~%" title)
  (format *ltoutf* "\\chead{}~%\\rhead{\\itshape{\\nouppercase{\\leftmark}}}")
  (format *ltoutf* "\\lfoot{v ~A}~%" version) ; e.g. "3.10"
  (write-lines *lt-preamble2*)
  (format *ltoutf* "\\title{\\Huge{\\textbf{~A}}}~%" title)
  (format *ltoutf* "\\author{\\textbf{\\normalsize{Version ~A}}}~%" version)
  (format *ltoutf* 
          "\\date{\\textbf{\\large{Copyright 2015 by Roger B. Dannenberg}}\\\\~
           \\vspace{1 in}\\today\\\\~
           \\vspace{2 in}Carnegie Mellon University\\\\~
           \\vspace{10 pt}School of Computer Science\\\\~
           \\vspace{10 pt}Pittsburgh, PA 15213, U.S.A.}~%")
  (write-lines *lt-preamble3*)
  (setf *lt-started* t))


(defun lt-end ()
  (format *ltoutf* "}"))

(defun lt-begin-code ()
  (setf *font-is-tt* t)
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\texttt{")))

(defun lt-end-code ()
  (setf *font-is-tt nil)
  (if (or *html* *omit*) nil
      (format *ltoutf* "}")))

(defun lt-begin-quotation ()
  (format *ltoutf* "\\quotation{"))

(setfn lt-end-quotation lt-end)

(defun lt-begin-example ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\begin{expl}")))

(defun lt-end-example ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\end{expl}")))

(defun lt-begin-enumerate ()
  (format *ltoutf* "\\begin{enumerate}\\item ~%"))

(defun lt-end-enumerate ()
  (format *ltoutf* "\\end{enumerate}"))

(defun lt-begin-itemize ()
  (format *ltoutf* "\\begin{itemize}~%\\item "))

(defun lt-list-paragraph ()
  (format *ltoutf* "~%\\item~%"))

(defun lt-end-itemize ()
  (format *ltoutf* "\\end{itemize}"))

(defun lt-begin-normal ()
  (format *ltoutf* "{\\fontfamily{cmr}\\selectfont "))

(setfn lt-end-normal lt-end)

(defun latex-escape (str)
  (let (s cs) ;; s will accumulate escaped chars of str
    (dotimes (i (length str))
      ;; map each char of str into an escaped version
      (setf cs (assoc (char str i) 
                      '((#\% "\\%") (#\& "\\&") (#\\ "\\textbackslash{}")
                        (#\$ "\\$") (#\# "\\#") (#\_ "\\_")
                        (#\{ "\\{") (#\} "\\}") (#\[ "{[}") (#\] "{]}")
                        (#\^ "\\textasciicircum{}")
                        (#\~ "\\textasciitilde{}"))))
      ;; if no substitute found, use original converted to a string
      (setf cs (if cs (second cs) (string (char str i))))
      (setf s (cons cs s)))
    (apply #'strcat (reverse s))))

(defun lt-write-index (str)
  (format *ltoutf* "\\index{~A}" (latex-escape str)))

(defun lt-write-indexsecondary (str n)
  (format *ltoutf* "\\index{~A!~A}" (latex-escape (get-primary str))
                                    (latex-escape (get-secondary str))))

(defun lt-linebreak (source)
  (if *s2h-dbg* (display "lt-linebreak" *line-empty* *fdescription* *pdescription*))
  (cond ((and *fdescription* (not *pdescription*) (not (eq source 'write-blankspace)))
         (display "lt-linebreak in fdesc" source)
         (break "lt-linebreak in fdescription"))
        (*pdescription* nil)
        ((not *line-empty*)
         (setf *line-empty* t)
         (format *ltoutf* "\\\\"))
        (t 
         (display "lt-linebreak when" *line-empty*)
         (format *ltoutf* "\\hspace*{1em} \\\\"))))

(defun lt-write-cite (link)
  (format *ltoutf* "\\cite{~A}" (car link)))

(defun lt-write-label (sym)
  (format *ltoutf* "\\label{~A}" sym))

(defun lt-write-ref (ref tok)
  (cond ((eq tok 'ref)
         (format *ltoutf* "\\ref{~A}" (car ref)))
        ((eq tok 'pageref)
         (format *ltoutf* "\\pageref{~A}" (car ref)))
        (t (break "lt-write-ref got bad tok argument"))))

(defun style-to-latex (style)
  (cond ((equal style "b") "textbf")
        ((equal style "i") "textit")
        (t style)))

(defun lt-begin-style (style)
  (format *ltoutf* "\\~A{" (style-to-latex style)))

(defun lt-end-style (style)
  (lt-end))

(defun lt-begin-bold-italic ()
  (format *ltoutf* "\\textbf{\\textit{"))

(defun lt-end-bold-italic ()
  (format *ltoutf* "}}"))

(defun lt-begin-underline ()
  (format *ltoutf* "\\underline{"))

(setfn lt-end-underline lt-end)

(defun lt-begin-write-dd ()
  (format *ltoutf* "<begin dd>"))

(defun lt-end-write-dd ()
  (format *ltoutf* "<end dd>"))

(defun lt-write-dd ()
  ; (format *ltoutf* "<lt-write-dd>")
  (cond (*description-item-is-open*
         (if *token-trace* (format t "LT-WRITE-DD:CLOSE"))
         (setf *skip-white-space* nil)
         (setf *description-item-is-open* nil))
        (t
         (break "lt-write-dd: *description-item-is-open* is false - were closing a description term, but there was no term to begin with. Turn on *token-trace* and continue to see where you are.")))
  (format *ltoutf* "]"))

(defun lt-break-dt ()
  (if (not *description-item-is-open*)
      (break "lt-break-dt: *description-item-is-open* is false"))
  (setf *description-item-is-open* nil)
  (setf *skip-white-space* nil)
  (format *ltoutf* "]"))

(defun write-fdescription-item ()
  ; (let ((item (get-output-stream-string *ltoutf*)))
  ;   (setf *ltoutf* *save-ltoutf*)
  ;   (setf *save-ltoutf* nil) ; mark as invalid
  ;   (setf *fdescription-item-capture* nil)
  ;   (format *ltoutf* "\\item[~A] " (string-trim "\n\t " item))))
  nil)

(defun write-pdescription-item ()
  ;(let ((item (get-output-stream-string *ltoutf*)))
  ;  (setf *ltoutf* *save-ltoutf*)
  ;  (setf *save-ltoutf* nil) ; mark as invalid
  ;  (setf *pdescription-item-capture* nil)
  ;  (format *ltoutf* "\\item[~A] " (string-trim "\n\t " item))))
  nil)


;; write "-" unless this is an @itemsep, which gets "--"
(defun lt-dash (tok)
  ; (cond ((and *pdescription-item-capture* (eq tok 'itemsep))
  ;       ;; we've capture the item, now write it:
  ;       (write-pdescription-item)))
  ;;(format *ltoutf* "WRITE-FDESCRIPTION-ITEM CALLED ~A ~A "
  ;;                 *fdescription-item-capture* tok)
  ;(cond ((and *fdescription-item-capture* (eq tok 'itemsep))
  ;       ;; we've capture the item, now write it:
  ;       (write-fdescription-item)))
  ;;(format *ltoutf* "WRITE-FDESCRIPTION-ITEM RETURNED ~A ~A "
  ;;                 *fdescription-item-capture* tok)
  (format *ltoutf* "~A" (if (eq tok 'itemsep) "--" "-")))


(defun lt-blankspace-in-fdescription ()
  (setf *line-empty* t)
  (if *html* (break))
  (format *ltoutf* "~%"))

(defun lt-begin-detail ()
  (format *ltoutf* "\\small{"))

(setfn lt-end-detail lt-end)

(defun lt-begin-write-description ()
  (setf *description-item-is-open* t)
  (setf *skip-white-space* t)
  (if *token-trace* (format t "LT-BEGIN-WRITE-DESCRIPTION:OPEN"))
  (format *ltoutf* "\\begin{description2}~%")
  (needspace 3)
  (format *ltoutf* "\\item["))


(defun lt-end-write-description ()
  (cond (*description-item-is-open*
         (break "lt-end-write-description: *description-item-is-open* indicates we are expecting a description term and description. Probably there is a blank line before @end(description) in the source file. Turn on *token-trace* and continue to see where you are.")))
  (format *ltoutf* "\\end{description2}"))

(defun lt-description-paragraph ()
  (cond (*description-item-is-open*
         (format *ltoutf* "]")))
  (setf *description-item-is-open* t)
  (setf *skip-white-space* t)
  (format *ltoutf* "~%")
  (needspace 3)
  (format *ltoutf* "\\item["))

;; string-stream captures items in fdescription or pdescription:
(setf *save-ltoutf* nil)

(defun take-to (str marker)
  (let ((loc (string-search marker str)))
    (cond ((not loc)
           (setf *rslt* nil)
           str)
          (t
           (setf *rslt* (subseq str 0 loc))
           (subseq str (+ loc (length marker)))))))

(defun trim (s) (string-trim " \t\n" s))

;; remove all \\ latex line breaks from end of string:
(defun remove-breaks (s)
  (if (and (>= (length s) 2)
           (equal "\\\\" (subseq s (- (length s) 2))))
      (remove-breaks (subseq s 0 (- (length s) 2)))
      s))

(defun newlines-to-break (s)
  (let ((loc (string-search "\n\n" s)))
    (if loc (strcat (trim (subseq s 0 loc)) "\\\\" (trim (subseq s (+ 2 loc))))
            s)))


;; anything not inside <!BEGIN-PDESC> <!END-PDESC> goes into item
;; first get the strings separated by <!BEGIN-*> <!END-*> <!BREAK-*>
;; mark them as FDESC or PDESC, finally generate output
;;
(setf showcount 0)
(defun lt-fdescription (body)
  (prog (lines line
         (pdlevel 0)) ;; pdlevel = nesting level for pdescription

;    (cond ((string-search "let*" body)
;           (setf showcount 1)
;           (print "------------------------------------------------------------")
;           (print body)
;           (print "------------------------------------------------------------")))

    (format *ltoutf* "\\begin{description2}~%")
  loop1
    (setf body (take-to body "<!"))
    (cond (*rslt*
           (setf line (remove-breaks (trim *rslt*)))
           (if (> showcount 0) (display "lt-fdesc before ntb" line))
           (if (= pdlevel 0) (setf line (newlines-to-break line)))
           (if (> showcount 0) (display "lt-fdesc after ntb" line))
           (cond ((> (length line) 0)
                  (push (list pdlevel line) lines)
                  ; (if (> pdlevel 1) (print (list pdlevel line)))
                  ))
           (setf body (take-to body ">"))
           (if (equal *rslt* "BEGIN-PDESC") (setf pdlevel (1+ pdlevel)))
           (if (equal *rslt* "END-PDESC") (setf pdlevel (1- pdlevel)))
           (go loop1) ))
    (setf lines (reverse lines))
    ;; now we have ((pdlevel "line1") (pdlevel "line2") (pdlevel "line3") ...)
    (if (<= 0 (decf showcount))
      (dolist (x lines) (format t "~A~%" x)))
    ;; collect FDESC (pdlevel == 0) lines as an item
    (while lines
      (format *ltoutf* "\\item[~A" (cadar lines))
      (setf lines (rest lines))
      (while (and lines (eql 0 (caar lines)))
        (format *ltoutf* "\\\\~%~A" (cadar lines))
        (setf lines (rest lines)))
      (format *ltoutf* "]~%")
      (cond ((and lines (/= 0 (caar lines)))
             (format *ltoutf* "~A~A"
                 (if (< 1 (caar lines)) "\\hspace*{3em}" "") (cadar lines))
             (setf lines (rest lines))
             (while (and lines (/= 0 (caar lines)))
               (format *ltoutf* "\\\\~%~A~A"
                 (if (< 1 (caar lines)) "\\hspace*{3em}" "") (cadar lines))
               (setf lines (rest lines)))
             (format *ltoutf* "~%"))))
    (format *ltoutf* "\\end{description2}~%")))
    

(defun lt-begin-write-fdescription ()
  (cond ((not *save-ltoutf*)
         (setf *save-ltoutf* *ltoutf*)
         (setf *ltoutf* (make-string-output-stream)))))

(defun lt-end-write-fdescription ()
  (let ((body (get-output-stream-string *ltoutf*)))
    (cond ((not *fdescription*)
           (setf *ltoutf* *save-ltoutf*)
           (setf *save-ltoutf* nil)
           (lt-fdescription body)))))

(defun lt-begin-write-pdescription ()
  (format *ltoutf* "<!BEGIN-PDESC>")
  nil)

(defun lt-end-write-pdescription ()
  (format *ltoutf* "<!END-PDESC>")
  nil)

(defun lt-fdescription-break ()
  (format *ltoutf* "<!BREAK-FDESC>")
  nil)

(defun lt-pdescription-break ()
  (format *ltoutf* "<!BREAK-PDESC>")
  nil)

(defun lt-begin-write-format ()
  (format *ltoutf* "<begin format>"))

(defun lt-end-write-format ()
  (format *ltoutf* "<end format>"))

(defun lt-begin-blockquote ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\begin{quote}")))

(defun lt-end-blockquote ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\end{quote}")))

(defun lt-begin-center ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\begin{center}")))

(defun lt-end-center ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\end{center}")))

(defun lt-begin-figure ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\begin{figure}~%")))

(defun lt-end-figure ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\end{figure}~%")))

(defun lt-label (name)
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\label{~A}~%" name)))

(defun lt-write-footnote (name number)
  (format *ltoutf* "\\footnote{"))

(defun lt-footnote-end ()
  (format *ltoutf* "}"))

(defun lt-write-fillcaption (n)
  (if (or *html* *omit*) nil
      (format *ltoutf* "\\caption{")))

(defun lt-end-fillcaption ()
  (if (or *html* *omit*) nil
      (format *ltoutf* "}")))

;; this is what starts generating output, including the
;; Latex preamble
;;
(defun lt-majorheading (title)
  (lt-title-info title *doc-version*))

(defun lt-write-chapter (body numbered)
  (setf *line-empty* t)
  (format *ltoutf* "\\chapter~A{~A}~%" 
     (if numbered "" "*") (latex-escape body)))

(defun lt-finish-chapter ()
  (setf *line-empty* t)
  ; (format *ltoutf* "~%<chapter finish>~%")
)

(defun lt-begin-h2 ()
  (format *ltoutf* "\\section{"))
(setfn lt-end-h2 lt-end)

(defun lt-begin-h3 ()
  (format *ltoutf* "\\subsection{"))
(setfn lt-end-h3 lt-end)

(defun lt-begin-h4 ()
  (format *ltoutf* "\\subsubsection{"))
(setfn lt-end-h4 lt-end)

(defun lt-begin-superscript ()
  (format *ltoutf* "\\textsuperscript{"))

(defun lt-end-superscript ()
  (format *ltoutf* "}"))

(defun lt-begin-subscript ()
  (format *ltoutf* "\\textsubscript{"))

(defun lt-end-subscript ()
  (format *ltoutf* "}"))

(defun lt-paragraph ()
  (if *html* (break))
  (terpri *ltoutf*)
  (terpri *ltoutf*))

(defun needspace (n)
  (cond ((<= skip-count 0)
         (format *ltoutf* "\\needspace{~A\\baselineskip}~%" n)
         (setf skip-count n))))
         

(defun lt-write-paragraph (name-number body)
  (needspace 3)
  (format *ltoutf* "\\subsubsection{~A}" (latex-escape body)))

(defun lt-write-subsection (name-number body)
  (needspace 3)
  (format *ltoutf* "\\subsection{~A}" (latex-escape body)))

(defun lt-write-section (name-number body)
  (needspace 4)
  (format *ltoutf* "\\section{~A}" (latex-escape body)))

(setf *lt-started-appendices* nil)

(defun lt-appendix (name-number body)
  (cond ((not *lt-started-appendices*)
         (setf *lt-started-appendices* t)
         (format *ltoutf* "\\appendix~%")
         (format *ltoutf* "\\renewcommand{\\thechapter}{\\Alph{chapter}}~%")
         (format *ltoutf* "\\titleformat{\\chapter}~%")
         (format *ltoutf* "  {\\normalfont\\sffamily\\huge\\bfseries%\\color{MidnightBlue}~%")
         (format *ltoutf* "}~%")
         (format *ltoutf* "  {Appendix \\thechapter}{24pt}{}~%")))
  (format *ltoutf* "\\chapter{~A}" (latex-escape body)))

(defun lt-end-document ()
  (setf *line-empty* t)
  (format *ltoutf* "\\clearpage~%\\addcontentsline{toc}{chapter}{Index}~%")
  (format *ltoutf* "\\printindex~%\\end{document}~%"))

