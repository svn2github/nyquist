;; xmpalindrometest.lsp - make-palindrome (palindrome pattern generator) test
;; 
;; Roger B. Dannenberg
;; Feb, 2018

;; palindrome test

(setf pap (make-palindrome '(a b c d)))
(setf papp (next pap t)) 
(xmassert (equal '(a b c d d c b a) papp) "papp") 
(setf papp (next pap t))
(xmassert (equal '(a b c d d c b a) papp) "papp2")
(dotimes (i 10) (xmassert (equal (next pap) (nth i '(a b c d d c b a a b))) "next of pap"))

(setf papf (make-palindrome '(a b c d) :for 3))
(setf papfp (next papf t))
(xmassert (equal '(a b c) papfp) "papfp")
(setf papfp (next papf t))
(xmassert (equal '(a b c) papfp) "papfp2")

(setf papa (make-palindrome (list (make-palindrome '(a b c)) (make-palindrome '(d e)))))
(setf cpap (next papa t))
(xmassert (equal '(a b c c b a) cpap) "cpap1")
(setf cpap (next papa t))
(xmassert (equal '(d e e d) cpap) "cpap2")
(setf cpap (next papa t))
(xmassert (equal '(d e e d) cpap) "cpap3")
(setf cpap (next papa t))
(xmassert (equal '(a b c c b a) cpap) "cpap4")

(setf papam (make-palindrome (list (make-palindrome '(a b c)) (make-palindrome '(d e))) :merge t))
(setf papamp (next papam t))
(xmassert (equal '(a b c c b a d e e d d e e d a b c c b a) papamp) "papamp1")
(setf papamp (next papam t))
(xmassert (equal '(a b c c b a d e e d d e e d a b c c b a) papamp) "papamp2")

(setf papaf (make-palindrome (list (make-palindrome '(a b c)) (make-palindrome '(d e) :for 3))))
(setf papafp (next papaf t))
(xmassert (equal '(a b c c b a) papafp) "papafp1")
(setf papafp (next papaf t))
(xmassert (equal '(d e e) papafp) "papafp2")
(setf papafp (next papaf t))
(xmassert (equal '(d e e) papafp) "papafp3")
(setf papafp (next papaf t))
(xmassert (equal '(a b c c b a) papafp) "papafp4")

(setf papamf (make-palindrome (list (make-palindrome '(a b c)) (make-palindrome '(d e) :for 3)) :merge t))
(setf papamfp (next papamf t))
(xmassert (equal '(a b c c b a d e e d e e a b c c b a) papamfp) "papamfp1")
(setf papamfp (next papamf t))
(xmassert (equal '(a b c c b a d e e d e e a b c c b a) papamfp) "papamfp2")

(setf papaff (make-palindrome (list (make-palindrome '(a b c) :name "palabc") 
                             (make-palindrome '(d e) :for 3 :name "palde")) :for 11))
(setf papaffp (next papaff t))
(xmassert (equal '(a b c c b a d e e d e) papaffp) "papaffp1")
(setf papaffp (next papaff t))
;; STRANGE CASE HERE, BUT CORRECT: previous :for 11 leaves palde with one more item
;; left to complete a full period; thus when the top-level palindrome is restarted
;; and comes back to visit palde, there's only one one item left (e) in the period.
;; Since the full top-level period is abc de de abc, we do de again, this time 
;; returning a full (d e e) period. Now we have (e d e e) and go back to abc to 
;; pick up our 11th item (a).
(xmassert (equal '(a b c c b a e d e e a) papaffp) "papaffp2")
;; get another period and test it just to be sure
(setf papaffp (next papaff t))
(xmassert (equal '(b c c b a d e e d e e) papaffp) "papaffp3")
(setf papaffp (next papaff t))
;; ANOTHER STRANGE CASE, BUT "CORRECT": Here, the top-level palindrome never
;; got the final +eonp+ from palde because it stopped after 11 items. When it
;; goes back for more, it gets +eonp+ so effectively there is a cycle of zero
;; length, so we just see one period of (d e e) before going back to (a b ...).
(xmassert (equal '(a b c c b a d e e a b) papaffp) "papaffp4")

;; patterns for parameters
(setf papfpp (make-palindrome '(a b) :for (make-cycle '(1 2 3) :name "forpal")))
(setf papfppp (next papfpp t))
(xmassert (equal '(a) papfppp) "papfppp")
(setf papfppp (next papfpp t))
(xmassert (equal '(a b) papfppp) "papfppp2")
(setf papfppp (next papfpp t))
(xmassert (equal '(a b b) papfppp) "papfppp3")
(setf papfppp (next papfpp t))
(xmassert (equal '(a) papfppp) "papfppp4")

(setf papampp (make-palindrome (list (make-palindrome '(a b)) (make-palindrome '(d e)))
                        :merge (make-cycle '(nil t) :name "mergepalindromee")))
(setf papamppp (next papampp t))
(xmassert (equal '(a b b a) papamppp) "papamppp1")
(setf papamppp (next papampp t))
(xmassert (equal '(d e e d) papamppp) "papamppp2")
(setf papamppp (next papampp t))
(xmassert (equal '(d e e d) papamppp) "papamppp3")
(setf papamppp (next papampp t))
(xmassert (equal '(a b b a) papamppp) "papamppp4")
(setf papamppp (next papampp t))
(xmassert (equal '(a b b a d e e d d e e d a b b a) papamppp) "papamppp5")
(setf papamppp (next papampp t))
(xmassert (equal '(a b b a) papamppp) "papamppp6")

(setf paep (make-palindrome '(a b c)
            :elide (make-cycle '(nil t :first :last)) :name "elidepalindromee"))
(setf paepp (next paep t))
(xmassert (equal '(a b c c b a) paepp) "paepp1")
(setf paepp (next paep t))
(xmassert (equal '(a b c b) paepp) "paepp2")
(setf paepp (next paep t))
(xmassert (equal '(a b c c b) paepp) "paepp3")
(setf paepp (next paep t))
(xmassert (equal '(a b c b a) paepp) "paepp4")
(setf paepp (next paep t))
(xmassert (equal '(a b c c b a) paepp) "paepp5")

;; make list be a pattern to palindrome

(setf palp (make-palindrome (make-cycle (list (make-cycle '(a b c d))
                                              (make-cycle '(e f g)))
                                        :trace nil :name "pcabcdefg")))
(setf palpp (next palp t)) 
(xmassert (equal '(a b c d d c b a) palpp) "palpp1") 
(setf palpp (next palp t))
(xmassert (equal '(e f g g f e) palpp) "palpp2")
(setf palpp (next palp t)) 
(xmassert (equal '(a b c d d c b a) palpp) "palpp3")

