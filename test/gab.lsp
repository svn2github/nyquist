(setf ts (/ s 2.0))

(defun trumpet (p)
  (double-carrier 0.6 (step-to-hz p) 1.0 1.0 0.5 3 1 (/ 3.0 1.5))


(defun tI_1 ()
  (transpose -2
             (seq
              (loud lmf
                    (seq
                     ;; measure 1
                     (s-rest h)
                     (sustain 1.3 (stretch i (trumpet d5)))
                     (stretch i (trumpet g4))
                     (stretch i (trumpet d5))
                     (stretch i (trumpet e5))
                     ;; measure 2
                     (sustain .9 (stretch qd (trumpet f5)))
                     (stretch i (trumpet f5))
                     (sustain 1.4 (stretch s (trumpet e5)))
                     (stretch s (trumpet d5))
                     (stretch s (trumpet c5))
                     (stretch s (trumpet bf4))
                     (stretch i (trumpet a4))
                     (stretch i (trumpet a5))
                     ;; measure 3
                     (stretch i (trumpet g5))
                     (stretch i (trumpet f5))
                     (sustain 1.1 (stretch q (trumpet e5)))
                     (stretch h (trumpet d5))
                     ;; measure 4
                     (s-rest w)
                     ;; measure 5
                     (s-rest w)
                     ;; measure 6
                     (s-rest w)))
              (loud lf
                    (seq
                     ;; measure 7
                     (sustain 1.3 (stretch i (trumpet d5)))
                     (stretch i (trumpet g4))
                     (stretch i (trumpet d5))
                     (stretch i (trumpet e5))
                     (sustain .9 (stretch qd (trumpet f5)))
                     (stretch i (trumpet f5))
                     ;; measure 8
                     (sustain 1.4 (stretch s (trumpet e5)))
                     (stretch s (trumpet d5))
                     (stretch s (trumpet c5))
                     (stretch s (trumpet bf4))
                     (stretch i (trumpet a4))
                     (stretch i (trumpet a5))
                     (stretch i (trumpet g5))
                     (stretch i (trumpet f5))
                     (sustain 1.1 (stretch q (trumpet e5)))
                     ;; measure 9
                     (stretch w (trumpet d5))
                     ;; measure 10
                     (s-rest w)
                     ;; measure 11
                     (sustain 1.3 (stretch i (trumpet d5)))
                     (stretch i (trumpet g4))
                     (stretch i (trumpet d5))
                     (stretch i (trumpet e5))
                     (sustain .9 (stretch qd (trumpet f5)))
                     (stretch i (trumpet f5))
                     ;; measure 12
                     (sustain 1.4 (stretch s (trumpet e5)))
                     (stretch s (trumpet d5))
                     (stretch s (trumpet c5))
                     (stretch s (trumpet bf4))
                     (stretch i (trumpet a4))
                     (stretch i (trumpet a5))
                     (stretch i (trumpet g5))
                     (stretch i (trumpet f5))
                     (sustain 1.1 (stretch q (trumpet e5)))
                     ;; measure 13
                     (sustain .9 (stretch qd (trumpet d5)))
                     (stretch i (trumpet d5))
                     (stretch i (trumpet c5))
                     (stretch i (trumpet bf4))
                     (sustain 1.1 (stretch q (trumpet a4)))
                     ;; measure 14
                     (stretch w (trumpet g4)))))))


(defun tII_1 ()
  (transpose -2
             (seq
              (loud lmf
                    (seq
                     ;; measure 1
                     (sustain 1.3 (stretch i (trumpet g4)))
                     (stretch i (trumpet d4))
                     (stretch i (trumpet g4))
                     (stretch i (trumpet a4))
                     (sustain .9 (stretch qd (trumpet bf4)))
                     (stretch i (trumpet bf4))
                     ;; measure 2
                     (sustain 1.4 (stretch s (trumpet a4)))
                     (stretch s (trumpet g4))
                     (stretch s (trumpet f4))
                     (stretch s (trumpet e4))
                     (stretch i (trumpet d4))
                     (stretch i (trumpet d5))
                     (sustain 1.4 (stretch s (trumpet c5)))
                     (stretch s (trumpet bf4))
                     (stretch s (trumpet a4))
                     (stretch s (trumpet g4))
                     (stretch q (trumpet f4))
                     ;; measure 3
                     (stretch s (trumpet bf4))
                     (stretch s (trumpet c5))
                     (stretch q (trumpet d5))
                     (stretch i (trumpet cs5))
                     (stretch h (trumpet d5))
                     ;; measure 4
                     (s-rest w)))
              (loud lf
                    (seq
                     ;; measure 5
                     (sustain 1.3 (stretch i (trumpet g4)))
                     (stretch i (trumpet d4))
                     (stretch i (trumpet g4))
                     (stretch i (trumpet a4))
                     (sustain .9 (stretch qd (trumpet bf4)))
                     (stretch i (trumpet bf4))
                     ;; measure 6
                     (sustain 1.4 (stretch s (trumpet a4)))
                     (stretch s (trumpet g4))
                     (stretch s (trumpet f4))
                     (stretch s (trumpet e4))
                     (stretch i (trumpet d4))
                     (stretch i (trumpet d5))
                     (stretch i (trumpet c5))
                     (stretch i (trumpet bf4))
                     (stretch q (trumpet a4))
                     ;; meaure 7
                     (stretch qd (trumpet bf4))
                     (stretch s (trumpet a4))
                     (stretch s (trumpet g4))
                     (sustain 1.4 (stretch s (trumpet a4)))
                     (stretch s (trumpet g4))
                     (stretch s (trumpet f4))
                     (stretch s (trumpet e4))
                     (stretch i (trumpet d4))
                     (stretch i (trumpet d5))
                     ;; measure 8
                     (sustain 1.4 (stretch s (trumpet c5)))
                     (stretch s (trumpet bf4))
                     (stretch s (trumpet a4))
                     (stretch s (trumpet g4))
                     (stretch i (trumpet f4))
                     (stretch i (trumpet a4))
                     (stretch s (trumpet b4))
                     (stretch s (trumpet cs5))
                     (stretch q (trumpet d5))
                     (stretch i (trumpet c5))
                     ;; measure 9
                     (stretch h (trumpet d5))
                     (s-rest h)
                     ;; measure 10
                     (s-rest h)
                     (sustain 1.3 (stretch i (trumpet g4)))
                     (stretch i (trumpet d4))
                     (stretch i (trumpet g4))
                     (stretch i (trumpet a4))
                     ;; measure 11
                     (sustain .9 (stretch qd (trumpet bf4)))
                     (stretch i (trumpet bf4))
                     (sustain 1.4 (stretch s (trumpet a4)))
                     (stretch s (trumpet g4))
                     (stretch s (trumpet f4))
                     (stretch s (trumpet e4))
                     (stretch i (trumpet d4))
                     (stretch i (trumpet d5))
                     ;; measure 12
                     (sustain 1.4 (stretch s (trumpet c5)))
                     (stretch s (trumpet bf4))
                     (stretch s (trumpet a4))
                     (stretch s (trumpet g4))
                     (stretch i (trumpet f4))
                     (stretch i (trumpet a4))
                     (stretch s (trumpet b4))
                     (stretch s (trumpet cs5))
                     (stretch q (trumpet d5))
                     (stretch i (trumpet c5))
                     ;; measure 13
                     (sustain .9 (stretch qd (trumpet d5)))
                     (stretch i (trumpet bf4))
                     (stretch i (trumpet a4))
                     (stretch q (trumpet g4))
                     (stretch i (trumpet fs4))
                     ;; measure 14
                     (stretch w (trumpet g4)))))))

(defun h_1 ()
  (transpose -7 (seq
                 (loud lmf
                       (seq
                        ;; measure 1
                        (s-rest w)
                        ;; measure 2
                        (s-rest w)
                        ;; measure 3
                        (s-rest h)
                        (sustain 1.3 (stretch i (trumpet g4)))
                        (stretch i (trumpet c4))
                        (stretch i (trumpet g4))
                        (stretch i (trumpet a4))
                        ;; measure 4
                        (sustain .9 (stretch qd (trumpet bf4)))
                        (stretch i (trumpet bf4))
                        (sustain 1.4 (stretch s (trumpet a4)))
                        (stretch s (trumpet g4))
                        (stretch s (trumpet f4))
                        (stretch s (trumpet ef4))
                        (stretch i (trumpet d4))
                        (stretch i (trumpet g4))
                        ;; measure 5
                        (stretch i (trumpet fs4))
                        (stretch q (trumpet g4))
                        (stretch i (trumpet fs4))
                        (sustain .9 (stretch qd (trumpet g4)))
                        (stretch i (trumpet ef4))
                        ;; measure 6
                        (stretch q (trumpet f4))
                        (stretch i (trumpet ef4))
                        (stretch i (trumpet g4))
                        (stretch s (trumpet a4))
                        (stretch s (trumpet b4))
                        (stretch q (trumpet c5))
                        (stretch i (trumpet b4))
                        ;; measure 7
                        (sustain 1.1 (stretch qd (trumpet c5)))
                        (sustain 1.4 (stretch s (trumpet bf4)))
                        (sustain 1.4 (stretch s (trumpet a4)))
                        (stretch h (trumpet g4))
                       ;; measure 8
                        (s-rest qd)
                        (stretch i (trumpet bf4))
                        (stretch s (trumpet c5))
                        (stretch s (trumpet d5))
                        (stretch i (trumpet ef5))
                        (stretch i (trumpet c5))
                        (stretch i (trumpet d5))
                        ;; measure 9
                        (stretch h (trumpet g4))))
                 (loud lf
                       (seq
                        (sustain 1.3 (stretch i (trumpet g4)))
                        (stretch i (trumpet c4))
                        (stretch i (trumpet g4))
                        (stretch i (trumpet a4))
                        ;; measure 10
                        (sustain .9 (stretch qd (trumpet bf4)))
                        (stretch i (trumpet bf4))
                        (sustain 1.1 (stretch qd (trumpet a4)))
                        (sustain 1.4 (stretch s (trumpet g4)))
                        (sustain 1.4 (stretch s (trumpet f4)))
                        ;; measure 11
                        (stretch h (trumpet g4))
                        (stretch h (trumpet g4))
                        ;; measure 12
                        (s-rest qd)
                        (stretch i (trumpet bf4))
                        (stretch s (trumpet c5))
                        (stretch s (trumpet d5))
                        (stretch i (trumpet ef5))
                        (stretch i (trumpet c5))
                        (stretch i (trumpet d5))
                        ;; measure 13
                        (sustain .9 (stretch qd (trumpet g4)))
                        (stretch i (trumpet bf4))
                        (stretch i (trumpet af4))
                        (stretch q (trumpet af4))
                        (stretch i (trumpet g4))
                        ;; measure 14
                        (stretch w (trumpet e4)))))))

(defun b_1 ()
  (seq
   (loud lmf
         (seq
          ;; measure 1
          (s-rest w)
          ;; measure 2
          (s-rest w)
          ;; measure 3
          (sustain 1.3 (stretch i (trumpet f3)))
          (stretch i (trumpet c3))
          (stretch i (trumpet f3))
          (stretch i (trumpet g3))
          (sustain .9 (stretch qd (trumpet af3)))
          (stretch i (trumpet af3))
          ;; measure 4
          (sustain 1.4 (stretch s (trumpet g3)))
          (stretch s (trumpet f3))
          (stretch s (trumpet ef3))
          (stretch s (trumpet d3))
          (stretch i (trumpet c3))
          (stretch i (trumpet c4))
          (sustain 1.4 (stretch s (trumpet bf3)))
          (stretch s (trumpet af3))
          (stretch s (trumpet g3))
          (stretch s (trumpet f3))
          (stretch i (trumpet ef3))
          (stretch s (trumpet d3))
          (stretch s (trumpet c3))
          ;; measure 5
          (stretch i (trumpet d3))
          (stretch i (trumpet ef3))
          (stretch q (trumpet d3))
          (sustain 1.4 (stretch s (trumpet c3)))
          (stretch s (trumpet bf2))
          (stretch s (trumpet af2))
          (stretch s (trumpet g2))
          (stretch i (trumpet f2))
          (stretch i (trumpet f3))
          ;; measure 6
          (stretch q (trumpet ef3))
          (stretch i (trumpet f3))
          (stretch i (trumpet af3))
          (stretch s (trumpet bf3))
          (stretch s (trumpet c4))
          (stretch i (trumpet df4))
          (stretch i (trumpet bf3))
          (stretch i (trumpet c4))
          ;; measure 7
          (stretch h (trumpet f3))
          (s-rest h)
          ;; measure 8
          (s-rest w)))
          ;; measure 9
   (loud lf
         (seq
          (sustain 1.3 (stretch i (trumpet f3)))
          (stretch i (trumpet c3))
          (stretch i (trumpet f3))
          (stretch i (trumpet g3))
          (sustain .9 (stretch qd (trumpet af3)))
          (stretch i (trumpet af3))
          ;; measure 10
          (sustain 1.4 (stretch s (trumpet g3)))
          (stretch s (trumpet f3))
          (stretch s (trumpet ef3))
          (stretch s (trumpet d3))
          (stretch i (trumpet c3))
          (stretch i (trumpet c4))
          (sustain 1.1 (stretch qd (trumpet bf3)))
          (sustain 1.4 (stretch s (trumpet af3)))
          (sustain 1.4 (stretch s (trumpet g3)))
          ;; measure 11
          (stretch h (trumpet f3))
          (s-rest h)
          ;; meausre 12
          (s-rest w)
          ;; measure 13
          (s-rest qd)
          (stretch i (trumpet af2))
          (stretch s (trumpet bf2))
          (stretch s (trumpet c3))
          (stretch i (trumpet df3))
          (stretch i (trumpet bf2))
          (stretch i (trumpet c3))
          ;; measure 14
          (stretch w (trumpet f2))))))

(defun tI_2 ()
  (transpose -2
             (loud lp
                   (seq
                    ;; measure 15
                    (sustain 1.1 (stretch qd (trumpet bf4)))
                    (sustain 1.3 (stretch i (trumpet c5)))
                    (stretch q (trumpet d5))
                    (stretch q (trumpet bf4))
                    (stretch q (trumpet g4))
                    (stretch q (trumpet bf4))
                    ;; measure 16
                    (sustain 1.1 (stretch qd (trumpet a4)))
                    (sustain 1.3 (stretch i (trumpet bf4)))
                    (stretch q (trumpet c5))
                    (stretch h (trumpet d5))
                    (stretch q (trumpet cs5))
                    ;; measure 17
                    (stretch wd (trumpet d5))
                    ;; measure 18
                    (s-rest wd)
                    ;; measure 19
                    (s-rest wd)
                    ;; measure 20
                    (s-rest wd)
                    ;; measure 21
                    (sustain 1.1 (stretch qd (trumpet bf4)))
                    (sustain 1.3 (stretch i (trumpet c5)))
                    (stretch q (trumpet d5))
                    (stretch q (trumpet bf4))
                    (stretch q (trumpet g4))
                    (stretch q (trumpet bf4))
                    ;; meausre 22
                    (sustain 1.1 (stretch qd (trumpet a4)))
                    (sustain 1.3 (stretch i (trumpet bf4)))
                    (stretch q (trumpet c5))
                    (stretch h (trumpet d5))
                    (stretch q (trumpet cs5))))))

(defun tII_2 ()
  (transpose -2
             (loud lp
                   (seq
                    ;; measure 15
                    (stretch w (trumpet g4))
                    (stretch h (trumpet g4))
                    ;; measure 16
                    (stretch hd (trumpet f4))
                    (stretch q (trumpet d4))
                    (stretch h (trumpet e4))
                    ;; measure 17
                    (stretch wd (trumpet d4))
                    ;; measure 18
                    (s-rest wd)
                    ;; meausre 19
                    (sustain 1.1 (stretch qd (trumpet f4)))
                    (sustain 1.3 (stretch i (trumpet g4)))
                    (stretch q (trumpet a4))
                    (stretch q (trumpet f4))
                    (stretch q (trumpet d4))
                    (stretch q (trumpet f4))
                    ;; measure 20
                    (sustain 1.1 (stretch qd (trumpet e4)))
                    (sustain 1.3 (stretch i (trumpet d4)))
                    (stretch q (trumpet e4))
                    (stretch h (trumpet g4))
                    (stretch q (trumpet fs4))
                    ;; measure 21
                    (sustain 1.1 (stretch qd (trumpet g4)))
                    (sustain 1.3 (stretch i (trumpet a4)))
                    (stretch q (trumpet bf4))
                    (stretch q (trumpet g4))
                    (stretch q (trumpet d4))
                    (stretch q (trumpet ef4))
                    ;; measure 22
                    (sustain 1.1 (stretch qd (trumpet f4)))
                    (sustain 1.3 (stretch i (trumpet g4)))
                    (stretch q (trumpet a4))
                    (stretch q (trumpet a4))
                    (sustain 1.1 (stretch h (trumpet a4)))))))

(defun h_2 ()
  (transpose -7
             (loud lp
                   (seq
                    ;; measure 15
                    (s-rest wd)
                    ;; measure 16
                    (s-rest wd)
                    ;; measure 17
                    (sustain 1.1 (stretch qd (trumpet ef4)))
                    (sustain 1.3 (stretch i (trumpet f4)))
                    (stretch q (trumpet g4))
                    (stretch q (trumpet ef4))
                    (stretch q (trumpet c4))
                    (stretch q (trumpet ef4))
                    ;; measure 18
                    (sustain 1.1 (stretch qd (trumpet d4)))
                    (sustain 1.3 (stretch i (trumpet ef4)))
                    (stretch q (trumpet f4))
                    (stretch h (trumpet g4))
                    (stretch q (trumpet fs4))
                    ;; measure 19
                    (stretch wd (trumpet g4))
                    ;; measure 20
                    (stretch hd (trumpet f4))
                    (stretch q (trumpet ef4))
                    (stretch h (trumpet d4))
                    ;; measure 21
                    (stretch w (trumpet g4))
                    (stretch h (trumpet ef4))
                    ;; measure 22
                    (stretch hd (trumpet f4))
                    (stretch q (trumpet bf4))
                    (sustain 1.1 (stretch h (trumpet a4)))))))


(defun b_2 ()
  (loud lp
        (seq
         ;; measure 15
         (s-rest wd)
         ;; measure 16
         (s-rest wd)
         ;; measure 17
         (stretch w (trumpet f3))
         (stretch h (trumpet f3))
         ;; meausre 18
         (stretch hd (trumpet ef3))
         (stretch q (trumpet c3))
         (stretch h (trumpet d3))
         ;; measure 19
         (stretch wd (trumpet c3))
         ;; measure 20
         (s-rest wd)
         ;; measure 21
         (stretch w (trumpet f3))
         (stretch h (trumpet f3))
         ;; measure 22
         (stretch hd (trumpet ef3))
         (stretch q (trumpet c3))
         (stretch h (trumpet g3)))))


(defun tI_3 ()
  (transpose -2
             (loud lmf
                   (seq
                    ;; measure 23
                    (stretch i (trumpet d5))
                    (stretch i (trumpet a4))
                    (sustain 1.4 (stretch s (trumpet bf4)))
                    (stretch s (trumpet c5))
                    (stretch s (trumpet d5))
                    (stretch s (trumpet f5))
                    (stretch i (trumpet e5))
                    (stretch i (trumpet d5))
                    (stretch i (trumpet c5))
                    (stretch i (trumpet bf4))
                    ;; measure 24
                    (stretch i (trumpet a4))
                    (stretch i (trumpet c5))
                    (stretch i (trumpet bf4))
                    (stretch i (trumpet a4))
                    (stretch q (trumpet g4))
                    (s-rest q)
                    ;; measure 25
                    (s-rest qd)
                    (stretch i (trumpet a4))
                    (sustain 1.4 (stretch s (trumpet bf4)))
                    (stretch s (trumpet c5))
                    (stretch s (trumpet d5))
                    (stretch s (trumpet f5))
                    (stretch i (trumpet e5))
                    (stretch i (trumpet d5))
                    ;; measure 26
                    (stretch i (trumpet c5))
                    (stretch i (trumpet bf4))
                    (stretch q (trumpet a4))
                    (stretch q (trumpet g4))
                    (s-rest q)
                    ;; measure 27 [C]
                    (s-rest i)
                    (stretch i (trumpet c5))
                    (sustain 1.4 (stretch s (trumpet c5)))
                    (stretch s (trumpet e5))
                    (stretch s (trumpet f5))
                    (stretch s (trumpet a5))
                    (stretch i (trumpet g5))
                    (stretch i (trumpet f5))
                    (stretch i (trumpet e5))
                    (stretch q (trumpet d5))
                    ;; meausre 28
                    (stretch i (trumpet cs5))
                    (stretch id (trumpet d5))
                    (stretch s (trumpet a4))
                    (sustain 1.4 (stretch s (trumpet bf4)))
                    (stretch s (trumpet c5))
                    (stretch s (trumpet d5))
                    (stretch s (trumpet f5))
                    (stretch i (trumpet ef5))
                    (stretch i (trumpet d5))
                    ;; measure 29
                    (stretch i (trumpet c5))
                    (stretch i (trumpet bf4))
                    (stretch i (trumpet a4))
                    (stretch i (trumpet g4))
                    (stretch i (trumpet fs4))
                    (stretch q (trumpet g4))
                    (stretch i (trumpet fs4))))))


(defun tII_3 ()
  (transpose -2
             (loud lmf
                   (seq
                    ;; measure 23
                    (stretch q (trumpet f4))
                    (s-rest i)
                    (stretch i (trumpet d4))
                    (sustain 1.4 (stretch s (trumpet e4)))
                    (stretch s (trumpet f4))
                    (stretch s (trumpet g4))
                    (stretch s (trumpet bf4))
                    (stretch i (trumpet a4))
                    (stretch i (trumpet g4))
                    ;; measure 24
                    (stretch i (trumpet f4))
                    (stretch i (trumpet e4))
                    (stretch q (trumpet d4))
                    (s-rest i)
                    (stretch i (trumpet d4))
                    (sustain 1.4 (stretch s (trumpet e4)))
                    (stretch s (trumpet f4))
                    (stretch s (trumpet g4))
                    (stretch s (trumpet bf4))
                    ;; measure 25
                    (stretch i (trumpet a4))
                    (stretch q (trumpet g4))
                    (stretch i (trumpet fs4))
                    (stretch i (trumpet g4))
                    (stretch i (trumpet d4))
                    (sustain 1.4 (stretch s (trumpet e4)))
                    (stretch s (trumpet f4))
                    (stretch s (trumpet g4))
                    (stretch s (trumpet bf4))
                    ;; measure 26
                    (stretch i (trumpet a4))
                    (stretch q (trumpet g4))
                    (stretch i (trumpet fs4))
                    (stretch i (trumpet g4))
                    (stretch i (trumpet d4))
                    (sustain 1.4 (stretch s (trumpet e4)))
                    (stretch s (trumpet f4))
                    (stretch s (trumpet g4))
                    (stretch s (trumpet bf4))
                    ;; measure 27 [C]
                    (stretch i (trumpet a4))
                    (stretch q (trumpet g4))
                    (stretch s (trumpet f4))
                    (stretch s (trumpet f4))
                    (sustain 1.4 (stretch s (trumpet bf4)))
                    (stretch s (trumpet c5))
                    (stretch s (trumpet d5))
                    (stretch s (trumpet a4))
                    (stretch i (trumpet c5))
                    (stretch i (trumpet g4))
                    ;; measure 28
                    (stretch id (trumpet a4))
                    (stretch s (trumpet e4))
                    (sustain 1.4 (stretch s (trumpet f4)))
                    (stretch s (trumpet g4))
                    (stretch s (trumpet a4))
                    (stretch s (trumpet c5))
                    (stretch i (trumpet bf4))
                    (stretch i (trumpet a4))
                    (stretch i (trumpet g4))
                    (stretch i (trumpet bf4))
                    ;; measure 29
                    (stretch i (trumpet a4))
                    (stretch i (trumpet g4))
                    (stretch i (trumpet ef4))
                    (stretch i (trumpet d4))
                    (sustain 1.1 (stretch h (trumpet d4)))))))


(defun h_3 ()
  (transpose -7
            (loud lmf
                  (seq
                   ;; measure 23
                   (stretch q (trumpet g4))
                   (s-rest hd)
                   ;; measure 24
                   (s-rest i)
                   (stretch i (trumpet d4))
                   (sustain 1.4 (stretch s (trumpet ef4)))
                   (stretch s (trumpet f4))
                   (stretch s (trumpet g4))
                   (stretch s (trumpet bf4))
                   (stretch i (trumpet a4))
                   (stretch i (trumpet g4))
                   (stretch i (trumpet f4))
                   (stretch i (trumpet ef4))
                   ;; measure 25
                   (stretch id (trumpet f4))
                   (stretch s (trumpet c4))
                   (stretch i (trumpet ef4))
                   (stretch i (trumpet d4))
                   (sustain 1.4 (stretch s (trumpet c4)))
                   (stretch s (trumpet d4))
                   (stretch s (trumpet ef4))
                   (stretch s (trumpet g4))
                   (stretch i (trumpet f4))
                   (stretch i (trumpet ef4))
                   ;; measure 26
                   (stretch q (trumpet f4))
                   (stretch i (trumpet g4))
                   (stretch i (trumpet g4))
                   (sustain 1.4 (stretch s (trumpet c4)))
                   (stretch s (trumpet d4))
                   (stretch s (trumpet ef4))
                   (stretch s (trumpet g4))
                   (stretch i (trumpet f4))
                   (stretch i (trumpet ef4))
                   ;; measure 27 [C]
                   (stretch id (trumpet f4))
                   (stretch s (trumpet c4))
                   (stretch i (trumpet ef4))
                   (stretch s (trumpet d4))
                   (stretch s (trumpet g4))
                   (stretch i (trumpet c4))
                   (s-rest s)
                   (stretch s (trumpet g4))
                   (sustain 1.4 (stretch s (trumpet a4)))
                   (stretch s (trumpet bf4))
                   (stretch s (trumpet c5))
                   (stretch s (trumpet g4))
                   ;; measure 28
                   (stretch i (trumpet bf4))
                   (stretch i (trumpet a4))
                   (stretch q (trumpet g4))
                   (s-rest i)
                   (stretch i (trumpet d4))
                   (sustain 1.4 (stretch s (trumpet ef4)))
                   (stretch s (trumpet f4))
                   (stretch s (trumpet g4))
                   (stretch s (trumpet bf4))
                   ;; meausre 29
                   (stretch i (trumpet af4))
                   (stretch i (trumpet g4))
                   (stretch i (trumpet f4))
                   (stretch i (trumpet ef4))
                   (stretch i (trumpet d4))
                   (stretch i (trumpet c4))
                   (sustain 1.1 (stretch q (trumpet d4)))))))


(defun b_3 ()
  (loud lmf
        (seq
         ;; measure 23
         (stretch h (trumpet c3))
         (s-rest h)
         ;; measure 24
         (s-rest qd)
         (stretch i (trumpet c3))
         (sustain 1.4 (stretch s (trumpet d3)))
         (stretch s (trumpet ef3))
         (stretch s (trumpet f3))
         (stretch s (trumpet af3))
         (stretch i (trumpet g3))
         (stretch i (trumpet f3))
         ;; measure 25
         (stretch i (trumpet ef3))
         (stretch i (trumpet d3))
         (stretch q (trumpet c3))
         (stretch q (trumpet f3))
         (s-rest q)
         ;; measure 26
         (s-rest qd)
         (stretch i (trumpet c3))
         (sustain 1.4 (stretch s (trumpet d3)))
         (stretch s (trumpet ef3))
         (stretch s (trumpet f3))
         (stretch s (trumpet af3))
         (stretch i (trumpet g3))
         (stretch i (trumpet f3))
         ;; measure 27 [C]
         (stretch i (trumpet ef3))
         (stretch i (trumpet d3))
         (stretch i (trumpet c3))
         (stretch i (trumpet c3))
         (sustain 1.4 (stretch s (trumpet f3)))
         (stretch s (trumpet g3))
         (stretch s (trumpet af3))
         (stretch s (trumpet c4))
         (stretch i (trumpet bf3))
         (stretch i (trumpet af3))
         ;; measure 28
         (stretch q (trumpet g3))
         (stretch i (trumpet c3))
         (stretch i (trumpet e3))
         (stretch i (trumpet f3))
         (stretch i (trumpet ef3))
         (stretch q (trumpet f3))
         ;; measure 29
         (stretch h (trumpet bf2))
         (stretch h (trumpet c3)))))
         

(defun tI_4 ()
  (transpose -2
             (seqrep (i 1)
                     (seq
                      (loud lpp
                            (seq
                             ;; measure 30
                             (stretch s (trumpet g4))
                             (stretch s (trumpet g4))
                             (stretch s (trumpet bf4))
                             (stretch s (trumpet c5))
                             (sustain 1.1 (stretch q (trumpet d5)))
                             (stretch i (trumpet d5))
                             (stretch i (trumpet bf4))
                             (sustain 1.1 (stretch q (trumpet c5)))
                             ;; measure 31
                             (stretch i (trumpet c5))
                             (stretch i (trumpet bf4))
                             (stretch i (trumpet bf4))
                             (sustain 1.4 (stretch s (trumpet a4)))
                             (sustain 1.4 (stretch s (trumpet g4)))
                             (stretch i (trumpet a4))
                             (stretch q (trumpet bf4))
                             (stretch i (trumpet a4))
                             ;; measure 32
                             (stretch q (trumpet bf4))
                             (sustain 1.1 (stretch q (trumpet d5)))
                             (stretch i (trumpet d5))
                             (stretch i (trumpet d5))
                             (sustain 1.1 (stretch q (trumpet d5)))
                             ;; measure 33
                             (stretch i (trumpet d5))
                             (stretch i (trumpet d5))
                             (stretch i (trumpet d5))
                             (stretch i (trumpet f5))
                             (sustain 1.3 (stretch i (trumpet ef4)))
                             (stretch i (trumpet d4))
                             (stretch i (trumpet c5))
                             (stretch i (trumpet bf4))
                             ;; measure 34  [D]
                             (stretch i (trumpet a4))
                             (stretch i (trumpet d5))
                             (stretch i (trumpet a4))
                             (stretch i (trumpet bf4))
                             (stretch qd (trumpet fs4))
                             (stretch i (trumpet d5))
                             ;; measure 35
                             (stretch i (trumpet a4))
                             (stretch i (trumpet bf4))
                             (stretch i (trumpet fs4))
                             (stretch i (trumpet g4))
                             (sustain 1.3 (stretch i (trumpet a4)))
                             (stretch i (trumpet bf4))
                             (stretch i (trumpet c5))
                             (stretch i (trumpet d5))
                             ;; meausre 36
                             (stretch i (trumpet e5))
                             (stretch i (trumpet f5))
                             (stretch s (trumpet e5))
                             (stretch i (trumpet d5))
                             (stretch s (trumpet cs5))
                             (stretch s (trumpet d5))
                             (stretch s (trumpet a4))
                             (stretch s (trumpet f4))
                             (stretch s (trumpet g4))
                             (stretch i (trumpet a4))
                             (stretch i (trumpet bf4))
                             ;; measure 37
                             (stretch i (trumpet a4))
                             (stretch i (trumpet g4))
                             (stretch s (trumpet fs4))
                             (stretch i (trumpet g4))
                             (stretch s (trumpet fs4))
                             (stretch s (trumpet g4))
                             (stretch s (trumpet d5))
                             (stretch s (trumpet bf4))
                             (stretch s (trumpet c5))
                             (stretch i (trumpet d5))
                             (stretch i (trumpet ef5))
                             ;; measure 38
                             (stretch i (trumpet d5))
                             (stretch i (trumpet c5))
                             (stretch s (trumpet b4))
                             (stretch i (trumpet c5))
                             (stretch s (trumpet b4))
                             (stretch s (trumpet c5))
                             (stretch s (trumpet g5))
                             (stretch s (trumpet e5))
                             (stretch s (trumpet f5))
                             (stretch i (trumpet g5))
                             (stretch i (trumpet a5))
                             ;; measure 39
                             (stretch i (trumpet g5))
                             (stretch i (trumpet f5))
                             (stretch s (trumpet e5))
                             (stretch i (trumpet f5))
                             (stretch s (trumpet e5))
                             (stretch s (trumpet f5))
                             (stretch s (trumpet c5))
                             (stretch s (trumpet a4))
                             (stretch s (trumpet bf4))
                             (stretch i (trumpet c5))
                             (stretch i (trumpet d5))
                             ;; measure 40
                             (stretch i (trumpet c5))
                             (stretch i (trumpet bf4))
                             (stretch s (trumpet a4))
                             (stretch i (trumpet bf4))
                             (stretch s (trumpet a4))
                             (stretch s (trumpet bf4))
                             (stretch s (trumpet f4))
                             (stretch s (trumpet d4))
                             (stretch s (trumpet e4))
                             (stretch i (trumpet f4))
                             (stretch i (trumpet g4))
                             ;; measure 41.1
                             (stretch i (trumpet a4))
                             (stretch i (trumpet bf4))
                             (stretch s (trumpet a4))
                             (stretch i (trumpet g4))
                             (stretch s (trumpet fs4))
                             ;; measure 41.2
                             (stretch i (trumpet a4))
                             (stretch i (trumpet bf4))
                             (stretch s (trumpet a4))
                             (stretch i (trumpet g4))
                             (stretch s (trumpet fs4))
(stretch q (trumpet g4))
(s-rest s)
(stretch s (trumpet d5))
(stretch s (trumpet b4))
(stretch s (trumpet c5))
;; measure 42
(stretch i (trumpet d5))
(stretch i (trumpet ef5))
(stretch i (trumpet d5))
(stretch i (trumpet c5))
(stretch i (trumpet b4))
(stretch i (trumpet c5))
(s-rest ts)
(sustain 1.1 (stretch ts (trumpet g5)))
(sustain 1.1 (stretch ts (trumpet f5)))
(stretch s (trumpet ef5))
(stretch s (trumpet c5))
;; measure 43
(stretch i (trumpet b4))
(stretch i (trumpet c5))
(s-rest ts)
(sustain 1.1 (stretch ts (trumpet g5)))
(sustain 1.1 (stretch ts (trumpet f5)))
(stretch s (trumpet ef5))
(stretch s (trumpet c5))
(stretch i (trumpet b4))
(stretch i (trumpet c5))
(stretch q (trumpet e5))
;; measure 44
(stretch w (trumpet d5))))))))



;; stretch .75 because q = 60 corresponds to a stretch of 1, so
;; q = 80 corresponds to 60/80 = .75


(defun gabrieli ()
  (loud lp (sim
            (seq (stretch .75 (tI_1)) (stretch .25 (tI_2)) (stretch .75 (tI_3)))
            (seq (stretch .75 (tII_1)) (stretch .25 (tII_2)) (stretch .75 (tII_3)))
            (seq (stretch .75 (h_1)) (stretch .25 (h_2)) (stretch .75 (h_3)))
            (seq (stretch .75 (b_1)) (stretch .25 (b_2)) (stretch .75 (b_3)))
            )))
