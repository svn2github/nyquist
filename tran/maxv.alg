(MAXV-ALG
  (NAME "maxv")
  (ARGUMENTS ("sound_type" "s1") ("sound_type" "s2")) 
  (SAMPLE-RATE (MAX s1 s2))
  (COMMUTATIVE (s1 s2))
  (START (MAX s1 s2))
  (INNER-LOOP "double x1 = s1;
            double x2 = s2;
            output = (sample_type) (x1 > x2 ? x1 : x2)")
  (TERMINATE (MIN s1 s2))
  (LOGICAL-STOP (MIN s1 s2))
)
