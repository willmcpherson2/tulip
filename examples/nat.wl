(0 [f x x])

(s [n
  [f x (f n)]])

(+ [x y
  (x
    [n (s (+ n y))]
    y)])

(> [x y
  (x
    [n
      (y
        [m (> n m)]
        True)]
    False)])

(1 (s 0))

(2 (+ 1 1))

(main (> 2 1))
