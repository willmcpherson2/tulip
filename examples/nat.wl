(z [f x x])

(s [n
  [f x (f n)]])

(plus [x y
  (x
    [n (s (plus n y))]
    y)])

(greater [x y
  (x
    [n
      (y
        [m (greater n m)]
        True)]
    False)])

(one (s z))

(two (plus one one))

(main (greater two one))
