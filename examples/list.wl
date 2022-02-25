(cons [x xs
  [f _ (f x xs)]])

(nil [_ x x])

(map [f xs
  (xs
    [x xs (cons (f x) (map f xs))]
    nil)])

(last [xs
  (xs
    [x xs
      (xs
        (last xs)
        x)]
    _)])

(true [x y x])

(false [x y y])

(not [x (x false true)])

(myList (cons true (cons false nil)))

(print [b (b True False)])

(main (print (last (map not myList))))
