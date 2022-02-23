(cons
  [x xs
    [f _ (f x xs)]])

(nil
  [_ x x])

(cat
  [xs ys
    (xs
      [x xs (cons x (cat xs ys))]
      ys)])

(map
  [f xs
    (xs
      [x xs (cons (f x) (map f xs))]
      nil)])
