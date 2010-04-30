(in-package :c)

(proc (main int) (x)
  (var y int 0)
  (for ((= x 0) (< x 9) (++ x))
    (switch x
      (0 (f x))
      (1 (g (+ x (+ 1 3))))
      (2)
      (3 (f (f (f (f)))))
      (default (do-while x (+ 1 (-- x))))
      (4 (continue))))
  (return (+ x y)))
