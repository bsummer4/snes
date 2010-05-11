(in-package :c)

(cl:defmacro 8-bit-mode () `(s::8-bit-mode))
(cl:defmacro 16-bit-mode () `(s::16-bit-mode))
(cl:defmacro funcall (f) `(s::%jsr ',f))
(cl:defmacro => (var) `(far-back::store-variable ,var))
(cl:defmacro finalize () `(s::vector-table))

[#LoROM]

(var z int 0)
(proc (main int) (x)
  (var y int 0)
  (if (> x y) (++ z))
  (if (>= x y) (++ x))
  (if (< x y) (++ z))
  (if (<= x y) (++ x))
  (for ((= x (+ (- 1 -1) 0)) (&& 1 (< x (+ 1 9))) (++ y))
    (switch x
      (0 (main x))
      (1 (main (+ x (+ 1 3))))
      (2)
      (3 (main (main (main (main (++ x))))))
      (default (do-while x (+ 1 (-- x))))
      (4 (continue))))
  (return (+ x y)))


(interrupt-handler :reset
  [INC A]
  (funcall main))

(finalize)
