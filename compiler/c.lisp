"
Here is a table of syntaxes for operators.  I don't like some of the
things I chose (like _ instead of |), so if you have some better ideas
tell me.

========= ============ ================
C         Sexp c       More lispy
========= ============ ================
+a        (+ a)        (+ a)
a + b     (+ a b)      (+ a b c ...)
++a       (++* a)      (preincr a)
a++       (++ a)       (incr a)
a += b    (+= a b)     (incr a b)
-a        (- a)        (- a)
a - b     (- a b)      (- a b c)
--a       (--* a)      (predecr a)
a--       (-- a)       (decr a)
a -= b    (-= a b)     (decr a b)
a * b     (* a b)      (* a b)
a *= b    (*= a b)     (set * a b)
a / b     (/ a b)      (/ a b)
a /= b    (/= a b)     (set / a b)
a % b     (% a b)      (mod a b)
a %= b    (%= a b)     (set mod a b)
a < b     (< a b)      (< a b ...)
a <= b    (<= a b)     (<= a b ...)
a > b     (> a b)      (> a b ...)
a >= b    (>= a b)     (>= a b ...)
a != b    (!= a b)     (!= a b ...)
a == b    (== a b)     (== a b ...)
!a        (! a)        (not a)
a && b    (&& a b)     (and a b ...)
a || b    (or a b)     (or a b ...)
a << b    (<< a b)     (lshift a b)
a <<= b   (<<= a b)    (set lshift a b)
a >> b    (>> a b)     (rshift a b)
a >>= b   (>>= a b)    (set rshift a b)
~a        (~ a)        (nnot a)
a & b     (& a b)      (nand a b)
a &= b    (&= a b)     (set nand a b)
a | b     (_ a b)      (!or a b)
a |= b    (_= a b)     (set !or a b)
a ^ b     (^ a b)      (^ a b ...)
a ^= b    (^= a b)     (set ^ a b ...)
a = b     (= a b)      (set a b)
a()       (a)          (a)
a[b]      ([] a b)     (a b)
*a        (@ a)        (a)
&a        (& a)        (? a)
a->b      (-> a :b)    (a :b)
(type)a   (type a)     (type a)
a , b     (do a b)     (do a b)
a ? b : c (? a b c)    (if a b c)
sizeof a  (sizeof a)   (size a)
========= ============ ================

Some expressions:

    a.b.c[2]
    (a :b :c 2)
    ([] (@. (@. a :b) :c) 2)

    a->b.c[(x == 3) ? 1 : 2] + 3 + a->a
    (+ (a :b :c (if (== x 3) 1 2)) 3 (a :a))
    (+ ([] (.@ (-> a :b) :c) (? f 1 2)) 3 (-> a a))


Some functions:

C:

    char *strcpy(char *dest, const char *src)
    {
       char *save = dest;
       while (*dest++ = *src++);
       return save;
    }


Sexp C:

    (defun strcpy (dest src)
      (type :*char (const :*char) -> :*char)
      (var save dest :*char)
      (while (== (@ (++ dest)) (@ (++ dest))))
      (return save))


More lispy:

    (defun strcpy (dest src)
      (type :*char (const :*char) -> :*char)
      (let ((save dest))
        (while (== ((++ dest)) ((++ src))))
        save))

Another example

void f (int n, char x[n])
{
  for (int i = 0; ii < n; ii++) {
    if (!(i % 3)) {
      x[i] = '\0'
    }
  }
}

(defun f (n x)
  (type :int (:char n))
  (for ((var i 0 :int) (< ii n) (++ i))
    (if (! (% i 3))
      (= ([] x i) #\null))))

(defun f (n x)
  (type int (char n))
  (iter (for i below n)
        (if (divisible i 3)
            (set (x i) #\null))))
"
