#!/usr/bin/sbcl --script

"This is the code that the user will actually run.  "

(load "asm.lisp")
(load "c.lisp")

(loop for x = (read *standard-input* nil +eof+)
      when (eq x +eof+) do (return)
      do (eval x))
