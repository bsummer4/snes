#!/usr/bin/sbcl --script

"
A unix interface to the c compiler.

If you want to dump a core image, define *script?* to nil before
loading this file.  Otherwise we simply run the c-compiler as script.
"

(defvar *script?* t)

(let ((*standard-output* *error-output*))
 (require :cs400-compiler))

(in-package :cs400-compiler)

(defun main (args)
  (declare (ignore args))
  (repl)
  (compile-c "compiler-lib.c.lisp")
  0)

(defun top ()
  (in-package :cs400-compiler)
  (sb-ext:quit
   :unix-status
   (handler-case (apply #'main sb-ext:*posix-argv*)
     (error (e)
       (format *error-output* "Fatal Error:~% ~a~%" e)
       2)
     (t (s)
       (declare (ignore s))
       1))))

(if cl-user::*script?*
    (top)
    (sb-ext:save-lisp-and-die "compiler"
                              :executable t
                              :toplevel #'top
                              :purify t))
