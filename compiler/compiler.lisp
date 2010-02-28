#!/usr/bin/sbcl --script

"A unix interface to the compiler.  "

(require :cs400-compiler)
(in-package :cs400-compiler)

(defun main (&optional program-name &rest args)
  (declare (ignore args program-name))
  (handler-case
      (let ((eof '#.(gensym "EOF-")))
        (loop for x = (read *standard-input* nil eof)
           until (eq x eof) do (eval x)))
    (error (e)
      (format *error-output* "Fatal Error:~%    ~a~%" e)
      1))
  0)

(sb-ext:quit :unix-status (apply #'main sb-ext:*posix-argv*))
