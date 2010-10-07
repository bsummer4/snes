"
This is just the package definition.  It's really too small to
justify having its own file, but I think it will grow.  Eventualy we
need to export symbols and stuff here.
"

(defpackage #:cs400-compiler
  (:use #:cl #:iterate #:cl-match #:split-sequence)
  (:shadow #:match)
  (:nicknames #:s))

(defpackage :c)

(in-package #:cs400-compiler)
(setf (readtable-case *readtable*) :invert)

(defconstant +asm-syntax-start+ #\[)
(defconstant +asm-syntax-stop+ #\])

(defun parse-asm (stream depth chars)
  (if (zerop depth)
      (coerce (nreverse (cdr chars)) 'string)
      (let* ((c (read-char stream))
             (depth (cond
                      ((char= c +asm-syntax-start+) (1+ depth))
                      ((char= c +asm-syntax-stop+) (1- depth))
                      (t depth))))
        (cons c depth)
        (parse-asm stream depth (cons c chars)))))

(set-macro-character #\[ (lambda (s c)
                           (declare (ignore c))
                           `(emit ,(parse-asm s 1 nil))))
