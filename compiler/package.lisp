"
This is just the package definition.  It's really too small to
justify having its own file, but I think it will grow.  Eventualy we
need to export symbols and stuff here.
"

(defpackage #:cs400-compiler (:use #:cl))
(in-package #:cs400-compiler)
(setf (readtable-case *readtable*) :invert)
