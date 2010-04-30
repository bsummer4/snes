"
# Expression Flattening -- The Third Compiler Pass

This pass simply expands out control structures.  Maybe some more
stuff... Who knows?
"

(in-package #:cs400-compiler)
(defpackage :codegen)
(define-constant +back-tags+
    '((back::goto label)
      (back::label name)
      (back::global-variable-declaration)
      (back::global-variable-initialization)
      (back::local-variable-declaration)
      (back::local-variable-initialization)
      (back::expr)))
