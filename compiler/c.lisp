"
# Syntax Analysis

This exposes all the user-visible macros as thin macros over compiler
primitives.

The purpose of these macros is to handle funky syntax and to make the
job of later transformations eaiser.  The following rules apply to
code in this file:

  1. No macro may perform non-trivial transformations.
  2. No knowledge of the target architecture is allowed.
  3. (Possibly) only target macros defined in :CL and :IR.

Here are the transformations that we do:

  - Tag expressions with (expr expr)
  - Expand Control Structures
"

(in-package #:cs400-compiler)

(defmacro c::goto (label) `(c-goto ,label))
(defmacro c::label (name) `(c-label ,name))

(defmacro c::continue () `(c-continue))
(defmacro c::break () `(c-break))
(defmacro c::return (expr) `(c-return ,expr))

(defmacro c::proto (name) `(c-proto ,name))
(defmacro c::proc ((name return-type) args &body code)
  (declare (symbol return-type name) (list args))
  `(c-proc (,name ,return-type) ,args ,@code))

(defmacro c::var (name type &optional value)
  (declare (symbol name type))
  (if value
      `(c-var ,name ,type ,value)
      `(c-var ,name ,type)))

(defmacro c::block (&body code) `(c-block ,@code))

(defmacro c::if (test then-form &optional else-form)
  (with-gensyms ((else "if_else_") (end "if_end_"))
    `(with-indent "_if"
       ,test
       (c-goto-<if-zero> ,(if else-form else end))
       (with-indent "_if_then" ,then-form)
       ,(when else-form `(c::goto ,end))
       ,(when else-form `(c::label ,else))
       (with-indent "_if_else" ,else-form)
       (c::label ,end))))

(defmacro c::while (test &body body)
  `(c-while ,test ,@body))

(defmacro c::do-while (test &body body)
  `(c-do-while ,test ,@body))

(defmacro c::for ((setup test iterate) &body body)
  `(c-for (,setup ,test ,iterate) ,@body))

(defmacro c::switch (expr &body cases) `(c-switch ,expr ,@cases))

; '(c::+ c::- c::^ c::& c::band c::\| c::bor c::_ c::++ c::-- c::@ c::$)
