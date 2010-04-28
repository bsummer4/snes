"
# Toplevel -- The First Compiler Pass

This defines the interface that is exposed to the C programmer.
Mostly this layer exists as a way to make invalid toplevel c code
invalid, and to document the general approach to layering.  All
interface code for this pass is defined in the :C package.

A pass interface in this compiler consists of a set of tags and a set
of toplevel macros.  Toplevel macros are responsible for doing the
transformations and passing control on to the next pass.  There are a
fixed set of toplevel macros that are defined in all passes.  They
are:

  - proc
  - var
  - proto
  - interrupt-handler
"

(in-package #:cs400-compiler)

(define-constant +c-operators+
    '((c::+ x y)
      (c::- x y)
      (c::^ x y)
      (c::& x y)
      (c::band x y)
      (c::\| x y)
      (c::bor x y)
      (c::_ x y)
      (c::++ x)
      (c::-- x)
      (c::@ x)
      (c::$ x)))

(define-constant +c-control-structures+
    '((c::block &body code)
      (c::if test then-form &optional else-form)
      (c::while test &body body)
      (c::do-while test &body body)
      (c::break)
      (c::continue)
      (c::return x)
      (c::for ((setup test iterate) &body body))
      (c::switch expr &body cases)
      (c::goto label)
      (c::label name)))

(define-constant +c-declarations+ '((c::proto name)))

(define-constant +c-tags+
    '#.(append
        +c-declarations+
        +c-operators+
        +c-control-structures+))

(define-constant +c-tag-names+
    (cons 'c::var
          #.`(quote
              ,(mapcar #'first +c-tags+))))


"# Utillity Functions"
(defun c-proc (code)
  (preexpand
   `(tag:let (c::var name type &optional value)
      ,code)))

(defun c-tag? (symbol)
  (declare (symbol symbol))
  (member symbol +c-tag-names+))

(defun c->front (symbol)
  (declare (symbol symbol))
  (assert (eq (symbol-package symbol)
              (find-package :c)))
  (intern (symbol-name symbol) (find-package :front)))

"# Pass Definition"
(always-eval
  #.`(define-pass toplevel
       ,@(mapcar #'list +c-tags+)
       ((c::var name type &optional value) ->
        `(progn
           (front::global-variable-declaration ,name ,type)
           ,(when value
                  `(front::global-variable-initialization ,name ,value))))
       ((c::proto name) -> front::proto)
       ((c::proc (name type) args &body code)
        -> `(front::proc ,name ,type ,args
                         ,(apply-pass
                           (c-proc `(progn ,@code))
                           'toplevel)))))

"TODO predicates defined later have higher priority.  "
(define-predicated-transformation (pass-to-front
                                   (fn1
                                     (print !1)
                                     (c-tag? (first !1)))
                                   toplevel)
    (tagname &rest args)
  "this is the fallback transformation.  it just renames tags to be in
   the :front package.  "
  `(,(c->front tagname) ,@args))

(define-named-transformation (c-var c::var toplevel)
    (var name type &optional value)
  (declare (ignore var))
  `(progn
     (front::local-variable-declaration ,name ,type)
     ,(when value
            `(front::local-variable-initialization ,name ,value))))
