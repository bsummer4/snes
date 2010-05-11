"
# Expression Flattening -- The Third Compiler Pass

This pass simply expands out control structures.  Maybe some more
stuff... Who knows?

TODO ++, -- need to be moved to the end of an expression
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
      (back::expr expr)))

(define-constant +back-operator-names+
  (mapcar #'front->cs400-compiler +front-operator-names+))

"# Utillities"
(require :memoize)
(memoize:def-memoized-function temporary-variable (number)
  "Returns a gensymed symbol.  Always returns the same symbol (EQ)
   given the same input.  "
  (gensym (format nil "TEMP-~d-" number)))

(defmacro spill (temp-var-num)
  (let ((var (temporary-variable temp-var-num)))
    `(progn
       (far-back::local-variable-declaration ,var c::int)
       (far-back::store-local-variable ,var))))

(defun primitive? (x)
  (typecase x (symbol t) (number t)
            (t (match x
                 ((list 'temp (type number)) t)))))

(defun all-primitive? (xs)
  (equalp xs (remove-if-not #'primitive? xs)))

(defun compound-forms (xs) (remove-if #'primitive? xs))

(defun replace-compounds-with-temps (forms used-temps)
  (iter (for form in forms)
        (if (primitive? form)
            (collect form)
            (collect (temporary-variable (incf used-temps))))))

(defun transform-operator (operator args used-temps &key funcall?)
  (let ((nice-op-name (package-change operator :far-back)))
    (if (all-primitive? args)
        (if funcall?
            `(far-back::funcall ,operator ,@args)
            `(,nice-op-name ,@args))
        `(progn
           ,@(let ((used-temps used-temps))
                  (iter (for form in (compound-forms args))
                        (collect `(expr ,form ,used-temps))
                        (collect `(spill ,(incf used-temps)))))
           (expr (,operator
                  ,@(replace-compounds-with-temps
                     args
                     used-temps))
                 ,used-temps)))))

(always-eval
  #.`(define-pass back
       ,@(mapcar #'list +back-tags+)
       ((back::proc name return-type args body)
        -> `(far-back::proc ,name ,return-type ,args
                            ,(apply-pass (taggify-form body
                                                       'back)
                                         'back)))))

(defun back->far-back (symbol)
  (package-change symbol :far-back))

(define-predicated-transformation (back-passthrough
                                   #l(let ((first (first !1)))
                                       (and
                                        (symbolp first)
                                        (symbol-in-package? first :back)))
                                   back)
    (head &rest tail)
  "Any tagged forms we don't otherwise handle are simply passed off
  to the 'back pass.  "
  `(,(back->far-back head) ,@tail))

(define-tag-substitution (expr back::expr back)
    (expr &optional (used-temps 0))
  (fare-matcher:match expr
    ((of-type number) `(far-back::load-number ,expr))
    ((of-type symbol) (when expr `(far-back::load-local-variable ,expr)))
    ((of-type string) `(far-back::comment ,expr))
    ((list* (and operator
                 (when (member operator +back-operator-names+)))
            args)
     (fare-matcher:match expr
       (`(++ ,x)
         (if (symbolp x) `(far-back::increment ,x) (wtf expr)))
       (`(-- ,x)
         (if (symbolp x) `(far-back::decrement ,x) (wtf expr)))
       (`(= ,x ,y)
         (if (symbolp x)
             `(progn
                (expr ,y ,used-temps)
                (far-back::store-local-variable ,x))
             (error
              "We don't handle non-trivial left-hand-sides.  So '~s is invalid.  "
              expr)))
       (`(&& ,x ,y)
         (with-gensyms ((skip "and-skip"))
           `(progn
              (expr (goto-if-not ,x ,skip) ,used-temps)
              (expr ,y ,used-temps)
              (far-back::label ,skip))))
       ;; TODO (`(|| ,x ,y)
       ;; TODO (`(and ,x ,y)
       ;; TODO (`(or ,x ,y)

       (`(< ,x ,y)
         (with-gensyms ((skip "compare-skip") (end "compare-end"))
           `(progn
              (expr (goto-if->= ,x ,y ,skip) ,used-temps)
              (expr 1)
              (far-back::goto ,end)
              (far-back::label ,skip)
              (expr 0)
              (far-back::label ,end))))

       ;; TODO Other comparisons
       ;; TODO goto-if-not should be optimized here instead of in
       ;;      c::front

       (* (transform-operator operator args used-temps))))
    ((list* (and fn (of-type symbol)) args)
     (transform-operator fn args used-temps :funcall? t))
    (x (wtf x))))
