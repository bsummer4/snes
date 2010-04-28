"
# Syntax Analysis -- The Second Compiler Pass

The purpose of these macros is to handle funky syntax and to make the
job of later transformations eaiser.  The following rules apply to
code in this file:

  1. No macro may perform non-trivial transformations.
  2. No knowledge of the target architecture is allowed.
  3. Target the next layer: :IR

Here are the transformations that we do:

  - Tag expressions with (ir::expr expr)
  - Tag variables with (ir::var-ref)
  - Expand Control Structures
"

(in-package #:cs400-compiler)

(define-constant +front-tags+
    '((front::goto label)
      (front::label name)
      (front::block &body code)

      (front::if test then-form &optional else-form)
      (front::while test &body body)
      (front::do-while test &body body)
      (front::for ((setup test iterate) &body body))
      (front::switch expr &body cases)

      (front::global-variable-declaration)
      (front::global-variable-initialization)
      (front::local-variable-declaration)
      (front::local-variable-initialization)

      (front::+ x y)
      (front::- x y)
      (front::^ x y)
      (front::& x y)
      (front::band x y)
      (front::\| x y)
      (front::bor x y)
      (front::_ x y)
      (front::++ x)
      (front::-- x)
      (front::@ x)
      (front::return x)
      (front::break)
      (front::continue)
      (front::$ x)))

(define-constant +front-tag-names+
    (cons 'c::var
          #.`(quote
              ,(mapcar #'first +front-tags+))))


"# Utillity Functions"
(defmacro with-break (break-label &body code)
  `(with-goto-macrolet c::break ,break-label ,@code))

(defmacro with-continue (continue-label &body code)
  `(with-goto-macrolet c::continue ,continue-label ,@code))

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
  (with-gensyms ((top "while_label_top") (end "while_label_end"))
    `(with-break ,end
       (with-continue ,top
         (with-indent "_while"
           (c::label ,top)
           (c::if ,test
                  (with-indent "_while_body"
                    ,@body
                    (c::goto ,top)))
           (c::label ,end))))))

(defmacro c::do-while (test &body body)
  (with-gensyms ((top "dowhile_label_repeat")
                 (end "dowhile_label_end"))
    `(with-break ,end
       (with-continue ,top
         (with-indent "_do_while"
           (c::label ,top)
           (with-indent "_do_while_body"
             ,@body)
           (c::if ,test (c::goto ,top))
           (c::label ,end))))))

(defmacro c::for ((setup test iterate) &body body)
  `(with-indent _for
     ,setup
     (c::while ,test
       ,@body
       ,iterate)))

"## Switches"
(always-eval
 (defun gen-switch-label (value)
   (match value
     ((type fixnum) (gensym (format nil "CASE_~a_" value)))
     ('c::default (nice-gensym value))
     (_ (error "case ~s is not a number" value))))

 (defun switch-case-values (cases) (mapcar #'first cases))
 (defun switch-case-codes (cases) (mapcar #'rest cases))

 (defun make-switch-target (target code)
   `(progn (c::label ,target)
      ,@code))

 (defun switch-default-hack (jumps break-label)
   "The 'default' case must be 'goto'ed at the end of jump clauses,
    but it isn't required to be placed at the end of the switch.  So,
    we look for the generated default clause (which looks like '(goto
    DEFAULT####)') and move it to the end.  "
   (multiple-value-bind (defaults numbers)
       (lpartition (lambda (jump-form)
                     (eq 'c::default (second jump-form)))
                   jumps)
     (append numbers (or defaults
                         `((c::goto ,break-label)))))))

(defmacro switch-jump-entry (value target)
  (declare (type (or number (eql c::default)) value)
           (symbol target))
  (case value
    (c::default `(c::goto ,target))
    (t `(%switch-jump-entry ,value ',target))))

(defmacro c::switch (expr &body cases)
  "The output is
     - Each test in the order given a single test is of the form:
           (CMP num BEQ case_label)
     - The default test (BRA default_label)
     - each label (in the order given) and it's code"
  (with-gensyms (switch_end)
    (let* ((values (switch-case-values cases))
           (codes (switch-case-codes cases))
           (labels (mapcar #'gen-switch-label values)))
      `(with-indent _switch
         (with-break ,switch_end
           ,expr
           (with-indent _switch_tests
             ,@(switch-default-hack
                (mapcar (fn (v l) `(switch-jump-entry ,v ,l))
                        values labels)
                switch_end))
           (with-indent _switch_targets
             ,@(mapcar #'make-switch-target labels codes))
           (c::label ,switch_end))))))

(define-pass :front
  ((c::var )))
