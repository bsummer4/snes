"
# Control Structures -- The Second Compiler Pass

This pass simply expands out control structures.

TODO Handle front::block

     The way to do this is: Annotate all variable references with,
     say, front::var, then bind front::var as a macrolet in
     front::block and front::proc, and also bind
     front::local-variable-declaration and
     front::local-variable-initialization.  Use these macrolets to
     rename variables within blocks.
"

(in-package #:cs400-compiler)

(define-constant +front-tags+
    '((front::goto label)
      (front::label name)
      (front::block &body code)
      (front::break)
      (front::continue)
      ;; (front::return) this is handled seaparatly in front::proc

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
      (front::$ x)))

(define-constant +front-tag-names+
    (cons 'c::var
          #.`(quote
              ,(mapcar #'first +front-tags+))))

"# Utillity Functions"
(defun symbol-in-package? (symbol package)
  (eq (symbol-package symbol) (find-package package)))

(always-eval
  (defun package-change (symbol to-package)
    (declare (symbol symbol) (keyword to-package))
    (intern (symbol-name symbol) to-package))

  (defun front->cs400-compiler (symbol) (package-change symbol :s))
  (defun front->back (symbol) (package-change symbol :back)))

(define-constant +front-operator-names+
  (append '(front::goto-if-not
            front::goto-if-<
            front::goto-if->
            front::goto-if-<=
            front::goto-if->=
            front::goto-if-!=
            front::goto-if-==)
          (mapcar #'c->front +c-operator-names+)))

(defun operators-to-the-compiler-package (form)
  (tree-walk
   (fn1 (if (member !1 +front-operator-names+)
            (front->cs400-compiler !1)
            !1))
   form))

(defmacro with-annotated-operators (&body code)
  "Uses macrolets to transform expressions like:
      (front::+ (front::+ 3 4) 5)

   into expressions like
       (back::expr (+ (+ 3 4) 5))"
  `(macrolet #.(mapcar (fn1 `(,!1 (&rest args)
                                   `(back::expr
                                     (,',(front->cs400-compiler !1)
                                         ,@(mapcar
                                            #'operators-to-the-compiler-package
                                            args)))))
                       +front-operator-names+)
             (macrolet ((front::funcall (f &rest args)
                          `(back::expr
                            (,f ,@(mapcar
                                   #'operators-to-the-compiler-package
                                   args)))))
               ,@code)))

(defmacro with-goto-macrolet (macro-name label &body code)
  `(macrolet ((,macro-name () `(back::goto ,',label)))
     ,@code))

(defmacro with-break (break-label &body code)
  `(with-goto-macrolet c-break ,break-label ,@code))

(defmacro with-continue (continue-label &body code)
  `(with-goto-macrolet c-continue ,continue-label ,@code))

(defmacro with-return (label &body code)
  `(macrolet ((front::return (expr)
                `(progn
                   ,expr
                   (back::goto ,',label))))
     ,@code))

"## Switch Utillities"
(always-eval
 (defun gen-switch-label (value)
   (match value
     ((type fixnum) (gensym (format nil "CASE_~a_" value)))
     ('c::default (nice-gensym value))
     (_ (error "case ~s is not a number" value))))

 (defun switch-case-values (cases) (mapcar #'first cases))
 (defun switch-case-codes (cases) (mapcar #'rest cases))

 (defun make-switch-target (target code)
   `(progn (back::label ,target)
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
                         `((back::goto ,break-label)))))))

(defmacro switch-jump-entry (value target)
  (declare (type (or number (eql c::default)) value)
           (symbol target))
  (case value
    (c::default `(back::goto ,target))
    (t `(%switch-jump-entry ,value ',target))))

(defun returnify (body)
  (preexpand
   (with-gensyms (return)
    `(with-return ,return
       ,body
       (back::label ,return)))))

(defun annotate-functions-and-variables (form)
  "Wraps function calls and variable references with BACK::EXPR"
  (code-walk
   (fn1 (cond ((and (listp !1) (eq (symbol-package (first !1))
                                   (find-package :c)))
               `(front::funcall ,@!1))
              ((symbolp !1) `(back::expr ,!1))
              (t !1)))
   form))

"# Pass Definition"
(always-eval
  #.`(define-pass front
       ,@(mapcar #'list +front-tags+)
       ((front::proc name return-type args body)
        -> `(back::proc ,name ,return-type ,args
                        ,(preexpand
                          `(with-annotated-operators
                             ,(annotate-functions-and-variables
                               (apply-pass (taggify-form (returnify body) 'front)
                                           'front))))))))


"# Pass Transformations"
(define-predicated-transformation (front-passthrough
                                   #l(let ((first (first !1)))
                                       (and
                                        (symbolp first)
                                        (symbol-in-package? first :front)))
                                   front)
    (head &rest tail)
  "Any tagged forms we don't otherwise handle are simply passed off
  to the 'back pass.  "
  `(,(front->back head) ,@tail))

(define-predicated-transformation (c-operators
                                   #l(member (first !1)
                                             +front-operator-names+)
                                   front)
    (&rest form)
  "We just untag operators.  They will be handled in front::proc with
   a macrolet .  "
  form)

(define-tag-substitution (c-break front::break front)
    ()
 (error "'break' statement outside of a switch or loop"))

(define-tag-substitution (c-continue front::continue front)
    ()
  (error "'continue' statement outside of a loop"))

(defmacro front-goto-if-not (expr label)
  (fare-matcher:match expr
    (`(front::> ,x ,y) `(front::goto-if-<= ,x ,y ,label))
    (`(front::< ,x ,y) `(front::goto-if->= ,x ,y ,label))
    (`(front::>= ,x ,y) `(front::goto-if-< ,x ,y ,label))
    (`(front::<= ,x ,y) `(front::goto-if-> ,x ,y ,label))
    (`(front::== ,x ,y) `(front::goto-if-!= ,x ,y ,label))
    (`(front::!= ,x ,y) `(front::goto-if-== ,x ,y ,label))
    (* `(front::goto-if-not ,expr ,label))))

(define-tag-substitution (c-if front::if front)
    (test then-form &optional else-form)
  (with-gensyms ((else "if_else_") (end "if_end_"))
    `(with-indent "_if"
       (front-goto-if-not ,test ,(if else-form else end))
       (with-indent "_if_then" ,then-form)
       ,(when else-form `(back::goto ,end))
       ,(when else-form `(back::label ,else))
       (with-indent "_if_else" ,else-form)
       (back::label ,end))))

(define-tag-substitution (c-while front::while front)
    (test &body code)
  (with-gensyms ((top "while_label_top") (end "while_label_end"))
    `(with-break ,end
       (with-continue ,top
         (with-indent "_while"
           (back::label ,top)
           (c-if ,test
                  (with-indent "_while_body"
                    ,@code
                    (back::goto ,top)))
           (back::label ,end))))))

(define-tag-substitution (c-do-while front::do-while front)
    (test &body body)
  (with-gensyms ((top "dowhile_label_repeat")
                 (end "dowhile_label_end"))
    `(with-break ,end
       (with-continue ,top
         (with-indent "_do_while"
           (back::label ,top)
           (with-indent "_do_while_body"
             ,@body)
           (c-if ,test (back::goto ,top))
           (back::label ,end))))))

(define-tag-substitution (c-for front::for front)
    ((setup test iterate) &body body)
  `(with-indent _for
     ,setup
     (c-while ,test
       ,@body
       ,iterate)))

(define-tag-substitution (c-switch front::switch front)
    (expr &rest cases)
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
           ,expr
         (with-break ,switch_end
           (with-indent _switch_tests
             ,@(switch-default-hack
                (mapcar (fn (v l) `(switch-jump-entry ,v ,l))
                        values labels)
                switch_end))
           (with-indent _switch_targets
             ,@(mapcar #'make-switch-target labels codes))
           (back::label ,switch_end))))))
