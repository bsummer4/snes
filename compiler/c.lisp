;; TODO you can define a function argument and a function local
;; variable with the same name.

(in-package #:cs400-compiler)

"## Scratch Space"
(defmacro c::funcall (function &rest args)
  (dolist (a args) (assert (or (symbolp a) (numberp a))))
  ;; TODO This is just for testing; it doesn't do anything.
  ;(format *error-output* "~%funcall: ~a(~{~a~^, ~})~%~%" function args)
  `(with-indent ,(format nil "_call_to_~a" function)
     (need-call-space ,(length args))
     ,@(loop for arg in (reverse args)
             for i from 1
             when (numberp arg)
             collect `(lda ,arg)
             when (symbolp arg)
             collect `(var->A ,arg)
             collect `(asm sta :stack-indexed ,(1- (* 2 i))))
     (asm jsr :absolute (c-fn-unique-name ',function))))

(defmacro need-call-space (amount)
  (declare (ignore amount))
  (values))

;; - TODO These should access global variables.
;; - TODO The macrolet A->var in a scope should fall through to this
;;   version if it can't be found.
(defmacro A->var (var)
  (declare (ignore var))
  (error "Code outside of a function!"))

(defmacro var->A (var)
  (declare (ignore var))
  (error "Code outside of a function!"))



"## Labels, Gotos, Break, and Continue.  "

"
### Lexically bound macros and functions
These macros and functions will be rebound as needed in other macros
using CL:MACROLET and CL:FLET.
"
(defmacro c::continue () (error "continue statement not within a loop"))
(defmacro c::break () (error "break statement not within a loop or switch"))
(defmacro c::goto (label)
  (error "goto (~a) statement outside of a function body" label))
(defmacro c::label (name)
  (error "label (~a) statement outside of a function body" name))
(eval-when (:compile-toplevel :load-toplevel :execute)
    (defun lookup-label (name)
  (error "Trying to lookup a label (~a) outside of a function body" name)))

(defmacro labels-block (&body code)
  "Binds the macros c::goto and c::label to generate branches and asm
   labels.  "
  (let ((code (transform-c-syntax
               (preexpand `(progn ,@code)))))
    (with-gensyms (labels)
      `(with-symbol-alias-alist ,labels ,(find-labels code)
         (flet ((lookup-label (label)
                  (or (lookup label ,labels)
                      (error "Undefined label ~a" label))))
           (declare (ignorable #'lookup-label))
           (macrolet ((c::goto (label)
                        (declare (type symbol label))
                        `(%goto (lookup-label ',label)))
                      (c::label (name)
                        `(%label (lookup-label ',name))))
             ,code))))))

(defmacro with-goto-macrolet (macro-name label &body code)
  `(macrolet ((,macro-name () `(c::goto ,',label)))
     ,@code))

(defmacro with-continue (continue-label &body code)
  `(with-goto-macrolet c::continue ,continue-label ,@code))

(defmacro with-break (break-label &body code)
  `(with-goto-macrolet c::break ,break-label ,@code))


"## Control Strutures"
(defmacro c::if (test then-form &optional else-form)
  (with-gensyms ((else "iflabel_else_") (end "iflabel_end_"))
    `(with-indent "_if"
       ,test
       (%branch-if-not (lookup-label
                        ',(if else-form else end)))
       (with-indent "_if_then_form"
         ,then-form)
       ,(when else-form `(c::goto ,end))
       ,(when else-form `(c::label ,else))
       (with-indent "_if_else_form"
         ,else-form)
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

(defmacro c::for ((setup test iterate) &body body)
  `(with-indent _for
     ,setup
     (c::while ,test
       ,@body
       ,iterate)))

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

"### Switch"
(eval-when (:compile-toplevel :load-toplevel :execute)
 (defun switch-case-values (cases) (mapcar #'first cases))
 (defun switch-case-codes (cases) (mapcar #'second cases))
 (defun gen-switch-label (value)
   (etypecase value
     (number (gensym (format nil "CASE_~a_" value)))
     (symbol (progn
               (or (eq value 'default)
                   (error "case ~a is not a number" value))
               (nice-gensym value)))))

 (defun make-switch-jump-entry (value target)
   (if (eq value 'default)
       `(c::goto ,target)
       `(progn
          (asm cmp :immediate-w ,value)
          (emit (format nil "BEQ {~a}" ',target)))))

 (defun make-switch-target (target code)
   `(progn (c::label ,target) ,code))

 (defun switch-default-hack (jumps break-label)
   "The 'default' case must be 'goto'ed at the end of jump clauses, but
   it doesn't need to be at the end of the switch.  So, we look for
   the generated default clause (which like '(goto DEFAULT####)') and
   moves it to the end.  "
   (multiple-value-bind (defaults numbers)
       (lpartition (lambda (jump-form)
                     (eq 'c::goto (first jump-form)))
                   jumps)
     (append numbers (or defaults
                         `((c::goto ,break-label)))))))

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
                (mapcar #'make-switch-jump-entry values labels)
                switch_end))
           (with-indent _switch_targets
             ,@(mapcar #'make-switch-target labels codes))
           (c::label ,switch_end))))))

(defmacro c::block (&body code)
  `(progn ,@code))

(eval-when (:compile-toplevel :load-toplevel :execute)
  "## Scopes and their identifiers and tags.  "
  (defstruct scope
    "The point of giving scopes names is to help generate better error
   messages and better names of generated names.  For exapmle an
   struct without a type name needs to have a type name generated."
    identifiers tags name)

  (defun new-scope (name)
    (make-scope :identifiers (make-hash-table)
                :tags (make-hash-table)
                :name name))

  (defparameter *scopes* (list (new-scope 'global)))
  (defparameter *global-scope* (first *scopes*))

  (defstruct c-var name type storage-class address)
  (defun new-var (name type storage-class address)
    (make-c-var :name name
                :type type
                :storage-class storage-class
                :address address))

  (defun identifiers-table () (scope-identifiers (first *scopes*)))
  (defun tags-table () (scope-tags (first *scopes*)))

  (defun bind-identifier (name value &optional (scope (first *scopes*)))
    (setf (gethash name (scope-identifiers scope)) value)))

(defmacro with-scope (name &body body)
  `(let ((*scopes* (cons (new-scope ,name) *scopes*)))
     ,@body))


"## Global Memory Allocation"
(defparameter *next-available-global-space* 0
  "This will be ***MODIFIED*** when new global space is requested.  ")

(defun allocate-global (size)
  (prog1
      *next-available-global-space*
      (incf *next-available-global-space* size)))

(defmacro c::var (name type &optional value)
  "This expands into code that declares a global variable.  A separate
   version for stack variables is bound with a MACROLET in
   c::proto.  "
  (when value (error "Setting variables is not implemented.  "))
  `(let ((size 2)) ;; replace 2 with (size-of type)
     (when (in-table? ',name (identifiers-table))
       (error "More than one declaration of global identifier ~a.  "
              ',name))
     (setf (gethash ',name (identifiers-table))
           (new-var ',name ',type :static
                    (allocate-global size)))))



"## Functions and Subrountines"
(defmacro c::subroutine (name &body code)
  "This is like #'C-FN except it doesn't do any stack manipulation so no
   variables, etc.  The only really supported constructs are labels
   and gotos.  'NAME will **not** be mangled, so make sure it's what
   you want.  "

  `(with-indent ,(format nil "_subrountine_~s" (intern (symbol-name name)))
     (labels-block
       (bind-identifier ',name (list ',name :scope *scopes* :code ',code))
       (asm-code ',name)
       ,@code
       (asm rts :implied))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun c-fn-unique-name (symbol)
    "Returns a unique name for a function or a suggested unique-name.
     Also returns whether the function was already bound and whether
     it was already definied.  "
    (flet ((prototype? (fn) (not (cdr fn))))
      (multiple-value-bind (fn already-bound?)
          (gethash symbol (scope-identifiers *global-scope*))
        (values (if already-bound?
                    (first fn)
                    (nice-gensym symbol))
                already-bound?
                (not (prototype? fn)))))))

(defmacro c::proto (name)
  "Binds a unique name to the function 'NAME in the global scope.
   This allows us to reference functions in asm code that haven't been
   defined yet.  Return the unique name.  "
  (multiple-value-bind (unique bound? defined?)
      (c-fn-unique-name name)
    (if defined? (error "global identifier ~a is already bound" name))
    `(progn
       ,(unless bound?
          `(setf (gethash ',name (scope-identifiers *global-scope*))
                 (list ',unique)))
       (asm-code ',unique :prototype t))))

(defmacro with-grown-stack (stack-size &body code)
  "Return a PROGN that grows the stack by STACK-SIZE, runs CODE, then
   shrinks the stack by STACK-SIZE.  "
  `(progn
     ,(when (plusp stack-size)
            `(with-indent "_growing_the_stack"
               (asm tsc :implied)
               (asm sec :implied)
               (asm sbc :immediate-w ,stack-size)
               (asm tcs :implied)))
     ,@code
     ,(when (plusp stack-size)
             `(with-indent "_shrinking_the_stack"
                (asm tsc :implied)
                (asm clc :implied)
                (asm adc :immediate-w ,stack-size)
                (asm tcs :implied)))))

(defmacro with-stack-lookup-macros (variable-spaces &body code)
  "Binds C::VAR to set the variable if given a value and binds REF and
   SET to read from and write to a variable by name.  "
  (with-gensyms (stack-space)
    `(let ((,stack-space ,variable-spaces))
       (flet ((var-addr-mode (varname)
                (declare (ignore varname))
                :stack-indexed)
              (var-addr (varname)
                (elookup varname ,stack-space)))
         (macrolet ((var->A (var)
                      (etypecase var
                        (symbol `(asm lda (var-addr-mode ',var)
                                      (var-addr ',var)))
                        (number `(lda ,var))))
                    (A->var (var)
                      `(asm sta (var-addr-mode ',var)
                            (var-addr ',var)))
                    (c::var (name type &optional value)
                      (declare (ignore type))
                      (when value
                        `(progn
                           (lda ,value)
                           (A->var ,name)))))
           ,@code)))))

(defmacro with-stack-variables (variable-declarations args call-space
                                &body code)
  (let* ((vars variable-declarations)
         (local-stack-size call-space)
         (variable-spaces (iter (for (name . type) in vars)
                                (collect
                                    (cons name
                                          (prog1 (1+ local-stack-size)
                                            (incf local-stack-size 2))))))
         (argument-spaces (iter (for name in (reverse args))
                                (for index from (+ 1 2 local-stack-size) by 2)
                                (collect (cons name index)))))
    `(with-grown-stack ,local-stack-size
       (with-stack-lookup-macros ',(append argument-spaces variable-spaces)
         ,@code))))

(defmacro with-return (label &body code)
  `(with-goto-macrolet c::return ,label ,@code))

(defmacro c::proc ((name return-type) args &body code)
  (declare (ignore return-type))
  (let* ((input-code `(progn ,@code))
         (unique-name (c-fn-unique-name name))
         (code (preexpand (transform-c-syntax (preexpand input-code))))
         (needed-call-space (needed-call-space code))
         (vars (find-vars code)))
    `(with-indent ,(format nil "_function_~s"
                           (intern (symbol-name name)))
       ;;(with-gensyms (return-label)
       ;;(with-return ,return-label
       (with-scope ',name
         (asm-code ',unique-name)
         (16-bit-mode)
         (with-stack-variables ,vars ,args ,needed-call-space
           (labels-block
             (bind-identifier ',name
                              (list ',unique-name :code ',code)
                              *global-scope*)
             ,code))
         (asm rts :implied)))))


"## Operators"
(defmacro c::++ (var)
  (declare (type symbol var))
  `(c::block
    (var->A ,var)
    (asm inc :accumulator)
    (A->var ,var)))

"## Testing"
(defun repl ()
  (let ((eof '#.(gensym "EOF-")))
    (loop for x = (read *standard-input* nil eof)
       until (eq x eof) do (eval x)
       do (force-output))))

(defun compiler-reset ()
  "For convience at the lisp repl; Clears out the global scope
   effectively destroying everything the compiler has definied.  "
  (clrhash (scope-identifiers *global-scope*))
  (clrhash (scope-tags *global-scope*)))

(defun interactive-compiler-test (file)
  (compiler-reset)
  (with-open-file (*standard-input* file)
    (repl)))

(defun compile-c (file)
  (with-open-file (*standard-input* file)
    (repl)))

#|
(c::proc c::main ()
  (c::var c::x c::int 1)
  (c::var c::y c::int 2)
  c::x
  (A->var c::y)
  (c::while c::x (lda 0) (A->var c::x))
  (c::f c::x)
  (c::while c::y
    (c::switch c::x
      (3 (c::block (lda 1)
           (A->var c::x)))
      (4) (5) (6) (7) (8) (9 (c::block
                                 (lda 0)
                               (A->var c::x)))
      (default (c::block
                   (lda 1) (A->var c::y)
                   (c::break)))
      (10 (c::continue))))
  c::y)
|#
