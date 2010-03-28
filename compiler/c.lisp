(in-package #:cs400-compiler)



"
## Code Analitics

This has a bunch of functions for scanning a code body looking for
certain forms.  We need to be able to, for example, find all the
labels or variable declarations in a function body.
"


(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((dh (n fs &body code)
               `(macroexpand-dammit::defhandler ,n ,fs ,@code)))
    "We tell macroexpand-dammit not to expand c::label c::goto or c::var,
   since we need to scan code-bodies for these.  "
    (dh c::label (label name) `(list ',label ',name))
    (dh c::goto (goto name) `(list ',goto ',name))
    (dh c::var (var name type &optional default-value)
        (if default-value
            `(list ',var ',name ',type ',default-value)
            `(list ',var ',name ',type))))

  (defun preexpand (expr)
    (macroexpand-dammit:macroexpand-dammit expr))

  (defun code-walk (function code)
    "Like calls FUNCTION on any non-special-from code fragments.
   Currently only these special forms are recognized:
    (LET FLET MACROLET IF PROGN)"
    ;; TODO Support all special forms
    (flet ((recur (code) (code-walk function code) (values)))
      (match code
        ((list* 'let forms body4) (mapc #'recur body4))
        ((list* 'flet forms body3) (mapc #'recur body3))
        ((list* 'macrolet forms body2) (mapc #'recur body2))
        ((list 'if expr then) (recur then))
        ((list 'if expr then else) (recur then) (recur else))
        ((list* 'progn body1) (mapc #'recur body1))
        ((as form *) (& function form))))
    (values))

  (defun find-forms (predicate code)
    (collecting
      (code-walk (fn1 (if (& predicate !1)
                           (collect !1)))
                  code)))

  (defun label-form? (expr) (match? (list 'c::label symbol) expr))
  (defun var-form? (expr)
    (match expr
      ((list 'c::var (type symbol) (type symbol)) t)
      ((list 'c::var (type symbol) (type symbol) (type number)) t)))

  (defun find-labels (code)
    "Returns all label names in the code block.  If a label is multiply
   defined, an error is signaled.  "
    (let ((labels (mapcar #'second
                          (find-forms #'label-form? code))))
      (aif (non-unique-items labels)
           (error
            "The following labels appear more than once in the same scope: ~a"
            it)
           labels)))

  (defun find-vars (code)
    (let* ((var-forms (find-forms #'var-form? code))
           (var-names (mapcar #'second var-forms))
           (var-types (mapcar #'third var-forms)))
      (aif (non-unique-items var-names)
           (error
            "The following variables are declared more than once in the
           same scope: ~a" it)
           (mapcar #'cons var-names var-types)))))

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
  (let ((code (preexpand `(progn ,@code))))
    (with-gensyms (labels)
      `(with-symbol-alias-alist ,labels ,(find-labels code)
         (flet ((lookup-label (label) (or (lookup label ,labels)
                                          (error "Undefined label ~a" label))))
           (macrolet ((c::goto (label)
                        (declare (type symbol label))
                        `(%goto (lookup-label ',label)))
                      (c::label (name)
                        (declare (type symbol name))
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
       ,@body ,iterate)))



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
  `(labels-block
     (bind-identifier ',name (list ',name :scope *scopes* :code ',code))
     (asm-code ',name)
     ,@code
     (asm rts :implied)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun c-fn-unique-name (symbol)
    "Returns a unique name for a function or a suggested unique-name.
   Also returns whether the function was already bound and whether it
   was already definied.  "
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
    (unless bound?
      `(setf (gethash ',name (scope-identifiers *global-scope*))
             (list ',unique)))
    `',unique))

(defmacro with-grown-stack (stack-size &body code)
  "Return a PROGN that grows the stack by STACK-SIZE, runs CODE, then
   shrinks the stack by STACK-SIZE.  "
  `(progn
     ,(when (plusp stack-size)
            `(with-indent "_growing_the_stack"
               (asm tsc :implied)
               (asm clc :implied)
               (asm sbc :immediate-w ,stack-size)
               (asm tcs :implied)))
     ,@code
     ,(when (plusp stack-size)
             `(with-indent "_shrinking_the_stack"
                (asm tsc :implied)
                (asm sec :implied)
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
         (macrolet ((c::ref (var)
                      `(asm lda (var-addr-mode ',var)
                            (var-addr ',var)))
                    (c::set (var)
                      `(asm sta (var-addr-mode ',var)
                            (var-addr ',var)))
                    (c::var (name type &optional value)
                      (declare (ignore type))
                      (when value
                        `(progn
                           (lda ,value)
                           (c::set ,name)))))
           ,@code)))))

(defmacro with-stack-variables (variable-declarations &body code)
  (let* ((vars variable-declarations)
         (local-stack-size 0)
         (variable-spaces (iter (for (name . type) in vars)
                                (collect
                                    (cons name
                                          (prog1 (1+ local-stack-size)
                                            (incf local-stack-size 2)))))))
    `(with-grown-stack ,local-stack-size
       (with-stack-lookup-macros ',variable-spaces
         ,@code))))

(defmacro c::proc (name args &body code)
  (when args (error "Function arguments are not supported.  "))
  (let* ((input-code `(progn ,@code))
         (unique-name (c-fn-unique-name name))
         (code (preexpand input-code))
         (vars (find-vars code)))
    `(with-scope ',name
       (asm-code ',unique-name)
       (with-stack-variables ,vars
         (labels-block
           (bind-identifier ',name (list ',name :scope *scopes* :code ',code))
           ,code))
       (asm rts :implied))))


"## Testing"
(defun repl ()
  (let ((eof '#.(gensym "EOF-")))
    (loop for x = (read *standard-input* nil eof)
       until (eq x eof) do (eval x)
       do (force-output))))

(defun interactive-compiler-test (file)
  (compiler-reset)
  (with-open-file (*standard-input* file)
    (repl)))

(defun compiler-reset ()
  "For convience at the lisp repl; Clears out the global scope
   effectively destroying everything the compiler has definied.  "
  (clrhash (scope-identifiers *global-scope*))
  (clrhash (scope-tags *global-scope*)))


"## Compiler Defined Code"
(c::proto main)

(c::subroutine reset
  (asm clc :implied)
  (asm xce :implied)
  (16-bit-mode)
  (asm jsr :immediate 'main))

(set-reset-handler "$8000")

(emit "#LoROM")
