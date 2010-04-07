"
## Code Analitics

This has a bunch of functions for scanning a code body looking for
certain forms.  We need to be able to, for example, find all the
labels or variable declarations in a function body.
"

(in-package :cs400-compiler)

(macrolet ((dh (n fs &body code)
             `(macroexpand-dammit::defhandler ,n ,fs ,@code)))
  "We tell macroexpand-dammit not to expand c::label c::goto c::var
     var->A or A->var, since we need to scan code-bodies for these (or
     because we want rebind them in a macrolet after preexpansion).  "
  (dh c::label (label name) `(list ',label ',name))
  (dh c::goto (goto name) `(list ',goto ',name))
  (dh A->var (set var) `(list ',set ',var))
  (dh var->A (ref var) `(list ',ref ',var))
  (dh need-call-space (macro-name amount) `(list ',macro-name ',amount))
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
    (flet ((recur (code) (code-walk function code)))
      (match code
        ((list* 'let forms body) `(let ,forms
                                     ,@(mapcar #'recur body)))
        ((list* 'flet forms body) `(flet ,forms
                                      ,@(mapcar #'recur body)))
        ((list* 'macrolet forms body) `(macrolet ,forms
                                          ,@(mapcar #'recur body)))
        ((list 'if expr then) `(if ,expr ,(recur then)))
        ((list 'if expr then else) `(if ,expr
                                        ,(recur then)
                                        ,(recur else)))
        ((list* 'progn body) `(progn ,@(mapcar #'recur body)))
        ((as form *) (& function form)))))

(defun find-forms (predicate code)
  (collecting
    (code-walk (fn1 (if (& predicate !1)
                        (collect !1))
                    (values))
               code)))

(defun c-symbol? (symbol)
  (eq (symbol-package symbol) (find-package :c)))

(defun call-space-request? (form)
  (match form
    ((list 'need-call-space (type number))
     t)))

(defun funcall-form? (expr)
  (match expr
    ((list* (as f (type symbol)) xs)
     (and (c-symbol? f)
          (not (fboundp f))))))

(defun var-ref-form? (expr)
  (match expr
    ((type symbol) (c-symbol? expr))))

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
         (mapcar #'cons var-names var-types))))

(defun find-call-space-requests (code)
  (find-forms #'call-space-request? code))

(defun needed-call-space (code)
  (* 2 ;; TODO This assumes that all types are two bytes
     (or (iter (for form in (find-call-space-requests code))
           (maximize (second form)))
         0)))

(defun transform-expr (expr)
  (cond ((funcall-form? expr) `(c::funcall ,@expr))
        ((var-ref-form? expr) `(var->A ,expr))
        ((numberp expr) `(var->A ,expr))
        (t expr)))

(defun transform-c-syntax (block)
  (code-walk #'transform-expr block))
