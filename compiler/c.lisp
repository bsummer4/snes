"Implements the c compiler.  "

(in-package #:cs400-compiler)

;; The point of giving scopes names is to help generate better error
;; messages and better names of generated names.  For exapmle an
;; struct without a type name needs to have a type name generated.
(defstruct scope objects types name)
(defun new-scope (name)
  (make-scope :objects (make-hash-table)
              :types (make-hash-table)
              :name name))

(defparameter *scopes* (list (new-scope 'global)))
(defparameter *global-scope* (first *scopes*))

;; Lables don't have nested scoping, so we simply keep track of the
;; current scope as a hash-table.  There is no global labels scope, so
;; it will be nil at the top level.
(defparameter *labels* nil)

(defun objects-table () (scope-objects (first *scopes*)))
(defun types-table () (scope-types (first *scopes*)))


(defmacro compile-c (expr)
  "We support numbers and addition.  "
  (etypecase expr
    (number `(asm lda :immediate ,expr))
    (list (progn (assert (eq '+ (first expr)))
                 (case (length expr)
                   (1 nil)
                   (2 `(compile-c ,(second expr)))
                   (t `(progn
                         (asm clc :implied)
                         (asm lda :immediate ,(second expr))
                         ,@(loop for x in (cddr expr)
                                 collect `(asm adc :immediate ,x)))))))))

(defmacro c-var (name type &optional value)
  "TODO This code will be seen eliminated by the c-fn macro if it's in
        a valid position.  "
  (declare (ignore name type value))
  (return-from c-var (values))
  (error "Variable declaration in an invalid position"))

(defmacro c-label (name)
  (unless (symbolp name) (insult))
  `(emit (format nil "{~a}" (gethash ',name *labels*))))

(defmacro c-goto (name)
  (unless (symbolp name) (insult))
  (let ((tmp (gensym)))
    `(let ((,tmp (gethash ',name *labels*)))
       (if ,tmp
           (emit (format nil "BRA {~a}" ,tmp))
           (error "Trying to goto a non-existent label: ~a" ',name)))))

(defun grab-vars (code)
  "Returns all variable names declared at the top of CODE.  "
  (flet ((is-var? (expr)
           (and (listp expr)
                (member (length expr) '(3 4) :test #'eql)
                (eq (first expr) 'c-var)
                (symbolp (second expr))
                (symbolp (third expr)))))
    (loop for expr in code
          until (not (or (stringp expr)
                         (is-var? expr)))
          unless (stringp expr) ;; Ignore Docstrings
          collect (second expr))))

(defun grab-labels (code)
  "Returns all label names in the code block.  "
  (flet ((is-label? (expr)
           (and (listp expr)
                (= 2 (length expr))
                (eq (first expr) 'c-label)
                (symbolp (second expr)))))
    (mapcar #'second (filter #'is-label? code))))

(defun scan-labels (code)
  "Return an alist mapping labels in code to their new globally-unique
   names. "
  (let ((labels (grab-labels code)))
    (aif (non-unique-items labels)
         (error
          "The following labels appear more than once in the same scope: ~a"
          it))
    (mapcar (lambda (label) (cons label (gensym (symbol-name label))))
            labels)))

(defun unprogn (code)
  (if (and (listp code) (eq (first code) 'progn))
      (flatten (mapcar #'unprogn (cdr code)))
      (list code)))

(defun c-preexpand (code)
  "Recursively pre-expand and unprogn some specific macros so we can
   analyze the resulting code.  "
  (let ((to-expand '(c-if)))
    (flatten
     (loop for stmt in code
        collect (if (and (listp stmt) (member (first stmt) to-expand))
                    (unprogn (macroexpand-1 stmt))
                    (list stmt))))))

(defmacro with-scope (name &body body)
  `(let ((*scopes* (cons (new-scope ,name) *scopes*)))
     ,@body
     (values)))

(defun in-function? () (not (eq (first *scopes*) *global-scope*)))
(defmacro c-fn (name args &body code)
  (when args (error "Function arguments are not supported.  "))
  (let* ((code (c-preexpand code))
         (c-labels (scan-labels code))
         (c-vars (grab-vars code)))
    (format *error-output* "~a" c-vars)
    `(let ((*labels* (make-hash-table)))
       (with-scope ',name
         (alist-to-table ',c-labels *labels*)
         (setf (gethash ',name (scope-objects *global-scope*))
               (traceme (list :scope *scopes*)))
         (emit ,(format nil "#Code w ~a" name)) ;;(gensym (symbol-name name))
         ,@code
         (asm rts :implied)))))

(defmacro c-if (test then else)
  (let ((else-label (gensym "ELSE"))
        (end-label (gensym "END")))
    `(progn
       (unless (in-function?)
         (error "All code must be inside a function.  "))
       ,test
       (emit (format nil "BEQ {~a}" (gethash ',else-label *labels*)))
       ,then
       (c-goto ,end-label)
       (c-label ,else-label)
       ,else
       (c-label ,end-label))))
