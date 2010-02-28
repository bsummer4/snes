(in-package #:cs400-compiler)

(defstruct scope labels objects types name)
(defun new-scope (name)
  (make-scope :labels (make-hash-table)
              :objects (make-hash-table)
              :types (make-hash-table)
              :name name))

(defparameter *scopes* (list (new-scope 'global)))
(defparameter *global-scope* (first *scopes*))

(defun labels-table () (scope-labels (first *scopes*)))
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

(defmacro c-label (name)
  (unless (symbolp name) (insult))
  `(emit (format nil "{~a}" (gethash ',name (labels-table)))))

(defmacro c-goto (name)
  (unless (symbolp name) (insult))
  (let ((tmp (gensym)))
    `(let ((,tmp (gethash ',name (labels-table))))
       (if ,tmp
           (emit (format nil "BRA {~a}" ,tmp))
           (error "Trying to goto a non-existent label: ~a" ',name)))))

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
     (traceme (hash-table->alist (labels-table)))
     (values)))

(defmacro c-fn (name args &body code)
  (when args (error "Function arguments are not supported.  "))
  (let ((code (c-preexpand code)))
    `(with-scope ',name
       (alist-to-table ',(scan-labels code) (labels-table))
       (setf (gethash ',name (scope-objects *global-scope*))
             (traceme (list :scope *scopes*)))
       (emit ,(format nil "#Code w ~a" name)) ;;(gensym (symbol-name name))
       ,@code
       (asm rts :implied))))

;; TODO Make sure we are in an actual function scope.
(defmacro c-if (test then else)
  (let ((else-label (gensym "ELSE"))
        (end-label (gensym "END")))
    `(progn
       (when (eq (first *scopes*) *global-scope*)
         (error "Conditional in global scope."))
       ,test
       (emit (format nil "BEQ {~a}" (gethash ',else-label (labels-table))))
       ,then
       (c-goto ,end-label)
       (c-label ,else-label)
       ,else
       (c-label ,end-label))))
