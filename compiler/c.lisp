"# C Compilaiton"

(in-package #:cs400-compiler)

"## Data structures and special variables.  "

(defstruct scope
  "The point of giving scopes names is to help generate better error
   messages and better names of generated names.  For exapmle an
   struct without a type name needs to have a type name generated."
  objects types name)
(defun new-scope (name)
  (make-scope :objects (make-hash-table)
              :types (make-hash-table)
              :name name))

(defparameter *scopes* (list (new-scope 'global)))
(defparameter *global-scope* (first *scopes*))

(defstruct c-var name type storage-class address)
(defun new-var (name type storage-class address)
  (make-c-var :name name
              :type type
              :storage-class storage-class
              :address address))

(defparameter *labels* nil
  "type: (or hash-table null).  Lables don't have nested scoping, so
   we only need to keep track of the current scope insead of a keeping
   a stack of scopes like other symbols.  There is no global labels
   scope, so it will be nil at the top level.  ")

(defun objects-table () (scope-objects (first *scopes*)))
(defun types-table () (scope-types (first *scopes*)))


"## Global Memory Allocation"
(defparameter *next-available-global-space* 0
  "This will be ***MODIFIED*** when new global space is requested.  ")

(defun allocate-global (size)
  (prog1
      *next-available-global-space*
      (incf *next-available-global-space* size)))


"## Compiler Macros.  "

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

(defun allocate-storage (size type)
  (ecase type
    (:static (allocate-global size))))

(defmacro c-var (name type &optional value)
  "This expands into code that modifies the table at (objects-table),
   and calls allocate-storage.  "
  (when value (error "Setting variables is not implemented.  "))
  (let ((size 2)) ;; replace 2 with (size-of type)
    `(let ((storage-class (prog1 :static (if (in-function?) :auto :static)))) ;; only :static storage for now
       (when (in-table? ',name (objects-table))
         (error "More than one declaration of variable ~a in the same scope"
                ',name))
       (setf (gethash ',name (objects-table))
             (new-var ',name ,type storage-class
                      (allocate-storage ,size storage-class))))))

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
  (let* ((input-code code)
         (code (c-preexpand code))
         (c-labels (scan-labels code))
         (c-vars (grab-vars code)))
    (format *error-output* "~a" c-vars)
    `(let ((*labels* (make-hash-table)))
       (with-scope ',name
         (alist-to-table ',c-labels *labels*)
         (setf (gethash ',name (scope-objects *global-scope*))
               (list :scope *scopes*
                            :code ',input-code))
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
