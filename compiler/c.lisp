"# C Compilaiton"

(in-package #:cs400-compiler)

"## Data structures and special variables.  "

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

(defparameter *labels* nil
  "type: (or hash-table null).  Lables don't have nested scoping, so
   we only need to keep track of the current scope insead of a keeping
   a stack of scopes like other symbols.  There is no global labels
   scope, so it will be nil at the top level.  ")

(defun identifiers-table () (scope-identifiers (first *scopes*)))
(defun tags-table () (scope-tags (first *scopes*)))

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


(defvar *stack-space* nil) ;; Alist of stack-offset addresses

(defun stack-allocate (name)
  (or (cdr (assoc name *stack-space*))
      (error "Variable declaration ~a not at the top of its scope.  "
             name)))

(defun allocate-storage (name size type)
  "Name is only used to look for pre-allocated space on the functions
   stack.  The protocol is:
     - c-fn scans it's code for valid declarations, and sets aside
       space for them.
     - We just return the space that we set up, or if signal an error
       if there no space was set up.  "
  (declare (ignore))
  (ecase type
    (:static (allocate-global size))
    (:auto (stack-allocate name))))

(defmacro c-var (name type &optional value)
  "This expands into code that modifies the table at (identifiers-table),
   and calls allocate-storage.  "
  (when value (error "Setting variables is not implemented.  "))
  (let ((size 2)) ;; replace 2 with (size-of type)
    `(let ((storage-class (if (in-function?) :auto :static)))
       (when (in-table? ',name (identifiers-table))
         (error "More than one declaration of variable ~a in the same scope"
                ',name))
       (setf (gethash ',name (identifiers-table))
             (new-var ',name ,type storage-class
                      (allocate-storage ',name ,size storage-class))))))

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
  "Returns a list of pairs of all the variable and their types seen at
   the top of CODE.  "
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
  collect (cons (second expr) (third expr)))))

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

(defmacro 16-bit-mode () `(asm rep :immediate #x30))
(defmacro 8-bit-mode () `(asm sep :immediate #x30))

(defun in-function? () (not (eq (first *scopes*) *global-scope*)))
(defmacro c-fn (name args &body code)
  (when args (error "Function arguments are not supported.  "))
  (let* ((input-code code)
         (code (c-preexpand code))
         (c-labels (scan-labels code))
         (c-vars (grab-vars code))
         (unique-name (gensym (symbol-name name)))
         (local-stack-size 0)
         (variable-spaces (loop for (name . type) in c-vars
                                collect
                               (cons name
                                     (prog1 (1+ local-stack-size)
                                       (incf local-stack-size 2))))))
    (format *error-output* "~%~a ~a~%" c-vars variable-spaces)
    `(let ((*labels* (make-hash-table))
           (*stack-space* ',variable-spaces))
       (with-scope ',name
         (alist-to-table ',c-labels *labels*)
         (setf (gethash ',name (scope-identifiers *global-scope*))
               (list ',unique-name
                     :scope *scopes*
                     :code ',input-code))
         (emit ,(format nil "#Code w ~a" unique-name))
         (asm clc :implied)
         (asm xce :implied)
         (16-bit-mode)
         ,@(when (plusp local-stack-size)
             `((asm tsc :implied)
               (asm clc :implied)
               (asm adc :immediate-w ,local-stack-size)
               (asm tcs :implied)))
         ,@code
         ,@(when (plusp local-stack-size)
             `((asm tsc :implied)
               (asm sec :implied)
               (asm sbc :immediate-w ,local-stack-size)
               (asm tcs :implied)))
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
