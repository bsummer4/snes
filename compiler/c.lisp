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

(defvar *stack-space* :undefined-stack-space
  "Alist of stack-offset addresses")
(defvar *breakpoint* :undefined-label)

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

(defun %c-goto (value)
  (emit (format nil "BRA {~a}" value)))

(defmacro c-goto (name)
  (unless (symbolp name) (insult))
  (let ((tmp (gensym)))
    `(let ((,tmp (gethash ',name *labels*)))
       (if ,tmp
           (%c-goto ,tmp)
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
    (iter (for expr in code)
          (until (not (or (stringp expr)
                          (is-var? expr))))
          (unless (stringp expr) ;; Ignore Docstrings in c-function
                                 ;; definitions
            (collect (cons (second expr) (third expr)))))))

(defun grab-labels (code)
  "Returns all label names in the code block.  "
  (flet ((is-label? (expr)
           (and (listp expr)
                (= 2 (length expr))
                (eq (first expr) 'c-label)
                (symbolp (second expr)))))
    ;; TODO This should see through LET statements; Maybe iterate
    ;; through code as a tree instead of as a list?
    (mapcar #'second (filter #'is-label? code))))

(defun scan-labels (code)
  "Return an alist mapping labels in code to their new globally-unique
   names.  "
  (let ((labels (grab-labels code)))
    (aif (non-unique-items labels)
         (error
          "The following labels appear more than once in the same scope: ~a"
          it))
    (mapcar (lambda (label) (cons label (nice-gensym label)))
            labels)))

(defun unprogn (code)
  "We return a list of statements in CODE.  If CODE is of the
  form (PROGN &rest code), we return (code).  "
  (if (and (listp code) (eq (first code) 'progn))
      (flatten (mapcar #'unprogn (cdr code)))
      (list code)))

(defun c-preexpand (code)
  "Recursively pre-expand and unprogn some specific macros so we can
   analyze the resulting code.  "
  (let ((to-expand '(c-if c-while c-for c-do-while c-switch)))
    (flatten
     (iter (for stmt in code)
           (collect
               (if (not (listp stmt))
                   (list stmt)
                   (cond
                     ((eq 'let (first stmt))
                      `((let ,(second stmt)
                         ,@(c-preexpand
                            (prog1 (cddr stmt)
                              (format t "~%--~s~%" (cddr stmt)))))))
                     ((member (first stmt) to-expand)
                      (c-preexpand
                       (unprogn (macroexpand-1 stmt))))
                     (t (list stmt)))))))))

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
         (unique-name (nice-gensym name))
         (local-stack-size 0)
         (variable-spaces (iter (for (name . type) in c-vars)
                                (collect
                                  (cons name
                                    (prog1 (1+ local-stack-size)
                                           (incf local-stack-size 2)))))))
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
                   (asm sbc :immediate-w ,local-stack-size)
                   (asm tcs :implied)))
         ,@code
         ,@(when (plusp local-stack-size)
                 `((asm tsc :implied)
                   (asm sec :implied)
                   (asm adc :immediate-w ,local-stack-size)
                   (asm tcs :implied)))
         (asm rts :implied)))))

;; TODO get rid of the real LOOKUP function which doesn't do the right
;; thing.
(defun -lookup (key alist)
  (or (cdr (assoc key alist))
      (error "key '~a was not found in alist '~a.  " key alist)))

(defmacro c-stack-variable-set (name)
  `(asm sta :stack-indexed (or (cdr (assoc ',name *stack-space*))
                               (error "Undefined identifier ~a" ',name))))

(defmacro c-stack-variable-reference (name)
  `(asm lda :stack-indexed (or (cdr (assoc ',name *stack-space*))
                               (error "Undefined identifier ~a" ',name))))

(defun nice-gensym (obj)
  (gensym (string-right-trim
           "0123456789"
           (format nil "~a" obj))))

(defmacro c-if (test then &optional else)
  (let ((else-label (gensym "ELSE"))
        (end-label (gensym "END")))
    `(progn
       (unless (in-function?)
         (error "All code must be inside a function.  "))
       ,test
       (emit (format nil "BEQ {~a}"
                     (gethash (quote ,(if else else-label end-label))
                              *labels*)))
       ,then
       ,(when else `(c-goto ,end-label))
       ,(when else `(c-label ,else-label))
       ,else
       (c-label ,end-label))))

(defmacro c-while (test &body body)
  (let ((repeat-label (gensym "LOOP"))
        (end-label (gensym "BREAK")))
    ;; TODO bind *breakpoint* to end-label (right now binding
    ;;      *breakpoint* breaks shit)
    (declare (ignore end-label))
    `(progn
       (unless (in-function?)
         (error "All code must be inside a function.  "))
       (c-label ,repeat-label)
       (c-if ,test (progn ,@body (c-goto ,repeat-label))))))

(defmacro c-do-while (test &body body)
  (let ((repeat-label (gensym "LOOP")))
    `(progn
       (unless (in-function?)
         (error "All code must be inside a function.  "))
       (c-label ,repeat-label)
       ,@body
       (c-if ,test (c-goto ,repeat-label)))))

(defmacro c-for ((setup test iterate) &body body)
  `(progn
     ,setup
     (while ,test
       ,@body ,iterate)))

#|
switch (getnum()) {
 case 3: return 3;
 case 4: break;
 default: exit(1); }
return 4;
|#

; JSR getnum                expr
; CMP #3w                   jumps
; BEQ case_3
; CMP #4w
; BEQ case_4
; BRA default
; {case_3}                  targes
; LDA #3w
; RTS
; {case_4}
; BRA break
; {default}
; JSR exit
; {break}
; LDA #4w
; RTS

(defun switch-case-values (cases) (mapcar #'first cases))
(defun switch-case-codes (cases) (mapcar #'second cases))
(defun switch-label (value)
  (etypecase value
    (number (gensym (format nil "CASE_~a_" value)))
    (symbol (progn
              (or (eq value 'default)
                  (error "case ~a is not a number" value))
              (nice-gensym value)))))

(defun make-switch-jump-entry (value target)
  (if (eq value 'default)
      `(c-goto ,target)
      `(progn
         (asm cmp :immediate-w ,value)
         (emit (format nil "BEQ {~a}" ',target)))))

(defun make-switch-target (target code)
  `(progn (c-label ,target) ,code))

(defun partition (predicate list)
  "Returns (values (remove-if-not predicate list)
                   (remove-if predicate list)), but in a single
   pass. "
  (iter (for item in list)
        (if (funcall predicate item)
            (collect item into good-list)
            (collect item into bad-list))
        (finally (return (values good-list bad-list)))))

(defun switch-default-hack (jumps break-label)
  "The 'default' case must be 'goto'ed at the end of jump clauses, but
   it doesn't need to be at the end of the switch.  So, we look for
   the generated default clause (which like '(goto DEFAULT####)') and
   moves it to the end.  "
  (print jumps)
  (multiple-value-bind (defaults numbers)
      (partition (lambda (jump-form)
                   (eq 'c-goto (first jump-form)))
                 jumps)
    (append numbers (or defaults
                        `((c-goto ,break-label))))))

(defmacro c-break () '(%c-goto *breakpoint*))

(defmacro c-switch (expr &body cases)
  ;; TODO This is broken like your mom!!
  (let* ((values (switch-case-values cases))
         (codes (switch-case-codes cases))
         (labels (mapcar #'switch-label values))
         (break-label (gensym "BREAK")))
    `(let ((*breakpoint* ',break-label))
       (unless (in-function?)
         (error "All code must be inside a function.  "))
       ,expr
       ,@(switch-default-hack
          (mapcar #'make-switch-jump-entry values labels)
          break-label)
       ,@(mapcar #'make-switch-target labels codes)
       (c-label ,break-label))))
