"Just some general utillity code.  "

(in-package #:cs400-compiler)

(defun insult () (error "u is dumb"))
(defun traceme (x) x)
(defun flatten (lists) (apply #'append lists))
(defun lookup (dict key) (second (assoc key dict)))
(defun filter (predicate seq) (remove-if-not predicate seq))
(defun in-table? (key table)
  (multiple-value-bind (value found?) (gethash key table)
    (declare (ignore value))
    found?))

(defun hash-table->alist (table)
  (loop for key being the hash-keys of table
       collect (cons key (gethash key table))))

(defun alist-to-table (alist &optional (table (make-hash-table)))
  "Modifies table if one is passed; Otherwise return a new table with
   only the given mapping.  "
  (loop for (key . value) in alist
        do (setf (gethash key table) value))
  table)

(defun non-unique-items (list)
  "Returns a list of all non-unique items"
  (let ((items (make-hash-table))
        (duplicated-items))
    (dolist (item list)
      (if (gethash item items)
          (setf duplicated-items (adjoin item duplicated-items))
          (setf (gethash item items) t)))
    duplicated-items))

(defun proper-set? (list) (null (non-unique-items list)))

;; Macros
(defmacro aif (form then &optional else)
  `(let ((it ,form))
     ,(if else
          `(if it ,then ,else)
          `(if it ,then))))

(defun alist->plist (alist)
  (loop for (key . value) in alist
        collect key
        collect value))

(defun hash-table->plist (table)
  (alist->plist (hash-table->alist table)))

(defmethod print-object ((table hash-table) stream)
  (format stream "[dict~{ ~s~}]" (hash-table->plist table)))


;; Junk
(defmacro with-gensyms (symbols &body code)
  `(let ,(mapcar (lambda (symbol)
                   `(,symbol (gensym (symbol-name ',symbol))))
                 symbols)
     ,@code))

(defmacro match (expr &body forms) `(cl-match:match ,expr ,@forms))
(defmacro ematch (expr &body forms)
  (with-gensyms (tmp)
    `(let ((,tmp ,expr))
       (or (match ,tmp ,@forms)
           (error "no match found for ~a in patterns ~a" ,tmp
                  ',(mapcar #'first forms))))))

(defun & (f &rest l) (apply f l))
(defmacro fn (lambda-list &body body) `(lambda ,lambda-list ,@body))
(defmacro fn1 (&body body) `(fn (!1) ,@body))

(defun eprint (object) (print object *error-output*))
