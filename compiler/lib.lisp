"Utillity Code"

(defparameter +eof+ (gensym "EOF"))

;; Functions
(defun insult () (error "u is dumb"))
(defun traceme (x) (declare (ignore x)) (values))

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
