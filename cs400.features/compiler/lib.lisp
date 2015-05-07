"Just some general utillity code.  "

(in-package #:cs400-compiler)

(defun insult () (error "u is dumb"))
(defun traceme (x) x)
(defun flatten (lists) (apply #'append lists))

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

(defun lookup (key alist)
  (aif (assoc key alist)
       (values (cdr it) t)))

(defun elookup (key alist)
  (multiple-value-bind (result found?) (lookup key alist)
      (if found? result
          (error "key '~a was not found in alist '~a.  " key alist))))

(defmacro match (expr &body forms)
  `(cl-match:match ,expr ,@forms))

(defmacro match? (form value)
  `(match ,value (,form t)))

(defmacro with-gensyms (symbols &body code)
  `(let ,(mapcar (lambda (form)
                   (match form
                     ((as symbol (type symbol))
                      `(,symbol (gensym (symbol-name ',symbol))))
                     ((list symbol gensym-hint)
                      `(,symbol (gensym ,gensym-hint)))))
                 symbols)
     ,@code))

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

(defmacro collecting (&body code)
  (with-gensyms (result)
    `(let ((,result))
       (flet ((collect (obj) (push obj ,result)))
         ,@code)
       (nreverse ,result))))

(defun nice-gensym (obj)
  (gensym (string-right-trim
           "0123456789"
           (format nil "~a" obj))))

(defmacro with-symbol-alias-alist (alist-varname symbols &body code)
  "Generates unique names for all SYMBOLS, and binds an alist
   (GIVEN-NAME . UNIQUE-NAME) to ALIST_VARNAME.  "
  `(let ((,alist-varname ',(mapcar (fn1 (cons !1 (nice-gensym !1)))
                                   symbols)))
       ,@code))

(defun lpartition (predicate list)
  "Returns (values (remove-if-not predicate list)
                   (remove-if predicate list)), but in a single
   pass. "
  (iter (for item in list)
        (if (funcall predicate item)
            (collect item into good-list)
            (collect item into bad-list))
        (finally (return (values good-list bad-list)))))

(defun listify (obj)
  (typecase obj (list obj) (t (list obj))))

(defmacro pluralize-macro (macro newname)
  (with-gensyms (forms)
    `(defmacro ,newname (&body ,forms)
       `(progn
          ,@(mapcar (fn1 (cons ',macro (listify !1))) ,forms)))))

(defun find-index (predicate list)
  (loop for item in list
        for index from 0
        when (& predicate item)
        return index
        finally (return nil)))

(defun take (n l)
  (loop for x below n
        for i in l
        collect i))

(defun drop (n l)
  (loop for x to n
        for i on l
        finally (return i)))
