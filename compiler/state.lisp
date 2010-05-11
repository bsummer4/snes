(in-package :cs400-compiler)

(defstruct transformation predicate function)
(defstruct compiler-pass name tags macros transformations)
(defstruct c-variable
  (type nil            :read-only t :type (member c::int c::void))
  (address nil         :read-only t :type (or symbol fixnum))
  initial-value)

(defstruct c-function
  (type      nil :read-only t   :type t)
  (asm-name  nil :read-only t   :type symbol)
  (prototype nil :read-only nil :type boolean))

(defun c-variable (type address)
  (make-c-variable :type type :address address))

(defun c-function (prototype? type name)
  (declare (symbol name) (boolean prototype?))
  "Name is the name in the c world not the asm name.  "
  (make-c-function :type type
                   :asm-name (gensym (format nil "fn.~a" name))
                   :prototype prototype?))

(defclass compiler ()
  ((passes :accessor .passes :initform (make-hash-table))
   (globals :accessor .globals :initform (make-hash-table))
   (next-available-address :accessor .next :initform #x0100)))

(defparameter *compiler* (make-instance 'compiler))

"# Convient access to compiler state"
(defun alloc (size)
  (prog1 (.next *compiler*)
    (incf (.next *compiler*) size)))

(defun get-id (identifier)
  "Returns NIL if unbound"
  (declare (symbol identifier))
  (gethash identifier (.globals *compiler*)))

(defun set-id (identifier value)
  "Returns NIL if unbound"
  (declare (symbol identifier))
  (setf (gethash identifier (.globals *compiler*))
        value))

(defun type->size (type)
  (match type
    ('c::int 2)
    ('c::void 0)
    (_ (error "Unknown type: ~a" type))))

(defun set-var (id var)
  (etypecase (get-id id)
    (c-variable (error "Redefining variable ~a" id))
    ((eql nil) (setf (gethash id (.globals *compiler*)) var))
    (t (error "~a is bound so somthing that's not a variable" id))))

(defun get-var (identifier)
  (let ((it (get-id identifier)))
    (etypecase it
      (c-variable it)
      ((eql nil) (error "Unbound identifier ~a" identifier))
      (t (error "~a is bound so somthing that's not a variable" identifier)))))

(defun alloc-variable (name type)
  (set-var name
           (c-variable type (alloc (type->size type)))))

(defun init-variable (name value)
  (setf (c-variable-initial-value (get-var name))
        value))

(defun get-transformations (pass-name)
  (compiler-pass-transformations
   (or (gethash pass-name (.passes *compiler*))
       (error "undefined compiler pass ~a" pass-name))))

(defun add-transformation (transformation pass-name)
  (push transformation
        (compiler-pass-transformations
         (gethash pass-name (.passes *compiler*)))))

(defun get-tags (pass-name)
  (slot-value
   (gethash pass-name (.passes *compiler*))
   'tags))

(defun %get-fn (id)
  (declare (symbol id))
  (let ((fn (get-id id)))
    (typecase fn
      (c-function fn)
      (c-variable (error "id is bound to something not a function: ~s" id))
      (t nil))))

(defun c-fn-unique-name (name)
  (let ((fn (get-id name)))
    (typecase fn
      (c-function (c-function-asm-name fn))
      (c-variable (error "~a is not a function" name))
      (t (error "Undefined identifier: ~s" name)))))

(defun prototype-fn (name type)
  (aif (%get-fn name)
       (if (equalp type (c-function-type it))
           it
           (error
            "Incompatible types between two declarations of ~s: (~s ~s)"
            name type (c-function-type it)))
       (progn
         (set-id name (c-function t type name))
         (%asm-code (c-fn-unique-name name) :prototype t)
         (get-id name))))

(defun instantiate-fn (name type)
  (let ((fn (prototype-fn name type)))
    ;; TODO This shouldn't be commented but it's annoying for
    ;; debugging:
    ;;    (unless (c-function-prototype fn)
    ;;      (error "Multiple definition of function ~a" name))
    (setf (c-function-prototype fn) nil)
    (%asm-code (c-fn-unique-name name))))
