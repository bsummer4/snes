(in-package :cs400-compiler)

(defstruct transformation predicate function)
(defstruct compiler-pass name tags macros transformations)
(defstruct c-variable
  (type nil            :read-only t :type (eql c::int))
  (address nil         :read-only t :type (or symbol fixnum))
  (addressing-mode nil :read-only t :type keyword))

(defstruct c-function
  (type      nil :read-only t   :type t)
  (asm-name  nil :read-only t   :type symbol)
  (prototype nil :read-only nil :type boolean))

(defun c-variable (address addressing-mode)
  (make-c-variable :type 'c::int
                   :address address
                   :addressing-mode addressing-mode))

(defun c-function (prototype? type name)
  (declare (symbol name) (boolean prototype?))
  "Name is the name in the c world not the asm name.  "
  (make-c-function :type type
                   :asm-name (gensym (format nil "fn.~a" name))
                   :prototype prototype?))

(defclass compiler ()
  ((passes :accessor .passes :initform (make-hash-table))
   (globals :accessor .globals :initform (make-hash-table))))

(defparameter *compiler* (make-instance 'compiler))

"# Convient access to compiler state"
(defun get-id (identifier)
  "Returns NIL if unbound"
  (declare (symbol identifier))
  (gethash identifier (.globals *compiler*)))

(defun set-id (identifier value)
  "Returns NIL if unbound"
  (declare (symbol identifier))
  (setf (gethash identifier (.globals *compiler*))
        value))

(defun get-var (identifier)
  (etypecase (get-id identifier)
    (c-variable identifier)
    ((eql nil) (error "Unbound identifier ~a" identifier))
    (t (error "~a is bound so somthing that's not a variable" identifier))))

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
