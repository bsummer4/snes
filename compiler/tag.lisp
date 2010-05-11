"
A tag is a way of separating macroexpansions in layers.  Basically, a
tag is some code of the form (tag:tag '()).  Since tag:tag is
undefined, this code will be an error if it is EVALuated.  The point
is that another macro can PREEXPAND your code and replace tags with
some real code.  This allows you to use PREEXPAND without it replacing
you shit.
"

(in-package :s)

(defun lambda-list-modifier? (symbol)
  (char= (elt (symbol-name symbol) 0) #\&))

(defun neg (f)
  (fn (&rest args)
    (not (apply f args))))

(defmacro tag:def (name pass-name &rest lambda-list)
  (declare (ignore lambda-list))
  `(progn
     (push ',name (slot-value
                 (gethash ',pass-name (.passes *compiler*))
                 'tags))))

(defmacro tag:let ((name &rest lambda-list) &body body)
  (declare (ignore lambda-list))
  `(macrolet
       ((,name (&rest args)
          `(tag:tag ,`(quote ,(cons ',name args)))))
     ,@body))

(pluralize-macro tag:def tag:defs)

(defun tag:.form (tag) (second (second tag)))
(defun tag:tag? (form)
  (match form
    ((list 'tag:tag _) t)))

(defun tag:replace (function form)
  "Replace instances of (tag:tag '(&rest SUBFORM)) anywhere in FORM.
   FUNCTION is called with SUBFORM."
  (tree-walk
   (fn (subform)
     (match subform
       ((list 'tag:tag (list 'quote subsubform))
        (& function subsubform))
       (_ subform)))
   form))

(defun tag:find (predicate form)
  (collecting
    (tag:replace #l(progn
                     (when (& predicate !1)
                       (collect !1))
                     !1)
                 form)))

"# Pass definition macros"
(defun transform-rule (rule invalid pass-name)
  (match rule
    ((list (list* name lambda-list))
     (print `(tag:def ,name ,pass-name ,@lambda-list))
     `(tag:def ,name ,pass-name ,@lambda-list))
    ((list (list* name lambda-list) '-> body)
     (typecase body
       (list `(defmacro ,name ,lambda-list ,body))
       (symbol `(defmacro ,name (&rest args)
                  `(,',body ,@args)))
       (t (& invalid rule))))
    (_ (& invalid rule))))

(defmacro define-pass (pass-name &body toplevel-rules)
  (declare (symbol pass-name))
  (flet ((invalid (rule)
           (error
            "Invalid transformation rule:~%  ~s~%~%in pass:~%  ~s~%~%"
            rule `(define-pass ,pass-name ,@toplevel-rules))))
    `(progn
       (setf (gethash ',pass-name (slot-value *compiler* 'passes))
             (make-compiler-pass :name ',pass-name))
       ,@(mapcar
          (fn1 (transform-rule !1 #'invalid pass-name))
          toplevel-rules))))


"# Transformations"
(defmacro define-predicated-transformation
    ((function-name predicate pass-name)
     lambda-list &body code)
  "PREDICATED-TRANSFORMATIONs are given a chance to tranform a
   tagged-form if there is no NAMED-TRANSFORMATION.

   A PREDICATED-TRANSFORMATION must return (VALUES FORM USE?), if USE?
   if NIL, then the transformation is not used, and the next
   predicated transformation will be tried. If no transformation
   applies to a tagged form, then it will not be transformated, and
   thus will be left tagged.  "
  (declare (symbol function-name pass-name))
  `(progn
     (defun ,function-name ,lambda-list
       ,@code)
     (add-transformation (make-transformation
                          :predicate ,predicate
                          :function #',function-name)
                         ',pass-name)))

(defmacro define-tag-substitution ((macro-name tag pass-name)
                                   lambda-list &body code)
  (declare (symbol macro-name pass-name tag))
  `(progn
     (defmacro ,macro-name ,lambda-list
       ,@code)
     (add-transformation (make-transformation
                          :predicate (fn1 (eq (car !1) ',tag))
                          :function (fn (&rest form)
                                      `(,',macro-name ,@(cdr form))))
                         ',pass-name)))



(defun taggify-form (form pass-name)
  (let ((tags (get-tags pass-name)))
    (tree-walk (fn1
                 (if (and (listp !1)
                          (member (car !1) tags))
                     (tag-quote !1)
                     !1))
               form)))

(defun tag-quote (form) `(tag:tag ,`(quote ,form)))

(defun transform-tag-form (form pass-name)
  (iter (for ts in (get-transformations pass-name))
        (with-slots (predicate function) ts
          (when (& predicate form)
            (return (apply function form))))
        (finally (return (tag-quote form)))))

(defun progn-clean (code)
  (tree-walk #'progn-flatten code))

(defun apply-pass (form pass-name)
  (progn-clean
   (preexpand
    (tag:replace (fn1 (transform-tag-form !1 pass-name))
                 (preexpand form)))))

(defun c->front (symbol)
  (declare (symbol symbol))
  (assert (eq (symbol-package symbol)
              (find-package :c)))
  (intern (symbol-name symbol) (find-package :front)))
