"
# Variables and identifiers -- The Fourth compiler Pass

local-variable-declaration -> Not required that there is a single
                              declaration per variable
load-number                -> Into the main register
load-variable              -> Into the main register
set-variable               -> To the value of the main register.
comment                    -> code comment
"

(in-package :cs400-compiler)
(define-constant +far-back-tags+
  '((far-back::decrement)
    (far-back::increment)
    (far-back::+)
    (far-back::-)
    (far-back::funcall)
    (far-back::label name)
    (far-back::goto name)
    (far-back::comment str)
    (far-back::goto-if-not)
    (far-back::goto-if-<)
    (far-back::goto-if->)
    (far-back::goto-if-<=)
    (far-back::goto-if->=)
    (far-back::goto-if-==)
    (far-back::goto-if-!=)
    (far-back::load-variable)
    (far-back::store-variable)
    (far-back::load-number)
    (far-back::local-variable-initialization)
    (far-back::local-variable-declaration)))

(always-eval
  #.`(define-pass far-back
       ,@(mapcar #'list +far-back-tags+)
       ((far-back::proc name return-type args body)
        ->
        (progn-clean
         (let ((tagged (taggify-form body 'far-back)))
           `(progn
              (instantiate-fn ',name ',return-type)
              (16-bit-mode)
              ,(progn-clean
                (preexpand
                 ;; Need to fudge labels to avoid conflicts!
                  (multiple-value-call #'handle-variables
                    (apply-pass tagged 'far-back)
                    (stack-analyze args tagged))))))))))

(defun %jsr (function-name)
  (asm :jsr :absolute (c-fn-unique-name function-name)))

(define-tag-substitution (load-num far-back::load-number far-back)
    (integer)
  `(%load-number ,integer))

(define-tag-substitution (c-label far-back::label far-back)
    (label)
  `(%label ',label))

(define-tag-substitution (c-goto far-back::goto far-back)
    (label)
  `(%goto ',label))

(define-tag-substitution (c-funcall far-back::funcall far-back)
    (function &rest args)
  `(with-indent ,(format nil "_call_to_~a" function)
     ,@(iter (for arg in (reverse args))
             (for i from 1)
             (when (numberp arg)
               (collect `(%load-number ,arg)))
             (when (symbolp arg)
               (collect `(tag:tag '(far-back::load-variable ,arg))))
             (collect `(%store-addr ,(1- (* 2 i)) :stack)))
     (%jsr ',function)))


(define-tag-substitution (_comment
                          far-back::comment
                          far-back)
    (str)
  (declare (string str))
  `(%comment ,str))

(define-tag-substitution (var-init
                          far-back::local-variable-initialization
                          far-back)
    (name value)
  (declare (integer value))
  `(progn
     (%load-number ,value)
     (tag:tag '(far-back::store-variable ,name))))

(define-tag-substitution (ignore-var-decr
                          far-back::local-variable-declaration
                          far-back)
    (name type)
  (declare (ignore name type)))

"# Stack Stuff"
(defun handle-variables (code stack-size variable-spaces)
  "Replace all references to variables with references to their stack
   address.  This happens in load/store-variable and in
   operators.  Operators are transformed in the following ways:

     (far-back::OP x) => (%OP (stack ADDR))
     (far-back::OP 3) => (%OP 3)"
  (labels ((var (id) (aif (assoc id variable-spaces)
                          (list :stack (cdr it))
                          (aif (gethash id (.globals *compiler*))
                               (list :global (c-variable-address it))
                               (error "Unbound Variable ~a" id))))
           (anno (operand) (typecase operand
                             (number operand)
                             (symbol `',(var operand)))))
    `(progn
       (%grow-stack ,stack-size)
       ,(tag:replace
         (fn1 (fare-matcher:match !1
                (`(far-back::load-variable ,id)
                  `(%load-addr ,@(reverse (var id))))
                (`(far-back::store-variable ,id)
                  `(%store-addr ,@(reverse (var id))))

                (`(far-back::goto-if-not ,(and val (of-type integer)) ,label)
                  (when (zerop val)
                    `(%goto ',label)))
                (`(far-back::goto-if-not ,id ,label)
                  `(%goto-if-not ,(anno id) ',label))

                (`(far-back::goto-if-> ,x ,y ,label)
                  (unless (and (numberp x) (numberp y) (<= x y))
                    `(%goto-if-> ,(anno x) ,(anno y) ',label)))
                (`(far-back::goto-if->= ,x ,y ,label)
                  (unless (and (numberp x) (numberp y) (< x y))
                    `(%goto-if->= ,(anno x) ,(anno y) ',label)))
                (`(far-back::goto-if-< ,x ,y ,label)
                  (unless (and (numberp x) (numberp y) (>= x y))
                    `(%goto-if-< ,(anno x) ,(anno y) ',label)))
                (`(far-back::goto-if-<= ,x ,y ,label)
                  (unless (and (numberp x) (numberp y) (> x y))
                    `(%goto-if-<= ,(anno x) ,(anno y) ',label)))

                (`(far-back::increment ,x)
                  `(%inc ,(anno x)))
                (`(far-back::decrement ,x)
                  `(%dec ,(anno x)))
                (`(far-back::- ,x ,y)
                  `(%- ,(anno x) ,(anno x)))
                (`(far-back::+ ,x ,y)
                  `(%+ ,(anno x) ,(anno y)))

                (* !1)))
         code)
       (%shrink-stack ,stack-size))))

(defun function-calls (code)
  (tag:find
   (match? (list* 'far-back::funcall _))
   code))

(defun uniq (list)
  (let ((result))
    (mapc #l(pushnew !1 result) list)
    result))

(defun locals (code)
  (uniq
   (mapcar #'second
           (tag:find (match?
                      (list* 'far-back::local-variable-declaration _))
                     code))))

(defun needed-call-slots (code)
  (or (iter (for form in (function-calls code))
            (maximize (- (length form) 2)))
      0))

(defun variables-to-addresses (variables initial-offset)
  (let ((offset initial-offset))
    (iter (for v in variables)
          (collect (cons v (prog1
                               offset
                             (incf offset 2)))))))

(defun stack-analyze (args code)
  (let* ((locals (locals code))
         (call-slots (needed-call-slots code))
         (stack-space (* 2 (+ (length locals) call-slots)))
         (var-addresses (variables-to-addresses
                         locals
                         (1+ (* 2 call-slots))))
         (arg-addresses (variables-to-addresses args
                                                (+ 3 stack-space))))
    (values stack-space (append var-addresses arg-addresses))))
