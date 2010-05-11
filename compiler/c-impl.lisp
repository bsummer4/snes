;; TODO you can define a function argument and a function local
;; variable with the same name.

(in-package #:cs400-compiler)

#|
"## Scratch Space"
(defmacro with-stack-lookup-macros (variable-spaces &body code)
  "Binds c-VAR to set the variable if given a value and binds REF and
   SET to read from and write to a variable by name.  "
  (with-gensyms (stack-space)
    `(let ((,stack-space ,variable-spaces))
       (flet ((var-addr-mode (varname)
                (declare (ignore varname))
                :stack-indexed)
              (var-addr (varname)
                (elookup varname ,stack-space)))
         (macrolet ((->A (var)
                      (etypecase var
                        (symbol `(asm lda (var-addr-mode ',var)
                                      (var-addr ',var)))
                        (number `(lda ,var))))
                    (A-> (var)
                      `(asm sta (var-addr-mode ',var)
                            (var-addr ',var)))
                    (c-var (name type &optional value)
                      (declare (ignore type))
                      (when value
                        `(progn
                           (lda ,value)
                           (A-> ,name)))))
           ,@code)))))

(defmacro with-stack-variables (variable-declarations args call-space
                                &body code)
  (let* ((vars variable-declarations)
         (local-stack-size call-space)
         (variable-spaces (iter (for (name . type) in vars)
                                (collect
                                    (cons name
                                          (prog1 (1+ local-stack-size)
                                            (incf local-stack-size 2))))))
         (argument-spaces (iter (for name in (reverse args))
                                (for index from (+ 1 2 local-stack-size) by 2)
                                (collect (cons name index)))))
    `(with-grown-stack ,local-stack-size
       (with-stack-lookup-macros ',(append argument-spaces variable-spaces)
         ,@code))))

(defmacro c-proc ((name return-type) args &body code)
  (declare (ignore return-type))
  (let* ((input-code `(progn ,@code))
         (unique-name (c-fn-unique-name name))
         (code (preexpand (transform-c-syntax (preexpand input-code))))
         (needed-call-space (needed-call-space code))
         (vars (append (find-vars code)
                       (find-temp-variables code))))
    `(with-indent ,(format nil "_function_~s"
                           (intern (symbol-name name)))
       (with-scope ',name
         (asm-code ',unique-name)
         (16-bit-mode)
         (with-stack-variables ,vars ,args ,needed-call-space
           (labels-block
             (bind-identifier ',name
                              (list ',unique-name :code ',code)
                              *global-scope*)
             ,code))
         (asm rts :implied)))))

"## Operators"
(defmacro nice-block (name &body code)
  `(with-indent ,(format nil "_~a" name)
     (c-block ,@code)))

(defmacro def-simple-binary-operator (operator mnemonic &key prelude)
  `(defmacro ,operator (operand-1 operand-2)
     `(nice-block ,',(symbol-name operator)
        ,',prelude
        (->A ,operand-1)
        ,(etypecase operand-2
           (symbol `(asm ,',mnemonic (var-addr-mode ',operand-2)
                         (var-addr ',operand-2)))
           (number `(asm ,',mnemonic :immediate-w ,operand-2))))))

(pluralize-macro def-simple-binary-operator def-simple-binary-operators)

(def-simple-binary-operators
  (c-+ adc :prelude (asm clc :implied))
  (c-- sbc :prelude (asm sec :implied))
  (c-^ eor)
  (c-& and)
  (c-band and)
  (c-\| ora)
  (c-bor ora))

(defmacro c-++ (var)
  (declare (type symbol var))
  `(c-block
    (->A ,var)
    (asm inc :accumulator)
    (A-> ,var)))

(defmacro c--- (var)
  `(c-block
     (->A ,var)
     (asm dec :accumulator)
     (A-> ,var)))

(defmacro c-@ (operand)
  "Address of a variable"
  (declare (type symbol operand))
  `(nice-block ,(format nil "address_of_~a" operand)
    (case (var-addr-mode ',operand)
      (:stack-indexed
       (asm clc :implied)
       (asm tsc :implied)
       (asm adc :immediate-w (var-addr ',operand)))
      (:absolute (var-addr ',operand)))))

(defmacro c-$ (operand)
  "Pointer dereference.  "
  `(nice-block 'dereference
     (->A ,operand)
     (asm sta :immediate-w 00)
     (asm lda :direct-indirect 00)))


"## Interrupt Handling"

(defvar *defined-interrupt-handlers* nil)
(defvar *reset-table-written?* nil)
(define-constant +interrupts+ '(:reset :irq :bk :cop :abort :nmi))
(define-constant +interrupt-handler-positions+
  '("$80 $FE" :empty :cop   :brk   :abort :nmi   :empty :irq
    :empty    :empty :empty :empty :empty :empty :reset :empty))

(deftype interrupt-handler-name () (cons 'member +interrupts+))

(defun get-interrupt-name (name)
  (declare (type interrupt-handler-name name))
  (symbol-name name))

(defun vector-table ()
  (unless (member :reset *defined-interrupt-handlers*)
    (error "You must have a handler for the reset interrupt.  "))
  (emit (format nil
                "#Data $00:FFE0 interrupt_vector {~{~a~^~%~a~}}"
                (iter (for interrupt in +interrupt-handler-positions+)
                      (collect
                          (cond
                            ((stringp interrupt)
                             interrupt)
                            ((member interrupt *defined-interrupt-handlers*)
                             (get-interrupt-name interrupt))
                            (t "$FFE0")))
                      (collect "                                   ")))))

(defmacro interrupt-handler (name &body code)
  (declare (type interrupt-handler-name name))
  (let* ((unique-name (get-interrupt-name name)))
    (if (member name *defined-interrupt-handlers*)
        (error "Multiple definitions of interrupt-handler ~a"
               unique-name)
        (push name *defined-interrupt-handlers*))
    `(labels-block
       (asm-code ',name)
       ,@code
       [RTI])))


"## Testing"
(defun repl ()
  (let ((eof '#.(gensym "EOF-")))
    (loop for x = (read *standard-input* nil eof)
       until (eq x eof) do (eval x)
       do (force-output))))

(defun compiler-reset ()
  "For convience at the lisp repl; Clears out the global scope
   effectively destroying everything the compiler has definied.  "
  (clrhash (scope-identifiers *global-scope*))
  (setf *reset-table-written?* nil)
  (setf *defined-interrupt-handlers* nil)
  (clrhash (scope-tags *global-scope*)))

(defun interactive-compiler-test (file)
  (compiler-reset)
  (with-open-file (*standard-input* file)
    (repl)))

(defun compile-c (file)
  (with-open-file (*standard-input* file)
    (repl)))

#|
(c-proc c-main ()
  (c-var c-x c-int 1)
  (c-var c-y c-int 2)
  c-x
  (A-> c-y)
  (c::while c-x (lda 0) (A-> c-x))
  (c-f c-x)
  (c::while c-y
    (c-switch c-x
      (3 (c-block (lda 1)
           (A-> c-x)))
      (4) (5) (6) (7) (8) (9 (c-block
                                 (lda 0)
                               (A-> c-x)))
      (default (c-block
                   (lda 1) (A-> c-y)
                   (c-break)))
      (10 (c-continue))))
  c-y)
|#
|#
