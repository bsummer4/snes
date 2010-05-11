#|
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
|#
