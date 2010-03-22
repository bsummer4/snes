"This is the code that actually creates the textual asm output.  "

(in-package #:cs400-compiler)

(defparameter +addressing-modes-and-syntax+
  '((:implied "")
    (:accumulator "A")
    (:immediate "#b")
    (:immediate-w "#w")
    #| TODO What are the other kinds of 'immediate's? |#
    (:direct "b")
    (:direct-x-indexed "b,X")
    (:direct-y-indexed "b,Y")
    (:stack-indexed "b,S")
    (:direct-indirect "(b)")
    (:direct-indirect-long "[b]")
    (:direct-x-indexed-indirect "(b,X)")
    (:direct-indirect-y-indexed "(b),Y")
    (:direct-indirect-long-y-indexed "[b],Y")
    (:stack-relative-indirect-y-indexed "(b,S),Y")
    (:block-move "b,b")
    (:absolute "w")
    (:absolute-x-indexed "w,X")
    (:absolute-y-indexed "w,Y")
    (:absolute-indirect "(w)")
    (:absolute-indirect-long "[w]")
    (:absolute-x-indexed-indirect "(w,X)")
    (:long "l")
    (:long-x-indexed "l,X")))

(defun emit (string) (format t "; ~a~%" string) (values))

(defun asm-subformat (format-char argument)
  (etypecase argument
    (symbol (symbol-name argument))
    (number (format nil (ecase format-char
                          (#\b "$~2,'0xb")
                          (#\w "$~4,'0xw")
                          (#\l "$~6,'0xl"))
                    argument))))

(defun asm-format (format-string &rest arguments)
  "Take an asm form from +addressing-modes-and-syntax+ and some arguements,
   returning the corresponding asm syntax.  "
  (coerce (flatten
           (loop for char across format-string
              collect (if (find char '(#\b #\w #\l))
                          (coerce (asm-subformat char (pop arguments)) 'list)
                          (list char))))
          'string))

(defmacro asm (command mode &rest args)
  `(emit (format nil "~a ~a"
                 ,(symbol-name command)
                 (asm-format ,(lookup +addressing-modes-and-syntax+ mode)
                             ,@args))))
