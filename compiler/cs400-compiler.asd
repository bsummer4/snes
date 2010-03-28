(asdf:defsystem #:cs400-compiler
  :depends-on (:iterate :cl-match :split-sequence)
  :components ((:file "package")
               (:file "macroexpand-dammit")
               (:file "lib")
               (:file "asm")
               (:file "c"))
  :serial t)
