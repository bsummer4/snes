(asdf:defsystem #:cs400-compiler
  :depends-on (:iterate :cl-match :split-sequence)
  :components ((:file "package")
               (:file "lib")
               (:file "macroexpand-dammit")
               (:file "c-analize")
               (:file "asm")
               (:file "c"))
  :serial t)
