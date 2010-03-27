(asdf:defsystem #:cs400-compiler
  :depends-on (:iterate :cl-match)
  :components ((:file "package")
               (:file "lib")
               (:file "asm")
               (:file "c"))
  :serial t)
