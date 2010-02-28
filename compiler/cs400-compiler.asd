(asdf:defsystem #:cs400-compiler
  :components ((:file "package")
               (:file "lib")
               (:file "asm")
               (:file "c"))
  :serial t)
