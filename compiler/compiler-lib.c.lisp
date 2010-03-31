"## Compiler Defined C-Code"
(c::subroutine c::reset
  (asm clc :implied)
  (asm xce :implied)
  (16-bit-mode)
  (c::main)
  (asm jsr :absolute (c-fn-unique-name 'c::main)))

(set-reset-handler "reset")
