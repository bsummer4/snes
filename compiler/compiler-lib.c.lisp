"## Compiler Defined C-Code"
(c::proto c::main)

(c::subroutine c::reset
  (asm clc :implied)
  (asm xce :implied)
  (16-bit-mode)
  (c::main))

(set-reset-handler "$8000")

(emit "#LoROM")
