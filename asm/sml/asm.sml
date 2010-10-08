structure L = AsmLex
structure U = AsmLex.UserDeclarations

fun getc s =
 case TextIO.input1 s
  of NONE => ""
   | SOME c => str c

val lexer = L.makeLexer (fn n => getc TextIO.stdIn)
fun newline () = print "\n"
fun println x = (print ("LEX: " ^ x); newline ())
fun printToks () =
 let fun r U.EOF = ()
       | r _ = printToks ()
     val x = lexer ()
 in (println (U.anno x); r x)
 end

;
; printToks ()
;
