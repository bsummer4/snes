fun inc x = (x := 1+(!x))

exception InternalError
exception InvalidQuotedCharacter of char
datatype lexresult = EOF | LINE of int | WORD of string |
                     NUMBER of string | SYMBOL of char |
                     STRING of string

val lineNum = ref 1
fun unquote s =
  let fun i r (#"\""::[]) = r
        | i r (#"\\"::(#"\\"::xs)) = i (#"\\"::r) xs
        | i r (#"\\"::(#":"::xs)) = i (#";"::r) xs
        | i r (#"\\"::(#"\""::xs)) = i (#"\""::r) xs
        | i r (#"\\"::(x::xs)) = raise InvalidQuotedCharacter x
        | i r (#"\\"::[]) = raise InternalError
        | i r (x::xs) = i (x::r) xs
        | i r [] = raise InternalError
  in case explode s
      of (#"\""::xs) => (implode o rev) (i [] xs)
       | _ => raise InternalError
  end

fun eof () = EOF
fun anno EOF = "eof"
  | anno (LINE x) = "line #" ^ (Int.toString x)
  | anno (WORD x) = "word: " ^ x
  | anno (NUMBER x) = "number: " ^ x
  | anno (SYMBOL x) = "symbol: " ^ str x
  | anno (STRING x) = "string: " ^ x

fun unstr s =
  case explode s
   of [x] => x
    | _ => (print ("ERROR not a character: " ^ s ^ "\n"); raise InternalError)
%%
%structure AsmLex
hexdigit = [0-9A-F];
wordchar = [a-zA-Z0-9_.];
special = [*+\-\=#{}(),\\/];
foo = [a-z];
%%
\n => (inc lineNum; LINE ((!lineNum)-1));
[%$]?(:|{hexdigit})+[bwl]? => (NUMBER yytext);
[A-Za-z_.]{wordchar}* => (WORD yytext);
([\]\[]|{special}) => (SYMBOL (unstr yytext));
\"(\\.|[^\\"])*\" => (STRING (unquote yytext));
. => (print ("wtf is: " ^ yytext ^ "\n"); lex ());
