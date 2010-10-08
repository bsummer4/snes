fun inc x = (x := 1+(!x))

exception InternalError
exception InvalidQuotedCharacter of char
datatype lexresult = EOF | LINE of int | WORD of string |
                     NUMBER of Number.t | SYMBOL of char |
                     STRING of string

val lineCount = ref 1
val charCount = ref 0
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

structure N = Number
fun eof () = EOF
fun anno EOF = "eof"
  | anno (LINE x) = "line #" ^ (Int.toString x)
  | anno (WORD x) = "word: " ^ x
  | anno (NUMBER {value=v,size=s}) =
  	   concat [ "number: ", Int.toString v, ", with size: "
	          , (case s of N.BYTE=>"byte" | N.WORD=>"word" | N.LONG=>"long")
				 , "\n" ]
  | anno (SYMBOL x) = "symbol: " ^ str x
  | anno (STRING x) = "string: " ^ x

fun accept s =
  let fun r [] = ()
        | r (#"\n"::xs) = (inc lineCount; charCount:=0; r xs)
        | r (_::xs) = (inc charCount; r xs)
  in (print (s^"\n"); r (explode s); ())
  end

fun wtf s =
  print (concat [ Int.toString (!lineCount), ":", Int.toString (!charCount)
                , " lexing failed at: ", s, "\n" ])

fun unstr s =
  case explode s
   of [x] => x
    | _ => (print ("ERROR not a character: " ^ s ^ "\n"); raise InternalError)

%%
%structure AsmLex
hexdigit = [0-9A-F];
wordchar = [a-zA-Z0-9_.];
special = [*+\-\=#{}(),\\/];
colon = [:];
dq = ["];
bq = [\]
dot = [.];

%%
"\n" => (accept yytext; LINE ((!lineCount)-1));
[A-Za-z_.]{wordchar}* => (accept yytext; WORD yytext);
[%$:0-9]?({hexdigit}|{colon})+[bwl]? => (accept yytext; NUMBER (Number.parse yytext));
([\]\[]|{special}) => (accept yytext; SYMBOL (unstr yytext));
{dq}(({bq}.)|[^\\"])*{dq} => (accept yytext; STRING (unquote yytext));
" " => (accept yytext; lex ());
. => (accept yytext; wtf yytext; lex ());
