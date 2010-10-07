structure AsmLex=
   struct
    structure UserDeclarations =
      struct
(*#line 1.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)fun inc x = (x := 1+(!x))

exception InternalError
exception InvalidQuotedCharacter of char
datatype lexresult = EOF | LINE of int | WORD of string |
                     NUMBER of string | SYMBOL of char |
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

fun eof () = EOF
fun anno EOF = "eof"
  | anno (LINE x) = "line #" ^ (Int.toString x)
  | anno (WORD x) = "word: " ^ x
  | anno (NUMBER x) = "number: " ^ x
  | anno (SYMBOL x) = "symbol: " ^ str x
  | anno (STRING x) = "string: " ^ x

fun accept s =
  let fun r [] = ()
        | r (#"\n"::xs) = (inc lineCount; charCount:=0; r xs)
        | r (_::xs) = (inc charCount; r xs)
  in r (explode s)
  end

fun wtf s =
  print (concat [ Int.toString (!lineCount), ":", Int.toString (!charCount)
                , " lexing failed at: ", s, "\n"
                ])

fun unstr s =
  case explode s
   of [x] => x
    | _ => (print ("ERROR not a character: " ^ s ^ "\n"); raise InternalError)
(*#line 53.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
end (* end of user routines *)
exception LexError (* raised if illegal leaf action tried *)
structure Internal =
	struct

datatype yyfinstate = N of int
type statedata = {fin : yyfinstate list, trans: string}
(* transition & final state table *)
val tab = let
val s = [ 
 (0, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (1, 
"\003\003\003\003\003\003\003\003\003\003\021\003\003\003\003\003\
\\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\003\
\\020\003\015\004\014\014\003\003\004\004\004\004\004\004\013\004\
\\012\012\012\012\012\012\012\012\012\012\012\004\004\004\003\003\
\\003\007\007\007\007\007\007\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\004\004\004\003\005\
\\003\005\005\005\005\005\005\005\005\005\005\005\005\005\005\005\
\\005\005\005\005\005\005\005\005\005\005\005\004\003\004\003\003\
\\003"
),
 (5, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\000\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\006\006\006\006\006\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\000\
\\000"
),
 (7, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000\
\\009\009\009\009\009\009\009\009\009\009\010\000\000\000\000\000\
\\000\009\009\009\009\009\009\006\006\006\006\006\006\006\006\006\
\\006\006\006\006\006\006\006\006\006\006\006\000\000\000\000\006\
\\000\006\008\006\006\006\006\006\006\006\006\006\008\006\006\006\
\\006\006\006\006\006\006\006\008\006\006\006\000\000\000\000\000\
\\000"
),
 (10, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000\010\010\010\010\010\010\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\011\000\000\000\000\000\000\000\000\000\011\000\000\000\
\\000\000\000\000\000\000\000\011\000\000\000\000\000\000\000\000\
\\000"
),
 (14, 
"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\010\010\010\010\010\010\010\010\010\010\010\000\000\000\000\000\
\\000\010\010\010\010\010\010\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\\000"
),
 (15, 
"\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\019\016\016\016\016\016\016\016\016\016\016\016\017\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\017\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\017\000\017\016\016\
\\016\016\016\016\017\016\016\016\016\016\016\016\016\016\016\017\
\\016\016\016\016\017\016\016\016\016\016\016\016\016\016\016\016\
\\016"
),
 (17, 
"\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\018\016\016\016\016\016\016\016\016\016\016\016\017\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\017\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\016\
\\016\016\016\016\016\016\016\016\016\016\016\017\016\017\016\016\
\\016\016\016\016\017\016\016\016\016\016\016\016\016\016\016\017\
\\016\016\016\016\017\016\016\016\016\016\016\016\016\016\016\016\
\\016"
),
(0, "")]
fun f x = x 
val s = map f (rev (tl (rev s))) 
exception LexHackingError 
fun look ((j,x)::r, i: int) = if i = j then x else look(r, i) 
  | look ([], i) = raise LexHackingError
fun g {fin=x, trans=i} = {fin=x, trans=look(s,i)} 
in Vector.fromList(map g 
[{fin = [], trans = 0},
{fin = [], trans = 1},
{fin = [], trans = 1},
{fin = [(N 24)], trans = 0},
{fin = [(N 14),(N 24)], trans = 0},
{fin = [(N 4),(N 24)], trans = 5},
{fin = [(N 4)], trans = 5},
{fin = [(N 4),(N 11),(N 24)], trans = 7},
{fin = [(N 4),(N 11)], trans = 5},
{fin = [(N 4),(N 11)], trans = 7},
{fin = [(N 11)], trans = 10},
{fin = [(N 11)], trans = 0},
{fin = [(N 11),(N 14),(N 24)], trans = 10},
{fin = [(N 4),(N 14),(N 24)], trans = 5},
{fin = [(N 24)], trans = 14},
{fin = [(N 24)], trans = 15},
{fin = [], trans = 15},
{fin = [], trans = 17},
{fin = [(N 20)], trans = 15},
{fin = [(N 20)], trans = 0},
{fin = [(N 22),(N 24)], trans = 0},
{fin = [(N 1)], trans = 0}])
end
structure StartStates =
	struct
	datatype yystartstate = STARTSTATE of int

(* start state definitions *)

val INITIAL = STARTSTATE 1;

end
type result = UserDeclarations.lexresult
	exception LexerError (* raised if illegal leaf action tried *)
end

structure YYPosInt : INTEGER = Int
fun makeLexer yyinput =
let	val yygone0= YYPosInt.fromInt ~1
	val yyb = ref "\n" 		(* buffer *)
	val yybl = ref 1		(*buffer length *)
	val yybufpos = ref 1		(* location of next character to use *)
	val yygone = ref yygone0	(* position in file of beginning of buffer *)
	val yydone = ref false		(* eof found yet? *)
	val yybegin = ref 1		(*Current 'start state' for lexer *)

	val YYBEGIN = fn (Internal.StartStates.STARTSTATE x) =>
		 yybegin := x

fun lex () : Internal.result =
let fun continue() = lex() in
  let fun scan (s,AcceptingLeaves : Internal.yyfinstate list list,l,i0) =
	let fun action (i,nil) = raise LexError
	| action (i,nil::l) = action (i-1,l)
	| action (i,(node::acts)::l) =
		case node of
		    Internal.N yyk => 
			(let fun yymktext() = substring(!yyb,i0,i-i0)
			     val yypos = YYPosInt.+(YYPosInt.fromInt i0, !yygone)
			open UserDeclarations Internal.StartStates
 in (yybufpos := i; case yyk of 

			(* Application actions *)

  1 => let val yytext=yymktext() in (*#line 59.10 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)accept yytext; LINE ((!lineCount)-1)(*#line 224.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
 end
| 11 => let val yytext=yymktext() in (*#line 61.42 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)accept yytext; NUMBER yytext(*#line 226.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
 end
| 14 => let val yytext=yymktext() in (*#line 62.24 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)accept yytext; SYMBOL (unstr yytext)(*#line 228.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
 end
| 20 => let val yytext=yymktext() in (*#line 63.31 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)accept yytext; STRING (unquote yytext)(*#line 230.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
 end
| 22 => let val yytext=yymktext() in (*#line 64.9 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)accept yytext; lex ()(*#line 232.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
 end
| 24 => let val yytext=yymktext() in (*#line 65.7 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)accept yytext; wtf yytext; lex ()(*#line 234.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
 end
| 4 => let val yytext=yymktext() in (*#line 60.27 "/home/ben.real/repo/cs400/assembler/sml/asm.lex"*)accept yytext; WORD yytext(*#line 236.1 "/home/ben.real/repo/cs400/assembler/sml/asm.lex.sml"*)
 end
| _ => raise Internal.LexerError

		) end )

	val {fin,trans} = Vector.sub(Internal.tab, s)
	val NewAcceptingLeaves = fin::AcceptingLeaves
	in if l = !yybl then
	     if trans = #trans(Vector.sub(Internal.tab,0))
	       then action(l,NewAcceptingLeaves
) else	    let val newchars= if !yydone then "" else yyinput 1024
	    in if (size newchars)=0
		  then (yydone := true;
		        if (l=i0) then UserDeclarations.eof ()
		                  else action(l,NewAcceptingLeaves))
		  else (if i0=l then yyb := newchars
		     else yyb := substring(!yyb,i0,l-i0)^newchars;
		     yygone := YYPosInt.+(!yygone, YYPosInt.fromInt i0);
		     yybl := size (!yyb);
		     scan (s,AcceptingLeaves,l-i0,0))
	    end
	  else let val NewChar = Char.ord(CharVector.sub(!yyb,l))
		val NewChar = if NewChar<128 then NewChar else 128
		val NewState = Char.ord(CharVector.sub(trans,NewChar))
		in if NewState=0 then action(l,NewAcceptingLeaves)
		else scan(NewState,NewAcceptingLeaves,l+1,i0)
	end
	end
(*
	val start= if substring(!yyb,!yybufpos-1,1)="\n"
then !yybegin+1 else !yybegin
*)
	in scan(!yybegin (* start *),nil,!yybufpos,!yybufpos)
    end
end
  in lex
  end
end
