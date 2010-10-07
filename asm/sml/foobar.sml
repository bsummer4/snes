(* TODO I'm doing a lot of (explode, implode) calls.  Is this ok? *)
structure NumberParse = struct
structure B = StringCvt

fun numVal radix str =
    case Int.scan radix List.getItem (explode str)
     of NONE => NONE
      | SOME (n,[]) => SOME n
      | SOME (_,_) => NONE

datatype prefix = PERCENT | DOLLAR
datatype suffix = BYTE | WORD | LONG
fun split s =
    let val ir = (implode o rev)
        fun rest (p,soFar) [] = (p, ir soFar, NONE)
          | rest (p,soFar) (#"b"::[]) = (p, ir soFar, SOME BYTE)
          | rest (p,soFar) (#"w"::[]) = (p, ir soFar, SOME WORD)
          | rest (p,soFar) (#"l"::[]) = (p, ir soFar, SOME LONG)
          | rest (p,soFar) (x::xs) = rest (p,x::soFar) xs
        fun prefix (#"%"::xs) = rest ((SOME PERCENT),[]) xs
          | prefix (#"$"::xs) = rest ((SOME DOLLAR),[]) xs
          | prefix xs = rest (NONE,[]) xs
    in prefix (explode s)
    end

(* The rest of the code in this structure is a mess.  Clean it up.  *)
datatype reason = TOO_LARGE | INVALID_LENGTH | INVALID_CHARS
                | NONMATCHING_SIZETAG | SIZETAG_TOO_SMALL

exception Failure' of reason
exception Failure of string * reason
fun fail r = raise (Failure' r)

fun base (SOME PERCENT) = B.BIN
  | base (SOME DOLLAR) = B.HEX
  | base NONE = B.DEC

fun smallestFit v =
    if v <= 0xFF then BYTE
    else if v <= 0xFFFF then WORD
    else if v <= 0xFFFFFF then LONG
    else fail TOO_LARGE

fun value (base, chars) =
    case numVal base (implode chars)
     of SOME x => x
      | NONE => fail INVALID_CHARS

fun impliedSize (base, chars) =
    case (base, length chars)
     of (B.BIN,8) => SOME BYTE | (B.HEX,2) => SOME BYTE
      | (B.BIN,16) => SOME WORD | (B.HEX,4) => SOME WORD
      | (B.BIN,24) => SOME LONG | (B.HEX,6) => SOME LONG
      | (B.BIN,_) => fail INVALID_LENGTH
      | (B.HEX,_) => fail INVALID_LENGTH
      | _ => NONE

fun size (impliedSize, suffix, smallestFit) =
    case (impliedSize, suffix)
     of (NONE, SOME x) => x
      | (NONE, NONE) => smallestFit
      | (SOME x, SOME y) => if x <> y
                            then fail NONMATCHING_SIZETAG
                            else x
      | (SOME x, NONE) => x

fun parse str =
    let val (p,c,s) = split str
        val cs = List.filter (fn x => x <> #":") (explode c)
        fun sizeNum x = case x of BYTE=>1|WORD=>2|LONG=>3
        val b = (base p)
        val (is, v) = (impliedSize (b,cs), value (b,cs))
        val sf = (smallestFit v)
    in if ((sizeNum sf) > (sizeNum (size (is,s,sf))))
       then fail SIZETAG_TOO_SMALL
       else {size=(size (is,s,sf)), value=v}
    end
    handle (Failure' x) => raise Failure (str,x)
end

structure N = NumberParse

;
; N.split "$23:43w"
; N.split "23"
; N.parse "254w"
;
