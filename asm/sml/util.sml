structure Util = struct
 fun eq x y = (x = y)
 fun neq x y = (x <> y)
 fun inc x = (x := 1+(!x))
 fun protect (SOME x) _ = x
    | protect NONE f = f ()
end

structure U = Util;
