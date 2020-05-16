
signature EQ = sig
    type t 
    val eq: t -> t -> bool
end

signature SET = sig 
    type key
    type map

    val empty: map
    val mem: key -> map -> bool
    val add: key -> map -> map
end

structure IntEq : EQ = struct
    type t = int 
    fun eq a b = a = b
end

functor Set(Key : EQ) : SET  = struct
    type key = Key.t
    type map = key list

    val empty = []
    fun mem k m = false
    fun add k m = k::m
end 

structure x = Set(IntEq)

val q = x.add 10 x.empty
val q = x.add 12 q
val _ = print "hello! \n";

