(*old zip3*)
exception ListLengthMismatch

fun old_zip3(xs,ys,zs) =
    if null xs andalso null ys andalso null zs
    then []
    else if null xs orelse null ys orelse null zs
    then raise ListLengthMismatch
    else (hd xs, hd ys, hd ys) :: old_zip3(tl xs, tl ys, tl zs)
                                          
fun zip3 list_triple =
    case list_triple of
        ([],[],[]) => []
      | (hd1::tl1, hd2::tl2, hd3::tl3 ) => (hd1,hd2,hd3)::zip3(tl1,tl2,tl3)
      | _ => raise ListLengthMismatch
                   

fun unzip3 lst =
    case lst of
        [] => ([],[],[])
      | (a,b,c)::tl => let
          val (l1,l2,l3) = unzip3 tl
      in
          (a::l1, b::l2, c::l3)
      end

fun nondecreasing xs =
    case xs of
        [] => true
      | x::xs' => case xs' of
                      [] => true
                    | y::ys' => x <= y andalso nondecreasing xs'
                                                             
fun nondecreasing1 xs =
    case xs of
        [] => true
      | _ ::[] => true
      | head::(neck::xs') => head <= neck andalso nondecreasing1 xs'

datatype sgn = Positive | Negative | Zero

fun multsign (x, y) =
    let
        fun sign x = if x = 0 then Zero else if x > 0 then Positive else Negative
    in
        case (sign x, sign y) of
            (Zero , _)           => Zero
          | (_, Zero)            => Zero
          | (Positive, Positive) => Positive
          | (Negative, Negative) => Positive
          | (_,_)                    => Negative 
    end

fun len xs =
    case xs of
        [] => 0
      | _ :: xs' => 1 + len xs'
 
(* Pattern matching is a recursive procedure *)

(*
a::b::c::d  matches with all lists with 3 elements
a::b::c::[] matches with all lists with 3 elements
((a,b),(c,d))::e matches with non-empty lists of pairs of pairs
*)
