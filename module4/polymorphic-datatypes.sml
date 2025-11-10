fun sum_list xs =
    case xs of
        [] => 0
      | x :: xs' => x + sum_list xs'

fun append (xs ,ys) =
    case xs of
        [] => ys
      | x :: xs' => x :: append(xs', ys)

datatype 'a option = NONE | SOME of 'a
datatype 'a mylist = Empty | Cons of 'a * 'a mylist
datatype ('a, 'b) tree =
         Node of 'a * ('a,'b) tree * ('a, 'b) tree
         | Leaf of 'b

fun sum_tree tr =
    case tr of
        Leaf i => i
      | Node(i, lft, rgt) => i + sum_tree lft + sum_tree rgt

fun sum_leaves tr =
    case tr of
        Leaf i => i
      | Node(i, lft, rgt) => sum_leaves lft + sum_leaves rgt

(* Function arguments are patterns *)
fun full_name1 (r : {first:string, middle:string, last:string}) =
    case r of
        {first=x, middle=y, last=z} => x ^ " " ^ y ^ " " ^ z

fun full_name2 (r : {first:string, middle:string, last:string}) =
    let val {first = x, middle = y, last = z} = r
    in
        x ^ " " ^ y ^ " " ^ z
    end

fun full_name3 {first=x, second=y, last=z} =
    x ^ " " ^ y ^ " " ^ z

fun sum_triple1 (x, y, z) =
    x + y + z

fun partial_sum(x, y, z) =
    x + z

fun partial_name{ first=x, middle=y, last=z} =
    x ^ " " ^ y " " ^ z

(* Polymorphic types and equality types *)

