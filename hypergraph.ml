(* Type definition *)
type vertex = int list;;
type hypergraph = int list list;;

(* Integer helper methods *)
let rec pow a = function
  | 0 -> 1
  | 1 -> a
  | n -> let b = pow a (n / 2) in b * b * (if n mod 2 = 0 then 1 else a)

(* Vertex helper methods *)
let rec find e l = match l with
  | []   -> false
  | h::t -> if h=e then true else find e t;;

let rec sort = function
    | []     -> []
    | h :: t -> ins h (sort t)
  and ins e = function
    | []     -> [e]
    | h :: t -> if e < h then e :: h :: t else h :: ins e t;;

let rec id v = match v with
  | []   -> 0
  | h::t -> (pow 2 h) + id t

let rec part = function
  | n when n <= 0 -> []
  | n -> let k = int_of_float (floor (log (float_of_int n) /. log 2.))
          in k :: part (n  - (pow 2 k))

(* Debug functions *)
let string_of_vertex v = "{" ^ (String.concat ", " (List.map string_of_int v)) ^ "}";;
let print_list l = print_string ("[" ^ (String.concat "; " (List.map string_of_int l)) ^ "]");;
let print_vertex v = print_string (string_of_vertex v);;
let print_hypergraph h = print_string ("{" ^ (String.concat ", " (List.map string_of_vertex h)) ^ "}\n");;

(* Ensemblist operations *)
let rec union a b = match a @ b with
  | []   -> []
  | h::t -> begin match union t [] with
    | u when find h u -> u
    | u               -> h::u
  end;;

let rec inter a b = match a with
  | []   -> []
  | h::t -> if find h b then h :: inter t b else inter t b;;

let rec diff a b = match a with
  | []   -> []
  | h::t -> if find h b then diff t b else h :: diff t b;;

let rec cardinal = function
  | []   -> 0
  | _::t -> 1 + cardinal t

(* Hypergraph treatment methods *)
let rec trace h u = match h with
  | []    -> []
  | v::tl -> (sort (inter u v)) :: (trace tl u);;

let rec shift h x =
  let rec _shift hr x = match hr with
  | []    -> []
  | v::tl -> begin match sort (diff v [x]) with
    | vn when find vn h -> (sort v) :: _shift tl x
    | vn                -> (sort vn) :: _shift tl x
    end in _shift h x;;

let rec unshift h x =
  let rec _unshift hr x = match hr with
  | []    -> []
  | v::tl -> begin match sort (union v [x]) with
    | vn when find vn h -> (sort v) :: _unshift tl x
    | vn                -> (sort vn) :: _unshift tl x
    end in _unshift h x;;


let h = [[0]; [0; 2]; [1; 2]; [0; 1; 2]];;
let dh = shift h 0;;

print_hypergraph h;;
print_hypergraph dh;;
print_hypergraph (unshift dh 0);;
