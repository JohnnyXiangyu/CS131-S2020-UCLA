(* part 1 *)
let rec belong_to el set = 
    if set = [] then    
        false 
    else
        if el = List.hd set then
            true
        else
            belong_to el (List.tl set)

let rec subset a b = 
    if a = [] then
        true
    else 
        if (belong_to (List.hd a) b) = false then
            false
        else 
            subset (List.tl a) b

(* part 2 *)
let equal_sets a b =
    (subset a b) && (subset b a)

(* part 3 *)
let set_union a b =
    List.append a b

(* part 4 *)
let rec set_intersection a b =
    if a = [] then 
        []
    else
        if belong_to (List.hd a) b then 
            (List.hd a) :: set_intersection (List.tl a) b 
        else 
            set_intersection (List.tl a) b

(* part 5 *)
let rec set_diff a b =
    if a = [] then 
        []
    else
        if (belong_to (List.hd a) b) = false then 
            (List.hd a) :: set_diff (List.tl a) b 
        else 
            set_diff (List.tl a) b

(* part 6 *)
let rec computed_fixed_point eq f x =
    if (eq) (f x) x then 
        x
    else 
        computed_fixed_point eq f (f x)
