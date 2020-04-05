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

(* part 7 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal


type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num


let awksub_rules =
   [Expr, [T"("; N Expr; T")"];
    Expr, [N Num];
    Expr, [N Expr; N Binop; N Expr];
    Expr, [N Lvalue];
    Expr, [N Incrop; N Lvalue];
    Expr, [N Lvalue; N Incrop];
    Lvalue, [T"$"; N Expr];
    Incrop, [T"++"];
    Incrop, [T"--"];
    Binop, [T"+"];
    Binop, [T"-"];
    Num, [T"0"];
    Num, [T"1"];
    Num, [T"2"];
    Num, [T"3"];
    Num, [T"4"];
    Num, [T"5"];
    Num, [T"6"];
    Num, [T"7"];
    Num, [T"8"];
    Num, [T"9"]]


(* let rec check_non_terminal non_t grammar  = 
    match non_t with 
    | T str -> []
    | N obj -> 
        obj::(get_reachable_nonterminals obj grammar) *)

(* let rec check_rhs rhs grammar = 
    match rhs with 
    | [] -> []
    | r_head::r_rest -> 
        let result_h = check_non_terminal r_head *)


let rec unpack_children rhs =
    match rhs with 
    | [] -> []
    | head::rest ->
        match head with 
        | T str -> unpack_children rest
        | N obj -> obj::(unpack_children rest)


let rec get_children non_t rules =
    match rules with
    | [] -> []
    | head::rest ->
        let h_list = match head with 
            | lhs, rhs -> 
                if lhs = non_t then 
                    unpack_children rhs 
                else 
                    []
        in 
            List.append h_list (get_children non_t rest)


let rec traverse non_t_list rules visited = 
(* non_t_list, in using, is the root node *)
(* visited is defaulted to have non_t_list root to begin with *)
    match non_t_list with 
    | root::non_t_rest ->
        let result_root = 
            if (belong_to root visited) = false then
                let children = get_children root rules in 
                    traverse children rules (root::visited)
            else 
                visited
        in 
            List.append result_root (traverse non_t_rest rules (List.append result_root visited))
    | [] -> visited


let rec filter_rules rules legit = 
    match rules with 
    | first::rest -> 
        let result_first = match first with
        | lhs, rhs ->
            if belong_to lhs legit then 
                [first]
            else 
                []
        in 
            List.append result_first (filter_rules rest legit)
    | [] -> []


let filter_reachable g = 
    match g with 
    | root, rules ->
        let legit = traverse [root] rules [] in 
            (root, (filter_rules rules legit))
