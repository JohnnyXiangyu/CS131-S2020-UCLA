(* global definitions *)
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
let awksub_grammar = Expr, awksub_rules

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

type awkish_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num | Term

let awkish_grammar =
(Expr, function
    | Expr ->
        [[N Term; N Binop; N Expr];
        [N Term]]
    | Term ->
        [[N Num];
        [N Lvalue];
        [N Incrop; N Lvalue];
        [N Lvalue; N Incrop];
        [T"("; N Expr; T")"]]
    | Lvalue ->
        [[T"$"; N Expr]]
    | Incrop ->
        [[T"++"];
        [T"--"]]
    | Binop ->
        [[T"+"];
        [T"-"]]
    | Num ->
        [[T"0"]; [T"1"]; [T"2"]; [T"3"]; [T"4"];
        [T"5"]; [T"6"]; [T"7"]; [T"8"]; [T"9"]])

(* part one *)
let convert_grammar gram1 = match gram1 with
    | starting, rules -> fun non_term ->
        let rec get_production non_term rules =
            match rules with
            | [] -> []
            | head::rest ->
                match head with
                | non_t, prod -> 
                    if non_t = non_term 
                    then prod::(get_production non_term rest)
                    else (get_production non_term rest)
        in
            (get_production non_term rules)


(* part 2 *)
let rec parse_tree_leaves tree = 
    let rec traverse_tree trees =
        match trees with
        | [] -> []
        | head1::rest1 -> match head1 with
            | Leaf term -> term::(traverse_tree rest1)
            | Node (non_t, children) -> traverse_tree (List.append children rest1)
    in traverse_tree [tree]

let test_tree = 
    Node (Expr, [
        Node (Expr, [
            Node (Expr, [
                Node (Expr, [
                    Node (Num, [
                        Leaf "3"
                    ])
                ])
            ]);
            Node (Binop, [Leaf "+"]);
            Node (Expr, [
                Node (Num, [Leaf "4"])
            ])
        ]);
        Node (Binop, [Leaf "-"]);
        Node (Expr, [
            Node (Num, [Leaf "7"])
        ])
    ])

let tree_test = parse_tree_leaves test_tree


(* part 3 *)
(* given a non-terminal symbol, return a list representing the new level *)
let level_gen gram start = 
    match gram with 
    | _, prod -> (prod start)

(* matcher acceptor approach *)
let rec print_list = function 
[] -> ()
| e::l -> print_string e ; print_string " " ; print_list l; print_string "|"

let rec match_symbol gram sym k frag = 
    (match frag with
    | [] -> None
    | h::t ->
        match sym with
        (* if it's a non-terminal, go to the next level, otherwise check equality *)
        | N non_t -> let new_level = (level_gen gram non_t)
            in 
            match_level gram new_level k frag
        | T term -> if h = term 
                    then (k t) 
                    else None )

and match_alt gram alt k frag =
    (match alt with
    | [] -> k frag 
    | h::t -> match_symbol gram h (match_alt gram t k) frag)

and match_level gram lev k frag = 
    (match lev with 
    | [] -> None  
    | h::t -> match (match_alt gram h k frag) with
        | Some result -> Some result 
        | None -> match_level gram t k frag )

let accept_all suffix = 
    Some suffix

let make_matcher gram accep frag =
    match gram with 
    | start, prod -> match_level gram (prod start) accep frag

(* part 4 *)
type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* type ('parent, 'children) parse_list = 
    | Subtree of 'parent * 'children list 
    | Leafnode of 'parent
    | Empty of 'parent *)

let rec parse_symbol gram sym accept (frag, progress) = 
    (match frag with
    | [] -> None
    | h::t ->
        match sym with
        (* if it's a non-terminal, go to the next level, otherwise check equality *)
        | N non_t -> let new_level = (level_gen gram non_t)
            in 
            parse_level gram new_level accept (frag, Node (non_t, []))
        | T term -> if h = term 
                    then (accept (t, Leaf term)) 
                    else None )

and parse_alt gram old_progress alt accept (frag, progress) =
    let new_progress = match progress with 
    | Node (parent_e, []) -> (* when given a no-children parent, it's a call from level *)
        old_progress
    | _ -> (match old_progress with (* if it's returning from symbol, append returned subtree *)
        | Node (parent_o, children_o) -> Node (parent_o, children_o@[progress])
        | Leaf _ -> old_progress) (* this pattern is just to eliminate the error, should never be used *)
    in
    match alt with
    | [] -> (* on completion, accept the remaining parts and the finished subtree *)
        accept (frag, new_progress) 
    | h::t -> parse_symbol gram h (parse_alt gram new_progress t accept) (frag, progress)

and parse_level gram lev k (frag, progress) = 
    (match lev with 
    | [] -> None  
    | h::t -> match (parse_alt gram progress h k (frag, progress)) with
        | Some (f, p) -> Some (f, p)
        | None -> parse_level gram t k (frag, progress) )

let accept_empty (suffix, progress) =
    match suffix with
    | [] -> Some (suffix, progress)
    | _ -> None

let make_parser gram frag = 
    match gram with
    | start, prod -> parse_level gram (prod start) accept_empty (frag, Node (start, []))
(* the return value from parse_level is not clear yet *)