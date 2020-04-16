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
let temp_progress = [[[N Num]; [N Lvalue]; [N Incrop; N Lvalue]]; [[N Binop; N Expr]; [N Num]]]

type ('top_el)progress_status =
| EmptyWhole | EmptyL1 | EmptyAlt1 | TopEl of 'top_el

(* get a symbol, return the non-terminal in it, this seems problemetic *)
let symToNT = function sym ->
    match sym with 
    | N x -> x 
    | T y -> y

(* return the first element in the first alternative of highest level
 * return 0 when any level has empty list: whole progress, first  *)
let getProgressTop = function progress ->
    match progress with 
    | [] -> EmptyWhole
    | l1::r1 -> match l1 with
        | [] -> EmptyL1
        | alt1::r2 -> match alt1 with 
            | [] -> EmptyAlt1
            | top_el::r3 -> TopEl top_el

(* pop the first element in the first alternative of highest level, return new list *)
let popProgressTop = function progress ->
    match progress with 
    | [] -> []
    | h1::r1 -> match h1 with
        | [] -> []::r1
        | h2::r2 -> match h2 with 
            | [] -> ([]::r2)::r1 
            | top_el::r3 -> (r3::r2)::r1 

(* pop the first element, and prepend its alternative list, return new list
 * only when there is a valid top element will this function modify original progress *)
let expandTop = function progress -> function rules ->
    match (getProgressTop progress) with
    | EmptyWhole -> []
    | EmptyL1 -> progress
    | EmptyAlt1 -> progress
    | TopEl top_el -> 
        let popped_progress = popProgressTop progress
        in 
        (rules (symToNT top_el))::popped_progress

(* pop the first alt of highest level *)
let popAlt = function progress ->
    match progress with 
    | [] -> []
    | lev_h::r1 -> match lev_h with
        | [] -> []::r1  
        | alt_1::r2 -> r2::r1 

(* pop the highest level (return empty if original is empty) *)
let popLevel = function progress ->
    match progress with 
    | [] -> []
    | lev_h::r1 -> r1 

