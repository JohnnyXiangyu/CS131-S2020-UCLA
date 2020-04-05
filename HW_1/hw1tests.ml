let my_subset_test0 = subset [1;3;2;4;4;4] [1;2;3;4;5]
let my_equal_sets_test0 = equal_sets [1;2;3;3;3;4] [4;3;2;1]
let my_set_union_test0 = equal_sets (set_union [1;2;3;4;4;4] [5;6;7;8]) [1;2;3;4;5;6;7;8]
let my_set_intersection_test0 = equal_sets (set_intersection [1;2;3;4] [4;3;3;3]) [3;4]
let my_set_diff_test0 = equal_sets (set_diff [1;2;3;4] [3;4;5;6]) [1;2]

let some_func a = a/2

let my_computed_fixed_point_test0 = (computed_fixed_point (=) some_func 5) = 0

type my_non_t = 
    | Hello 
    | World 
    | Foo
    | Bar 

let my_rules = [
    Hello, [T "h"; N Foo; T "o"];
    World, [T "DIO"];
    Foo, [N Hello];
    Bar, [T "nothing"]
]

let my_filter_reachable_test0 = (filter_reachable (Hello, my_rules)) = (Hello, [Hello, [T "h"; N Foo; T "o"];Foo, [N Hello]])
