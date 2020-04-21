(* part 5 *)
let accept_all suffix = 
    Some suffix

type english_symbols = 
| Sentence | NP | VP | Noun | Verb | Adj | Adv | Article | Reason

let my_test_grammar =
  (Sentence,
   function
     | Sentence -> [
         [N NP; N VP; N NP];
         [N Reason; N Sentence]
     ]
     | NP -> [
         [N Article; N Noun];
         [N Article; N Adj; N Noun];
         [N Article; N Noun; N Noun]
     ]
     | VP -> [
         [N Adv; N Verb];
         [N Verb]
     ]
     | Noun -> [
         [T "apple"];
         [T "orange"]
     ]
     | Verb -> [
         [T "eat"];
         [T "drink"]
     ]
     | Adj -> [
         [T "red"];
         [T "yellow"]
     ]
     | Adv -> [
         [T "happily"];
         [T "sadly"]
     ]
     | Article -> [
         [T "the"]
     ]
     | Reason -> [
         [T "because"; N Sentence]
     ]
)

let my_test_sentence = [
    "because"; 
        "because"; 
            "the"; "red"; "apple"; "happily"; "drink"; "the"; "apple"; "orange"; 
        "the"; "red"; "orange"; "eat"; "the"; "yellow"; "apple"; 
    "the"; "orange"; "apple"; "happily"; "drink"; "the"; "yellow"; "apple";]

let make_matcher_test = (make_matcher my_test_grammar accept_all my_test_sentence) = Some []

(* part 6 *)
let my_parse_results =
    Node (Sentence, [
        Node (Reason, [
            Leaf "because";
            Node (Sentence, [
                Node (Reason, [
                    Leaf "because";
                    Node (Sentence, [
                        Node (NP, [
                            Node (Article, [Leaf "the"]);
                            Node (Adj, [Leaf "red"]);
                            Node (Noun, [Leaf "apple"])
                        ]);
                        Node (VP, [
                            Node (Adv, [Leaf "happily"]);
                            Node (Verb, [Leaf "drink"])
                        ]);
                        Node (NP, [
                            Node (Article, [Leaf "the"]);
                            Node (Noun, [Leaf "apple"]);
                            Node (Noun, [Leaf "orange"])
                        ])
                    ])
                ]);
                Node (Sentence, [
                    Node (NP, [
                        Node (Article, [Leaf "the"]);
                        Node (Adj, [Leaf "red"]);
                        Node (Noun, [Leaf "orange"])
                    ]);
                    Node (VP, [Node (Verb, [Leaf "eat"])]);
                    Node (NP, [
                        Node (Article, [Leaf "the"]);
                        Node (Adj, [Leaf "yellow"]);
                        Node (Noun, [Leaf "apple"])
                    ])
                ])
            ])
        ]);

        Node (Sentence, [
            Node (NP, [
                Node (Article, [Leaf "the"]);
                Node (Noun, [Leaf "orange"]);
                Node (Noun, [Leaf "apple"])
            ]);
            Node (VP, [
                Node (Adv, [Leaf "happily"]);
                Node (Verb, [Leaf "drink"])
            ]);
            Node (NP, [
                Node (Article, [Leaf "the"]);
                Node (Adj, [Leaf "yellow"]);
                Node (Noun, [Leaf "apple"])
            ])
        ])
    ])

let make_parser_test = let make_parser_correctness = (make_parser my_test_grammar my_test_sentence) = Some my_parse_results
and make_parser_reverse = match (make_parser my_test_grammar my_test_sentence) with 
    | Some tree -> (parse_tree_leaves tree) = my_test_sentence
    | None -> false
in
if make_parser_correctness = true && make_parser_reverse = true then true else false
