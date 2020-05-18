; test if x is lambda
(define (lambda? x)
        (member x '(lambda λ)))

; compare structure of x and y
(define (similar? x y)
    (cond 
        ; both are empty
        [(and (null? x) (null? y)) #t]
        ; 2 cases for lambda
        [(and (eq? (car x) (car y)) (eq? (car x) 'lambda)) #t]
        [(and (eq? (car x) (car y)) (eq? (car x) 'λ)) #t]
        ; if
        [(and (eq? (car x) (car y)) (eq? (car x) 'if)) #t]
        ; quote, only one quote needed?
        [(and (eq? (car x) (car y)) (eq? (car x) 'quote)) #t]
        ; any other statements are treated as custom procedures, they are tested on list length
        [(eq? (length x) (length y)) #t]
        
        [else #f]
    )
)

; get length of a list
(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ (length (cdr ls)) 1))))

; combine singleton elements
(define (combine-singleton x y)
        (cond 
            [(and (boolean? x) (boolean? y))
                (if (eq? x y)
                    x
                    (if x '% '(not %))
                )
            ]
            [(and (lambda? x) (lambda? y)) ; try combining them into λ
                    (if (and (member x '(lambda)) (member y '(lambda))) 
                        'lambda 
                        'λ
                    )
            ]
            [else (if (eq? x y)
                    x
                    (cons 'if (cons '% (cons x (cons y '()))))
                )
            ]
        )
)

(define 
translate-single 
    (lambda (ls dp map)                                
        (if (null? ls)
            '()
            (if (list? ls)
                (if (and (eq? dp 1) (and (lambda? (car ls)) (member (car map) (cadr ls)))) ; if variable redefined
                    ls ; return unchanged
                    (cons 
                        (translate-single (car ls) 1 map) 
                        (translate-single (cdr ls) 0 map)
                    )
                )
                (if (eq? (car map) ls) (cdr map) ls) ; if it's not a list, translate
            )
        )                                
    )
)



(define 
translate-all
    (lambda (x map)
        (if (null? map)
            x ; terminal condition
            (translate-all (translate-single x 0 (car map)) (cdr map))
        )
    )
)

(translate-all '(lambda (a b c) (a b c)) '((a . a!) (b . b!) (c . c!)))
(translate-single '(lambda (a b c) (a b c)) 0 '(a . a!))

(define
combine-args
    (lambda (x y)
        (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
    )
)

(define
similar-lambda? ; only pass actual lambda expressions
                (lambda(x y)
                    (and (eq? (length (car (cdr x))) (length (car (cdr y)))) (>= (length (car (cdr x))) 3))
                )
)

