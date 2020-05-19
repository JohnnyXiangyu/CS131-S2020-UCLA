(define (test-expr-compare x y)
    (and
        (equal? 
            (eval x)
            (eval `(let ([% #t]), (expr-compare x y)))
        )
        (equal? 
            (eval y)
            (eval `(let ([% #f]), (expr-compare x y)))
        )
    )
)

(test-x 12 21)


