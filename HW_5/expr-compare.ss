#lang racket
(provide expr-compare)

#| don't forget to include the lines above!
	otherwise there might be troubles
	this file is directly runnable by either
	    racket FILENAME.ss
	or, open it in DrRacket and click run

    Also, it can be loaded from racket using

    (require "FILENAME.ss")

    for basic syntax introduction, please see slides and hello.ss
|#

; test if x is lambda
(define (lambda? x)
        (member x '(lambda λ)))

; compare structure of x and y
(define (same-structure? x y)
        #t
)

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
            [else (cons '(if %) (cons x (cons y '())))]
        )
)

; assignment
(define (expr-compare x y) 
    (letrec (
            [iterate-el
                (lambda (x y)
                    (if (null? x)
                        '()
                        (cons 
                            (comp (car x) (car y))
                            (iterate-el (cdr x) (cdr y))
                        )
                    )
                )
            ]
            [comp
                (lambda (x y)
                    (if (and (list? x) (list? y))
                        (if (same-structure? x y)
                            (iterate-el x y) ; iterate element expressions
                            ('(if % x y)) ; combine elements
                        )
                        (combine-singleton x y) ; put into if block, convert 
                    )
                )
            ]
        )
        
        (comp x y)
    )
)
