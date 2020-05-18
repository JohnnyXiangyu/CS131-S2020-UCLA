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
        [(and (eq? (car x) (car y)) (eq? (car x) 'lambda)) #t]
        
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

; assignment
(define (expr-compare x y) 
    (letrec (
            [iterate-el
                (lambda (x y)
                    (if (null? x)
                        '()
                        (if (and (lambda? (car x)) (lambda? (car y)))
                            (process-lambda x y)
                            (cons 
                                (comp (car x) (car y))
                                (iterate-el (cdr x) (cdr y))
                            )
                        )
                    )
                )
            ]
            [similar-lambda? ; only pass actual lambda expressions
                (lambda(x y)
                    (eq? (length (car (cdr x))) (length (car (cdr y))))
                )
            ]
            [translate-variable
                (lambda (x o d)
                    (if (null? x)
                        '()
                        (cons (if (eq? (car x) o) d (car x)) (translate-variable (cdr x) o d))
                    )
                )
            ]
            [process-lambda ; 
                (lambda (x y)
                    (if (similar-lambda? x y) 
                        ; if they ARE similar, substitute all corresponding variable arguments and process the remaining list
                        ;   special case: when there's nested lambda expresion, don't substitute that
                        ()
                        ()
                    )
                )
            ]
            [comp
                (lambda (x y)
                    (if (and (list? x) (list? y))
                        (if (similar? x y)
                            (iterate-el x y) ; iterate element expressions
                            (combine-singleton x y);
                        )
                        (combine-singleton x y) ; put into if block, convert 
                    )
                )
            ]
        )
        
        (comp x y)
    )
)

; test cases;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(expr-compare 12 21) ; (if % 12 21)
(expr-compare '(#t #f) '(#f #f)) ; (% #f)
(expr-compare '(lambda 12) '(lambda 1)) ;
(expr-compare '(if x (cdr [a b]) '()) '(if b (cdr [c d]) '())) ;