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

; get length of a list
(define length
  (lambda (ls)
    (if (null? ls)
        0
        (+ (length (cdr ls)) 1))))

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
            
            [similar-lambda? ; only pass actual lambda expressions
                (lambda(x y)
                    (eq? (length (car (cdr x))) (length (car (cdr y))))
                )
            ]
            [is-arg? ; find x's argument list
                (lambda (arg x)
                    (member arg (car (cdr x)))
                )
            ]
            [combine-args
                (lambda (x y)
                    (if (eq? x y)
                        x
                        (string->symbol (string-append (symbol->string x) "!" (symbol->string y)))
                    )
                )
            ]
            [translate-var ; take etire lambda expression, translate arguments when necessary, RETURN PAIR!
                (lambda (x y)
                    (letrec (
                        [make-mapping ; areguments are the argument list of 2 lambda expressions
                            (lambda (x y invert)
                                (if (null? x)
                                    '()
                                    (cons ; create a list of pairs of symbol mapping
                                        (cons (if invert (car x) (car y)) (combine-args (car x) (car y)))
                                        (make-mapping (cdr x) (cdr y) invert)
                                    )
                                )
                            )
                        ]
                        [translate-single 
                            (lambda (ls dp map)                                
                                (if (null? ls)
                                    '()
                                    (if (list? ls)
                                        (if (and (eq? dp 1) (and (lambda? (car ls)) (member (car map) (cadr ls)))) ; if redefined
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
                        ]
                        [translate-all
                            (lambda (x map)
                                (if (null? map)
                                    x ; terminal condition
                                    (translate-all (translate-single x 0 (car map)) (cdr map))
                                )
                            )
                        ]
                        [map-x (make-mapping (cadr x) (cadr y) #t)]
                        [map-y (make-mapping (cadr x) (cadr y) #f)]
                        )
                        (cons 
                            ; translated x
                            (translate-all x map-x)
                            (translate-all y map-y)
                        )
                    )
                )
            ]
            [process-lambda ; 
                (lambda (x y)
                    (if (similar-lambda? x y) 
                        ; if they ARE similar, substitute all corresponding variable arguments and process the remaining list
                        ;   special case: when there's nested lambda expresion, don't substitute that
                        (let ([translated (translate-var x y)])
                            ; lambda . everything later : so program won't infinitely loop
                            (cons (combine-singleton (car (car translated)) (car (cdr translated))) (comp (cdr (car translated)) (cdr (cdr translated)))) 
                        )
                        (combine-singleton x y) ; combine into if expr
                    )
                )
            ]
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

(expr-compare '(cons a lambda) '(cons a λ))

;;; (eq?
;;;     (expr-compare '(cons a lambda) '(cons a λ))
;;;     '(cons a (if % lambda λ))
;;; )

;;; (eq? 
;;;     (expr-compare '(lambda (a) a) '(lambda (b) b))
;;;     '(lambda (a!b) a!b))

;;; (eq? (expr-compare '(lambda (a) b) '(cons (c) b))
;;;     '(if % (lambda (a) b) (cons (c) b)))

;;; (eq? (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
;;;     '((λ (if!fi) (+ if!fi 1)) 3))
    
;;; (eq? (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
;;;     '(λ (lambda!λ) lambda!λ))

;;; (eq? (expr-compare ''lambda '(quote λ))
;;;     '(if % 'lambda 'λ))

;;; (eq? (expr-compare '(lambda (a b) a) '(λ (b) b))
;;;     '(if % (lambda (a b) a) (λ (b) b)))

;;; (eq? (expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
;;;     '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b))))

;;; (eq? (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
;;;     '(λ (let) (let (((if % x y) 1)) (if % x y))))

;;; (eq? (expr-compare '(λ (x) ((λ (x) x) x))
;;;               '(λ (y) ((λ (x) y) x)))
;;;     '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))

;;; (eq? (expr-compare '(((λ (g)
;;;                    ((λ (x) (g (λ () (x x))))     ; This is the way we define a recursive function
;;;                     (λ (x) (g (λ () (x x))))))   ; when we don't have 'letrec'
;;;                  (λ (r)                               ; Here (r) will be the function itself
;;;                    (λ (n) (if (= n 0)
;;;                               1
;;;                               (* n ((r) (- n 1))))))) ; Therefore this thing calculates factorial of n
;;;                 10)
;;;               '(((λ (x)
;;;                    ((λ (n) (x (λ () (n n))))
;;;                     (λ (r) (x (λ () (r r))))))
;;;                  (λ (g)
;;;                    (λ (x) (if (= x 0)
;;;                               1
;;;                               (* x ((g) (- x 1)))))))
;;;                 9))
;;;                 '(((λ (g!x)
;;;                     ((λ (x!n) (g!x (λ () (x!n x!n))))
;;;                      (λ (x!r) (g!x (λ () (x!r x!r))))))
;;;                   (λ (r!g)
;;;                     (λ (n!x) (if (= n!x 0)
;;;                                  1
;;;                                  (* n!x ((r!g) (- n!x 1)))))))
;;;                  (if % 10 9)))