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
        ; lambda has 2 notations
        [(and (lambda? (car x)) (lambda? (car y))) #t]
        ; if
        [(and (eq? (car x) (car y)) (eq? (car x) 'if)) #t]
        ; quote, only one quote needed?
        [(and (eq? (car x) (car y)) (eq? (car x) 'quote)) #t]
        ; any other statements are treated as custom procedures, they are tested on list length
        [(and
            (eq? (length x) (length y))
            (not (member (car x) '(lambda λ if quote))) 
            (not (member (car x) '(lambda λ if quote)))
         )
             #t]
        
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
            [else (if (eq? x y)
                    x
                    (list 'if '%  x y )
                )
            ]
        )
)

; assignment
(define (expr-compare x y) 
    (letrec (
            
            [similar-lambda? ; only pass actual lambda expressions
                (lambda(x y)
                    (and 
                        (eq? (length x) 3)
                        (eq? (length y) 3)
                        (eq? (length (car (cdr x))) (length (car (cdr y))))
                    )
                ) ; TODO this harsh condition might cause problems 
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
                                        (if (and (eq? dp 1) (lambda? (car ls)) (eq? (length ls) 3) (member (car map) (cadr ls))) ; if redefined
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
                            (cons 
                                (if (and (eq? (car x) 'lambda) (eq? (car y) 'lambda)) 
                                        'lambda 
                                        'λ ; combine lambda symbols
                                )
                                (comp (cdr (car translated)) (cdr (cdr translated)))) 
                        )
                        (combine-singleton x y) ; combine into if expr
                    )
                )
            ]
            [iterate-el
                (lambda (x y dp)
                    (if (null? x)
                        '()
                        (cond 
                            ; if this is a lambda expression
                            [(and (eq? dp 0) (lambda? (car x)) (lambda? (car y))) (process-lambda x y)]
                            ; quoted expression
                            [(and (eq? (car x) (car y)) (eq? (car x) 'quote)) (combine-singleton x y)]
                            ; else
                            [else (cons (comp (car x) (car y)) (iterate-el (cdr x) (cdr y) 1))]
                        )
                    )
                )
            ]
            [comp
                (lambda (x y)
                    (if (and (list? x) (list? y))
                        (if (similar? x y)
                            (iterate-el x y 0) ; a fresh list has depth 0
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

;;; (equal?
;;;     (expr-compare '(cons a lambda) '(cons a λ))
;;;     '(cons a (if % lambda λ))
;;; )


;;; (equal? 
;;;     (expr-compare '(lambda (a) a) '(lambda (b) b))
;;;     '(lambda (a!b) a!b))

;;; (equal? (expr-compare '(lambda (a) b) '(cons (c) b))
;;;     '(if % (lambda (a) b) (cons (c) b)))

;;; (equal? (expr-compare '((λ (if) (+ if 1)) 3) '((lambda (fi) (+ fi 1)) 3))
;;;     '((λ (if!fi) (+ if!fi 1)) 3))


;;; (equal? (expr-compare '(lambda (lambda) lambda) '(λ (λ) λ))
;;;     '(λ (lambda!λ) lambda!λ))


;;; (equal? (expr-compare ''lambda '(quote λ))
;;;     '(if % 'lambda 'λ))

;;; (equal? (expr-compare '(lambda (a b) a) '(λ (b) b))
;;;     '(if % (lambda (a b) a) (λ (b) b)))

;;; (equal? (expr-compare '(λ (a b) (lambda (b) b)) '(lambda (b) (λ (b) b)))
;;;     '(if % (λ (a b) (lambda (b) b)) (lambda (b) (λ (b) b))))

;;; (equal? (expr-compare '(λ (let) (let ((x 1)) x)) '(lambda (let) (let ((y 1)) y)))
;;;     '(λ (let) (let (((if % x y) 1)) (if % x y))))

;;; (equal? (expr-compare '(λ (x) ((λ (x) x) x))
;;;               '(λ (y) ((λ (x) y) x)))
;;;     '(λ (x!y) ((λ (x) (if % x x!y)) (if % x!y x))))

;;; (equal? (expr-compare '(((λ (g)
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

;;; (equal? (expr-compare 12 12)     12)
;;; (equal? (expr-compare 12 20)     '(if % 12 20))
;;; (equal? (expr-compare #t #t)     #t)
;;; (equal? (expr-compare #f #f)     #f)
;;; (equal? (expr-compare #t #f)     '%)
;;; (equal? (expr-compare #f #t)     '(not %))
;;; (equal? (expr-compare 'a '(cons a b))     '(if % a (cons a b)))
;;; (equal? (expr-compare '(cons a b) '(cons a b))     '(cons a b))
;;; (equal? (expr-compare '(cons a lambda) '(cons a λ))     '(cons a (if % lambda λ)))

;;; (equal? (expr-compare '(cons (cons a b) (cons b c))
;;;               '(cons (cons a c) (cons a c)))

;;;     '(cons (cons a (if % b c)) (cons (if % b a) c)))

;;; (equal? (expr-compare '(cons a b) '(list a b))     '((if % cons list) a b))
;;; (equal? (expr-compare '(list) '(list a))     '(if % (list) (list a)))
;;; (equal? (expr-compare ''(a b) ''(a c))     '(if % '(a b) '(a c)))
;;; (equal? (expr-compare '(quote (a b)) '(quote (a c)))     '(if % '(a b) '(a c)))
;;; (equal? (expr-compare '(quoth (a b)) '(quoth (a c)))     '(quoth (a (if % b c))))
;;; (equal? (expr-compare '(if x y z) '(if x z z))     '(if x (if % y z) z))

;;; (equal? (expr-compare '(if x y z) '(g x y z))
;;;     '(if % (if x y z) (g x y z)))

;;; (equal? (expr-compare '((lambda (a) (f a)) 1) '((lambda (a) (g a)) 2))
;;;     '((lambda (a) ((if % f g) a)) (if % 1 2)))

;;; (equal? (expr-compare '((lambda (a) (f a)) 1) '((λ (a) (g a)) 2))
;;;     '((λ (a) ((if % f g) a)) (if % 1 2)))

;;; (equal? (expr-compare '((lambda (a) a) c) '((lambda (b) b) d))
;;;     '((lambda (a!b) a!b) (if % c d)))

;;; (equal? (expr-compare ''((λ (a) a) c) ''((lambda (b) b) d))
;;;     '(if % '((λ (a) a) c) '((lambda (b) b) d)))

;;; (equal? (expr-compare '(+ #f ((λ (a b) (f a b)) 1 2))
;;;               '(+ #t ((lambda (a c) (f a c)) 1 2)))
;;;     '(+
;;;      (not %)
;;;      ((λ (a b!c) (f a b!c)) 1 2)))

;;; (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
;;;               '((λ (a b) (f b a)) 1 2))
;;;     '((λ (a b) (f (if % a b) (if % b a))) 1 2))

;;; (equal? (expr-compare '((λ (a b) (f a b)) 1 2)
;;;               '((λ (a c) (f c a)) 1 2))
;;;     '((λ (a b!c) (f (if % a b!c) (if % b!c a)))
;;;      1 2))

;;; (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3) '((lambda (if) (+ if if (f λ))) 3))

;;; '(
;;;     (lambda 
;;;         (lambda!if) 
;;;         (+ lambda!if 
;;;             (if % if lambda!if) 
;;;             (f 
;;;                 (if % lambda!if λ)
;;;             )
;;;         )
;;;     )
;;;     3
;;; )

;;; (equal? (expr-compare '((lambda (lambda) (+ lambda if (f lambda))) 3)
;;;               '((lambda (if) (+ if if (f λ))) 3))
;;;     '((lambda (lambda!if) (+ lambda!if (if % if lambda!if) (f (if % lambda!if λ)))) 3))

;;; (equal? (expr-compare '((lambda (a) (eq? a ((λ (a b) ((λ (a b) (a b)) b a))
;;;                                     a (lambda (a) a))))
;;;                 (lambda (b a) (b a)))
;;;               '((λ (a) (eqv? a ((lambda (b a) ((lambda (a b) (a b)) b a))
;;;                                 a (λ (b) a))))
;;;                 (lambda (a b) (a b))))
;;;   '((λ (a)
;;;       ((if % eq? eqv?)
;;;        a
;;;        ((λ (a!b b!a) ((λ (a b) (a b)) (if % b!a a!b) (if % a!b b!a)))
;;;         a (λ (a!b) (if % a!b a)))))
;;;      (lambda (b!a a!b) (b!a a!b))))