#lang racket
(struct var  (string) #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)    #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)  #:transparent)  ;; add two expressions
;(struct andalso (e1 e2) #:transparent)  ;; logical conjunction two expressions

(struct munit   ()      #:transparent)

(define (racketlist->numexlist xs)
  (cond [(null? xs) (munit)]
        [(null? (cdr xs)) (cons [car xs] (cons (munit) null))]
        [true (cons [car xs][racketlist->numexlist [cdr xs]])]
        ))

(define (numexlist->racketlist xs)
  (cond [(munit? xs) `()]
        [(munit? (car(cdr xs))) (cons [car xs] null)]
        [true (cons [car xs] [numexlist->racketlist [cdr xs]])]
        ))

(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (car env) str) (car env)]
        [true (envlookup [cdr env] str)]
        )
  )

(racketlist->numexlist `(43 78 454 hjhk 78)) ; test
(numexlist->racketlist (racketlist->numexlist `(43 78 454 hjhk 78))) ; test
(envlookup `("hello" "me" "y" "x") "y") ; test
(envlookup `("hello" "me" "y" "x") "hello") ; test
;(envlookup `("hello" "me" "y" "x") "nooo") ; test