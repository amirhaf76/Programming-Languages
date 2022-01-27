;; PL Project - Fall 2021
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

(define (extend-env s v env)
  (if (null? (envlookup env s))
      (cons [var s v] env)
      (error (format "variable +v is bound" s))))

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string)   #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)      #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool  (b)       #:transparent)  ;; a boolean
(struct plus  (e1 e2)   #:transparent)  ;; add two expressions
(struct minus (e1 e2)   #:transparent)  ;; subtract two expressions
(struct mult (e1 e2)    #:transparent)  ;; multiplication two expressions
(struct div (e1 e2)     #:transparent)  ;; division two expressions
(struct neg (e1)        #:transparent)  ;; negation one expression
(struct andalso (e1 e2) #:transparent)  ;; logical conjunction two expressions
(struct orelse (e1 e2)  #:transparent)  ;; logical disjunction two expressions

(struct cnd (e1 e2 e3)      #:transparent)  ;; condition
(struct iseq (e1 e2)        #:transparent)  ;; comparison
(struct ifnzero (e1 e2 e3)  #:transparent)  ;; checking if n is zero
(struct ifleq (e1 e2 e3 e4) #:transparent)  ;; strictly greater
(struct lam (s1 s2 e)       #:transparent)  ;; a recursive(?) 1-argument function
(struct apply (e1 e2)       #:transparent)  ;; function application

(struct with (s e1 e2) #:transparent)  ;; it's like let s,e1 in e2
(struct apair (e1 e2)  #:transparent)  ;; pair constructor
(struct 1st (e1)       #:transparent)  ;; the first part of a pair
(struct 2nd (e1)       #:transparent)  ;; the second part of a pair


(struct letrec (s1 e1 s2 e2 s3 e3 s4 e4 e5)  #:transparent)  ;; recursive definition


(struct munit   ()      #:transparent) ;; unit value -- good for ending a list
(struct ismunit (e1)     #:transparent) ;; if e1 is unit then true else false


;; a closure is not in "source" programs; it is what functions evaluate to
(struct closure (env f) #:transparent) 


(struct key  (s e) #:transparent) ;; key holds corresponding value of s which is e
(struct record (k r) #:transparent) ;; record holds several keys
(struct value (s r) #:transparent) ;; value returns corresponding value of s in r


;; Type structures
;; Primitive types are: "int", "bool" and "null"
(struct collection (type) #:transparent) ;; collection of a certain type, e.g., (collection "int")
(struct function (input-type output-type) #:transparent) ;; e.g. (function ("int" int")) means fn f "int" -> "int"

;; Problem 1

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

;; Problem 2

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (var-string (car env)) str) (car env)]
        [true (envlookup [cdr env] str)]
        )
  )

(define test-env (cons (var "fda") (cons (var "hello") (cons (var "nooo") null))))

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env) ;(eval-under-env () test-env)
  (cond [(var? e)  ; ** var
         (if [string? (var-string e)]
             [envlookup env (var-string e)]
             [error "NUMEX var applied to non-string"])
         ]
        [(num? e)  ; ** num
         (if [number? (num-int e)]
             e
             [error "NUMEX var applied to non-number"])
         ]
        [(bool? e) ; ** bool
         (if [boolean? (bool-b e)]
             e
             [error "NUMEX var applied to non-boolean"])
         ]
        [(plus? e) ; ** plus
         (let ([v1 (eval-under-env (plus-e1 e) env)]
               [v2 (eval-under-env (plus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (+ (num-int v1) 
                       (num-int v2)))
               (error "NUMEX addition applied to non-number")))]
        [(minus? e) ; ** minus
         (let ([v1 (eval-under-env (minus-e1 e) env)]
               [v2 (eval-under-env (minus-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (- (num-int v1) 
                       (num-int v2)))
               (error "NUMEX subtraction applied to non-number")))]
        [(mult? e) ; ** mult
         (let ([v1 (eval-under-env (mult-e1 e) env)]
               [v2 (eval-under-env (mult-e2 e) env)])
           (if (and (num? v1)
                    (num? v2))
               (num (* (num-int v1) 
                       (num-int v2)))
               (error "NUMEX multiplication applied to non-number")))]
        [(div? e) ; ** div
         (let ([v1 (eval-under-env (div-e1 e) env)]
               [v2 (eval-under-env (div-e2 e) env)])
           (cond [(num? v2) (if (= 0 v2)
                                     (error "NUMEX devide by zero")
                                     [(and (num? v1) (num? v2))
                                      (num (/ (num-int v1) (num-int v2)))]
                                     )]
                 [true (error "NUMEX division applied to non-number")]
                 ))]
        [(neg? e) ; ** negation
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (cond [(bool? v1) (bool (not (bool-b v1)))]
                 [(num? v1) (num (* (num-int v1) -1))]
                 [true (error "NUMEX negation applied to non-boolean")]
                 ))]
        [(andalso? e) ; ** logical conjunction
         (let ([v1 (eval-under-env (andalso-e1 e) env)]) ; [v2 (eval-under-env (andalso-e2 e) env)]
           [if (bool? v1)
               (if (bool-b v1)
                   (let ([v2 (eval-under-env (andalso-e2 e) env)])
                        [if (bool? v2)
                            (bool-b v2)
                            (error "NUMEX logical conjunction applied to non-boolean")])
                   (bool #f))
               (error "NUMEX logical conjunction applied to non-boolean")])]
        [(orelse? e) ; ** logical disjunction
         (let ([v1 (eval-under-env (orelse-e1 e) env)]) ; [v2 (eval-under-env (andalso-e2 e) env)]
           [if (bool? v1)
               (if (bool-b v1)
                   (bool #t)
                   (let ([v2 (eval-under-env (orelse-e2 e) env)])
                        [if (bool? v2)
                            (bool-b v2)
                            (error "NUMEX logical disjunction applied to non-boolean")]))
               (error "NUMEX logical disjunction applied to non-boolean")])]
        [(cnd? e) ; ** condition
         (let ([v1 (eval-under-env (cnd-e1 e) env)])
           (if (bool? v1)
               (if (bool-b v1)
                   (eval-under-env (cnd-e2 e) env)
                   (eval-under-env (cnd-e3 e) env))
               (error "NUMEX condition applied to non-boolean")))]
        [(iseq? e) ; ** is equal?
         (let ([v1 (eval-under-env (iseq-e1 e) env)]
               [v2 (eval-under-env (iseq-e2 e) env)])
           (cond [(and (bool? v1) (bool? v2)) (bool (equal? (bool-b v1)  (bool-b v2)))]
                 [(and (bool? v1) (num? v2))  (bool (equal? (bool-b v1)  (num-int v2)))]
                 [(and (num? v1) (bool? v2))  (bool (equal? (num-int v1) (bool-b v2)))]
                 [(and (num? v1) (num? v2))   (bool (equal? (num-int v1) (num-int v2)))]
                 [true (error "NUMEX iseq applied to non-boolean or non-number")])
           )]
        [(ifnzero? e) ; ** if n is zero, do e2 else e3
         (let ([v1 (eval-under-env (ifnzero-e1 e) env)])
           (if [num? v1]
               [if (= (num-int v1) 0)
                   (eval-under-env (ifnzero-e3 e) env)
                   (eval-under-env (ifnzero-e2 e) env)]
               [true (error "NUMEX ifnzero applied to non-number")])
           )]
        [(ifleq? e) ; ** if stricly
         (let ([v1 (eval-under-env (ifleq-e1 e) env)]
               [v2 (eval-under-env (ifleq-e2 e) env)])
           (if [and (num? v1) (num? v2)]
               [if [> v1 v2]
                   [eval-under-env (ifleq-e4 e) env]
                   [eval-under-env (ifleq-e3 e) env]]
               [error "NUMEX ifleq applied to non-number"])
           )]
        [(with? e)
         (if [string? (with-s e)]
             [eval-under-env (with-e2 e)
                             (cons (var (with-s e)
                                        (eval-under-env (with-e1 e) env))
                                   env)]
             [error "NUMEX with appliedt to non-string"])]
        [(apply? e)
         (let ([v1 (eval-under-env (apply-e1 e) env)])
           [if (closure? v1)
               (#t)
               (error "Result of e1 is not closure")])] ; it needs to complete later
        [(apair? e) ; ** apair
         (let ([v1 (eval-under-env (apair-e1) env)]
               [v2 (eval-under-env (apair-e2) env)])
           (apair v1 v2))]
        [(1st? e)  ; ** first element of a apair
         (let ([v1 (eval-under-env (1st-e1 e) env)])
           [if (apair? v1)
               (apair-e1)
               (error "e is not a apair:" v1)])]
        [(2nd? e)  ; ** second element of a apair
         (let ([v1 (eval-under-env (2nd-e1 e) env)])
           [if (apair? v1)
               (apair-e2)
               (error "e is not a apair:" v1)])]
        [(ismunit? e)  ; ** is e a munit
         (let ([v1 (eval-under-env (ismunit-e1 e) env)])
           [bool (munit? v1)]
           )]
        [(letrec? e)
         (cond [(not(string? (letrec-s1 e))) (error "s1 is not a string:" (letrec-s1 e))]
               [(not(string? (letrec-s2 e))) (error "s2 is not a string:" (letrec-s2 e))]
               [(not(string? (letrec-s3 e))) (error "s3 is not a string:" (letrec-s3 e))]
               [(not(string? (letrec-s4 e))) (error "s4 is not a string:" (letrec-s4 e))]
               [#t (num 0)])] ; bug !!!!!!!!!!!!!!!!!!!!!!!!!!!!1111
        [(key? e)
         (if [string? (key-s e)]
             [key (key-s e) (eval-under-env (key-e e) env)]
             [error "NUMEX key applied to non-string"])]
        [(record? e)
         (let ([v1 (eval-under-env (record-k e) env)]
               [v2 (eval-under-env (record-r e) env)])
           (cond [(and (key? v1) (munit? v2)) (record v1 v2)]
                 [(and (key? v1) (record? v2)) (record v1 v2)]
                 [error "NUMEX record applied to non-string"]))
         ]
        [(value? e)
         (let ([v1 (eval-under-env (value-s e) env)]
               [v2 (eval-under-env (value-r e) env)])
           (if [and (string? v1) (record? v2)]
               [cond [(equal? v1 (key-s (record-k v2))) (key-e (record-k v2))]
                     [(munit? (record-r v2)) (munit)]
                     [#t (value (key-s (record-k v2)) (record-r v2))]
                     ]
               [error "NUMEX value applied to non-string or non-record"]
               ))]
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

