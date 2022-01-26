;; PL Project - Fall 2021
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

(define (extend-env s e env)
  (if (is-in-env env s)
      (error (format "variable +v is bound" s))
      (cons [cons (var s) (eval-under-env e env)] env)
      ))

(define (is-in-env env str)
  (cond [(null? env) #f]
        [(equal? (var-string (car (car env))) str) #t]
        [true (is-in-env [cdr env] str)]
        ))

(define (append x xs)
  (cond [(null? xs) (cons x null)]
        [(null? (cdr xs)) (cons (car xs) (cons x null))]
        [#t (cons (car xs) (append x (cdr xs)))]
        ))

(define (append-env env1 env2)
  (cond [(and (null? env1) (null? env2)) null]
        [(null? env1) env2]
        [(null? env2) env1]
        [(null? (cdr env1)) (append (car env1) env2)]
        [#t (append-env (cdr env1) (append (car env1) env2))]))

(define (extend-type-env s v env)
  (if (is-in-env env s)
      (error (format "variable +v has a type" s))
      (cons [cons (var s) (infer-under-env v env)] env)
      ))

(define (assign s e env)
  (cond [(null? env) null]
        [(equal? s (var-string (car(car env)))) (cons (cons (var s) e) (cdr env))]
        [#t (cons (car env)
                  (assign s e (cdr env)))]
        ))

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
(struct tlam (s1 s2 type e) #:transparent)

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

(define test-env (cons [cons (var "x") (num 4)]
                       (cons [cons (var "hello") (num "name")]
                             (cons [cons (var "isU") (bool #t)] null))))
;(define test-env (cons (var "fda") (cons (var "hello") (cons (var "nooo") null))))

;; lookup a variable in an environment
;; Complete this function
(define (envlookup env str)
  (cond [(null? env) (error "unbound variable during evaluation" str)]
        [(equal? (var-string (car (car env))) str) (cdr (car env))]
        [true (envlookup [cdr env] str)]
        )
  )


;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env) ; (eval-under-env () test-env)
  (cond [(var? e)  ; ** var
         (if [string? (var-string e)]
             [envlookup env (var-string e)]
             [error "NUMEX var applied to non-string"])
         ]
        [(num? e)  ; ** num
         (if [number? (num-int e)]
             e
             [error "NUMEX num applied to non-number"])
         ]
        [(closure? e)  ; ** closure
         e
         ]
        [(bool? e) ; ** bool
         (if [boolean? (bool-b e)]
             e
             [error "NUMEX bool applied to non-boolean"])
         ]
        [(munit? e)
         e]
        [(string? e)
         e]
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
           (if (and (num? v1)
                    (num? v2))
               (if (= 0 (num-int v2))
                   (error "NUMEX devide by zero")
                   (num (/ (num-int v1) (num-int v2)))
                   )
               (error "NUMEX division applied to non-number")
                 ))]
        [(neg? e) ; ** negation
         (let ([v1 (eval-under-env (neg-e1 e) env)])
           (cond [(bool? v1) (bool (not (bool-b v1)))]
                 [(num? v1) (num (* (num-int v1) -1))]
                 [true (error "NUMEX negation applied to non-boolean or non-number")]
                 ))]
        [(andalso? e) ; ** logical conjunction
         (let ([v1 (eval-under-env (andalso-e1 e) env)])
           [if (bool? v1)
               (if (bool-b v1)
                   (let ([v2 (eval-under-env (andalso-e2 e) env)])
                        [if (bool? v2)
                            v2
                            (error "NUMEX logical conjunction applied to non-boolean")])
                   (bool #f))
               (error "NUMEX logical conjunction applied to non-boolean")])]
        [(orelse? e) ; ** logical disjunction
         (let ([v1 (eval-under-env (orelse-e1 e) env)]) 
           [if (bool? v1)
               (if (bool-b v1)
                   (bool #t)
                   (let ([v2 (eval-under-env (orelse-e2 e) env)])
                        [if (bool? v2)
                            v2
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
        [(ifnzero? e) ; ** if n is not zero, do e2 else e3
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
               [if [> (num-int v1)  (num-int v2)]
                   [eval-under-env (ifleq-e3 e) env]
                   [eval-under-env (ifleq-e4 e) env]]
               [error "NUMEX ifleq applied to non-number"])
           )]
        [(with? e) ; ** with
         (if [string? (with-s e)]
             [eval-under-env (with-e2 e)
                             (extend-env (with-s e) (with-e1 e) env)]
             [error "NUMEX with applied to non-string"])]
        [(lam? e)
         (cond [(and (string? (lam-s1 e))
                     (string? (lam-s2 e))) (closure (cons [cons (var (lam-s1 e))
                                                               e]
                                                         [cons [cons (var (lam-s2 e))
                                                                     (munit)]
                                                               env]
                                                         )
                                                   e)]
               [(and (null? (lam-s1 e))
                     (string? (lam-s2 e))) (closure (cons [cons (var (lam-s2 e))
                                                                (munit)]
                                                          env)
                                                    e)]
               [#t (error "NUMEX lam applied to non-string")]
             )]
        [(apply? e) ; ** apply
         (let ([v1 (eval-under-env (apply-e1 e) env)])
           [if (closure? v1)
               (eval-under-env (lam-e (closure-f v1))
                               (append-env env
                                           (assign (lam-s2 (closure-f v1))
                                                   (eval-under-env (apply-e2 e) env)
                                                   (closure-env v1)
                                                   )
                                           )
                               )
               (error "Result of e1 is not closure")]
           )]
        [(apair? e) ; ** apair
         (let ([v1 (eval-under-env (apair-e1 e) env)]
               [v2 (eval-under-env (apair-e2 e) env)])
           (apair v1 v2))]
        [(1st? e)  ; ** first element of a apair
         (let ([v1 (eval-under-env (1st-e1 e) env)])
           [if (apair? v1)
               (apair-e1 v1)
               (error "e is not a apair:" v1)])]
        [(2nd? e)  ; ** second element of a apair
         (let ([v1 (eval-under-env (2nd-e1 e) env)])
           [if (apair? v1)
               (apair-e2 v1)
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
        [(key? e) ; ** key
         (if [string? (key-s e)]
             [key (key-s e) (eval-under-env (key-e e) env)]
             [error "NUMEX key applied to non-string"])]
        [(record? e) ; ** record
         (let ([v1 (eval-under-env (record-k e) env)]
               [v2 (eval-under-env (record-r e) env)])
           (cond [(and (key? v1) (munit? v2)) (record v1 v2)]
                 [(and (key? v1) (record? v2)) (record v1 v2)]
                 [error "NUMEX record applied to non-string"]))
         ]
        [(value? e) ; ** value
         (let ([v1 (eval-under-env (value-s e) env)]
               [v2 (eval-under-env (value-r e) env)])
           (if [and (string? v1) (record? v2)]
               [cond [(equal? v1 (key-s (record-k v2))) (key-e (record-k v2))]
                     [(munit? (record-r v2)) (munit)]
                     [#t (eval-under-env (value v1 (record-r v2)) env)]
                     ]
               [error "NUMEX value applied to non-string or non-record"]
               ))]
        ;; CHANGE add more cases here
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
;; Complete more cases for other kinds of NUMEX expressions.
;; We will test infer-under-env by calling its helper function, infer-exp.
(define (infer-under-env e env)
  (cond [(var? e) ; ** var
         (infer-under-env (envlookup env (var-string e)) env)
         ]
        [(num? e) ; ** num
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])
         ]
        [(bool? e) ; ** bool
         (cond [(boolean? (bool-b e)) "bool"]
               [#t (error "NUMEX TYPE ERROR: bool should be #t or #f")])
         ]
        [(munit? e) ; ** munit
         "null"]
        [(string? e) ; ** string
         "string"]
        [(plus? e) ; ** plus
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: addition applied to non-integer")))
         ]
        [(andalso? e) ; ** conjunction
         (let ([t1 (infer-under-env (andalso-e1 e) env)]
               [t2 (infer-under-env (andalso-e2 e) env)])
           (if (and (equal? "bool" t1)
                    (equal? "bool" t2))
               "bool"
               (error "NUMEX TYPE ERROR: conjunction applied to non-boolean")))
         ]
        [(neg? e) ; ** negation
         (infer-under-env (neg-e1 e) env)
         ]
        [(cnd? e) ; ** condition
         (let ([t1 (infer-under-env (cnd-e1 e) env)])
           (if (equal? "bool" t1)
               (let ([t2 (infer-under-env (cnd-e2 e) env)]
                     [t3 (infer-under-env (cnd-e3 e) env)])
                 (if (equal? t1 t2)
                     t1
                     (error "NUMEX TYPE ERROR: output of cnd aren't same type"))
                 )
               (error "NUMEX TYPE ERROR: cnd applied to non-boolean")))
         ]
        [(iseq? e) ; ** iseq
         (let ([t1 (infer-under-env (iseq-e1 e) env)]
               [t2 (infer-under-env (iseq-e2 e) env)])
           (if (equal? t1 t2)
                     "bool"
                     (error "NUMEX TYPE ERROR: output of cnd aren't same type"))
           )
         ]
        [(with? e) ; ** with
         (infer-under-env (with-e2 e) (extend-type-env (with-s e) (with-e1 e) env))
         ]
        [(apair? e) ; ** apair
         (let ([t1 (infer-under-env (apair-e1 e) env)]
               [t2 (infer-under-env (apair-e2 e) env)])
           (if [or (equal? t2 (collection t1))
                   (equal? t2 "null")]
               [collection t1]
               [error "NUMEX TYPE ERROR: t2 is not type of t1 or null"])
           )]
        [(1st? e) ; ** 1st
         (let ([t1 (infer-under-env (1st-e1 e) env)])
           (if [collection? t1]
               [collection-type t1]
               [error "NUMEX TYPE ERROR: 1st applied to non-collection"]))
         ]
        [(2nd? e) ; ** 2nd
         (let ([t1 (infer-under-env (2nd-e1 e) env)])
           (if [collection? t1]
               t1
               [error "NUMEX TYPE ERROR: 2st applied to non-collection"]))
         ]
        [(ismunit? e) ; ** ismunit
         (let ([t1 (infer-under-env (ismunit-e1 e) env)])
           (if [or (collection? t1)
                   (equal? "null")]
               "bool"
               [error "NUMEX TYPE ERROR: ismunit applied to non-collection or null"]))
         ]
        [(apply? e)
         (let ([t1 (infer-under-env (apply-e1 e) env)]
               [t2 (infer-under-env (apply-e2 e) env)])
           (if [function? t1]
               [if [equal? (function-input-type t1) t2]
                   [function-output-type t1]
                   [error "NUMEX TYPE ERROR: apply applied to non-" t2]
                   ]
               [error "NUMEX TYPE ERROR: apply applied to non-funtion type"]
               ))]    
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))
