;; PL Project - Fall 2021
;; NUMEX interpreter

#lang racket
(provide (all-defined-out)) ;; so we can put tests in a second file

;; definition of structures for NUMEX programs

;; CHANGE add the missing ones

(struct var  (string)   #:transparent)  ;; a variable, e.g., (var "foo")
(struct num  (int)      #:transparent)  ;; a constant number, e.g., (num 17)
(struct bool  (b)      #:transparent)  ;; a constant number, e.g., (num 17)
(struct plus  (e1 e2)   #:transparent)  ;; add two expressions
(struct munus (e1 e2)   #:transparent)  ;; subtract two expressions
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
(struct apply (e1 e2)       #:transparent) ;; function application

(struct with (s e1 e2) #:transparent)  ;; it's like let s,e1 in e2
(struct apair (e1 e2)  #:transparent)  ;; pair constructor
(struct 1st (e1)       #:transparent)  ;; the first part of a pair
(struct 2nd (e1)       #:transparent)  ;; the second part of a pair

(struct closure (env f)  #:transparent)  ;; envrioment and function

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
        [(equal? (car env) str) (car env)]
        [true (envlookup [cdr env] str)]
        )
  )

;; Complete more cases for other kinds of NUMEX expressions.
;; We will test eval-under-env by calling it directly even though
;; "in real life" it would be a helper function of eval-exp.
(define (eval-under-env e env)
  (cond [(var? e)  ; ** var
         (envlookup env (var-string e))]
        [(num? e)  ; ** num
         (e)]
        [(bool? e)
         (e)]
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
         (let [v1 (eval-under-env (andalso-e1 e) env)] ; [v2 (eval-under-env (andalso-e2 e) env)]
           [if (bool? v1)
               (if (bool-b v1)
                   (let [v2 (eval-under-env (andalso-e2 e) env)]
                        [if (bool? v2)
                            (bool-b v2)
                            (error "NUMEX logical conjunction applied to non-boolean")])
                   #f)
               (error "NUMEX logical conjunction applied to non-boolean")])]
        [(orelse? e) ; ** logical disjunction
         (let [v1 (eval-under-env (orelse-e1 e) env)] ; [v2 (eval-under-env (andalso-e2 e) env)]
           [if (bool? v1)
               (if (bool-b v1)
                   #t
                   (let [v2 (eval-under-env (orelse-e2 e) env)]
                        [if (bool? v2)
                            (bool-b v2)
                            (error "NUMEX logical disjunction applied to non-boolean")]))
               (error "NUMEX logical disjunction applied to non-boolean")])]
        [(cnd e) ; ** condition
         (let [v1 (eval-under-env (cnd-e1 e) env)]
           (if (bool? v1)
               (if (bool-b v1)
                   (eval-under-env (cnd-e2 e) env)
                   (eval-under-env (cnd-e3 e) env))
               (error "NUMEX condition applied to non-number")))]
        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (eval-exp e)
  (eval-under-env e null))

;; Problem 3
;; Complete more cases for other kinds of NUMEX expressions.
;; We will test infer-under-env by calling its helper function, infer-exp.
(define (infer-under-env e env)
  (cond [(var? e) 
         (infer-under-env (envlookup env (var-string e)) env)]

        [(plus? e) 
         (let ([t1 (infer-under-env (plus-e1 e) env)]
               [t2 (infer-under-env (plus-e2 e) env)])
           (if (and (equal? "int" t1)
                    (equal? "int" t2))
               "int"
               (error "NUMEX TYPE ERROR: addition applied to non-integer")))]

        [(num? e)
         (cond
           [(integer? (num-int e)) "int"]
           [#t (error "NUMEX TYPE ERROR: num should be a constant number")])]

        [(bool? e)
         (cond
           [(boolean? (bool-b e)) "bool"]
           [#t (error "NUMEX TYPE ERROR: bool should be #t or #f")])]

        ;; CHANGE add more cases here
        [(string? e) e]
        [#t (error (format "bad NUMEX expression: ~v" e))]))

;; Do NOT change
(define (infer-exp e)
  (infer-under-env e null))

;; Problem 4

(define (ifmunit e1 e2 e3) "CHANGE")

(define (with* bs e2) "CHANGE")

(define (ifneq e1 e2 e3 e4) "CHANGE")

;; Problem 5

(define numex-filter "CHANGE")

(define numex-all-gt
  (with "filter" numex-filter
        "CHANGE (notice filter is now in NUMEX scope)"))

;; Problem 6

(define type-error-but-evaluates-ok "CHANGE")
(define type-ok-but-evaluates-error "CHANGE")

;; Challenge Problem

(struct fun-challenge (nameopt formal body freevars) #:transparent) ;; a recursive(?) 1-argument function

;; We will test this function directly, so it must do
;; as described in the assignment
(define (compute-free-vars e) "CHANGE")

;; Do NOT share code with eval-under-env because that will make grading
;; more difficult, so copy most of your interpreter here and make minor changes
(define (eval-under-env-c e env) "CHANGE")

;; Do NOT change this
(define (eval-exp-c e)
  (eval-under-env-c (compute-free-vars e) null))
