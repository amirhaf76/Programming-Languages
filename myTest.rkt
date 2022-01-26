#lang racket

(require "desk.rkt")

;(define test-env (cons (var "fda") (cons (var "hello") (cons (var "nooo") null))))
(define test-env (cons [cons (var "x") (num 4)]
                       (cons [cons (var "hello") (num "name")]
                             (cons [cons (var "isU") (bool #t)] null))))
(define test-env2 (cons [cons (var "x") (num 78)]
                       (cons [cons (var "hello") (num "name2")]
                             (cons [cons (var "isUr") (bool #t)] null))))
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

(define (assign s e env)
  (cond [(null? env) null]
        [(equal? s (var-string (car(car env)))) (cons (cons (var s) e) (cdr env))]
        [#t (cons (car env)
                  (assign s e (cdr env)))]
        ))

(append 1 `())
(append 3 `(4 5 5 dfkg "sfa" a))
(append-env `(4 5 5 dfkg "sfa" a) `(4r er g 56))
(append-env test-env test-env2)


;(extend-env "hello" (eval-under-env (plus (num 4) (num 5)) test-env) test-env)
(extend-env "yy" (eval-under-env (plus (num 4) (num 5)) test-env) test-env)

(eval-under-env (var "hello") test-env)
;(eval-under-env (var #t) test-env)

(eval-under-env (num 42) test-env)
(eval-under-env (num 3.1234) test-env)
;(eval-under-env (num "hello") test-env)

(eval-under-env (bool #f) test-env)
;(eval-under-env (bool 1) test-env)

(eval-under-env (plus (num 4) (num 432)) test-env)
;(eval-under-env (plus "fdfs" (num 432)) test-env)
(eval-under-env (minus (num 4) (num 432)) test-env)
(eval-under-env (minus (num 0.48) (num 432)) test-env)
(eval-under-env (mult (num 2) (num 24)) test-env)

(eval-under-env (div (num 2) (num 24)) test-env)
(eval-under-env (div (num 0) (num 24)) test-env)
;(eval-under-env (div (num 5) (num 0)) test-env)

(eval-under-env (neg (bool #t)) test-env)
(eval-under-env (neg (num 78)) test-env)
;(eval-under-env (neg (var "hello")) test-env)

(eval-under-env (andalso (bool #f) (num 4)) test-env)
;(eval-under-env (andalso (bool #t) (num 4)) test-env)
(eval-under-env (orelse (bool #f) (bool #t)) test-env)
(eval-under-env (orelse (bool #t) (bool #t)) test-env)


(eval-under-env (cnd (orelse (bool #t) (bool #f)) (num 4) (num 5)) test-env)
(eval-under-env (cnd (orelse (bool #f) (bool #f)) (num 4) (num 5)) test-env)

(eval-under-env (iseq (num 5) (bool #t)) test-env)
(eval-under-env (iseq (num 5) (num 5)) test-env)
(eval-under-env (iseq (bool #t) (bool #f)) test-env)
(eval-under-env (iseq (bool #t) (num 5)) test-env)

(eval-under-env (ifnzero (num 4) (var "hello") (bool #t)) test-env)
(eval-under-env (ifnzero (num 0) (var "hello") (bool #t)) test-env)

(eval-under-env (ifleq (num 5) (num 4) (var "hello") (bool #t)) test-env)
(eval-under-env (ifleq (num 4) (num 4) (var "hello") (bool #t)) test-env)
(eval-under-env (ifleq (num 5) (num 5.5) (var "hello") (bool #t)) test-env)

(eval-under-env (with "v1" (num 3) (plus (var "v1") (num 2))) test-env)
(eval-under-env (with "v2" (num 3) (with "v1" (num 3) (plus (var "v1") (var "v2")))) test-env)

(eval-under-env (apair (num 3) (var "hello")) test-env)

(eval-under-env (1st (apair (num 3) (var "hello"))) test-env)
(eval-under-env (2nd (apair (num 3) (var "hello"))) test-env)

(eval-under-env (ismunit (munit)) test-env)
(eval-under-env (ismunit (num 3)) test-env)

(eval-under-env (key "dafda" (plus (num 4) (num 5))) test-env)
(eval-under-env (record (key "dafda" (plus (num 4) (num 5))) (munit)) test-env)
(eval-under-env (record (key "dafda" (plus (num 4) (num 5))) (record (key "dafda" (num 6)) (munit))) test-env)

(eval-under-env (value "ttt" (record (key "dafda" (plus (num 4) (num 5))) (record (key "ttt" (num 6)) (munit)))) test-env)
(eval-under-env (value "dafda" (record (key "dafda" (plus (num 4) (num 5))) (record (key "ttt" (num 6)) (munit)))) test-env)
(eval-under-env (value "r" (record (key "dafda" (plus (num 4) (num 5))) (record (key "ttt" (num 6)) (munit)))) test-env)

(eval-under-env (lam "f" "x" (num (var "x"))) null)

