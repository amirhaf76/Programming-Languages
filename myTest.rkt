#lang racket

(require "desk.rkt")

;(define test-env (cons (var "fda") (cons (var "hello") (cons (var "nooo") null))))

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

