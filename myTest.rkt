#lang racket

(require "desk.rkt")

(define test-env (cons (var "fda") (cons (var "hello") (cons (var "nooo") null))))

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
(eval-under-env (div (num 5) (num 0)) test-env)

(eval-under-env (neg (bool #t)) test-env)
(eval-under-env (neg (num 78)) test-env)
;(eval-under-env (neg (var "hello")) test-env)
