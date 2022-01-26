#lang racket
(require "desk.rkt")

(define (ifmunit e1 e2 e3) (cnd [ismunit e1] e2 e3))
(eval-under-env (ifmunit (num 4) (plus (num 3) (num 5)) (mult (num 3) (num 4))) null)
(eval-under-env (ifmunit (munit) (plus (num 3) (num 5)) (mult (num 3) (num 4))) null)


(eval-under-env (with "v2" (num 3) (with "v1" (num 3) (plus (var "v1") (var "v2")))) test-env)
(define (with* bs e2) )