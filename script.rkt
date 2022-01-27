#lang racket
(require "desk.rkt")

(define (ifmunit e1 e2 e3) (cnd [ismunit e1] e2 e3))
(ifmunit (num 4) (plus (num 3) (num 5)) (mult (num 3) (num 4)))
(eval-under-env (ifmunit (num 4) (plus (num 3) (num 5)) (mult (num 3) (num 4))) null)
(eval-under-env (ifmunit (munit) (plus (num 3) (num 5)) (mult (num 3) (num 4))) null)

(with "v2" (num 3) (with "v1" (num 7) (plus (var "v1") (var "v2"))))
(eval-under-env (with "v2" (num 3) (with "v1" (num 7) (plus (var "v1") (var "v2")))) test-env)

(define (with* bs e2)
  (if [null? bs]
      e2
      [let ([p (car bs)])
        (with (car p) (cdr p) (with* (cdr bs) e2))
        ]))

(list (cons "x" (num 4)) (cons "y" (num 10)) (cons "res" (plus (var "x") (var "y"))))
(with*
 (list (cons "x" (num 4)) (cons "y" (num 10)) (cons "res" (plus (var "x") (var "y"))))
 (mult (num 10) (var "res")))
(eval-under-env
 (with*
 (list (cons "x" (num 4)) (cons "y" (num 10)) (cons "res" (plus (var "x") (var "y"))))
 (mult (num 10) (var "res"))) null)

(define (ifneq e1 e2 e3 e4)
  (cnd (neg (iseq e1 e2)) e3 e4))

(ifneq (num 5) (num 8) (num 7) (num 6))
(eval-under-env (ifneq (num 5) (num 5) (num 7) (num 6)) null)
(eval-under-env (ifneq (num 71) (num 5) (num 7) (num 6)) null)
;; ====================================================================

(define filter
  (lam "f"
       "arg"
       (cnd [ismunit (var "arg")]
            [munit]
            [cnd (ismunit (2nd (var "arg")))
                 (apair (apply (var "arg1") (1st (var "arg"))) (munit))
                 (apair (apply (var "arg1") (1st (var "arg"))) (apply (var "f") (2nd (var "arg"))))]
            ))
       )

(define numex-filter (lam null
                          "arg1"
                          (lam "f"
                               "arg"
                               (cnd [ismunit (var "arg")]
                                    [munit]
                                    [cnd (ismunit (2nd (var "arg")))
                                         (apair (apply (var "arg1") (1st (var "arg"))) (munit))
                                         (apair (apply (var "arg1") (1st (var "arg"))) (apply (var "f") (2nd (var "arg"))))]
                                    ))
                          ))



(define test-list (apair (num 3) (apair (num 8) (munit))))
(define test-list2 (apair (num 8) (munit)))

(eval-exp (apply (apply numex-filter (lam null "x" (neg (var "x")))) test-list2))