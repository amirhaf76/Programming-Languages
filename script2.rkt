#lang racket
(require "desk.rkt")
(require rackunit)
(require rackunit/text-ui)

(eval-exp (div (num -5) (num -2)))

(define tests
  (test-suite
   "Project Tests"
   (check-equal? (eval-exp (div (num -5) (num -2))) (num 2) "test4")
   (check-exn exn:fail?
              (lambda () (eval-exp (plus (num 5.2) (num 0.0)))
              "test8"))
   (check-equal? (eval-exp (neg (mult (num 5) (plus
                                                (div (num -2) (num 3))
                                                (minus (num 1) (num -1)
                                                       )))))
                           (num -10) "test7")
      (check-equal? (numexlist->racketlist
                  (eval-exp (apply (apply numex-all-gt (num 5))
                                  (racketlist->numexlist 
                                   (list (num 10) (num 4) (num 5) (num 15))))))
                 (list (num 10) (num 15))
                 "test45")
      (test-equal? "test46"
    (list (num 3) (num 4) (bool #t))
    (numexlist->racketlist (apair (num 3) (apair (num 4) (apair (bool #t) (munit))))))
   ))
(define result (run-tests tests))
result

(numexlist->racketlist (apair (num 3) (apair (num 4) (apair (bool #t) (munit)))))
(numexlist->racketlist (racketlist->numexlist (list (num 3) (num 4))))