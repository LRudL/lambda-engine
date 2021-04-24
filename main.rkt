#lang racket

(require "interpreter.rkt"
         "syntax.rkt"
         "term_interpret.rkt")

(define (make-evaller (print-steps? #f) (interpreter (λ (x) x)))
  (λ (term)
    (interpreter (multireduce (transform-syntax term)
                              print-steps? interpreter))))

(define lce ; basic lambda calculus evaluator
  (make-evaller #f interpret))
; Example usage: (lce '(add three two)) -> CN-5 (i.e. church numeral 5)

(define lce-steps ; same, but also prints the steps of the evaluation
  (make-evaller #t interpret))

(define lce-raw ; does not interpret the result, shows the pure lambda calculus result
  (make-evaller))
; Example usage: (lce '(add three two)) -> (λ f (λ x (f (f (f (f (f x)))))))

(define lce-raw-steps ; same, but also prints every intermediate step
  (make-evaller #t))

#|
HOW TO MAKE TERMS:
- either use lambda-term and app-term in terms.rkt, with Racket symbols as variables, or
- write it directly:
  - variable -> Racket symbol
  - lambda term binding x to M -> (λ x M)
  - application of M to N -> (M N
(+ remember to quote the thing you pass into one of the evaluators, because
   otherwise Racket will try to evaluate it as Racket code; e.g. (lce '(λ x x)), not (lce (λ x x))
|#

#|
HOW TO USE THE LAMBDA–CALCULUS-IN–LAMBDA–CALCULUS:
See definitions.rkt for details. For example, to reduce ((λ x0 x0) x1) to x1, run:
  (lce '(reduce (make-app (make-lambda (make-var zero) (make-var zero)) (make-var one))))
This will become VAR-1, which is the representation of x1
(VAR-n is a label the interpreter assigns; the raw representation is
 (λ f ((f (λ t (t (λ f (λ g (λ h f)))))) N))), where N is the nth Church numeral)
Notes:
- variable renaming not supported; every variable has to be unique
- it is SLOW
|#