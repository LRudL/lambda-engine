#lang racket

(provide (all-defined-out))

(define-syntax-rule (if-let (name val) body)
  (let ((name val))
    (if name body #f)))

(define (butlast l)
  (take l (- (length l) 1)))

(define (uniques l)
  (length (set->list (apply set l))))

(define (not-fn fn)
  (λ args (not (apply fn args))))

