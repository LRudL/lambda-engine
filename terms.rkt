#lang racket

(require "utils.rkt")
(provide (all-defined-out))

(define (var? t)
  (and (symbol? t)
       (not (equal? t 'λ))
       (let ((s (symbol->string t)))
         (or (= (string-length s) 1)
             (string->number (substring (symbol->string t) 1))))))

(define (next-var var)
  ; takes a variable like "x4" or "x" and returns "x5" or "x1"
  ; (used when renaming)
  (string->symbol
   (let ((s (symbol->string var)))
     (if (= (string-length s) 1)
         (string-append s "1")
         (string-append (substring s 0 1)
                        (number->string
                         (+ 1 (string->number (substring s 1)))))))))

(define (lambda-term x M) (list 'λ x M))

(define lambda-length 3)

(define (lambda-term? M)
  (and (list? M)
       (equal? (car M) 'λ)))

(define lambda-var cadr)
(define lambda-body caddr)

(define (app-term M N)
  (list M N))

(define app-length 2)

(define app-fun car)
(define app-arg cadr)

(define (app-term? M)
  (and (list? M)
       (not (lambda-term? M))))

(define term-error-fn
  (λ (t) (error "Invalid term type in expression dealer: " t)))

(define (term-type-dispatcher var-func lambda-func app-func (other-func term-error-fn))
  (λ (term)
    (cond ((var? term) (var-func term))
          ((lambda-term? term) (lambda-func term))
          ((app-term? term) (app-func term))
          (#t (other-func term)))))

(define (expression-dealer var-func lambda-func app-func
                           (other-func term-error-fn))
  (term-type-dispatcher
   var-func
   (λ (λ-term) (lambda-func (lambda-var λ-term) (lambda-body λ-term)))
   (λ (app-tm) (app-func (app-fun app-tm) (app-arg app-tm)))
   other-func))