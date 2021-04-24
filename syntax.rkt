#lang racket

(require "utils.rkt"
         "terms.rkt"
         "definitions.rkt")

(provide multiarg-transform
         transform-syntax)

(define (λ-clipper t)
  (if (= (length t) lambda-length)
      t
      (lambda-term (lambda-var t)
                   (λ-clipper (append (list 'λ (caddr t))
                                      (cdddr t))))))

(define (app-clipper t)
  (if (= (length t) app-length)
      t
      (app-clipper
       (append (list (app-term (car t) (cadr t)))
               (cddr t)))))

(define (multiarg-transform term)
  ((term-type-dispatcher
    (λ (t) t)
    (λ (t) (if (= (length t) lambda-length)
               (lambda-term (lambda-var t)
                            (multiarg-transform (lambda-body t)))
               (let ((expanded-lambda (λ-clipper t)))
                 (lambda-term (lambda-var expanded-lambda)
                              (multiarg-transform (lambda-body expanded-lambda))))))
    (λ (t) (app-clipper
            (map multiarg-transform t))))
   term))

(define (transform-syntax t)
  ; Replace definitions with what they're tied to, and replace
  ; and multi-argument lambda terms or applications with the basic versions
  ; e.g. (λ x y z) -> (λ x (λ y z)), and (a b c) -> ((a b) c)
  (multiarg-transform
   (definitions-transform t definitions)))