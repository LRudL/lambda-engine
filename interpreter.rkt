#lang racket

(require "utils.rkt"
         "terms.rkt")

(provide free-variables
         reduce
         multireduce)

(define (free-variables M)
  ((expression-dealer
    (λ (y) (set y))
    (λ (var body) (set-subtract (free-variables body)
                                (set var)))
    (λ (fun arg) (set-union (free-variables fun)
                            (free-variables arg))))
   M))

(define (rename x term)
  ((expression-dealer
    (λ (y) (if (equal? y x)
               (next-var y)
               y))
    (λ (var body) (lambda-term (rename x var)
                               (rename x body)))
    (λ (fun arg) (app-term (rename x fun)
                           (rename x arg))))
   term))

(define (substituter M x)
  (expression-dealer
   (λ (y) (if (equal? y x) M y))
   (λ (var body)
     (cond ((set-member? (free-variables M) var)
            ((substituter M x) (rename var (lambda-term var body))))
           ((equal? x var) (lambda-term var body))
           (#t (lambda-term var
                            ((substituter M x) body)))))
   (λ (fun arg)
     (app-term ((substituter M x) fun)
               ((substituter M x) arg)))))

(define (signalling-reduce term)
  ((expression-dealer
    (λ (y)
      (cons #f y))
    (λ (var body)
      (let* ((body-res (signalling-reduce body))
             (did-reduce? (car body-res))
             (reduced-body (cdr body-res)))
        (cons did-reduce? (lambda-term var reduced-body))))
    (λ (fun arg)
      (if (lambda-term? fun)
          (cons #t
                ((substituter arg (lambda-var fun))
                 (lambda-body fun)))
          (let ((fun-res (signalling-reduce fun)))
            (if (car fun-res)
                (cons #t
                      (app-term (cdr fun-res) arg))
                (let ((arg-res (signalling-reduce arg)))
                  (cons (car arg-res)
                        (app-term fun (cdr arg-res)))))))))
   term))

(define (reduce term)
  (cdr (signalling-reduce term)))

(define (multireduce term (print? #f) (interpreter (λ (x) x)) (seen (set)))
  (if (set-member? seen term)
      'no-normal-form
      (let ((reduced (signalling-reduce term)))
        (when print? (println (interpreter (cdr reduced))))
        (if (car reduced)
            (multireduce (cdr reduced)
                         print? interpreter
                         (set-union seen (set term)))
            (cdr reduced)))))