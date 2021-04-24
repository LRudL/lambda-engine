#lang racket

(require "utils.rkt"
         "terms.rkt")

(provide definitions-transform
         definitions)

(define (definition-transformer definition)
  (λ (term)
    ((term-type-dispatcher
      (λ (t) t)
      (λ (t) (append (butlast t) (list ((definition-transformer definition) (last t)))))
      (λ (t) (map (definition-transformer definition) t))
      (λ (t)
        (if (equal? t (car definition))
            (cadr definition)
            t)))
     term)))

(define (definitions-transform term definitions)
  ((apply compose (map definition-transformer definitions)) term))

(define (unrolled-definitions-creator definitions (acc '()))
  ; Allows 
  (if (empty? definitions)
      acc
      (unrolled-definitions-creator
       (cdr definitions)
       (cons (definitions-transform (car definitions) acc) acc))))

(define definitions
  (unrolled-definitions-creator
   '((succ (λ n (λ f x (f (n f x)))))
     (zero (λ f x x))
     (one (succ zero))
     (two (succ one))
     (three (succ two))
     (four (succ three))
     (pair (λ x y (λ f (f x y))))
     (first (λ x y x))
     (second (λ x y y))
     (fst (λ p (p first)))
     (snd (λ p (p second)))
     (true first)
     (false second)
     (eq0 (λ x (x (λ y false) true)))
     (if (λ p c a (p c a)))
     (add (λ m n (λ f x ((m f) (n f x)))))
     (mult (λ m n (λ f x ((m (n f)) x))))
     (exp (λ m n (n m)))
     (pred (λ n
             (fst ((n (λ p
                        (pair (snd p)
                              (succ (snd p)))))
                   (pair zero zero)))))
     (not (λ p (if p false true)))
     (and (λ a b (a b false)))
     (or (λ a b (a true b)))
     (YC (λ f
           ((λ x (f (x x)))
            (λ y (f (y y))))))
     (eqn (λ f m n
            (if (eq0 m)
                (eq0 n)
                (if (eq0 n)
                    (eq0 m)
                    (f (pred m) (pred n))))))
     (add-to (λ f x (if (eq0 x)
                        x
                        (add x (f (pred x))))))
     ; [LAMBDA CALCULUS IN LAMBDA CALCULUS - DEFINITIONS BEGIN]
     ; (note: this does not detect name collisions - every variable has to be fresh,
     ;  and variables are made by calling make-var with a Church numeral)
     ; Two basic recursive functions that let us take different actions based on type of expression
     (caserec (λ t ; take the function triple before YCing it so present in recursive calls
                (λ f x
                  (((fst x) t) f x))))
     (caserec2 (λ t
                 (λ f m x n ; only change to caserec is taking more args here ...
                   (((fst n) t) f m x n)))) ; ... and passing them here
     ; Triples and functions to take the 1st/2nd/3rd are the core of the type-tagging system:
     (triple (λ f g h s (s f g h)))
     (triple-1 (λ t (t (λ f g h f))))
     (triple-2 (λ t (t (λ f g h g))))
     (triple-3 (λ t (t (λ f g h h))))
     ; A var is a pair of triple-1 (type id) and a number i:
     (make-var (λ i (pair triple-1 i)))
     (var-eq (λ i j ((YC eqn) (snd i) (snd j))))
     ; A lambda is a pair of triple-2 (type id) and a pair of the variable and body:
     (make-lambda (λ v b (pair triple-2 (pair v b))))
     (lambda-var (λ l (fst (snd l)))) ; return lambda var
     (lambda-body (λ l (snd (snd l)))) ; return lambda body
     ; An application has the same structure as a lambda, but triple-3 instead of triple-2:
     (make-app (λ f a (pair triple-3 (pair f a))))
     (app-fun (λ t (fst (snd t))))
     (app-arg (λ t (snd (snd t))))
     ; Need to test for lambda terms to reduce:
     ; (uses the fact that (fst lambda) is a function that returns 2nd of 3 args)
     (lambda? (λ l (eq0 ((fst l) (triple one zero one)))))
     ; Define the substitution recursive functions:
     (substitute-var (λ f m x v
                       (if (var-eq x v)
                           m
                           v)))
     (substitute-lambda (λ f m x l
                          (make-lambda (lambda-var l)
                                       (f m x (lambda-body l)))))
     (substitute-app (λ f m x t
                       (make-app (f m x (app-fun t))
                                 (f m x (app-arg t)))))
     (substitute ; <-- the main driver, note that a triple of the case-specific functions is passed in (before Y applied)
      (λ m x t
        ((YC (caserec2 (triple substitute-var substitute-lambda substitute-app)))
         m x t)))
     ; And likewise for the reduction functions:
     (reduce-var (λ f v v))
     (reduce-lambda (λ f l
                      (make-lambda (lambda-var l)
                                   (f (lambda-body l)))))
     ; Also to get normal-order reduction + full reduction, need to be able to check if a term reduces.
     ; So, before getting to defining reduce and reduce-app, need to define:
     (reduces?-var (λ f v false))
     (reduces?-lambda (λ f l
                        (f (lambda-body l))))
     (reduces?-app (λ f x
                     (or (f (app-fun x))
                         (f (app-arg x)))))
     (reduces? (λ x
                 ((YC (caserec (triple reduces?-var reduces?-lambda reduces?-app)))
                  x)))
     (reduce-app (λ f x
                   (if (lambda? (app-fun x))
                       (substitute (app-arg x)
                                   (lambda-var (app-fun x))
                                   (lambda-body (app-fun x)))
                       (if (reduces? (app-fun x))
                           (make-app (f (app-fun x))
                                     (app-arg x))
                           (make-app (app-fun x)
                                     (f (app-arg x)))))))
     ; note: on the last line above, we don't reduce the arg until we have a lambda
     ; -> arg will not end up in reduced form if its function does not reduce to a lambda
     (reduce ; <-- the main reduction function, call this repeatedly to reduce an expression
      (λ x
        ((YC (caserec (triple reduce-var reduce-lambda reduce-app))) 
         x)))
     (n-reduce ; reduces an expression n times (where n is a Church numeral)
      (λ n x ((n reduce) x))))))