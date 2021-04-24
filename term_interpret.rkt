#lang racket

(require "utils.rkt"
         "terms.rkt"
         "definitions.rkt"
         "syntax.rkt")

(provide interpret)

(define (term-match pattern-term term (mp (hash)))
  ((term-type-dispatcher
    (λ (pattern-var)
      (if (hash-has-key? mp pattern-var)
          (if (equal? (hash-ref mp pattern-var)
                      term)
              mp
              #f)
          (hash-set mp pattern-var term)))
    (λ (pattern-λ)
      (if (lambda-term? term)
          (term-match (lambda-body pattern-λ)
                      (lambda-body term)
                      (hash-set mp
                                (lambda-var pattern-λ)
                                (lambda-var term)))
          #f))
    (λ (app)
      (if (app-term? term)
          (if-let (new-mp (term-match (app-fun app)
                                      (app-fun term)
                                      mp))
                  (term-match (app-arg app)
                              (app-arg term)
                              new-mp))
          #f)))
   pattern-term))

(define (term-placeholder? sym)
  (let ((s (symbol->string sym)))
    (equal? (string-upcase s) s)))

(define (uniques l)
  (length (set->list (apply set l))))

(define (not-fn fn)
  (λ args (not (apply fn args))))

(define (non-placeholder-vars-bijective? mp)
  (let* ((var-keys (filter (not-fn term-placeholder?)
                           (hash-keys mp)))
         (var-mappings (map ((curry hash-ref) mp) var-keys)))
    (= (length var-keys)
       (uniques var-mappings))))

(define (term-mapping pattern-term term)
  (if-let (naive-mapping (term-match pattern-term term))
          (if (non-placeholder-vars-bijective? naive-mapping)
              naive-mapping
              #f)))

(define (alpha-equivalent t1 t2)
  (if-let (mapping (term-mapping t1 t2))
          (foldl (λ (x y) (and x y)) #t (map var? (hash-values mapping)))))

(define (church-numeral-body-parse f x term)
  (if (equal? term x)
      0
      (if-let (mapping (term-mapping '(f M) term))
              (if (equal? f (hash-ref mapping 'f))
                  (if-let (inner (church-numeral-body-parse f x (hash-ref mapping 'M)))
                          (+ 1 inner))
                  #f))))

(define (church-numeral-interpret cn)
  (if-let (mapping (term-mapping '(λ f (λ x M)) cn))
          (let ((body (hash-ref mapping 'M)))
            (church-numeral-body-parse (hash-ref mapping 'f)
                                       (hash-ref mapping 'x)
                                       body))))

(define (list-interpret cn)
  (if-let (mapping (term-mapping '(λ f ((f i) B)) cn))
          (let* ((B (hash-ref mapping 'B))
                 (b-list (list-interpret B)))
            (if b-list
                (cons (interpret (hash-ref mapping 'i))
                      b-list)
                (if (alpha-equivalent B '(λ x (λ y y))) ; a list ends in false
                    (list (interpret (hash-ref mapping 'i)))
                    #f)))))

(define (pair-mapping pt)
  (if-let (mapping (term-mapping '(λ f ((f A) B)) pt))
          (cons (hash-ref mapping 'A)
                (hash-ref mapping 'B))))

(define (pair-interpret pt)
  (if-let (pair-map (pair-mapping pt))
          (list 'pair-of
                (interpret (car pair-map))
                (interpret (cdr pair-map)))))

(define (lambda-term-interp-maker triple-taker body-interp-fun)
  (λ (t)
    (if-let (pair (pair-mapping t))
            (and (term-mapping (car pair) (transform-syntax triple-taker))
                 (body-interp-fun (cdr pair))))))

(define var-interpret
  (lambda-term-interp-maker 'triple-1
                            (λ (cn)
                              (if-let (n (church-numeral-interpret cn))
                                      (string->symbol (string-append "VAR-" (number->string n)))))))

(define (lambda-pair-term-interp t)
  (if-let (pair (pair-mapping t))
          (list (interpret (car pair))
                (interpret (cdr pair)))))

(define lambda-interpret
  (lambda-term-interp-maker 'triple-2
                            (λ (lt)
                              (if-let (pair-interp (lambda-pair-term-interp lt))
                                      (cons 'LAMBDA pair-interp)))))
                            
(define app-interpret
  (lambda-term-interp-maker 'triple-3
                            (λ (at)
                              (if-let (pair-interp (lambda-pair-term-interp at))
                                      (cons 'APP pair-interp)))))

(define (lc-term-interpret t)
  (or (var-interpret t)
      (lambda-interpret t)
      (app-interpret t)))

(define (unzip lol (l1 '()) (l2 '()))
  (if (empty? lol)
      (cons (reverse l1) (reverse l2))
      (unzip (cdr lol)
             (cons (caar lol) l1)
             (cons (cadar lol) l2))))

(define (merge l1 l2)
  (if (empty? l1)
      '()
      (cons (list (car l1) (car l2))
            (merge (cdr l1) (cdr l2)))))

(define (mapcar f lol)
  (let* ((unzipped (unzip lol))
         (new-car (map f (car unzipped))))
    (merge new-car (cdr unzipped))))

(define (alpha-definition-transformer definition)
  (λ (term)
    (if (alpha-equivalent (car definition) term)
        (cadr definition)
        term)))

(define (inverse-definitions-transform term definitions)
  (let ((rev-defs (mapcar multiarg-transform (map reverse definitions))))
    ((apply compose (map alpha-definition-transformer rev-defs)) term)))

(define-syntax-rule (term-displayer interp-fun display-fun)
  (λ (t)
    (if-let (interp (interp-fun t))
            (display-fun interp))))

(define (term-interpret term)
  (let ((t (inverse-definitions-transform term definitions))
        (idfun (λ (interp) interp)))
    (or ((term-displayer church-numeral-interpret
                         (λ (interp)
                           (string->symbol (string-append
                                            "CN-" (number->string interp)))))
         t)
        ((term-displayer lc-term-interpret idfun) t)
        ((term-displayer pair-interpret idfun) t)
        #|((term-displayer list-interpret
                         (λ (interp) (cons 'LIST interp)))
         t)|#
        t)))

(define (interpret term)
  (let ((entire-term-interp (term-interpret term)))
    (if (equal? entire-term-interp term) ; entire term does not have an interpretation
        ((expression-dealer
          (λ (y) y)
          (λ (var body) (lambda-term var (interpret body)))
          (λ (fun arg) (app-term (interpret fun) (interpret arg)))
          (λ (unknown-term)
            (interpret (definitions-transform unknown-term definitions))))
         term)
        entire-term-interp)))