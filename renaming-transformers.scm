(library (renaming-transformers)
  (export ir-macro-transformer er-macro-transformer
    (rename (syntax->datum strip-syntax)))
  (import (rnrs))

  (define (syntax->tree stx)
    (syntax-case stx ()
      [(a . b) (cons (syntax->tree #'a) (syntax->tree #'b))]
      [#(a ...) (list->vector (map syntax->tree #'(a ...)))]
      [a #'a]))

  (define (syntax-car stx)
    (syntax-case stx ()
      [(a . d) #'a]))

  (define (rewrap tree default)
    (let wrapper ([t tree])
      (syntax-case t ()
        [(a . b) (cons (wrapper #'a) (wrapper #'b))]
        [#(a ...) (list->vector
                    (map (lambda (x)
                           (rewrap x default))
                      (list->vector #'(a ...))))]
        [a
         (if (symbol? #'a)
             (default #'a)
             #'a)])))

  (define (make-compare wrap)
    (letrec
      ([compare
        (lambda (x y)
          (cond
            [(and (symbol? x) (symbol? y)) (symbol=? x y)]
            [(symbol? x) (compare (wrap x) y)]
            [(symbol? y) (compare x (wrap y))]
            [(and (identifier? x) (identifier? y))
             (free-identifier=? x y)]
            [else (equal? x y)]))])
      compare))

  (define (make-wrap scope)
    (lambda (x)
      (datum->syntax scope x)))

  (define-syntax ir-macro-transformer
    (lambda (stx)
      (syntax-case stx ()
        [(k p) #'(ir-macro-transformer-impl #'k p)]
        [k (identifier? #'k)
         #'(lambda (p)
             (ir-macro-transformer-impl #'k p))])))

  (define (ir-macro-transformer-impl definition-scope proc)
    (lambda (stx)
      (let* ([usage-scope (syntax-car stx)]
             [inject (make-wrap usage-scope)]
             [rename (make-wrap definition-scope)]
             [compare (make-compare rename)])
        (rewrap
          (proc (syntax->tree stx) inject compare)
          rename))))

  (define-syntax er-macro-transformer
    (lambda (stx)
      (syntax-case stx ()
        [(k p) #'(er-macro-transformer-impl #'k p)]
        [k (identifier? #'k)
         #'(lambda (p)
             (er-macro-transformer-impl #'k p))])))

  (define (er-macro-transformer-impl definition-scope proc)
    (lambda (stx)
      (let* ([usage-scope (syntax-car stx)]
             [inject (make-wrap usage-scope)]
             [rename (make-wrap definition-scope)]
             [compare (make-compare inject)])
        (rewrap
          (proc (syntax->tree stx) rename compare)
          inject))))
)
