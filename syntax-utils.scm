(library (syntax-utils)
  (export syntax-map rewrap syntax->tree syntax-car make-wrap)
  (import (rnrs))

  (define (syntax-map f stx)
    (let f^ ([t stx])
      (syntax-case t ()
        [(a . b) (cons (f^ #'a) (f^ #'b))]
        [#(v ...) (list->vector (map f^ #'(v ...)))]
        [x (f #'x)])))

  (define (rewrap tree default)
    (syntax-map
      (lambda (x)
        (if (symbol? x)
            (default x)
            x))
      tree))

  (define (syntax->tree stx)
    (syntax-map values stx))

  (define (syntax-car stx)
    (syntax-case stx ()
      [(a . _) #'a]))

  (define (make-wrap scope)
    (lambda (x)
      (datum->syntax scope x)))
)
