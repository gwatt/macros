(library (sc-macro-transformer)
  (export sc-macro-transformer make-syntactic-closure)
  (import (rnrs) (syntax-utils)
    (only (chezscheme) printf))

  (define-syntax sc-macro-transformer
    (lambda (stx)
      (syntax-case stx ()
        [(k p) #'(sc-macro-transformer-impl #'k p)])))

  (define (sc-macro-transformer-impl definition-scope proc)
    (lambda (form)
      (rewrap (proc (syntax->tree form) definition-scope)
        (make-wrap definition-scope))))

  (define (make-syntactic-closure env syms tree)
    (letrec ([w (make-wrap env)]
             [r (lambda (x)
                  (if (memq x syms)
                      (w x)
                      x))])
      (syntax-map
        (lambda (x)
          (cond
            [(identifier? x) (r (syntax->datum x))]
            [(symbol? x) (r x)]
            [else x]))
        tree)))
)
