(import (rnrs)
  (renaming-transformers))

(define (println . args)
  (for-each display args)
  (newline))

(define-syntax t
  (syntax-rules ()
    [(_ r x)
     (guard (e (else (println "Test of " 'x " failed with error")))
       (cond
         [(equal? r x) (println "Test of " 'x " succeeded")]
         [else (println "Test of" 'x " failed, expected " r)]))]))

(define-syntax f
  (syntax-rules ()
    [(_ x)
     (guard (e (else (println "Failed as expected: " 'x)))
       x
       (println "Succeeded unexpectedly " 'x))]))

;;;
;;; Tests shamelessly lifted from chicken-scheme
;;; 
;;;

; this test doesn't work in r6rs, but neither does the syntax-rules equivalent

(define (fac n)
  (let-syntax ((m1
                (er-macro-transformer
                  (lambda (n r c)
                    (list (r 'sub1) (cadr n))))))
    (define (sub1 . _)
      (error #f "argh."))
    (if (zero? n)
        1
        (* n (fac (m1 n))))))


(t 3628800 (fac 10))

(let ((a 1))
  (letrec-syntax
    ((foo (er-macro-transformer
            (lambda (x r c)
              `(,(r 'bar) ,(r 'a) ,(cadr x)))))
     (bar (er-macro-transformer
            (lambda (x r c)
              (let ((c (cadr x))
                    (d (caddr x)))
                `(,(r 'cons) ,c
                  (,(r 'let) ((,c 3))
                   (,(r 'list) ,d ,c ',c))))))))
    (let ((a 2))
      (t '(1 2 3 a) (foo a)))))

(let ((a 1))
  (letrec-syntax
    ((foo (ir-macro-transformer
            (lambda (x i c)
              `(bar a ,(cadr x)))))
     (bar (ir-macro-transformer
            (lambda (x i c)
              (let ((c (cadr x))
                    (d (caddr x)))
                `(cons ,c
                   (let ((,c 3))
                     (list ,d ,c ',c))))))))
    (let ((a 2))
      (t '(1 2 3 a) (foo a)))))

(define bar 1)

(define-syntax baz
  (er-macro-transformer
    (lambda (e r c)
      `',(strip-syntax (r 'bar)))))

(t "bar" (symbol->string (baz bar)))
(t "bar" (symbol->string (baz void)))

(define-syntax loop
  (er-macro-transformer
    (lambda (x r c)
      (let ((body (cdr x)))
        `(,(r 'call/cc)
          (,(r 'lambda) (exit)
           (,(r 'let) ,(r 'f) () ,@body (,(r 'f)))))))))

(t 0 (let ((n 10))
       (loop
         (set! n (- n 1))
         (when (zero? n) (exit n)))))

(define-syntax while
  (er-macro-transformer
    (lambda (x r c)
      `(,(r 'loop)
        (,(r 'if) (,(r 'not) ,(cadr x)) (exit #f))
        ,@(cddr x)))))

#;(let ((n 10))
  (while (not (zero? n))
    (set! n (- n 1))))

(define-syntax define-macro
  (syntax-rules ()
    ((_ (name . llist) body ...)
     (define-syntax name
       (er-macro-transformer
         (lambda (x r c)
           (apply (lambda llist body ...) (strip-syntax (cdr x)))))))))

(define-macro (loop . body)
  (let ((loop (gensym)))
    `(call/cc
       (lambda (exit)
         (let ,loop () ,@body (,loop))))))

(t 11
  (let ((i 10))
    (loop (when (> i 10) (exit i))
      (set! i (+ i 1)))))

(define-syntax loop2
  (ir-macro-transformer
    (lambda (x i c)
      (let ((body (cdr x)))
        `(call/cc
           (lambda (,(i 'exit))
             (let f () ,@body (f))))))))

(t 0
  (let ((n 10))
    (loop2
      (set! n (- n 1))
      (when (zero? n) (exit n)))))

(define-syntax while2
  (ir-macro-transformer
    (lambda (x i c)
      ;;; Should this be loop2?
      `(loop
         (if (not ,(cadr x)) (,(i 'exit) #f))
         ,@(cddr x)))))

(t #f
  (let ((n 10))
    (while2 (not (zero? n))
      (set! n (- n 1)))))

(define-syntax nest-me
  (ir-macro-transformer
    (lambda (x i c)
      `(let ((,(i 'captured) 1))
         ,@(cdr x)))))

(t '(1 #(1 #(1)))
  (nest-me (list captured
             (let ((captured 2)
                   (let 'not-cpatured)
                   (list vector))
               (nest-me (list captured (nest-me (list captured))))))))

(define-syntax cond-test
  (ir-macro-transformer
    (lambda (x i c)
      (let lp ((exprs (cdr x)))
        (cond
          ((null? exprs) '(void))
          ((c (caar exprs) 'else)
           `(begin ,@(cdar exprs)))
          ((c (cadar exprs) '=>)
           `(let ((tmp ,(caar exprs)))
              (if tmp
                  (,(caddar exprs) tmp)
                  ,(lp (cdr exprs)))))
          ((c (cadar exprs) (i '==>))
           `(let ((tmp ,(caar exprs)))
              (if tmp
                  (,(caddar exprs) tmp)
                  ,(lp (cdr exprs)))))
          (else
           `(if ,(caar exprs)
                (begin ,@(cdar exprs))
                ,(lp (cdr exprs)))))))))

(t 'yep
  (cond-test
    (#f 'false)
    (else 'yep)))

(t 1
  (cond-test
    (#f 'false)
    (1 => (lambda (x) x))
    (else 'yep)))

(let ((=> #f))
  (t 'a-procedure
    (cond-test
      (#f 'false)
      (1 => 'a-procedure)
      (else 'yep))))

(let ((else #f))
  (t (if #f #f)
    (cond-test
      (#f 'false)
      (else 'nope))))

(t 1
  (cond-test
    (#f 'false)
    (1 ==> (lambda (x) x))
    (else 'yep)))

(let ((==> #f))
  (t 1
    (cond-test
      (#f 'false)
      (1 ==> (lambda (x) x))
      (else 'yep))))

(define-syntax run
  (ir-macro-transformer
    (lambda (e i c)
      `(quote ,(i 'void)))))

(t 'void (run))
