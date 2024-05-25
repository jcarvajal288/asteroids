(define-module (stdlib)
  #:pure
  #:use-module (scheme base)
  #:export (filter))

(define (filter fn lst)
  (cond ((null? lst) '())
        ((fn (car lst)) 
         (cons (car lst)
               (filter fn (cdr lst))))
        (else (filter fn (cdr lst)))))
