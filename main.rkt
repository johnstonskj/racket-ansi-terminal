#lang racket/base

(require racket/base
         racket/contract
         racket/string)

(provide (contract-out (char-ascii/c flat-contract?)
                       (code-param/c flat-contract?)
                       (code-param-list/c contract?)
                       (code-param-separator char-ascii/c)
                       (code-param->string (-> code-param/c string?))
                       (code-param-list->string
                        (-> (or/c code-param/c code-param-list/c) string?))))

(define (char-ascii/c ch)
  (lambda (from-char) (if (char<=? #\u00 ch #\u7F)
                          #t
                          (raise-argument-error
                           'ch (format "#\u00 <= ~s <= #\u7F" ch) ch))))

(define code-param/c (and/c natural-number/c (>=/c #x00) (<=/c #x7F)))

(define code-param-list/c (listof code-param/c))

(define code-param-separator #\;)

(define (code-param->string v)
  (number->string v))


(define (code-param-list->string vs)
  (if (list? vs)
      (string-join
       (map code-param->string vs)
       (string code-param-separator))
      (code-param->string vs)))
