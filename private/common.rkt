#lang racket/base

(require racket/list
         racket/string
         "../control.rkt")

(provide (all-defined-out))

;; or/c code-param/c
;;      char-ascii/c
;;      string?
;;      code-param-list/c
(define (flatten-command-args args)
  (string-join
   (flatten
    (map
     (λ (arg)
       (cond
         ((exact-nonnegative-integer? arg) (number->string arg))
         ((char? arg) (string arg))
         ((string? arg) arg)
         ((list? arg) (flatten-command-args arg))
         (else (raise-argument-error 'args "command-arg?" arg))))
     (if (list? args) args (list args))))
   ";"))

;; char-ascii/c
;; code-param-list/c
;; (char-ascii/c)
(define (escape command-char args (prefix #f))
  (format "~a~a~a~a"
          c0/escape
          (or prefix "")
          (flatten-command-args args)
          command-char))

;; char-ascii/c
;; code-param-list/c
(define (escape/csi command-char args)
  (escape args command-char #\[))

;; code-param-list/c
(define (escape/sgr args)
  (escape/csi args #\m))
