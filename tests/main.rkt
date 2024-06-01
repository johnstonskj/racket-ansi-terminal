#lang racket/base

(require rackunit
         rackunit/text-ui
         "../main.rkt")

(provide ansi-term-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define csi (string #\033 #\[))

(define ansi-term-test-suite
  (test-suite
   "Module ansi-term"

   ;;(test-case
   ;; "function `color->code`"
   ;; (check-equal? (color->code 'red #t) '(31))
   ;; (check-equal? (color->code 'default #f) '(49))
   ;; (check-equal? (color->code 128 #t) '(38 5 128))
   ;; (check-equal? (color->code '(52 48 122) #f) '(48 2 52 48 122)))
   ;;
   ;;(test-case
   ;; "function `make-style`"
   ;; (check-equal? (style->list (make-style 'blue))
   ;;               '((34)))
   ;; (check-equal? (style->list (make-style 'red '(bg green) 'underline))
   ;;               '((31) (42) 4)))

   (test-case
    "simple styling"
    (check-equal?
     (string-append "Hello "
                    (red-text
                     (bold "cruel"))
                    " "
                    (faint "world")
                    ".")
     (string-append "Hello "
                    csi "31m"
                    csi "1m" "cruel" csi "22m" csi "39m"
                    " "
                    csi "2m" "world" csi "22m"
                    ".")))))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests ansi-term-test-suite)
