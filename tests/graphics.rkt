#lang racket/base

(require rackunit
         rackunit/text-ui
         "../graphics.rkt")

(provide ansi-term-test-suite)

;; -----------------------------------------------------------------------------------------------
;; Test Suite(s)
;; -----------------------------------------------------------------------------------------------

(define (csi s)
  (format "~a[~am" #\033 s))

(define ansi-term-test-suite
  (test-suite
   "Module ansi-term"

   (test-case
       "function `with-attribute`"
     (check-equal? (with-attribute "text" 'bold #f)
                   (string-append (csi "1") "text"))
     (check-equal? (with-attribute "text" 'bold 'auto)
                   (string-append (csi "1") "text" (csi "22")))
     (check-equal? (with-attribute "text" 'bold 'underline)
                   (string-append (csi "1") "text" (csi "4"))))

   (test-case
       "function `text-color`"
     (check-equal? (text-color "text" 'red #f)
                   (string-append (csi "31") "text"))
     (check-equal? (text-color "text" 'bright-red #f)
                   (string-append (csi "91") "text"))
     (check-equal? (text-color "text" 'red 'default)
                   (string-append (csi "31") "text" (csi "39")))
     (check-equal? (text-color "text" 'red 'green)
                   (string-append (csi "31") "text" (csi "32"))))

   (test-case
       "function `on-background`"
     (check-equal? (on-background "text" 'red #f)
                   (string-append (csi "41") "text"))
     (check-equal? (on-background "text" 'bright-red #f)
                   (string-append (csi "101") "text"))
     (check-equal? (on-background "text" 'red 'default)
                   (string-append (csi "41") "text" (csi "49")))
     (check-equal? (on-background "text" 'red 'green)
                   (string-append (csi "41") "text" (csi "42"))))

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
                    (csi "31")
                    (csi "1") "cruel" (csi "22") (csi "39")
                    " "
                    (csi "2") "world" (csi "22")
                    ".")))

   (test-case
       "function `make-style`"
     (check-equal? (style->string (make-style 'bold 'red '(on yellow)))
                   (string-append (csi "1;31;43"))))

   (test-case
       "function `style-string`"
     (let ((style-error (make-style 'blink-slow 'bold 'red '(on yellow))))
       (check-equal? (style-string "Oops!" style-error)
                     (string-append (csi "5;1;31;43") "Oops!" (csi "0")))))
   ))

;; -----------------------------------------------------------------------------------------------
;; Test Runner
;; -----------------------------------------------------------------------------------------------

(run-tests ansi-term-test-suite)
