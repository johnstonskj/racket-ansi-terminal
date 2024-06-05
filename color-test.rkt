#lang racket/base

(require racket/format
         racket/function
         racket/list
         racket/string
         ansi-terminal/graphics)

(define table-column-line (faint "│"))
(define table-column-start (string-append table-column-line " "))
(define table-column-mid (string-append " " table-column-line " "))
(define table-column-end (string-append " " table-column-line))

(define (list->table-row lst)
  (string-append table-column-start
                 (string-join lst
                              table-column-mid)
                 table-column-end))

(define (list->table-horizontal-line lst (kind 'double))
  (let ((left-edge (string (if (eq? kind 'double) #\╞ #\├)))
        (horizontal (if (eq? kind 'double) #\═ #\─))
        (crossing (string (if (eq? kind 'double) #\╪ #\┼)))
        (right-edge (string (if (eq? kind 'double) #\╡ #\┤))))
    (faint
   (string-append left-edge
                 (string-join (map
                               (λ (n) (make-string (+ n 2) horizontal))
                               lst)
                              crossing)
                 right-edge))))

(define (cell str width)
  (~a str #:width width))

(define (color-example color bright-color attribute)
  (string-append
   (attribute (format "~a ~a"
                      (text-color "exa" color)
                      (text-color "ple" bright-color)))
   "   "))

(module* main #f
  (displayln (bold "Color Table"))
  (newline)

  (displayln (list->table-row (map
                               (compose bold  (curryr cell 10))
                               '("Color" "Normal" "Bright" "Bold" "Faint" "Italic" "Underline"))))
  (displayln (list->table-horizontal-line (make-list 7 10)))

  (for-each
   (λ (color)
     (let ((bright-color (string->symbol (string-append "bright-" (symbol->string color)))))
       (displayln
        (format "~s" (list->table-row
         (list
          (cell (symbol->string color) 10)
          (text-color "example   " color)
          (text-color "example   " bright-color)
          (color-example color bright-color bold)
          (color-example color bright-color faint)
          (color-example color bright-color italic)
          (color-example color bright-color underline)))))))
   '(black red green yellow blue magenta cyan white)))
