#lang racket/base

(require "./control.rkt")

(define csi c1-str/single-graphic-character-introducer)

(define (cursor-up (n 1))
  (string-append csi (number->string n) "A"))

(define (cursor-down (n 1))
  (string-append csi (number->string n) "B"))

(define (cursor-forward (n 1))
  (string-append csi (number->string n) "C"))

(define (cursor-back (n 1))
  (string-append csi (number->string n) "D"))

(define (cursor-next-line (n 1))
  (string-append csi (number->string n) "E"))

(define (cursor-previous-line (n 1))
  (string-append csi (number->string n) "F"))

(define (cursor-set-column (n 1))
  (string-append csi (number->string n) "G"))

(define (cursor-move (n 1) (m 1))
  (string-append csi (number->string n) (number->string m) "H"))

(define (erase-in-display (part 'cursor-to-end))
  (string-append csi
                 (cond
                   ((eq? part 'cursor-to-end) "0")
                   ((eq? part 'beginning-to-cursor) "1")
                   ((eq? part 'entire-screen) "2")
                   ((eq? part 'entire-screen-and-scrollback) "3"))
                 "J"))

(define (erase-in-line (part 'cursor-to-end))
  (string-append csi
                 (cond
                   ((eq? part 'cursor-to-end) "0")
                   ((eq? part 'beginning-to-cursor) "1")
                   ((eq? part 'entire-line) "2"))
                 "K"))

(define (scroll-up (n 1))
  (string-append csi (number->string n) "S"))

(define (scroll-down (n 1))
  (string-append csi (number->string n) "T"))
