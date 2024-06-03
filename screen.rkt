#lang racket/base

(require racket/contract
         "./private/common.rkt")

(define screen-mode/c
  (or/c 'monochrome-40x25
        'color-40x25
        'monochrome-80x25
        'color-80x25
        'color/4-320x200
        'monochrome-320x200
        'monochrome-640x200
        'line-wrapping
        'color-320x200
        'monochrome-640x200
        'color/16-640x200
        'monochrome-640x350
        'color/16-640x350
        'monochrome-640x480
        'color/16-640x480
        'color/256-320x200))

(define screen-mode-number
  (make-hash
   '((monochrome-40x25 . 0)
        (color-40x25 . 1)
        (monochrome-80x25 . 2)
        (color-80x25 . 3)
        (color/4-320x200 . 4)
        (monochrome-320x200 . 5)
        (monochrome-640x200 . 6)
        (line-wrapping . 7)
        (color-320x200 . 13)
        (color/16-640x200 . 14)
        (monochrome-640x350 . 15)
        (color/16-640x350 . 16)
        (monochrome-640x480 . 17)
        (color/16-640x480 . 18)
        (color/256-320x200 . 19))))

(define (screen-mode-set m)
  (escape/csi (hash-ref screen-mode-number m) #\h #\= ))

(define (screen-mode-reset m)
  (escape/csi (hash-ref screen-mode-number m) #\l #\= ))
