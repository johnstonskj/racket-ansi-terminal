#lang racket/base

(require racket/contract
         (only-in "./main.rkt" code-param/c)
         "./private/common.rkt")

(provide (contract-out
          (cursor-backward-tab (->* () (code-param/c) string?))))

;; CBT  Cursor Backward Tab
(define (cursor-backward-tab (n 1)) (escape/csi n #\Z))

;; CCH  Cancel Previous Character
(define (cancel-previous-char) (escape/csi '() #\T))

;; CHA  Cursor Horizontal Absolute
(define (cursor-horizontal-absolute (n 1)) (escape/csi n #\G))

;; CHT  Cursor Horizontal Tab
(define (cursor-horizontal-tab (n 1)) (escape/csi n #\I))

;; CNL  Cursor Next Line
(define (cursor-next-line (n 1)) (escape/csi n #\E))

;; CPL  Cursor Preceding Line
(define (cursor-previous-line (n 1)) (escape/csi n #\F))

;; CPR  Cursor Position Report
;; [see graphics.rkt] CSI  Control Sequence Intro
;; CTC  Cursor Tab Control

;; CUB  Cursor Backward
(define (cursor-back (n 1)) (escape/csi n #\D))

;; CUD  Cursor Down
(define (cursor-down (n 1)) (escape/csi n #\B))

;; CUF  Cursor Forward
(define (cursor-forward (n 1)) (escape/csi n #\C))

;; CUP  Cursor Position
(define (cursor-position (n 1) (m 1)) (escape/csi (list n m) #\H))
(define (cursor-position-home) (escape/csi '() #\H))

;; CUU  Cursor Up
(define (cursor-up (n 1)) (escape/csi n #\A))

;; CVT  Cursor Vertical Tab
(define (cursor-vertical-tab (n 1)) (escape/csi n #\Y))

;; DA   Device Attributes
(define (device-attributes (n 0)) (escape/csi n #\c))

;; DAQ  Define Area Qualification
(define (define-area-qualification (n 0)) (escape/csi n #\o))

;; DCH  Delete Character
;; DCS  Device Control String
;; DL   Delete Line
;; DMI  Disable Manual Input
;; DSR  Device Status Report
;; EA   Erase in Area
;; ECH  Erase Character
;; ED   Erase in Display
(define (erase-in-display (part 'cursor-to-end))
  (escape/csi (cond
                ((eq? part 'cursor-to-end) 0)
                ((eq? part 'beginning-to-cursor) 1)
                ((eq? part 'entire-screen) 2)
                ((eq? part 'entire-screen-and-scrollback) 3))
              #\J))

;; EF   Erase in Field
;; EL   Erase in Line
(define (erase-in-line (part 'cursor-to-end))
  (escape/csi
                 (cond
                   ((eq? part 'cursor-to-end) 0)
                   ((eq? part 'beginning-to-cursor) 1)
                   ((eq? part 'entire-line) 2))
                 #\K))

;; EMI  Enable Manual Input
;; EPA  End of Protected Area
;; ESA  End of Selected Area
;; FNT  Font Selection
;; GSM  Graphic Size Modify
;; GSS  Graphic Size Selection
;; HPA  Horz Position Absolute
;; HPR  Horz Position Relative
;; HTJ  Horz Tab w/Justification
;; HTS  Horizontal Tab Set
;; HVP  Horz & Vertical Position
;; ICH  Insert Character
;; IL   Insert Line
;; IND  Index
;; INT  Interrupt
;; JFY  Justify
;; MC   Media Copy
;; MW   Message Waiting
;; NEL  Next Line
;; NP   Next Page
;; OSC  Operating System Command
;; PLD  Partial Line Down
;; PLU  Partial Line Up
;; PM   Privacy Message
;; PP   Preceding Page
;; PU1  Private Use 1
;; PU2  Private Use 2
;; QUAD Typographic Quadding
;; REP  Repeat Char or Control
;; RI   Reverse Index
;; RIS  Reset to Initial State
;; RM   Reset Mode
;; SD   Scroll Down
(define (scroll-down (n 1)) (escape/csi n #\T))

;; SEM  Select Edit Extent Mode
;; SGR  Select Graphic Rendition
;; SL   Scroll Left
;; SM   Select Mode
;; SPA  Start of Protected Area
;; SPI  Spacing Increment
;; SR   Scroll Right
;; SS2  Single Shift 2 (G2 set
;; SS3  Single Shift 3 (G3 set)
;; SSA  Start of Selected Area
;; ST   String Terminator
;; STS  Set Transmit State
;; SU   Scroll Up
(define (scroll-up (n 1)) (escape/csi n #\S))

;; TBC  Tab Clear
;; TSS  Thin Space Specification
;; VPA  Vert Position Absolute
;; VPR  Vert Position Relative
;; VTS  Vertical Tabulation Set

