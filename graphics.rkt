#lang racket/base

(require racket/bool
         racket/contract
         racket/format
         racket/list
         racket/set
         racket/string
         ;; --------------------------------------
         "./main.rkt"
         "./private/common.rkt")

(provide (contract-out
          (rgb-color/c contract?)
          (color-name? flat-contract?)
          (attribute-name? flat-contract?)
          ;; --------------------------------------
          (with-attribute
           (->* (string? attribute-name?)
                ((or/c #f 'auto attribute-name?))
                string?))
          (bold (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (faint (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (italic (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (underline (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (blink-slow (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (blink-rapid (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (reversed (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (conceal (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (crossed-out (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-1 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-2 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-3 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-4 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-5 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-6 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-7 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-8 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-alternate-9 (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (font-fraktur (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (underline-double (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (proportional-spacing (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (framed (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (encircled (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (overline (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (ideogram-underline-or-right (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (ideogram-double-underline-or-right (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (ideogram-overline-or-left (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (ideogram-double-overline-or-left (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (ideogram-stress-marking (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (superscript (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          (subscript (->* (string?) ((or/c #f 'auto attribute-name?)) string?))
          ;; - - - - - - - - - - - - - - - - - - -
          (text-color (->* (string? color/c) ((or/c #f color/c)) string?))
          (black-text (->* (string?) ((or/c #f color/c)) string?))
          (red-text (->* (string?) ((or/c #f color/c)) string?))
          (green-text (->* (string?) ((or/c #f color/c)) string?))
          (yellow-text (->* (string?) ((or/c #f color/c)) string?))
          (blue-text (->* (string?) ((or/c #f color/c)) string?))
          (magenta-text (->* (string?) ((or/c #f color/c)) string?))
          (cyan-text (->* (string?) ((or/c #f color/c)) string?))
          (white-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-black-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-red-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-green-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-yellow-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-blue-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-magenta-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-cyan-text (->* (string?) ((or/c #f color/c)) string?))
          (bright-white-text (->* (string?) ((or/c #f color/c)) string?))
          (on-background (->* (string? color/c) ((or/c #f color/c)) string?))
          (on-black (->* (string?) ((or/c #f color/c)) string?))
          (on-red (->* (string?) ((or/c #f color/c)) string?))
          (on-green (->* (string?) ((or/c #f color/c)) string?))
          (on-yellow (->* (string?) ((or/c #f color/c)) string?))
          (on-blue (->* (string?) ((or/c #f color/c)) string?))
          (on-magenta (->* (string?) ((or/c #f color/c)) string?))
          (on-cyan (->* (string?) ((or/c #f color/c)) string?))
          (on-white (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-black (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-red (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-green (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-yellow (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-blue (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-magenta (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-cyan (->* (string?) ((or/c #f color/c)) string?))
          (on-bright-white (->* (string?) ((or/c #f color/c)) string?))
          ;; --------------------------------------
          (style? (-> any/c boolean?))
          (style->list (-> style? code-param-list/c))
          (style-append (-> style? style? style?))
          (style-push (-> style? code-param-list/c style?))
          (style-pop (-> style? style?))
          (style->string (-> style? string?))
          (make-style-param/c contract?)
          (make-style (-> make-style-param/c ... make-style-param/c style?))
          (reset style?)
          ;; --------------------------------------
          (style-string (->* (string? style?) (#:return-to style?) string?))
          (~a/styled (->* (any/c style?) (#:return-to style?) string?))
          (~s/styled (->* (any/c style?) (#:return-to style?) string?))))

;; -------------------------------------------------------------------------------------------------
;; Code Mappings
;; -------------------------------------------------------------------------------------------------

(define sgr-reset '(0))

(define sgr-attributes
  (make-hash
   '((bold . 1)
     (faint . 2)
     (italic . 3)
     (underline . 4)
     (blink-slow . 5)
     (blink-rapid . 6)
     (reversed . 7)
     (conceal . 8)
     (crossed-out . 9)
     (font-default . 10)
     (font-alternate-1 . 11)
     (font-alternate-2 . 12)
     (font-alternate-3 . 13)
     (font-alternate-4 . 14)
     (font-alternate-5 . 15)
     (font-alternate-6 . 16)
     (font-alternate-7 . 17)
     (font-alternate-8 . 18)
     (font-alternate-9 . 19)
     (font-fraktur . 20)
     (underline-double . 21)
     (normal-intensity . 22)
     (not-italic-or-fraktur . 23)
     (not-underline . 24)
     (not-blinking . 25)
     (proportional-spacing . 26)
     (not-reversed . 27)
     (reveal . 28)
     (not-crossed-out . 29)
     ;; skip color attributes
     (not-proportional-spacing . 50)
     (framed . 51)
     (encircled . 52)
     (overline . 53)
     (not-framed-or-encircled . 54)
     (not-overline . 55)
     ;; 58 - underline color
     ;; 59 - default underline color
     (ideogram-underline-or-right . 60)
     (ideogram-double-underline-or-right . 61)
     (ideogram-overline-or-left . 62)
     (ideogram-double-overline-or-left . 63)
     (ideogram-stress-marking . 64)
     (no-ideogram-attributes . 65)
     (superscript . 73)
     (subscript . 74)
     (not-superscript-or-subscript . 75))))

(define sgr-onoff-attributes
  (make-hash
   '((bold . normal-intensity)
     (faint . normal-intensity)
     (italic . not-italic-or-fraktur)
     (underline . not-underline)
     (blink-slow . not-blinking)
     (blink-rapid . not-blinking)
     (reversed . not-reversed)
     (conceal . reveal)
     (crossed-out . not-crossed-out)
     (font-alternate-1 . font-default)
     (font-alternate-2 . font-default)
     (font-alternate-3 . font-default)
     (font-alternate-4 . font-default)
     (font-alternate-5 . font-default)
     (font-alternate-6 . font-default)
     (font-alternate-7 . font-default)
     (font-alternate-8 . font-default)
     (font-alternate-9 . font-default)
     (font-fraktur . not-italic-or-fraktur)
     (underline-double . not-underline)
     (proportional-spacing . not-proportional-spacing)
     (framed . not-framed-or-encircled)
     (encircled . not-framed-or-encircled)
     (overline . not-overline)
     (ideogram-underline-or-right . no-ideogram-attributes)
     (ideogram-double-underline-or-right . no-ideogram-attributes)
     (ideogram-overline-or-left . no-ideogram-attributes)
     (ideogram-double-overline-or-left . no-ideogram-attributes)
     (ideogram-stress-marking . no-ideogram-attributes)
     (superscript . not-superscript-or-subscript)
     (subscript . not-superscript-or-subscript))))

(define sgr-attribute-names
  (list->set (hash-map sgr-attributes (λ (k _) k))))

(define sgr-foreground-color
  (make-hash
   '((black . 30)
     (red . 31)
     (green . 32)
     (yellow . 33)
     (blue . 34)
     (magenta . 35)
     (cyan . 36)
     (white . 37)
     (bright-black . 90)
     (bright-red . 91)
     (bright-green . 92)
     (bright-yellow . 93)
     (bright-blue . 94)
     (bright-magenta . 95)
     (bright-cyan . 96)
     (bright-white . 97))))

(define sgr-background-color
  (make-hash
   (hash-map
    sgr-foreground-color
    (λ (name value) (cons name (+ value 10))))))

(define sgr-color-names
  (list->set (hash-map sgr-foreground-color (λ (k _) k))))

;; -------------------------------------------------------------------------------------------------
;; Code-based procedures
;; -------------------------------------------------------------------------------------------------

(define (attribute->code attr)
  (if (hash-has-key? sgr-attributes attr)
      (hash-ref sgr-attributes attr)
      (raise-argument-error 'attr "attribute-name?" attr)))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define rgb-color/c (list/c code-param/c code-param/c code-param/c))

(define (color-name? val)
  (set-member? sgr-color-names val))

(define color/c (or/c 'default color-name? code-param/c rgb-color/c))

(define (attribute-name? val)
  (set-member? sgr-attribute-names val))

(define (color->code color foreground?)
  (cond
    ((eq? color 'default)
     (list (if foreground? 39 49)))
    ((color-name? color)
     (list (if foreground?
               (hash-ref sgr-foreground-color color)
               (hash-ref sgr-background-color color))))
    ((code-param/c color)
     (if foreground?
         `(38 5 ,color)
         `(48 5 ,color)))
    ((list? color)
     (append
      (if foreground?
          '(38 2)
          '(48 2))
      color))
    (else (raise-argument-error 'color "color/c" color))))

;; -------------------------------------------------------------------------------------------------
;; Code-based simple styling
;; -------------------------------------------------------------------------------------------------

(define (code-style-string str style-code return-code)
  (string-append
   (escape/sgr style-code)
   str
   (if return-code
       (escape/sgr return-code)
       "")))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (with-attribute str attribute (return-to 'auto))
  (let ((return-to (cond
                    ((false? return-to) #f)
                    ((eq? return-to 'auto)
                     (attribute->code (hash-ref sgr-onoff-attributes attribute)))
                    (else (attribute->code return-to)))))
    (code-style-string str
                       (attribute->code attribute)
                       return-to)))

(define (bold str (return-to 'auto))
  (with-attribute str 'bold return-to))

(define (faint str (return-to 'auto))
  (with-attribute str 'faint return-to))

(define (italic str (return-to 'auto))
  (with-attribute str 'italic return-to))

(define (underline str (return-to 'auto))
  (with-attribute str 'underline return-to))

(define (blink-slow str (return-to 'auto))
  (with-attribute str 'blink-slow return-to))

(define (blink-rapid str (return-to 'auto))
  (with-attribute str 'blink-rapid return-to))

(define (reversed str (return-to 'auto))
  (with-attribute str 'reverse return-to))

(define (conceal str (return-to 'auto))
  (with-attribute str 'conceal return-to))

(define (crossed-out str (return-to 'auto))
  (with-attribute str 'crossed-out return-to))

(define (font-alternate-1 str (return-to 'auto))
  (with-attribute str 'font-alternate-1 return-to))

(define (font-alternate-2 str (return-to 'auto))
  (with-attribute str 'font-alternate-2 return-to))

(define (font-alternate-3 str (return-to 'auto))
  (with-attribute str 'font-alternate-3 return-to))

(define (font-alternate-4 str (return-to 'auto))
  (with-attribute str 'font-alternate-4 return-to))

(define (font-alternate-5 str (return-to 'auto))
  (with-attribute str 'font-alternate-5 return-to))

(define (font-alternate-6 str (return-to 'auto))
  (with-attribute str 'font-alternate-6 return-to))

(define (font-alternate-7 str (return-to 'auto))
  (with-attribute str 'font-alternate-7 return-to))

(define (font-alternate-8 str (return-to 'auto))
  (with-attribute str 'font-alternate-8 return-to))

(define (font-alternate-9 str (return-to 'auto))
  (with-attribute str 'font-alternate-9 return-to))

(define (font-fraktur str (return-to 'auto))
  (with-attribute str 'font-fraktur return-to))

(define (underline-double str (return-to 'auto))
  (with-attribute str 'underline-double return-to))

(define (proportional-spacing str (return-to 'auto))
  (with-attribute str 'proportional-spacing return-to))

(define (framed str (return-to 'auto))
  (with-attribute str 'framed return-to))

(define (encircled str (return-to 'auto))
  (with-attribute str 'encircled return-to))

(define (overline str (return-to 'auto))
  (with-attribute str 'overline return-to))

(define (ideogram-underline-or-right str (return-to 'auto))
  (with-attribute str 'ideogram-underline-or-right return-to))

(define (ideogram-double-underline-or-right str (return-to 'auto))
  (with-attribute str 'ideogram-double-underline-or-right return-to))

(define (ideogram-overline-or-left str (return-to 'auto))
  (with-attribute str 'ideogram-overline-or-left return-to))

(define (ideogram-double-overline-or-left str (return-to 'auto))
  (with-attribute str 'ideogram-double-overline-or-left return-to))

(define (ideogram-stress-marking str (return-to 'auto))
  (with-attribute str 'ideogram-stress-marking return-to))

(define (superscript str (return-to 'auto))
  (with-attribute str 'superscript return-to))

(define (subscript str (return-to 'auto))
  (with-attribute str 'subscript return-to))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (text-color str color (return-to 'default))
  (code-style-string str
                     (color->code color #t)
                     (if return-to
                         (color->code return-to #t)
                         #f)))

(define (black-text str (return-to 'default))
  (text-color str 'black (if (eq? return-to 'none) #f return-to)))

(define (red-text str (return-to 'default))
  (text-color str 'red (if (eq? return-to 'none) #f return-to)))

(define (green-text str (return-to 'default))
  (text-color str 'green (if (eq? return-to 'none) #f return-to)))

(define (yellow-text str (return-to 'default))
  (text-color str 'yellow (if (eq? return-to 'none) #f return-to)))

(define (blue-text str (return-to 'default))
  (text-color str 'blue (if (eq? return-to 'none) #f return-to)))

(define (magenta-text str (return-to 'default))
  (text-color str 'magenta (if (eq? return-to 'none) #f return-to)))

(define (cyan-text str (return-to 'default))
  (text-color str 'cyan (if (eq? return-to 'none) #f return-to)))

(define (white-text str (return-to 'default))
  (text-color str 'white (if (eq? return-to 'none) #f return-to)))

(define (bright-black-text str (return-to 'default))
  (text-color str 'bright-black (if (eq? return-to 'none) #f return-to)))

(define (bright-red-text str (return-to 'default))
  (text-color str 'bright-red (if (eq? return-to 'none) #f return-to)))

(define (bright-green-text str (return-to 'default))
  (text-color str 'bright-green (if (eq? return-to 'none) #f return-to)))

(define (bright-yellow-text str (return-to 'default))
  (text-color str 'bright-yellow (if (eq? return-to 'none) #f return-to)))

(define (bright-blue-text str (return-to 'default))
  (text-color str 'bright-blue (if (eq? return-to 'none) #f return-to)))

(define (bright-magenta-text str (return-to 'default))
  (text-color str 'bright-magenta (if (eq? return-to 'none) #f return-to)))

(define (bright-cyan-text str (return-to 'default))
  (text-color str 'bright-cyan (if (eq? return-to 'none) #f return-to)))

(define (bright-white-text str (return-to 'default))
  (text-color str 'bright-white (if (eq? return-to 'none) #f return-to)))

(define (on-background  str color (return-to 'default))
  (code-style-string str
                     (color->code color #f)
                     (if return-to
                         (color->code return-to #f)
                         #f)))

(define (on-black str (return-to 'default))
  (on-background str 'black (if (eq? return-to 'none) #f return-to)))

(define (on-red str (return-to 'default))
  (on-background str 'red (if (eq? return-to 'none) #f return-to)))

(define (on-green str (return-to 'default))
  (on-background str 'green (if (eq? return-to 'none) #f return-to)))

(define (on-yellow str (return-to 'default))
  (on-background str 'yellow (if (eq? return-to 'none) #f return-to)))

(define (on-blue str (return-to 'default))
  (on-background str 'blue (if (eq? return-to 'none) #f return-to)))

(define (on-magenta str (return-to 'default))
  (on-background str 'magenta (if (eq? return-to 'none) #f return-to)))

(define (on-cyan str (return-to 'default))
  (on-background str 'cyan (if (eq? return-to 'none) #f return-to)))

(define (on-white str (return-to 'default))
  (on-background str 'white (if (eq? return-to 'none) #f return-to)))

(define (on-bright-black str (return-to 'default))
  (on-background str 'bright-black (if (eq? return-to 'none) #f return-to)))

(define (on-bright-red str (return-to 'default))
  (on-background str 'bright-red (if (eq? return-to 'none) #f return-to)))

(define (on-bright-green str (return-to 'default))
  (on-background str 'bright-green (if (eq? return-to 'none) #f return-to)))

(define (on-bright-yellow str (return-to 'default))
  (on-background str 'bright-yellow (if (eq? return-to 'none) #f return-to)))

(define (on-bright-blue str (return-to 'default))
  (on-background str 'bright-blue (if (eq? return-to 'none) #f return-to)))

(define (on-bright-magenta str (return-to 'default))
  (on-background str 'bright-magenta (if (eq? return-to 'none) #f return-to)))

(define (on-bright-cyan str (return-to 'default))
  (on-background str 'bright-cyan (if (eq? return-to 'none) #f return-to)))

(define (on-bright-white str (return-to 'default))
  (on-background str 'bright-white (if (eq? return-to 'none) #f return-to)))

;; -------------------------------------------------------------------------------------------------
;; Style struct
;; -------------------------------------------------------------------------------------------------

(struct style (lst)
  #:constructor-name mkstyle)

(define reset (mkstyle '(0)))

(define (style->list style)
  (style-lst style))

(define (style-append st1 st2)
  (mkstyle (append  (style->list st1) (style->list st2))))

(define (style-push style new)
  (mkstyle (cons new (style->list style))))

(define (style-pop style)
  (mkstyle (cdr (style->list style))))

(define (style->string style)
  (escape/sgr (flatten (style->list style))))

;; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define make-style-param/c
  (or/c
   (list/c (or/c 'text 'fg 'foreground) color-name?)
   (list/c (or/c 'on 'bg 'background) color-name?)
   (list/c 'not attribute-name?)
   color-name?
   attribute-name?))

(define (make-style/one st)
  (cond
    ((and (pair? st) (member (car st) '(text fg foreground)))
     (color->code (cadr st) #t))
    ((and (pair? st) (member (car st) '(on bg background)))
     (color->code (cadr st) #f))
    ((and (pair? st) (eq? (car st) 'not) (hash-has-key? sgr-onoff-attributes (cadr st)))
     (attribute->code (hash-ref sgr-onoff-attributes (cadr st))))
    ((symbol? st)
     (if (color-name? st)
         (color->code st #t)
         (attribute->code st)))
    (else (error "do not understand: ~s" st))))

(define (make-style st1 . rest)
  (mkstyle (map make-style/one (cons st1 rest))))

;; -------------------------------------------------------------------------------------------------
;; Style-based functions
;; -------------------------------------------------------------------------------------------------

(define (style-string str style #:return-to (return-style reset))
  (string-append (style->string style) str (style->string return-style)))

(define (~a/styled val style #:return-to (return-style reset))
  (style-string (~a val) style))

(define (~s/styled val style #:return-to (return-style reset))
  (style-string (~s val) style))
