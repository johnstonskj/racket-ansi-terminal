#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          (for-label (except-in racket/base
                                reverse)
                     ansi-terminal))

@;{============================================================================}

@(define example-eval (make-base-eval '(require ansi-terminal)))

@;{============================================================================}

@title[#:version  "1.0"]{ANSI Terminal Escape Codes.}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]
@defmodule[ansi-terminal]

Interface to ANSI escape sequences for terminals. CSI codes, ...

@section[]{Predicates}

@defthing[exact-byte? flat-contract?]{
An unsigned byte with range @tt{0..255}.
}

@defthing[style-code-value/c flat-contract?]{
An individual value within a style code.
}

@defthing[style-code/c flat-contract?]{
A list of code values comprising a complete CSI code.
}

@defthing[color-name? flat-contract?]{
The set of 8 major colors specified by name.
}

@defthing[rgb-color/c contract?]{
An RGB color specification represented as a three-member list of bytes.
}

@defthing[attribute-name? flat-contract?]{
The set of text attribute names; note that this includes both enable and disable names.
}

@section[]{Simple Styling}

The following functions are intended to ...

@subsection{Text Attributes}

@defproc[(with-attribute
          [str string?]
          [attribute attribute-name?]
          [return-to (or/c 'auto 'none attribute-name?) 'auto])
         string?]{
TBD
}

@deftogether[(
  @defproc[(bold [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(faint [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(italic [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(underline [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(blink-slow [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(blink-rapid [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(reversed [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(conceal [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(crossed-out [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-1 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-2 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-3 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-4 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-5 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-6 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-7 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-8 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-9 [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(font-fraktur [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(underline-double [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(proportional-spacing [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(framed [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(encircled [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(overline [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(ideogram-underline-or-right [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(ideogram-double-underline-or-right [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(ideogram-overline-or-left [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(ideogram-double-overline-or-left [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(ideogram-stress-marking [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(superscript [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
  @defproc[(subscript [str string?] [return-to (or/c 'auto 'none attribute-name?) 'auto]) string?]
)]{
TBD

@examples[#:eval example-eval
(string-append "Hello from " (bold (underline "racket")) "!")
]

@examples[#:eval example-eval
(require racket/string)
(define table-column-line (faint "|"))
(define table-column-start (string-append table-column-line " "))
(define table-column-mid (string-append " " table-column-line " "))
(define table-column-end (string-append " " table-column-line))

(string-append table-column-start
               (string-join '("Here" "There" "Everywhere")
                            table-column-mid)
               table-column-end)
]

The following example defines a formatting function @racket[~f] which formats numbers in an accounting fashion. For
the purposes of this example note that negative amounts are rendered in red and totals have an overline, underline,
and are bold.

@examples[#:eval example-eval
(require racket/format)
(define (~f v #:total? (total? #f)
              #:currency (currency "$")
              #:currency-position (position 'left)
              #:currency-separator (separator #f)
              #:zero (zero "-"))
  (let* ((number (if (= (abs v) 0)
                     (~a zero
                         #:min-width 12
                         #:pad-string " "
                         #:align 'right)
                     (~r v
                         #:notation 'positional
                         #:sign 'parens
                         #:precision 2
                         #:min-width 12
                         #:pad-string " "
                         #:groups '(3)
                         #:group-sep ","
                         #:decimal-sep ".")))
         (pad-string (if separator separator ""))
         (amount (cond
                  ((eq? position 'left)
                   (string-append currency pad-string number))
                  ((eq? position 'right)
                   (string-append number pad-string currency))
                  (else number)))
         (colored (if (< v 0) (red-text amount) amount))
         (styled (if total?
                     (bold (underline (overline colored)))
                     colored)))
  styled))
(~f 19.99)
(~f -19.99)
(~f 0)
(~f 19.99 #:currency "USD" #:currency-position 'right)
(~f 19.99 #:total? #t)
]
}

@subsection{Color, Foreground and Background}

@defproc[(text-color
          [str string?]
          [color color-name?]
          [return-to (or/c 'default 'none color-name?) 'default])
          string?]{
TBD
}

@deftogether[(
  @defproc[(black-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(red-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(green-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(yellow-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(blue-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(magenta-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(cyan-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(white-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-black-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-red-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-green-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-yellow-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-blue-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-magenta-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-cyan-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(bright-white-text [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
)]{
TBD

@examples[#:eval example-eval
(string-append "Hello from " (blue-text "racket") "!")
]
}

@defproc[(on-background
          [str string?]
          [color color-name?]
          [return-to (or/c 'default 'none color-name?) 'default])
          string?]{
TBD
}

@deftogether[(
  @defproc[(on-black [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-red [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-green [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-yellow [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-blue [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-magenta [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-cyan [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-white [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-black [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-red [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-green [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-yellow [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-blue [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-magenta [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-cyan [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
  @defproc[(on-bright-white [str string?] [return-to (or/c 'default 'none color-name?) 'default]) string?]
)]{
TBD

@examples[#:eval example-eval
(string-append "Hello from " (white-text (on-blue "racket")) "!")
]
}

@section[]{Complex Styling}

@defproc[#:kind "predicate"
         (style?
          [val any/c])
          boolean?]{
Returns @racket[#t] if @racket[val], ...
}

@defproc[(style->list
          [style style?])
         (listof style-code/c)]{
TBD
}

@defproc[(style-append
          [style-1 style?]
          [style-2 style?])
         style?]{
TBD
}

@defproc[(style-push
          [style-1 style?]
          [code-list style-code/c])
         style?]{
TBD
}

@defproc[(style-pop
          [style style?])
         style?]{
TBD
}

@defproc[(style->string
          [style style?])
         string?]{
TBD
}


@defthing[reset style?]{
TBD
}

@defthing[make-style-param/c contract?]{
TBD
}


@;; (make-style (-> make-style-param/c ... make-style-param/c style?))

@defproc[(style-string
          [str string?]
          [style style?]
          [#:return-to return-to style? reset])
         string?]{
TBD
}

@defproc[(~a/styled
          [val any/c]
          [style style?]
          [#:return-to return-to style? reset])
         string?]{
TBD
}

@defproc[(~s/styled
          [val any/c]
          [style style?]
          [#:return-to return-to style? reset])
         string?]{
TBD
}
