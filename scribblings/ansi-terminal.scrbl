#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          (for-label (except-in racket/base
                                reverse)
                     ansi-terminal
                     ansi-terminal/control
                     ansi-terminal/terminal
                     ansi-terminal/graphics))

@;{============================================================================}

@(define example-eval (make-base-eval '(require ansi-terminal
                                                ansi-terminal/control
                                                ansi-terminal/terminal
                                                ansi-terminal/graphics)
                                      '(define (some-function) #f)))

@;{============================================================================}
@;{============================================================================}
@title[#:version  "1.1"]{ANSI Terminal Escape Codes.}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

This package provides an interface to ANSI escape codes for terminals.

For more information, see
@hyperlink["https://en.wikipedia.org/wiki/ANSI_escape_code"]{ANSI escape code} (Wikipedia).

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[]{Common Predicates}
@defmodule[ansi-terminal]

Many control codes accept parameters which are either numeric values or ASCII characters. The following
predicates are used for parameter checking.

@defthing[char-ascii/c flat-contract?]{
Ensures a character is in the range of the 7-bit ASCII character set.
}

@defthing[code-param/c flat-contract?]{
Ensures a numeric value in the range of an 8-bit unsigned byte.
}

@defthing[code-param-list/c contract?]{
Ensures either a single @racket[code-param/c] value or a list of @racket[code-param/c] values.
}

@defthing[code-param-separator char-ascii/c]{
This value is the separator character used to print a list of parameters with @racket[code-param-list->string].
}

@defproc[(code-param->string
          [val code-param/c])
         string?]{
Convert a single @racket[code-param/c] to a string.

@examples[#:eval example-eval
(code-param->string 22)
(code-param->string 7)
(code-param->string (char->integer #\λ))
]
}

@defproc[(code-param-list->string
          [val (or/c code-param/c code-param-list/c)])
         string?]{
Convert a @racket[code-param-list/c] to a string.

@examples[#:eval example-eval
(code-param-list->string '(22 34))
(code-param-list->string '(41))
(code-param-list->string 46)
(code-param-list->string (char->integer #\λ))
]
}

@;{============================================================================}
@;{============================================================================}
@section[]{Control Sequences}
@defmodule[ansi-terminal/control]

The @tt{C0} and @tt{C1} control code or control character sets define control codes for use in text by computer systems
that use ASCII and derivatives of ASCII. The codes represent additional information about the text, such as the position
of a cursor, an instruction to start a new line, or a message that the text has been received.

@tt{C0} codes are the range @racket[#\u00]–@racket[#\u1F] and the default @tt{C0} set was originally defined in ISO 646
(ASCII). @tt{C1} codes are the range @racket[#\u80]–@racket[#\u9F] and the default @tt{C1} set was originally defined in
@cite["ECMA48"] (harmonized later with ISO 6429). The ISO/IEC 2022 system of specifying control and graphic characters
allows other @tt{C0} and @tt{C1} sets to be available for specialized applications, but they are rarely used.

For more information, see
@hyperlink["https://en.wikipedia.org/wiki/C0_and_C1_control_codes"]{C0 and C1 control codes} (Wikipedia).

@deftogether[(
  @defthing[c0/null char-ascii/c]
  @defthing[c0/start-of-heading char-ascii/c]
  @defthing[c0/start-of-text char-ascii/c]
  @defthing[c0/end-of-text char-ascii/c]
  @defthing[c0/end-of-transmission char-ascii/c]
  @defthing[c0/enquiry char-ascii/c]
  @defthing[c0/acknowledge char-ascii/c]
  @defthing[c0/bell char-ascii/c]
  @defthing[c0/backspace char-ascii/c]
  @defthing[c0/horizontal-tabulation char-ascii/c]
  @defthing[c0/line-feed char-ascii/c]
  @defthing[c0/vertical-tabulation char-ascii/c]
  @defthing[c0/form-feed char-ascii/c]
  @defthing[c0/carriage-return char-ascii/c]
  @defthing[c0/shift-out char-ascii/c]
  @defthing[c0/shift-in char-ascii/c]
  @defthing[c0/data-link-escape char-ascii/c]
  @defthing[c0/device-control-one char-ascii/c]
  @defthing[c0/device-control-two char-ascii/c]
  @defthing[c0/device-control-three char-ascii/c]
  @defthing[c0/device-control-four char-ascii/c]
  @defthing[c0/negative-acknowledge char-ascii/c]
  @defthing[c0/synchronous-idle char-ascii/c]
  @defthing[c0/end-of-transmission-block char-ascii/c]
  @defthing[c0/cancel char-ascii/c]
  @defthing[c0/end-of-medium char-ascii/c]
  @defthing[c0/substitute char-ascii/c]
  @defthing[c0/escape char-ascii/c]
  @defthing[c0/file-separator char-ascii/c]
  @defthing[c0/group-separator char-ascii/c]
  @defthing[c0/record-separator char-ascii/c]
  @defthing[c0/unit-separator char-ascii/c]
)]{
Almost all users assume some functions of some single-byte characters. Initially defined as part of ASCII, the
default @tt{C0} control code set is now defined in ISO 6429 (@cite["ECMA48"]), making it part of the same standard as
the @tt{C1} set invoked by the ANSI escape sequences (although ISO 2022 allows the ISO 6429 @tt{C0} set to be used
without the ISO 6429 @tt{C1} set, and vice versa, provided that @racket[#\u1B] is always @tt{ESC}).
}

@defthing[c0/delete char-ascii/c]{
ANSI added this as an additional control character and while it behaves as a @tt{C0} control it is outside of the
@tt{C0} range.
}

@deftogether[(
  @defthing[c1/padding-character char-ascii/c]
  @defthing[c1/high-octal-preset char-ascii/c]
  @defthing[c1/break-permitted-here char-ascii/c]
  @defthing[c1/no-break-here char-ascii/c]
  @defthing[c1/index char-ascii/c]
  @defthing[c1/next-line char-ascii/c]
  @defthing[c1/start-of-selected-area char-ascii/c]
  @defthing[c1/end-of-selected-area char-ascii/c]
  @defthing[c1/horizontal-tabulation-set char-ascii/c]
  @defthing[c1/horizontal-tabulation-with-justification char-ascii/c]
  @defthing[c1/vertical-tabulation-set char-ascii/c]
  @defthing[c1/parial-line-down char-ascii/c]
  @defthing[c1/parial-line-up char-ascii/c]
  @defthing[c1/reverse-line-feed char-ascii/c]
  @defthing[c1/single-shift-2 char-ascii/c]
  @defthing[c1/single-shift-3 char-ascii/c]
  @defthing[c1/device-control-string char-ascii/c]
  @defthing[c1/private-use-1 char-ascii/c]
  @defthing[c1/private-use-2 char-ascii/c]
  @defthing[c1/set-transmission-state char-ascii/c]
  @defthing[c1/cancel-character char-ascii/c]
  @defthing[c1/message-waiting char-ascii/c]
  @defthing[c1/start-of-protected-area char-ascii/c]
  @defthing[c1/end-of-protected-area char-ascii/c]
  @defthing[c1/start-of-string char-ascii/c]
  @defthing[c1/single-graphic-character-introducer char-ascii/c]
  @defthing[c1/single-character-introducer char-ascii/c]
  @defthing[c1/control-sequence-introducer char-ascii/c]
  @defthing[c1/string-terminator char-ascii/c]
  @defthing[c1/operating-system-command char-ascii/c]
  @defthing[c1/privacy-message char-ascii/c]
  @defthing[c1/application-program-command char-ascii/c]
)]{
TBD
}

@;{============================================================================}
@subsection[]{Control Sequence Predicates}


@defproc[(char-c0-code? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a character and within the @tt{C0} range.
}

@deftogether[(
@defproc[(char-c1-code? [v any/c]) boolean?]
)]{
Returns @racket[#t] if @racket[v] is a character and within the @tt{C1} range.
}

@deftogether[(
@defproc[(char-fe-code? [v any/c]) boolean?]
)]{
Returns @racket[#t] if @racket[v] is a character and within the @tt{Fe} range.
}

@deftogether[(
@defproc[(char-fs-code? [v any/c]) boolean?]
)]{
Returns @racket[#t] if @racket[v] is a character and within the @tt{Fs} range.
}

@deftogether[(
@defproc[(char-fp-code? [v any/c]) boolean?]
)]{
Returns @racket[#t] if @racket[v] is a character and within the @tt{Fp} range.
}

@deftogether[(
@defproc[(char-nf-code? [v any/c]) boolean?]
)]{
Returns @racket[#t] if @racket[v] is a character and within the @tt{nF} range.
}

@;{============================================================================}
@subsection[]{C0 Unicode Symbols}

Unicode defines a set of Control Pictures is a Unicode block containing characters for graphically representing
the @tt{C0} control codes, and other control characters.

@deftogether[(
  @defthing[c0-symbol/null char-ascii/c]
  @defthing[c0-symbol/start-of-heading char-ascii/c]
  @defthing[c0-symbol/start-of-text char-ascii/c]
  @defthing[c0-symbol/end-of-text char-ascii/c]
  @defthing[c0-symbol/end-of-transmission char-ascii/c]
  @defthing[c0-symbol/enquiry char-ascii/c]
  @defthing[c0-symbol/acknowledge char-ascii/c]
  @defthing[c0-symbol/bell char-ascii/c]
  @defthing[c0-symbol/backspace char-ascii/c]
  @defthing[c0-symbol/horizontal-tabulation char-ascii/c]
  @defthing[c0-symbol/line-feed char-ascii/c]
  @defthing[c0-symbol/vertical-tabulation char-ascii/c]
  @defthing[c0-symbol/form-feed char-ascii/c]
  @defthing[c0-symbol/carriage-return char-ascii/c]
  @defthing[c0-symbol/shift-out char-ascii/c]
  @defthing[c0-symbol/shift-in char-ascii/c]
  @defthing[c0-symbol/data-link-escape char-ascii/c]
  @defthing[c0-symbol/device-control-one char-ascii/c]
  @defthing[c0-symbol/device-control-two char-ascii/c]
  @defthing[c0-symbol/device-control-three char-ascii/c]
  @defthing[c0-symbol/device-control-four char-ascii/c]
  @defthing[c0-symbol/negative-acknowledge char-ascii/c]
  @defthing[c0-symbol/synchronous-idle char-ascii/c]
  @defthing[c0-symbol/end-of-transmission-block char-ascii/c]
  @defthing[c0-symbol/cancel char-ascii/c]
  @defthing[c0-symbol/end-of-medium char-ascii/c]
  @defthing[c0-symbol/substitute char-ascii/c]
  @defthing[c0-symbol/escape char-ascii/c]
  @defthing[c0-symbol/file-separator char-ascii/c]
  @defthing[c0-symbol/group-separator char-ascii/c]
  @defthing[c0-symbol/record-separator char-ascii/c]
  @defthing[c0-symbol/unit-separator char-ascii/c]
  @defthing[c0-symbol/space char-ascii/c]
  @defthing[c0-symbol/delete char-ascii/c]
  @defthing[c0-symbol/blank char-ascii/c]
  @defthing[c0-symbol/open-box char-ascii/c]
  @defthing[c0-symbol/newline char-ascii/c]
  @defthing[c0-symbol/delete-form-two char-ascii/c]
  @defthing[c0-symbol/substitute-form-two char-ascii/c]
)]{
Each value is a representation of a control character in the @tt{C0} group. The last 5 values are provided alternatives.
}

@defproc[(char-c0-symbol? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a character and within the @tt{C0} symbol character range.
}

@defproc[(c0-symbol [c char-c0-code?])
         (or/c char-c0-symbol? #f)]{
Returns the symbol associated with @racket[c] if it is a @racket[char-c0-code?], else @racket[#f].

@examples[#:eval example-eval
(map (lambda (c) (or (c0-symbol c) c))
     (string->list "Hello\r\n\tWorld."))
]
}

@;{============================================================================}
@;{============================================================================}
@section[]{Terminal Control}
@defmodule[ansi-terminal/terminal]

@;@defproc[(cursor-up [n code-param/c 1]) string?]{
@;TBD
@;}

@defproc[(cursor-up [n code-param/c 1]) string?]{
TBD
}

@defproc[(cursor-down [n code-param/c 1]) string?]{
TBD
}


@defproc[(cursor-forward [n code-param/c 1]) string?]{
TBD
}


@defproc[(cursor-back [n code-param/c 1]) string?]{
TBD
}


@defproc[(cursor-previous-line [n code-param/c 1]) string?]{
TBD
}


@defproc[(cursor-set-column [n code-param/c 1]) string?]{
TBD
}


@defproc[(cursor-move
          [n code-param/c 1]
          [m code-param/c 1])
         string?]{
TBD
}

@defproc[(cursor-move-home) string?]{
TBD
}


@defproc[(erase-in-display
          [part (or/c  'cursor-to-end
                       'beginning-to-cursor
                       'entire-screen
                       'entire-screen-and-scrollback)
                'cursor-to-end])
         string?]{
TBD
}


@defproc[(erase-in-line
          [part (or/c  'cursor-to-end
                       'beginning-to-cursor
                       'entire-line)
                'cursor-to-end])
         string?]{
TBD
}


@defproc[(scroll-up [n code-param/c 1]) string?]{
TBD
}


@defproc[(scroll-down [n code-param/c 1]) string?]{
TBD
}



@;{============================================================================}
@;{============================================================================}
@section[]{Graphics}
@defmodule[ansi-terminal/graphics]

@;{============================================================================}
@subsection[]{Graphics Predicates}

@defthing[color-name? flat-contract?]{
The set of 8 major colors specified by name.
}

@defthing[rgb-color/c contract?]{
An RGB color specification represented as a three-member list of bytes.
}

@defthing[color/c contract?]{
TBD

@itemlist[
  @item{@racket[color-name?] -- One of 16 predefined colors, 8 standard and 8 bright.}
  @item{@racket[code-param/c] -- Specification of a 256 color.}
  @item{@racket[rgb-color/c] -- Specification of a color as an RGB value.}
]

If one of the 256 colors are selected, the meaning is as follows:

@itemlist[
  @item{@tt{0..7} -- standard 8 colors}
  @item{@tt{8..15} -- standard 8 bright colors}
  @item{@tt{16..231} -- 6×6×6 color cube}
  @item{@tt{232..252} -- 24 greyscale shades from dark to light}
]

}

@defthing[attribute-name? flat-contract?]{
The set of text attribute names; note that this includes both enable and disable names.
}

@;{============================================================================}
@subsection[]{Simple Styling}

The following functions are intended to provide a very direct style approach for both text attributes and colors.
The core functions are @racket[with-attribute], @racket[text-color] and @racket[on-background] which are then
simplified further by attribute- and color-specific functions. In all cases the function will take an optional
parameter @italic{return-to} which will specify an attribute or color to set once the provided string has been
formatted. In the case of attributes the value @racket['auto] will use a corresponding code to turn off the attribute.
For colors the value @racket['default] will use a specific code for either the default foreground or default background
color.

@defproc[(with-attribute
          [str string?]
          [attribute attribute-name?]
          [return-to (or/c #f 'auto attribute-name?) 'auto])
         string?]{
Returns a string with @racket[attribute] turned into a control string before @racket[str] and followed by
@racket[return-to] as a control string.

As mentioned above the value @racket['auto] for @racket[return-to] will use the control sequence to turn off
@racket[attribute]. In the following example the return-to value is the attribute @racket['not-underline].

@examples[#:eval example-eval
(with-attribute "text" 'underline 'auto)
]

A value of @racket[#f] for @racket[return-to] will skip adding a return-to control sequence.

@examples[#:eval example-eval
(with-attribute "text" 'underline #f)
]

Alternatively it is possible to specify an entirely different attribute to end the sequence with.

@examples[#:eval example-eval
(with-attribute "text" 'underline 'bold)
]
}

@deftogether[(
  @defproc[(bold [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(faint [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(italic [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(underline [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(blink-slow [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(blink-rapid [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(reversed [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(conceal [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(crossed-out [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-1 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-2 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-3 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-4 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-5 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-6 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-7 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-8 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-alternate-9 [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(font-fraktur [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(underline-double [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(proportional-spacing [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(framed [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(encircled [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(overline [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(ideogram-underline-or-right [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(ideogram-double-underline-or-right [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(ideogram-overline-or-left [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(ideogram-double-overline-or-left [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(ideogram-stress-marking [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(superscript [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
  @defproc[(subscript [str string?] [return-to (or/c #f 'auto attribute-name?) 'auto]) string?]
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

@defproc[(text-color
          [str string?]
          [color color-name?]
          [return-to (or/c #f color/c) 'default])
          string?]{
Returns a string with @racket[color] turned into a control string before @racket[str] and followed by
@racket[return-to] as a control string.

As mentioned above the value @racket['default] for @racket[return-to] will use the control sequence for the
default foregound color.

@examples[#:eval example-eval
(text-color "text" 'red 'default)
]

A value of @racket[#f] for @racket[return-to] will skip adding a return-to control sequence.

@examples[#:eval example-eval
(text-color "text" 'red #f)
]

Alternatively it is possible to specify an entirely different color to end the sequence with.

@examples[#:eval example-eval
(text-color "text" 'red 'green)
]
}

@deftogether[(
  @defproc[(black-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(red-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(green-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(yellow-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(blue-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(magenta-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(cyan-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(white-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-black-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-red-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-green-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-yellow-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-blue-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-magenta-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-cyan-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(bright-white-text [str string?] [return-to (or/c #f color/c) 'default]) string?]
)]{
TBD

@examples[#:eval example-eval
(string-append "Hello from " (blue-text "racket") "!")
]
}

@defproc[(on-background
          [str string?]
          [color color-name?]
          [return-to (or/c #f color/c) 'default])
          string?]{
Returns a string with @racket[color] turned into a control string before @racket[str] and followed by
@racket[return-to] as a control string.

As mentioned above the value @racket['default] for @racket[return-to] will use the control sequence for the
default backgound color.

@examples[#:eval example-eval
(on-background "text" 'red 'default)
]

A value of @racket[#f] for @racket[return-to] will skip adding a return-to control sequence.

@examples[#:eval example-eval
(on-background "text" 'red #f)
]

Alternatively it is possible to specify an entirely different color to end the sequence with.

@examples[#:eval example-eval
(on-background "text" 'red 'green)
]
}

@deftogether[(
  @defproc[(on-black [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-red [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-green [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-yellow [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-blue [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-magenta [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-cyan [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-white [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-black [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-red [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-green [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-yellow [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-blue [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-magenta [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-cyan [str string?] [return-to (or/c #f color/c) 'default]) string?]
  @defproc[(on-bright-white [str string?] [return-to (or/c #f color/c) 'default]) string?]
)]{
TBD

@examples[#:eval example-eval
(string-append "Hello from " (white-text (on-blue "racket")) "!")
]
}

@;{============================================================================}
@subsection[]{Complex Styling}

This section allows for more complex styling in that styles can be more easily combined and also defined in a reusasable
structure, the @racket[style?].

@defproc[#:kind "predicate"
         (style?
          [val any/c])
          boolean?]{
Returns @racket[#t] if @racket[val], is a style structure instance.
}

@defproc[(style->list
          [style style?])
         (listof code-param-list/c)]{
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
          [code-list code-param-list/c])
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

@itemlist[
  @item{@racket['underline] -- Turns on the specified text attribute.}
  @item{@racket['(not bold)] -- Turns off the specified text attribute.}
  @item{@racket['blue] -- Sets the foreground color.}
  @item{@racket['(text blue)] -- Sets the foreground color, note that the first symbol may be also be @racket['fg] or
  @racket['foreground].}
  @item{@racket['(on green)] -- Sets the background color, note that the first symbol may be also be @racket['bg] or
  @racket['background].}
]
}

@defproc[(make-style
          [st make-style-param/c] ...+)
         style?]{
TBD

@examples[#:eval example-eval
(define normal-style (make-style 'bright-white '(on bright-black)))
(define success-style (make-style 'green))
(define error-style (make-style 'red '(on black) 'blink-slow 'bold))
(if (some-function)
    (style-string "OK" success-style #:return-to normal-style)
    (style-string "Oops" error-style #:return-to normal-style))
]
}

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


@;{============================================================================}
@;{============================================================================}

@(bibliography

  (bib-entry #:key "ECMA6"
             #:title "7-bit coded character set, 6th edition"
             #:location "ECMA"
             #:url "https://ecma-international.org/publications-and-standards/standards/ecma-6/"
             #:date "December 1991"
             #:note " See also ISO/IEC number 646")

  (bib-entry #:key "ECMA48"
             #:title "Control Functions for Coded Character Sets, 5th Edition"
             #:location "ECMA"
             #:url "https://ecma-international.org/publications-and-standards/standards/ecma-48/"
             #:date "June 1991"
             #:note " See also ISO/IEC number 6429")

  (bib-entry #:key "ECMA35"
             #:title "Character code structure and extension techniques, 6th Edition"
             #:location "ECMA"
             #:url "https://ecma-international.org/publications-and-standards/standards/ecma-35/"
             #:date "December 1994"
             #:note " See also ISO/IEC number 2022")
)

@;{============================================================================}
@;{============================================================================}

@index-section[]
