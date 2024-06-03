#lang scribble/manual

@(require racket/sandbox
          scribble/eval
          (for-label (except-in racket/base
                                reverse)
                     ansi-terminal/control
                     ansi-terminal/terminal
                     ansi-terminal/graphics))

@;{============================================================================}

@(define example-eval (make-base-eval '(require ansi-terminal/control
                                                ansi-terminal/terminal
                                                ansi-terminal/graphics)))

@;{============================================================================}
@;{============================================================================}
@title[#:version  "1.1"]{ANSI Terminal Escape Codes.}
@author[(author+email "Simon Johnston" "johnstonskj@gmail.com")]

This package provides an interface to ANSI escape codes for terminals. This package focuses primarily on the
Control Sequence Introducer (CSI) codes for terminal control.

For more information, see
@hyperlink["https://en.wikipedia.org/wiki/ANSI_escape_code"]{ANSI escape code} and
@hyperlink["https://en.wikipedia.org/wiki/C0_and_C1_control_codes"]{C0 and C1 control codes} (Wikipedia).

@table-of-contents[]

@;{============================================================================}
@;{============================================================================}
@section[]{Control Sequences}
@defmodule[ansi-terminal/control]

The C0 and C1 control code or control character sets define control codes for use in text by computer systems that
use ASCII and derivatives of ASCII. The codes represent additional information about the text, such as the position
of a cursor, an instruction to start a new line, or a message that the text has been received.

C0 codes are the range @racket[#\u00]–@racket[#\u1F] and the default C0 set was originally defined in ISO 646 (ASCII).
C1 codes are the range @racket[#\u80]–@racket[#\u9F] and the default C1 set was originally defined in @cite["ECMA48"]
(harmonized later with ISO 6429). The ISO/IEC 2022 system of specifying control and graphic characters allows other
C0 and C1 sets to be available for specialized applications, but they are rarely used.

@deftogether[(
  @defthing[c0/null char?]
  @defthing[c0/start-of-heading char?]
  @defthing[c0/start-of-text char?]
  @defthing[c0/end-of-text char?]
  @defthing[c0/end-of-transmission char?]
  @defthing[c0/enquiry char?]
  @defthing[c0/acknowledge char?]
  @defthing[c0/bell char?]
  @defthing[c0/backspace char?]
  @defthing[c0/horizontal-tabulation char?]
  @defthing[c0/line-feed char?]
  @defthing[c0/vertical-tabulation char?]
  @defthing[c0/form-feed char?]
  @defthing[c0/carriage-return char?]
  @defthing[c0/shift-out char?]
  @defthing[c0/shift-in char?]
  @defthing[c0/data-link-escape char?]
  @defthing[c0/device-control-one char?]
  @defthing[c0/device-control-two char?]
  @defthing[c0/device-control-three char?]
  @defthing[c0/device-control-four char?]
  @defthing[c0/negative-acknowledge char?]
  @defthing[c0/synchronous-idle char?]
  @defthing[c0/end-of-transmission-block char?]
  @defthing[c0/cancel char?]
  @defthing[c0/end-of-medium char?]
  @defthing[c0/substitute char?]
  @defthing[c0/escape char?]
  @defthing[c0/file-separator char?]
  @defthing[c0/group-separator char?]
  @defthing[c0/record-separator char?]
  @defthing[c0/unit-separator char?]
)]{
Almost all users assume some functions of some single-byte characters. Initially defined as part of ASCII, the
default C0 control code set is now defined in ISO 6429 (ECMA-48), making it part of the same standard as the C1
set invoked by the ANSI escape sequences (although ISO 2022 allows the ISO 6429 C0 set to be used without the
ISO 6429 C1 set, and vice versa, provided that 0x1B is always ESC).
}

@defthing[c0/delete char?]{
ANSI added this as an additional control character and while it behaves as a C0 control it is outside of the C0 range.
}

@deftogether[(
  @defthing[c1/padding-character char?]
  @defthing[c1/high-octal-preset char?]
  @defthing[c1/break-permitted-here char?]
  @defthing[c1/no-break-here char?]
  @defthing[c1/index char?]
  @defthing[c1/next-line char?]
  @defthing[c1/start-of-selected-area char?]
  @defthing[c1/end-of-selected-area char?]
  @defthing[c1/horizontal-tabulation-set char?]
  @defthing[c1/horizontal-tabulation-with-justification char?]
  @defthing[c1/vertical-tabulation-set char?]
  @defthing[c1/parial-line-down char?]
  @defthing[c1/parial-line-up char?]
  @defthing[c1/reverse-line-feed char?]
  @defthing[c1/single-shift-2 char?]
  @defthing[c1/single-shift-3 char?]
  @defthing[c1/device-control-string char?]
  @defthing[c1/private-use-1 char?]
  @defthing[c1/private-use-2 char?]
  @defthing[c1/set-transmission-state char?]
  @defthing[c1/cancel-character char?]
  @defthing[c1/message-waiting char?]
  @defthing[c1/start-of-protected-area char?]
  @defthing[c1/end-of-protected-area char?]
  @defthing[c1/start-of-string char?]
  @defthing[c1/single-graphic-character-introducer char?]
  @defthing[c1/single-character-introducer char?]
  @defthing[c1/control-sequence-introducer char?]
  @defthing[c1/string-terminator char?]
  @defthing[c1/operating-system-command char?]
  @defthing[c1/privacy-message char?]
  @defthing[c1/application-program-command char?]
)]{
TBD
}

@deftogether[(
  @defthing[c1-str/padding-character string?]
  @defthing[c1-str/high-octal-preset string?]
  @defthing[c1-str/break-permitted-here string?]
  @defthing[c1-str/no-break-here string?]
  @defthing[c1-str/index string?]
  @defthing[c1-str/next-line string?]
  @defthing[c1-str/start-of-selected-area string?]
  @defthing[c1-str/end-of-selected-area string?]
  @defthing[c1-str/horizontal-tabulation-set string?]
  @defthing[c1-str/horizontal-tabulation-with-justification string?]
  @defthing[c1-str/vertical-tabulation-set string?]
  @defthing[c1-str/parial-line-down string?]
  @defthing[c1-str/parial-line-up string?]
  @defthing[c1-str/reverse-line-feed string?]
  @defthing[c1-str/single-shift-2 string?]
  @defthing[c1-str/single-shift-3 string?]
  @defthing[c1-str/device-control-string string?]
  @defthing[c1-str/private-use-1 string?]
  @defthing[c1-str/private-use-2 string?]
  @defthing[c1-str/set-transmission-state string?]
  @defthing[c1-str/cancel-character string?]
  @defthing[c1-str/message-waiting string?]
  @defthing[c1-str/start-of-protected-area string?]
  @defthing[c1-str/end-of-protected-area string?]
  @defthing[c1-str/start-of-string string?]
  @defthing[c1-str/single-graphic-character-introducer string?]
  @defthing[c1-str/single-character-introducer string?]
  @defthing[c1-str/control-sequence-introducer string?]
  @defthing[c1-str/string-terminator string?]
  @defthing[c1-str/operating-system-command string?]
  @defthing[c1-str/privacy-message string?]
  @defthing[c1-str/application-program-command string?]
)]{
TBD
}

@;{============================================================================}
@subsection[]{Control Sequence Predicates}


@defproc[(char-c0-code? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a character and within the C0 range.
}

@defproc[(char-c0-symbol? [v any/c]) boolean?]{
Returns @racket[#t] if @racket[v] is a character and within the C0 range.
}

@deftogether[(
@defproc[(char-c1-code? [v any/c]) boolean?]
@defproc[(string-c1-code? [v any/c]) boolean?]
)]{
Returns @racket[#t] if @racket[v] is a character and within the C0 range, or a string matching one of the @tt{c1-str}
values.
}

@deftogether[(
@defproc[(char-fe-code? [v any/c]) boolean?]
@defproc[(string-fe-code? [v any/c]) boolean?]
)]{
TBD
}

@deftogether[(
@defproc[(char-fs-code? [v any/c]) boolean?]
@defproc[(string-fs-code? [v any/c]) boolean?]
)]{
TBD
}

@deftogether[(
@defproc[(char-fp-code? [v any/c]) boolean?]
@defproc[(string-fp-code? [v any/c]) boolean?]
)]{
TBD
}

@deftogether[(
@defproc[(char-nf-code? [v any/c]) boolean?]
@defproc[(string-nf-code? [v any/c]) boolean?]
)]{
TBD
}

@;{============================================================================}
@subsection[]{C0 Unicode Symbols}

Unicode defines a set of Control Pictures is a Unicode block containing characters for graphically representing
the C0 control codes, and other control characters.

@deftogether[(
  @defthing[c0-symbol/null char?]
  @defthing[c0-symbol/start-of-heading char?]
  @defthing[c0-symbol/start-of-text char?]
  @defthing[c0-symbol/end-of-text char?]
  @defthing[c0-symbol/end-of-transmission char?]
  @defthing[c0-symbol/enquiry char?]
  @defthing[c0-symbol/acknowledge char?]
  @defthing[c0-symbol/bell char?]
  @defthing[c0-symbol/backspace char?]
  @defthing[c0-symbol/horizontal-tabulation char?]
  @defthing[c0-symbol/line-feed char?]
  @defthing[c0-symbol/vertical-tabulation char?]
  @defthing[c0-symbol/form-feed char?]
  @defthing[c0-symbol/carriage-return char?]
  @defthing[c0-symbol/shift-out char?]
  @defthing[c0-symbol/shift-in char?]
  @defthing[c0-symbol/data-link-escape char?]
  @defthing[c0-symbol/device-control-one char?]
  @defthing[c0-symbol/device-control-two char?]
  @defthing[c0-symbol/device-control-three char?]
  @defthing[c0-symbol/device-control-four char?]
  @defthing[c0-symbol/negative-acknowledge char?]
  @defthing[c0-symbol/synchronous-idle char?]
  @defthing[c0-symbol/end-of-transmission-block char?]
  @defthing[c0-symbol/cancel char?]
  @defthing[c0-symbol/end-of-medium char?]
  @defthing[c0-symbol/substitute char?]
  @defthing[c0-symbol/escape char?]
  @defthing[c0-symbol/file-separator char?]
  @defthing[c0-symbol/group-separator char?]
  @defthing[c0-symbol/record-separator char?]
  @defthing[c0-symbol/unit-separator char?]
  @defthing[c0-symbol/space char?]
  @defthing[c0-symbol/delete char?]
  @defthing[c0-symbol/blank char?]
  @defthing[c0-symbol/open-box char?]
  @defthing[c0-symbol/newline char?]
  @defthing[c0-symbol/delete-form-two char?]
  @defthing[c0-symbol/substitute-form-two char?]
)]{
Each value is a representation of a control character in the C0 group. The last 5 values are provided alternatives.
}

@defproc[(c0-symbol [c char?]) (or/c char? #f)]{
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

@defproc[(cursor-up [n exact-nonnegative-integer? 1]) string?]{
TBD
}

@defproc[(cursor-down [n exact-nonnegative-integer? 1]) string?]{
TBD
}


@defproc[(cursor-forward [n exact-nonnegative-integer? 1]) string?]{
TBD
}


@defproc[(cursor-back [n exact-nonnegative-integer? 1]) string?]{
TBD
}


@defproc[(cursor-previous-line [n exact-nonnegative-integer? 1]) string?]{
TBD
}


@defproc[(cursor-set-column [n exact-nonnegative-integer? 1]) string?]{
TBD
}


@defproc[(cursor-move
          [n exact-nonnegative-integer? 1]
          [m exact-nonnegative-integer? 1])
         string?]{
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


@defproc[(scroll-up [n exact-nonnegative-integer? 1]) string?]{
TBD
}


@defproc[(scroll-down [n exact-nonnegative-integer? 1]) string?]{
TBD
}



@;{============================================================================}
@;{============================================================================}
@section[]{Graphics}
@defmodule[ansi-terminal/graphics]

@;{============================================================================}
@subsection[]{Graphics Predicates}

@defthing[exact-byte? flat-contract?]{
An unsigned byte with range @tt{0..255}.
}

@defthing[style-code-value/c flat-contract?]{
An individual value within a style code; this happens to be a @racket[exact-byte?].
}

@defthing[style-code/c flat-contract?]{
A list of @racket[style-code-value/c] comprising a complete CSI code.
}

@defthing[color-name? flat-contract?]{
The set of 8 major colors specified by name.
}

@defthing[rgb-color/c contract?]{
An RGB color specification represented as a three-member list of bytes.
}

@defthing[color/c contract?]{
TBD
}

@defthing[attribute-name? flat-contract?]{
The set of text attribute names; note that this includes both enable and disable names.
}

@;{============================================================================}
@subsection[]{Simple Styling}

The following functions are intended to ...

@;{----------------------------------------------------------------------------}
@subsubsection{Text Attributes}

@defproc[(with-attribute
          [str string?]
          [attribute attribute-name?]
          [return-to (or/c #f 'auto attribute-name?) 'auto])
         string?]{
TBD
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

@;{----------------------------------------------------------------------------}
@subsubsection{Color, Foreground and Background}

@defproc[(text-color
          [str string?]
          [color color-name?]
          [return-to (or/c #f color/c) 'default])
          string?]{
TBD
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
TBD
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
             #:note " See also ISO/IEC number 2022"
)

@;{============================================================================}
@;{============================================================================}

@index-section[]
