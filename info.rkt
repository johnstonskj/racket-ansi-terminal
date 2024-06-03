#lang info

(define collection "ansi-terminal")
(define pkg-desc "Interface to ANSI escape sequences for terminals.")
(define version "1.0")
(define pkg-authors '(johnstonskj))
(define license 'Apache-2.0)

(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc" "sandbox-lib" "rackunit-lib"))
(define scribblings '(("scribblings/ansi-terminal.scrbl" (multi-page) (library))))
(define test-omit-paths '("scribblings"))
