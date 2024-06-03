# Racket Package ansi-terminal

This package provides an interface to ANSI escape codes for terminals. This package focuses primarily on the
Control Sequence Introducer (CSI) codes for terminal control.

[![raco pkg install ansi-terminal](https://img.shields.io/badge/raco%20pkg%20install-ansi--terminal-blue.svg)](http://pkgs.racket-lang.org/package/ansi-terminal)
[![Documentation](https://img.shields.io/badge/raco%20docs-ansi--terminal-blue.svg)](http://docs.racket-lang.org/ansi-terminal/index.html)
[![Racket](https://github.com/johnstonskj/racket-ansi-terminal/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-ansi-terminal/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-ansi-terminal.svg?style=flat-square)](https://github.com/johnstonskj/racket-ansi-terminal/releases)
[![GitHub stars](https://img.shields.io/github/stars/johnstonskj/racket-ansi-terminal.svg)](https://github.com/johnstonskj/racket-ansi-terminal/stargazers)

## Example

Hopefully, the following is self-explanatory, the text `"cruel"` is bold, red on black.

``` racket
(require ansi-terminal/graphics)

(displayln
  (string-append "goodbye " (bold (red-text (on-black "cruel"))) " world"))
```

## Changes

**Version 1.1**

Refactor into three modules:

- `control` the set of C0, C1, and other control characters and sequences.
- `terminal` the set of CSI codes for terminal control.
- `graphics` the SGR subset of CSI codes for color and text attributes.

**Version 1.0**

Initial release.
