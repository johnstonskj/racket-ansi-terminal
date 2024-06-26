# Racket Package ansi-terminal

This package provides an interface to ANSI escape codes for terminals. This package focuses primarily on the
Control Sequence Introducer (CSI) codes for terminal control.

[![raco pkg install ansi-terminal](https://img.shields.io/badge/raco%20pkg%20install-ansi--terminal-blue.svg)](http://pkgs.racket-lang.org/package/ansi-terminal)
[![Documentation](https://img.shields.io/badge/raco%20docs-ansi--terminal-blue.svg)](http://docs.racket-lang.org/ansi-terminal/index.html)
[![Racket](https://github.com/johnstonskj/racket-ansi-terminal/actions/workflows/racket.yml/badge.svg)](https://github.com/johnstonskj/racket-ansi-terminal/actions/workflows/racket.yml)
[![GitHub release](https://img.shields.io/github/release/johnstonskj/racket-ansi-terminal.svg?style=flat-square)](https://github.com/johnstonskj/racket-ansi-terminal/releases)
[![GitHub
stars](https://img.shields.io/github/stars/johnstonskj/racket-ansi-terminal.svg)](https://github.com/johnstonskj/racket-ansi-terminal/stargazers)

Currently the package includes the following modules:

- `ansi-terminal` contains common predicates and escape functions.
- `ansi-terminal/control` contains single character control codes (C0 and C1).
- `ansi-terminal/terminal` contains terminal control such as cursor movement codes.
- `ansi-terminal/screen` contains screen mode codes (in progress).
- `ansi-terminal/graphics` contains SGR codes for text attributes and color.


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

## Reference

```
 ANSI Standard (X3.64) Control Sequences for Video Terminals and Peripherals
                       in alphabetic order by mnemonic

     (Inspired by the article "Toward Stardardized Video Terminals: ANSI
      X3.64 Device Control" by Mark L. Siegel, April 1984 BYTE, page 365)

               (Ps and Pn are parameters expressed in ASCII.)
               (Numeric parameters are given in decimal radix.)
               (Abbreviations are explained in detail at end.)
               (Spaces used in this table for clarity are not
                used in the actual codes.)

                                                           Default    Type
Sequence     Sequence                                      Parameter   or
Mnemonic     Name              Sequence                    Value      Mode
---------------------------------------------------------------------------
APC  Applicatn Program Command Esc Fe                                 Delim
CBT  Cursor Backward Tab       Esc [ Pn Z                   1         EdF
CCH  Cancel Previous Character Esc T
CHA  Cursor Horzntal Absolute  Esc [ Pn G                   1         EdF
CHT  Cursor Horizontal Tab     Esc [ Pn I                   1         EdF
CNL  Cursor Next Line          Esc [ Pn E                   1         EdF
CPL  Cursor Preceding Line     Esc [ Pn F                   1         EdF
CPR  Cursor Position Report    Esc [ Pn ; Pn R              1, 1
CSI  Control Sequence Intro    Esc [                                  Intro
CTC  Cursor Tab Control        Esc [ Ps W                   0         EdF
CUB  Cursor Backward           Esc [ Pn D                   1         EdF
CUD  Cursor Down               Esc [ Pn B                   1         EdF
CUF  Cursor Forward            Esc [ Pn C                   1         EdF
CUP  Cursor Position           Esc [ Pn ; Pn H              1, 1      EdF
CUU  Cursor Up                 Esc [ Pn A                   1         EdF
CVT  Cursor Vertical Tab       Esc [ Pn Y                             EdF
DA   Device Attributes         Esc [ Pn c                   0
DAQ  Define Area Qualification Esc [ Ps o                   0
DCH  Delete Character          Esc [ Pn P                   1         EdF
DCS  Device Control String     Esc P                                  Delim
DL   Delete Line               Esc [ Pn M                   1         EdF
DMI  Disable Manual Input      Esc \                                  Fs
DSR  Device Status Report      Esc [ Ps n                   0
EA   Erase in Area             Esc [ Ps O                   0         EdF
ECH  Erase Character           Esc [ Pn X                   1         EdF
ED   Erase in Display          Esc [ Ps J                   0         EdF
EF   Erase in Field            Esc [ Ps N                   0         EdF
EL   Erase in Line             Esc [ Ps K                   0         EdF
EMI  Enable Manual Input       Esc b                                  Fs
EPA  End of Protected Area     Esc W
ESA  End of Selected Area      Esc G
FNT  Font Selection            Esc [ Pn ; Pn Space D        0, 0      FE
GSM  Graphic Size Modify       Esc [ Pn ; Pn Space B        100, 100  FE
GSS  Graphic Size Selection    Esc [ Pn Space C             none      FE
HPA  Horz Position Absolute    Esc [ Pn                     1         FE
HPR  Horz Position Relative    Esc [ Pn a                   1         FE
HTJ  Horz Tab w/Justification  Esc I                                  FE
HTS  Horizontal Tab Set        Esc H                                  FE
HVP  Horz & Vertical Position  Esc [ Pn ; Pn f              1, 1      FE
ICH  Insert Character          Esc [ Pn @                   1         EdF
IL   Insert Line               Esc [ Pn L                   1         EdF
IND  Index                     Esc D                                  FE
INT  Interrupt                 Esc a                                  Fs
JFY  Justify                   Esc [ Ps ; ... ; Ps Space F  0         FE
MC   Media Copy                Esc [ Ps i                   0
MW   Message Waiting           Esc U
NEL  Next Line                 Esc E                                  FE
NP   Next Page                 Esc [ Pn U                   1         EdF
OSC  Operating System Command  Esc ]                                  Delim
PLD  Partial Line Down         Esc K                                  FE
PLU  Partial Line Up           Esc L                                  FE
PM   Privacy Message           Esc ^                                  Delim
PP   Preceding Page            Esc [ Pn V                   1         EdF
PU1  Private Use 1             Esc Q
PU2  Private Use 2             Esc R
QUAD Typographic Quadding      Esc [ Ps Space H             0         FE
REP  Repeat Char or Control    Esc [ Pn b                   1
RI   Reverse Index             Esc M                                  FE
RIS  Reset to Initial State    Esc c                                  Fs
RM   Reset Mode                Esc [ Ps l                   none
SD   Scroll Down               Esc [ Pn T                   1         EdF
SEM  Select Edit Extent Mode   Esc [ Ps Q                   0
SGR  Select Graphic Rendition  Esc [ Ps m                   0         FE
SL   Scroll Left               Esc [ Pn Space @             1         EdF
SM   Select Mode               Esc [ Ps h                   none
SPA  Start of Protected Area   Esc V
SPI  Spacing Increment         Esc [ Pn ; Pn Space G        none      FE
SR   Scroll Right              Esc [ Pn Space A             1         EdF
SS2  Single Shift 2 (G2 set)   Esc N                                  Intro
SS3  Single Shift 3 (G3 set)   Esc O                                  Intro
SSA  Start of Selected Area    Esc F
ST   String Terminator         Esc \                                  Delim
STS  Set Transmit State        Esc S
SU   Scroll Up                 Esc [ Pn S                   1         EdF
TBC  Tab Clear                 Esc [ Ps g                   0         FE
TSS  Thin Space Specification  Esc [ Pn Space E             none      FE
VPA  Vert Position Absolute    Esc [ Pn d                   1         FE
VPR  Vert Position Relative    Esc [ Pn e                   1         FE
VTS  Vertical Tabulation Set   Esc J                                  FE

---------------------------------------------------------------------------

Abbreviations:

Intro  an Introducer of some kind of defined sequence; the normal 7-bit
       X3.64 Control Sequence Introducer is the two characters "Escape ["

Delim  a Delimiter

x/y    identifies a character by position in the ASCII table (column/row)

EdF    editor function (see explanation)

FE     format effector (see explanation)

F      is a Final character in
            an Escape sequence (F from 3/0 to 7/14 in the ASCII table)
            a control sequence (F from 4/0 to 7/14)

Gs     is a graphic character appearing in strings (Gs ranges from
       2/0 to 7/14) in the ASCII table

Ce     is a control represented as a single bit combination in the C1 set
       of controls in an 8-bit character set

C0     the familiar set of 7-bit ASCII control characters

C1     roughly, the set of control characters available only in 8-bit systems.
       This is too complicated to explain fully here, so read Jim Fleming's
       article in the February 1983 BYTE, especially pages 214 through 224.

Fe     is a Final character of a 2-character Escape sequence that has an
       equivalent representation in an 8-bit environment as a Ce-type
       (Fe ranges from 4/0 to 5/15)

Fs     is a Final character of a 2-character Escape sequence that is
       standardized internationally with identical representation in 7-bit
       and 8-bit environments and is independent of the currently
       designated C0 and C1 control sets (Fs ranges from 6/0 to 7/14)

I      is an Intermediate character from 2/0 to 2/15 (inclusive) in the
       ASCII table

P      is a parameter character from 3/0 to 3/15 (inclusive) in the ASCII
       table

Pn     is a numeric parameter in a control sequence, a string of zero or
       more characters ranging from 3/0 to 3/9 in the ASCII table

Ps     is a variable number of selective parameters in a control sequence
       with each selective parameter separated from the other by the code
       3/11 (which usually represents a semicolon); Ps ranges from
       3/0 to 3/9 and includes 3/11

v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v

Format Effectors versus Editor Functions

A format effector specifies how the final output is to be created.
An editor function allows you to modify the specification.

For instance, a format effector that moves the "active position" (the 
cursor or equvalent) one space to the left would be useful when you want to
create an overstrike, a compound character made of two standard characters
overlaid. Control-H, the Backspace character, is actually supposed to be a
format effector, so you can do this. But many systems use it in a
nonstandard fashion, as an editor function, deleting the character to the
left of the cursor and moving the cursor left. When Control-H is assumed to
be an editor function, you cannot predict whether its use will create an
overstrike unless you also know whether the output device is in an "insert
mode" or an "overwrite mode". When Control-H is used as a format effector,
its effect can always be predicted. The familiar characters carriage
return, linefeed, formfeed, etc., are defined as format effectors.

v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^

ANSI X3.64 Mode-Changing Parameters for use with the
           Select Mode (SM) and Reset Mode (RM) functions

    Parameter           Mode          Mode Function
    Characters          Mnemonic
 column/    graphic
  row       repres.
----------------------------------------------------------------------------
3/0           0                       an error condition
3/1           1          GATM         guarded-area transfer mode
3/2           2          KAM          keyboard action mode
3/3           3          CRM          control representation mode
3/4           4          IRM          insertion/replacement mode
3/5           5          SRTM         status-reporting transfer mode
3/6           6          ERM          erasure mode
3/7           7          VEM          vertical editing mode
3/8           8                       reserved for future standardization
3/9           9                       reserved for future standardization
3/10          :                       reserved separator for parameters
3/11          ;                       Standard separator for parameters
3/12          <                       reserved for private (experimental) use
3/13          =                       reserved for private (experimental) use
3/14          >                       reserved for private (experimental) use
3/15          ?                       reserved for private (experimental) use
3/1  3/0      10         HEM          horizontal editing mode
3/1  3/1      11         PUM          positioning unit mode
3/1  3/2      12         SRM          send/receive mode
3/1  3/3      13         FEAM         format effector action mode
3/1  3/4      14         FETM         format effector transfer mode
3/1  3/5      15         MATM         multiple area transfer mode
3/1  3/6      16         TTM          transfer termination mode
3/1  3/7      17         SATM         selected area transfer mode
3/1  3/8      18         TSM          tabulation stop mode
3/1  3/9      19         EBM          editing boundary mode
3/1  3/10     1:                      reserved separator for parameters
3/1  3/11     1;                      Standard separator for parameters
3/1  3/12     1<                      error condition--unspecified recovery
3/1  3/13     1=                      error condition--unspecified recovery
3/1  3/14     1>                      error condition--unspecified recovery
3/1  3/15     1?                      error condition--unspecified recovery
3/2  3/0      20         LNM          linefeed/newline mode (not in ISO 6429)
3/2  3/1      21
 .            .
 .            .                       reserved for future standardization
 .            .
3/9  3/9      99

3/12 3/0      <0
 .            .
 .            .                       reserved for private (experimental) use
 .            .
3/15 3/15     ??

v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v
^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^v^

NOTES ON THE DEC VT100 IMPLEMENTATION

In the case of the popular DEC VT100 video-terminal implementation,
the only mode that may be altered is the linefeed/newline (LNM) mode.
Other modes are considered permanently set, reset, or not applicable
as follows:

     Set:   ERM
     Reset: CRM, EBM, FEAM, FETM, IRM, KAM, PUM, SRTM, TSM
     N/A:   GATM, HEM, MATM, SATM, TTM, VEM

Control sequences implemented in the VT100 are as follows:

     CPR, CUB, CUD, CUF, CUP, CUU, DA, DSR, ED, EL, HTS, HVP, IND,
     LNM, NEL, RI, RIS, RM, SGR, SM, TBC

plus several private DEC commands.

Erasing parts of the display (EL and ED) in the VT100 is performed thus:

     Erase from cursor to end of line           Esc [ 0 K    or Esc [ K
     Erase from beginning of line to cursor     Esc [ 1 K
     Erase line containing cursor               Esc [ 2 K
     Erase from cursor to end of screen         Esc [ 0 J    or Esc [ J
     Erase from beginning of screen to cursor   Esc [ 1 J
     Erase entire screen                        Esc [ 2 J

The VT100 responds to receiving the DA (Device Attributes) control

     Esc [ c    (or Esc [ 0 c)

by transmitting the sequence

     Esc [ ? l ; Ps c

where Ps is a character that describes installed options.

The VT100's cursor location can be read with the DSR (Device Status
Report) control

     Esc [ 6 n

The VT100 reports by transmitting the CPR sequence

     Esc [ Pl ; Pc R

where Pl is the line number and Pc is the column number (in decimal).
```
