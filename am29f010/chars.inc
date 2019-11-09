;-------------------------------------------------------------------------------
;
;                     ASCII control character definitions
;
;-------------------------------------------------------------------------------

NUL                 equ       $00                 ; [^@] Null - string terminator
SOH                 equ       $01                 ; [^A] Start of Header
STX                 equ       $02                 ; [^B] Start of Text
ETX                 equ       $03                 ; [^C] End of Text
EOT                 equ       $04                 ; [^D] End of Text
ENQ                 equ       $05                 ; [^E] Enquiry
ACK                 equ       $06                 ; [^F] Acknowledge
BEL                 equ       $07                 ; [^G] Bell
BS                  equ       $08                 ; [^H] Backspace
HT                  equ       $09                 ; [^I] Horizontal tab
LF                  equ       $0A                 ; [^J] Linefeed
VT                  equ       $0B                 ; [^K] Vertical tab
FF                  equ       $0C                 ; [^L] Form feed
CR                  equ       $0D                 ; [^M] Carriage return
SO                  equ       $0E                 ; [^N] Shift out
SI                  equ       $0F                 ; [^O] Shift in
DLE                 equ       $10                 ; [^P]
DC1                 equ       $11                 ; [^Q] Device control 1
DC2                 equ       $12                 ; [^R] Device control 2
DC3                 equ       $13                 ; [^S] Device control 3
DC4                 equ       $14                 ; [^T] Device control 4
NAK                 equ       $15                 ; [^U] Negative acknowledge
SYN                 equ       $16                 ; [^V] Synchronize
ETB                 equ       $17                 ; [^W]
CAN                 equ       $18                 ; [^X] Cancel
EM                  equ       $19                 ; [^Y]
SUB                 equ       $1A                 ; [^Z] Substitute
ESC                 equ       $1B                 ; [^[] Escape
FS                  equ       $1C                 ; [^\] Field separator
GS                  equ       $1D                 ; [^]] Group separator
RS                  equ       $1E                 ; [^^] Record separator
US                  equ       $1F                 ; [^_] Unit separator
SPC                 equ       $20                 ; Space
