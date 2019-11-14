PORTD               equ       $1008               ; Port D data Register
DDRD                equ       $1009               ; D Data Direction Register
SPCR                equ       $1028               ; SPI Control Register
SPSR                equ       $1029               ; SPI Status Register
SPDR                equ       $102a               ; SPI Data Register
HPRIO               equ       $103c               ; Interrupt Priority

XMESS0              equ       $00                 ; Transmit Message Buffer 0
XMESS1              equ       $01                 ; Transmit Message Buffer 1
XMESS2              equ       $02                 ; Transmit Message Buffer 2
XMESS3              equ       $03                 ; Transmit Message Buffer 3
MESSCT              equ       $04                 ; Message Byte Count
PREPE               equ       $05                 ; CS, Pre, Pe Bit settings
EXTWR               equ       $0b                 ; Set if write takes Extra Time
WRACTV              equ       $0c                 ; Set if write still active after
                                                  ; message count goes to 00
XADDR               equ       $06                 ; Transmit Address
XDATLO              equ       $07                 ; Transmit Data low byte
XDATHI              equ       $08                 ; Transmit Data high bype
RDATLO              equ       $09                 ; Receive Data low byte
RDATHI              equ       $0a                 ; Receive Data high byte
