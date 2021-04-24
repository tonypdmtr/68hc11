; EVB11 Demo Program
; A very simple example which implements a
; terminal echo program and writes the incomming
; and outgoing characters to some LED displays.

ACIA_CONTROL        equ       $9800
ACIA_STATUS         equ       $9800
ACIA_DATA           equ       $9801

PORTC               equ       $1003
PORTB               equ       $1004
PORTCL              equ       $1005
DDRC                equ       $1007

;*******************************************************************************

                    #VECTORS  $FFFE
                    dw        Start

;*******************************************************************************
                    #ROM      $E000
;*******************************************************************************

Start               proc
                    lds       #$FF

                    lda       #$03                ; Reset
                    sta       ACIA_CONTROL

                    lda       #$1D                ; 8 bits, odd, 1 stop, div 16 clocks
                    sta       ACIA_CONTROL

                    lda       #$FF
                    sta       DDRC
;                   bra       waitrx

;*******************************************************************************

waitrx              proc
Loop@@              lda       ACIA_STATUS
                    anda      #$01
                    bne       readrx
                    clra
Delay@@             deca
                    bne       Delay@@
                    bra       Loop@@

;*******************************************************************************

readrx              proc
                    ldb       ACIA_DATA
                    stb       PORTB

Loop@@              lda       ACIA_STATUS
                    anda      #$02
                    beq       Loop@@

                    stb       ACIA_DATA
                    stb       PORTC

                    bra       waitrx
