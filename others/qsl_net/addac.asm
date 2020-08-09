;*******************************************************************************
; ADDAC.ASM
;
; Control MAXIM 512 via MOSI port
; 23-Dec-1997 dl3mhb@qsl.net
;*******************************************************************************

PORTC               equ       $1003
DDRC                equ       $1007
PORTD               equ       $1008
DDRD                equ       $1009
SPCR                equ       $1028
SPSR                equ       $1029
SPDR                equ       $102A
ADCTL               equ       $1030
ADR1                equ       $1031
OPTION              equ       $1039

STACK               equ       $00ff

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       $B600

Start               proc
                    lds       #STACK              ; we know where he is

                    lda       OPTION
                    ora       #%10000000          ; ADC on
                    sta       OPTION
                    clr       ADCTL               ; reset A/D registers

                    lda       #%11111111          ; make 'em outputs
                    sta       DDRC

                    lda       #%00111000          ; make SPI to output
                    sta       DDRD
                    lda       #%00100000
                    sta       PORTD               ; and set SS high

                    lda       #%01011100          ; SPE ! MSTR ! SCK ! CPHA ! clock
                    sta       SPCR

Loop@@              bsr       Convert             ; into A
                    sta       PORTC
                    bsr       Daca                ; go to DA on port A
                    bra       Loop@@

;*******************************************************************************

Convert             proc
                    lda       ADCTL
                    sta       ADCTL
Loop@@              tst       ADCTL
                    bpl       Loop@@
                    lda       ADR1
                    rts

;*******************************************************************************

Daca                proc
                    clr       PORTD               ; enable SS
                    ldb       #%00110001          ; DAC A on and select
                    stb       SPDR                ; store control byte
                    bsr       ?WaitSPSR
                    sta       SPDR                ; send data byte
                    bsr       ?WaitSPSR
                    ldb       #%00100000          ; execute instruction
                    stb       PORTD
                    rts

;*******************************************************************************

?WaitSPSR           proc
Loop@@              tst       SPSR
                    bpl       Loop@@
                    rts
