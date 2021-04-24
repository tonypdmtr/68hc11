;*******************************************************************************
; ARM_5.ASM
;
; Driver for three SERVOs connected to PA3 .. PA5
; Servo on port PA6 will be initialized - as spare
;*******************************************************************************
; Created:      21-Sep-1997     fweld@augsburg.baynet.de
; Finished:     26-Sep-1997     fweld
; Modified:     10-Oct-1997     fweld removed : and blanks for compat.
;               12-Oct-1997     fweld removed trace
;*******************************************************************************

OC1M                equ       $100c
OC1D                equ       $100d
TOC1                equ       $1016               ; timer register
TOC2                equ       $1018               ; timer out compare register for PA6
TOC3                equ       $101a               ; ... for PA5
TOC4                equ       $101c               ; ... for PA4
TOC5                equ       $101e               ; ... for PA3
TCTL1               equ       $1020
ADCTL               equ       $1030
ADR1                equ       $1031
OPTION              equ       $1039

;*******************************************************************************

STACK               equ       $01ff               ; fix the stack top

;*******************************************************************************
; Constants for servos (limits) etc.
;*******************************************************************************

CW90                equ       $fb00               ; cw 90 deg = 0,7 ms
CENTER              equ       $f480               ; center pos = 1,5 ms
CCW90               equ       $ee00               ; ccw 90 deg = 2,3 ms
UP                  equ       $f100
DOWN                equ       $f600
OPEN                equ       $f700
CLOSE               equ       $f400

;*******************************************************************************
                    #ROM      $B600               ; start of EEPROM
;*******************************************************************************

Start               proc
                    lds       #STACK              ; stack top
                    lda       OPTION
                    ora       #%10000000          ; a/d Converter on
                    sta       OPTION

                    bsr       InitTOC             ; set up all timer routines

                    ldx       #Home
                    bra       Loop@@              ; first move home

Go@@                bsr       Convert             ; wait there for any object

                    ldx       #Table              ; get table pointer
Loop@@              ldd       ,x                  ; fetch first word
                    beq       Go@@                ; this was last so reset x-index
                    std       TOC5
                    ldd       2,x
                    std       TOC4
                    ldd       4,x
                    std       TOC3
                    ldd       6,x
                    std       TOC2
                    ldy       8,x

                    bsr       Pause

                    ldb       #10                 ; point to next field
                    abx                           ; w/ x-index
                    bra       Loop@@

;*******************************************************************************
; Subroutines
;*******************************************************************************

Convert             proc
Go@@                clr       ADCTL               ; select CH0
Loop@@              tst       ADCTL               ; conversion done
                    bpl       Loop@@              ; no
                    lda       ADR1
                    cmpa      #$b0                ; loop here until object seen
                    bhi       Go@@
                    rts

;*******************************************************************************

Pause               proc
Go@@                clra
Loop@@              deca
                    bne       Loop@@
                    dey
                    bne       Go@@
                    rts

;*******************************************************************************
; Initialize patterns and routines
;*******************************************************************************

InitTOC             proc
                    lda       #$78                ; mask for OC2/OC5
                    sta       OC1M
                    clr       OC1D                ; off status
                    lda       #$ff                ; on condition
                    sta       TCTL1
                    clrd                          ; start timer on OC1
                    std       TOC1
                    rts

;*******************************************************************************
; These are the coordinates to move to
;*******************************************************************************
; x-axis y-axis claw   spare   delay

Table               dw        CCW90,DOWN,OPEN,CENTER,$0100  ; time never 0
                    dw        CCW90,DOWN,CLOSE,CENTER,$0200
                    dw        CCW90,UP,CLOSE,CENTER,$0100
                    dw        CW90,UP,CLOSE,CENTER,$0500
                    dw        CW90,UP,OPEN,CENTER,$0100
Home                dw        CCW90,UP,OPEN,CENTER,$0500
                    dw        CCW90,DOWN,OPEN,CENTER,$0100
                    dw        $0000,$0000,$0000,$0000,$0000  ; termination
