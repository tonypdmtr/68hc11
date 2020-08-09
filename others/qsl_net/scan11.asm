;*******************************************************************************
; SCAN11.ASM
;
; Convert 16 channels A/D data, put it in RAM,
; and send it every minute to serial in BCD format
;
; Created:      19-Oct-1997 fweld@augsburg.baynet.de
; Updated:      23-Oct-1997 dl3mhb@qsl.net
;
; Status:       NOT RELEASED
;*******************************************************************************

CR                  equ       13
LF                  equ       10

PORTB               equ       $1004
TMSK2               equ       $1024
TFLG2               equ       $1025
PACTL               equ       $1026
SPCR                equ       $1028
BAUD                equ       $102b
SCCR1               equ       $102c
SCCR2               equ       $102d
SCSR                equ       $102e
SCDR                equ       $102f
ADCTL               equ       $1030
ADR1                equ       $1031
OPTION              equ       $1039
          ;--------------------------------------
TICK                equ       $0102               ; event counter
TFLAG               equ       $0104               ; timer flag
SEQU                equ       $0105               ; counter
ADBUFF              equ       $0110               ; buffer for 16 bytes
ASCII               equ       $0120               ; ascii string starts here
          ;--------------------------------------
B9600               equ       $b0                 ; hopefully for 8 MHz clock
TRENA               equ       $0c                 ; X/R enable

REVECT              equ       $00eb               ; revectored RTI
STACK               equ       $00ff               ; good to know where it is

;*******************************************************************************
                    #ROM                          ; Main program section
;*******************************************************************************
                    org       $B600

Start               proc
                    lds       #STACK
                    jsr       SetupISR
                    jsr       SetupSCC

                    ldx       #Msg@@              ; say hello
                    jsr       Print

                    ldx       #0
                    stx       TICK
                    inc       TFLAG               ; make one message w/o waiting
                    clr       SEQU                ; counter start at 0

                    lda       OPTION
                    ora       #%10000000          ; a/d converter on
                    sta       OPTION

Loop@@              tst       TFLAG
                    beq       Loop@@
                    bsr       Sample              ; 16 channels to RAM
                    bsr       Prepare             ; make it BCD

                    ldx       #ASCII              ; point to beginning of string
                    jsr       Print
                    clr       TFLAG
                    bra       Loop@@              ; again

Msg@@               fcs       CR,LF,"%SCAN11-I-V0.1 [C]1997 FWK"

;*******************************************************************************
; Subroutines start here
;*******************************************************************************

Sample              proc
                    ldy       #ADBUFF             ; pointer
                    clra                          ; select first external ch
                    sta       ADCTL               ; reset all bits
Loop@@              lda       PORTB
                    anda      #%00111111          ; reset 6:7 on PORTB
_1@@                sta       PORTB
                    bsr       ConvertAD
                    sta       ,y                  ; store data in RAM
                    iny                           ; point next
                    lda       PORTB               ; get pattern
                    adda      #%01000000          ; next ext channel
                    bne       _1@@
                    inc       ADCTL               ; next internal ch
                    lda       ADCTL               ; check it
                    anda      #%00000111          ; mask off
                    cmpa      #%00000100          ; last ?
                    bne       Loop@@              ; nope
                    rts                           ; out of here

;*******************************************************************************

ConvertAD           proc
                    lda       ADCTL
                    sta       ADCTL               ; trigger converter
Loop@@              tst       ADCTL
                    bpl       Loop@@
                    lda       ADR1                ; return result in a
                    rts

;*******************************************************************************

Prepare             proc
                    ldx       #ASCII              ; to be stored in BCD

                    lda       SEQU
                    bsr       ConvertBCD
                    lda       #':'
                    sta       ,x                  ; put it in string
                    inx

                    ldy       #ADBUFF             ; pointer to converted data

Loop@@              lda       #' '                ; space
                    sta       ,x                  ; put it in string
                    inx

                    lda       ,y                  ; get byte
                    bsr       ConvertBCD          ; translate it
                    iny                           ; next converted value
                    cpy       #ASCII              ; see if last point
                    blo       Loop@@

                    clr       ,x                  ; terminator for print
                    rts

;*******************************************************************************

ConvertBCD          proc
                    ldb       #100
                    bsr       Convert@@
                    ldb       #10
                    bsr       Convert@@
                    ldb       #1
;                   bra       Convert@@
          ;--------------------------------------
Convert@@           clr       ,x
Loop@@              cba
                    blo       Done@@
                    sba
                    inc       ,x
                    bra       Loop@@
Done@@              psha
                    lda       ,x                  ; get counted
                    adda      #'0'                ; make it ascii
                    sta       ,x                  ; store ascii
                    inx
                    pula
                    rts

;*******************************************************************************

Print               proc
Loop@@              lda       ,x
                    beq       NewLine@@           ; if 0 terminator
                    bmi       Done@@              ; if $80 terminator (negative)
_1@@                tst       SCSR
                    bpl       _1@@                ; wait until ready
                    sta       SCDR                ; send accu
                    inx
                    bra       Loop@@
NewLine@@           ldx       #Msg@@
                    bra       Print
Done@@              equ       :AnRTS

Msg@@               db        CR,LF,$80

;*******************************************************************************
; System initialisation routines
;*******************************************************************************

SetupSCC            proc
                    clr       SPCR
                    lda       #B9600
                    sta       BAUD
                    lda       #TRENA
                    sta       SCCR2
                    rts

;*******************************************************************************

SetupISR            proc
                    sei
                    ldd       #RTI_Handler
                    std       REVECT+1
                    lda       #$7e                ; jmp instruction
                    sta       REVECT

                    lda       #%00000011          ; 32,8 ms rate
                    sta       PACTL
                    lda       #%01000000          ; enable RTII
                    sta       TMSK2
                    sta       TFLG2
                    cli                           ; allow interrupts
                    rts

;*******************************************************************************

RTI_Handler         proc
                    ldx       TICK                ; get word
                    inx                           ; count_up
                    cpx       #1831               ; 32,768x1831=1 minute
                    blo       Go@@
                    ldx       #0                  ; clx is not valid code
                    inc       TFLAG               ; flag minute
                    inc       SEQU                ; counter
Go@@                stx       TICK
                    lda       #%01000000          ; allow further ints
                    sta       TFLG2
                    rti

;*******************************************************************************
