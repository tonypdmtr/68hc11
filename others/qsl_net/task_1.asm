;*******************************************************************************
; TASK_1.ASM
; Use the RTI (real time interrupt) for a task switcher (sceduler)
; Created 1997-09-25 fweld@augsburg.baynet.de
;*******************************************************************************

PORTB               equ       $1004               ; just to show ...
TMSK2               equ       $1024               ; timer masks
TFLG2               equ       $1025               ; timer flags
PACTL               equ       $1026               ; for interrupt rate
;RTIVEC             equ       $bff0               ; interrupt vector in bootstrap mode
REVECT              equ       $00eb               ; revectored to RAM
STACK               equ       $01ff               ; high stack

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       $B600               ; in EEPROM

Start               proc
                    sei                           ; disable interrupts

                    lds       #STACK              ; fix stack
                    lda       #%00000001          ; red LED on
                    sta       PORTB

                    ldd       #RTI_Handler
                    std       REVECT+1            ; bootstrap revectored
                    lda       #$7e                ; JMP instruction
                    sta       REVECT

                    lda       #%00000011          ; 32,8 ms rate
                    sta       PACTL               ; set rate

                    lda       #%01000000          ; enable RTII
                    sta       TMSK2
                    sta       TFLG2               ; kill flag

                    cli                           ; allow interrupts

                    lda       PORTB
                    ora       #%0000010           ; yellow on others off
                    sta       PORTB

                    bra       *                   ; loop here

;*******************************************************************************

RTI_Handler         proc
                    inc       PORTB
                    lda       #%01000000          ; enable RTII
                    sta       TFLG2
                    rti
