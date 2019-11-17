;*******************************************************************************
;* Program   : SONG.ASM
;* Programmer: Ruth Herbst
;* Purpose   : Plays "The Entertainer" on the speaker attached to TOC5
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;* Status    : Public Domain
;* History   : 03.04.02 v1.00 Original
;*******************************************************************************

#ifmain ;-----------------------------------------------------------------------
                    #ListOff
                    #Uses     mcu.inc
                    #ListOn
#endif ;------------------------------------------------------------------------

;*******************************************************************************
; Data Section
;*******************************************************************************

;*******************************************************************************
                    #RAM                          ;.area DATA (rel,con)
;*******************************************************************************

Song                rmb       38
Note                rmb       2

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    lds       #STACKTOP

                    ldx       #Song

                    ldd       #6810
                    std       ,x

                    ldd       #6428
                    std       2,x

                    ldd       #6068
                    std       4,x

                    ldd       #3822
                    std       6,x

                    ldd       #6068
                    std       8,x

                    ldd       #3822
                    std       10,x

                    ldd       #6068
                    std       12,x

                    ldd       #3822
                    std       14,x

                    ldd       #0002
                    std       16,x

                    ldd       #3822
                    std       18,x

                    ldd       #3405
                    std       20,x

                    ldd       #3034
                    std       22,x

                    ldd       #3822
                    std       24,x

                    ldd       #3405
                    std       26,x

                    ldd       #3034
                    std       28,x

                    ldd       #4050
                    std       30,x

                    ldd       #3405
                    std       32,x

                    ldd       #3822
                    std       34,x

                    ldd       #0003
                    std       36,x
;                   bra       Main

;*******************************************************************************

Main                proc
                    ldx       #Song
                    ldd       ,x
                    std       Note

                    bsr       TOC5INI             ; jump to subroutine
                    cli                           ; enable interrupts

Loop@@              bsr       Wait250ms
                    inx:2
                    ldd       ,x
                    std       Note
                    cmpd      #3
                    bne       Loop@@

                    bra       Main

;*******************************************************************************
; TMRINI subroutine
;*******************************************************************************

TOC5INI             proc
                    pshd

                    lda       PACTL
                    anda      #%11111011
                    sta       PACTL

                    ldd       #Note
                    addd      TCNT
                    std       TOC5

                    lda       TCTL1
                    anda      #%11111100
                    ora       #%00000001
                    sta       TCTL1

                    lda       TMSK1               ; use TMSK to enable the TOC5 interrupt
                    ora       #%00001000
                    sta       TMSK1

                    lda       TFLG1
                    ora       #%00001000
                    sta       TFLG1

                    puld
                    rts

;*******************************************************************************
; TOC5ISR Subroutine
;*******************************************************************************

TOC5ISR             proc
                    ldd       TOC5
                    addd      Note
                    std       TOC5

                    lda       #%00001000
                    sta       TFLG1               ; clear the TOI interrupt flag
                    rti

;*******************************************************************************

Wait250ms           proc
                    pshx
                    psha
                    tpa                           ; save condition codes

                    ldx       #250                ; wait 250 milliseconds
Loop@@              bsr       Wait1ms             ; local label
                    dex
                    bne       Loop@@              ; Branch if not equal to

                    tap
                    pula
                    pulx
                    rts

;*******************************************************************************

                              #Cycles
Wait1ms             proc
                    pshx
                    psha
                    tpa
                              #Cycles
                    ldx       #DELAY@@
Loop@@              dex
                    nop:2
                    bne       Loop@@
                              #temp :cycles
                    tap
                    pula
                    pulx
                    rts

DELAY@@             equ       BUS_KHZ-:cycles-:ocycles/:temp

;*******************************************************************************

                    @vector   Vtoc5,TOC5ISR
