;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************

; This program is used to reverse engineer infrared remote control protocols.
; Much documentation needs to be added by the student.

MHZ                 equ       8
                    #ListOff
                    #Uses     exp-f1.inc
                    #ListOn

OUTSTR              equ       $FFC7
OUTLHL              equ       $FFB2
OUTRHL              equ       $FFB5

;*******************************************************************************
                    #RAM
;*******************************************************************************

.table              rmb       2
start_time          rmb       2

;*******************************************************************************
                    #XRAM
;*******************************************************************************

timing_table        rmb       256

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    jsr       InitTimingTable
          ;-------------------------------------- ; Wait for oscillations to begin
WaitToStart@@       bsr       ChangeCnt
                    jsr       IsOsc
                    tsta
                    beq       WaitToStart@@
          ;--------------------------------------
          ; Oscillations have started. Time the width
          ; of the first oscillating period.
          ;--------------------------------------
NextBit@@           ldx       #REGS
                    ldd       [TCNT,x
                    std       start_time
          ;-------------------------------------- ; Wait for oscillations to stop
WaitForStop@@       bsr       ChangeCnt
                    jsr       IsOsc
                    tsta
                    bne       WaitForStop@@
          ;-------------------------------------- ; Compute time of oscillating period and add to table
                    ldd       [TCNT,x
                    subd      start_time
                    jsr       AppendTable
          ;-------------------------------------- ; Wait for up to 30ms for non-oscillations to cease
                    ldd       [TCNT,x
                    std       start_time
                    addd      #BUS_KHZ*30         ;(overflows on 4MHz bus)
                    std       [TOC2,x
                    bclr      [TFLG1,x,$BF
          ;--------------------------------------
WaitForStart@@      bsr       ChangeCnt
                    bsr       IsOsc
                    tsta
                    bne       OscRestart@@
                    brclr     [TFLG1,x,$40,WaitForStart@@
                    bra       Done@@
OscRestart@@        ldd       [TCNT,x
                    subd      start_time
                    bsr       AppendTable
                    bra       NextBit@@
Done@@              ldx       #Msg@@
                    jsr       OUTSTR
                    ldd       .table
                    bsr       PrintD
                    swi

Msg@@               fcc       'End of table: ',4

;*******************************************************************************

PrintD              proc
                    pshd
                    jsr       OUTLHL
                    pula
                    psha
                    jsr       OUTRHL
                    puld
          ;--------------------------------------
                    psha
                    pshb
                    tba
                    jsr       OUTLHL
                    pulb
          ;--------------------------------------
                    pshb
                    tba
                    jsr       OUTRHL
                    pulb
                    pula
                    rts

;*******************************************************************************

ChangeCnt           proc
                    ldx       #REGS
                    ldd       [TCNT,x
                    addd      #BUS_KHZ/10
                    std       [TOC1,x
                    bclr      [TFLG1,x,$7F
                    clrb
                    brset     [PORTE,x,$80,WaitForZero@@
WaitForOne@@        brset     [TFLG1,x,$80,Done@@
                    brclr     [PORTE,x,$80,WaitForOne@@
                    incb
WaitForZero@@       brset     [TFLG1,x,$80,Done@@
                    brset     [PORTE,x,$80,WaitForZero@@
                    incb
                    bra       WaitForOne@@
Done@@              clra
                    rts

;*******************************************************************************

IsOsc               proc
                    cmpd      #1
                    blo       Done@@              ;if branch is taken A = 0
                    lda       #1
Done@@              rts

;*******************************************************************************

AppendTable         proc
                    pshx
                    ldx       .table
                    cpx       #timing_table+::timing_table
                    bhs       TableOverflow
                    std       ,x
                    inx:2
                    stx       .table
                    pulx
                    rts

;*******************************************************************************

InitTimingTable     proc
                    ldx       #timing_table
                    ldd       #$FFFF
Loop@@              std       ,x
                    inx:2
                    cpx       #timing_table+::timing_table
                    blo       Loop@@
                    ldx       #timing_table
                    stx       .table
                    rts

;*******************************************************************************

TableOverflow       proc
                    ldx       #Msg@@
                    jsr       OUTSTR
                    swi

Msg@@               fcc       'Table overflow!',4

;*******************************************************************************
                    end       Start
;*******************************************************************************
