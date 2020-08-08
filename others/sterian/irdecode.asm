;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************

; This program is used to decode an infrared remote control.
; This program MUST BE CUSTOMIZED and completed by the student to match a
; particular remote control protocol.

MHZ                 equ       8
                    #ListOff
                    #Uses     exp-f1.inc
                    #ListOn

REM_CODE            def       0                   ; What should REM_CODE be for your remote control?
                    #Hint     Remote control code REM_CODE = {REM_CODE(x)}

OUTLHL              equ       $FFB2
OUTRHL              equ       $FFB5

; Useful constants to make the code more readable

START_BIT           def       0                   ; On-symbols
LONG_ON_TIME        def       1
SHORT_ON_TIME       def       2
NOISE_PULSE         def       3
TIMEOUT             def       0                   ; Off-symbols
SHORT_OFF_TIME      def       1
LONG_OFF_TIME       def       2

;*******************************************************************************
                    #RAM
;*******************************************************************************

start_time          rmb       2
bit_count           rmb       1

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
          #ifdef TONYP
                    lds       #STACKTOP

                    clrd
                    clrx
                    clry
          #endif
;                   bra       StartBit

;*******************************************************************************
; This example program waits for a start bit, waits for some
; silence after the start bit, then counts how many on-durations
; occur before the signal stops completely. This number of
; on-durations is then displayed as an integer. NOTE: This isn't
; how remote controls work. But it does display the type of
; concepts you need to follow.

StartBit            proc
                    jsr       WaitForStartBit
          ;-------------------------------------- ; After start bit comes silence
PostStartBit@@      bsr       MeasureOffTime
                    jsr       OffSymbol
                    cmpa      #START_BIT          ; Is it a timeout already?
                    beq       StartBit            ; If so, it was spurious noise.
          ;--------------------------------------
          ; We've had a start bit and the silence that comes after
          ; it. Now let's count on-intervals. The bit_count memory
          ; location will be used for this purpose.
          ;--------------------------------------
                    clr       bit_count

NextBit@@           bsr       MeasureOnTime
                    jsr       OnSymbol
                    cmpa      #START_BIT          ; What's a start bit doing here???
                    beq       PostStartBit@@
                    cmpa      #NOISE_PULSE        ; This on-time was too short...it's noise
                    beq       StartBit            ; so ignore the whole thing.
          ;-------------------------------------- ; Anything else is a bona fide on-pulse
                    inc       bit_count
          ;-------------------------------------- ; Now wait for inter-pulse silence
                    bsr       MeasureOffTime
                    jsr       OffSymbol
                    cmpa      #TIMEOUT            ; Timeout means we're done
                    bne       NextBit@@
          ;-------------------------------------- ; All done
                    lda       bit_count           ; Display bit count
                    jsr       OUTLHL
                    lda       bit_count
                    jsr       OUTRHL

                    swi

;*******************************************************************************
; ChangeCnt -- Count the number of state changes at PE:7 in 100us
;
; This subroutine counts the number of times Port E bit 7 changes in the
; span of 100 microseconds. This subroutine will function properly for
; change rates up to 10, representing an input frequency of up to 50 kHz.
;
; Inputs:
;     - None
;
; Outputs:
;     - The D register contains the number of state changes at PE:7
;       in 100us.
;
; Modifies:
;     - X is set to REGS
;*******************************************************************************

ChangeCnt           proc
                    ldx       #REGS
                    ldd       [TCNT,x
                    addd      #BUS_KHZ/10
                    std       [TOC1,x
                    bclr      [TFLG1,x,$7F
                    clrb
                    brset     [PORTE,x,$80,Zero@@
One@@               brset     [TFLG1,x,$80,Done@@
                    brclr     [PORTE,x,$80,One@@
                    incb
Zero@@              brset     [TFLG1,x,$80,Done@@
                    brset     [PORTE,x,$80,Zero@@
                    incb
                    bra       One@@
Done@@              clra
                    rts

;*******************************************************************************
; IsOsc -- Determine whether or not the infrared detector is oscillating
;
; This subroutine returns A=1 if the infrared detector connected to PE:7
; is deemed to be oscillating. It does so by examining the number of
; state changes at PE:7 in 100us. If there are at least 2 state changes
; in 100us (i.e., the input is oscillating at at least 10 kHz) then
; it is assumed that oscillations are present.
;
; Inputs:
;    - The D register contains the number of state changes in 100us
;      (i.e., the return value from ChangeCnt)
;
; Outputs:
;    - The A register is set to 1 if there are at least 2 state
;      changes (i.e., the input signal is oscillating at 10 kHz or
;      above) and is set to 0 otherwise.
;
; Modifies:
;    - No registers modified
;*******************************************************************************

IsOsc               proc
                    cmpd      #2
                    blo       Done@@              ;if branch is taken A = 0
                    lda       #1
Done@@              rts

;*******************************************************************************
; MeasureOnTime -- Measure the duration of an on-interval
;
; This subroutine measures the duration of an on-interval, i.e., a period
; of time in which the infrared detector is oscillating. It is assumed at
; the start of this subroutine that oscillations have been detected. This
; subroutine waits until there are no oscillations detected (as determined
; by IsOsc) and then returns the number of cycles that have elapsed.
;
; This subroutine can only time on-interval durations up to 65535 cycles
; after which rollover effects will give incorrect results.
;
; Note that when this subroutine returns, it is true that IsOsc has returned
; 0 hence oscillations have ceased.
;
; Inputs:
;     - None
;
; Outputs:
;     - The D register contains the number of cycles that elapse
;       until oscillations cease.
;
; Modifies:
;     - start_time memory location used
;     - X is set to REGS
;*******************************************************************************

MeasureOnTime       proc
                    ldd       TCNT
                    std       start_time

Loop@@              bsr       ChangeCnt
                    bsr       IsOsc
                    tsta
                    bne       Loop@@

                    ldd       TCNT
                    subd      start_time
                    rts

;*******************************************************************************
; MeasureOffTime -- Measure the duration of an off-interval
;
; This subroutine measures the duration of an off-interval, i.e., a period
; of time in which the infrared detector is not oscillating. It is assumed at
; the start of this subroutine that oscillations are not present. This
; subroutine waits until there are oscillations detected (as determined
; by IsOsc) or after 65536 cycles elapse (about 32ms at a 2 MHz E-clock).
;
; If this subroutine returns because oscillations are detected, the number
; of cycles that have elapsed (i.e., the duration of the off-interval)
; are returned in D. If this subroutine returns because 65536 cycles have
; elapsed (i.e., timeout) then this subroutine returns with D=0.
;
; Inputs:
;     - None
;
; Outputs:
;     - The D register contains the number of cycles that elapse
;       until oscillations begin. If 65536 cycles go by without
;       detecting oscillation, the D register will be 0.
;
; Modifies:
;     - start_time memory location used
;     - X is set to REGS
;     - The OC2 peripheral is used (OC2F/TOC2)
;*******************************************************************************

MeasureOffTime      proc
                    ldx       #REGS
                    ldd       [TCNT,x
                    std       start_time

                    std       [TOC2,x             ; Use OC2 for timeout
                    bclr      [TFLG1,x,%10111111  ; Clear OC2F

Loop@@              brset     [TFLG1,x,%01000000,Timeout@@

                    bsr       ChangeCnt
                    bsr       IsOsc
                    tsta
                    beq       Loop@@

                    ldd       [TCNT,x
                    subd      start_time
                    rts

Timeout@@           clrd
                    rts

;*******************************************************************************
; OnSymbol -- Convert on-time duration to a symbol
;
; This subroutine converts the duration of an on-interval (in cycles, as
; returned by MeasureOnTime) into a "symbol", i.e., an indication of what
; information was conveyed by this interval.
;
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; ! THIS SUBROUTINE MUST BE ADAPTED TO YOUR OWN REMOTE CONTROL PROTOCOL !
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
; Inputs:
;    - The D register contains the duration of an on-interval in cycles
;
; Outputs:
;    - The A register contains:
;         START_BIT     if the on-interval represents a start bit
;         SHORT_ON_TIME if the on-interval represents a "short" on-time
;         LONG_ON_TIME  if the on-interval represents a "long" on-time
;         NOISE_PULSE   if the on-interval is just "noise" that should be ignored
;
; Modifies:
;    - No registers modified
;*******************************************************************************

OnSymbol            proc
                    cmpd      #REM_CODE
                    blo       _1@@
                    lda       #START_BIT          ; It's a start bit.
                    rts

_1@@                cmpd      #REM_CODE
                    blo       _2@@
                    lda       #LONG_ON_TIME
                    rts

_2@@                cmpd      #REM_CODE
                    blo       _3@@
                    lda       #SHORT_ON_TIME
                    rts

_3@@                lda       #NOISE_PULSE
                    rts

;*******************************************************************************
; OffSymbol -- Convert off-time duration to a symbol
;
; This subroutine converts the duration of an off-interval (in cycles, as
; returned by MeasureOffTime) into a "symbol", i.e., an indication of what
; information was conveyed by this interval.
;
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; ! THIS SUBROUTINE MUST BE ADAPTED TO YOUR OWN REMOTE CONTROL PROTOCOL !
; !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;
; Inputs:
;    - The D register contains the duration of an off-interval in cycles
;      or perhaps 0 to indicate that no oscillations have been detected
;      within 65536 cycles.
;
; Outputs:
;    - The A register contains:
;         TIMEOUT        if the off-interval represents timeout
;         SHORT_OFF_TIME if the off-interval represents a "short" off-time
;         LONG_OFF_TIME  if the off-interval represents a "long" off-time
;
; Modifies:
;    - No registers modified
;*******************************************************************************

OffSymbol           proc
                    cmpd      #0
                    bhi       _1@@
                    lda       #TIMEOUT
                    rts

_1@@                cmpd      #REM_CODE
                    bhi       _2@@
                    lda       #SHORT_OFF_TIME
                    rts

_2@@                lda       #LONG_OFF_TIME
                    rts

;*******************************************************************************
; WaitForStartBit -- Wait for a start bit to be detected
;
; This subroutine waits until a start bit is detected, as determined by
; OnSymbol.
;
; Inputs:
;    - None
;
; Outputs
;    - None
;
; Modifies:
;    - X,D,start_time memory location
;*******************************************************************************

WaitForStartBit     proc
Loop@@              jsr       ChangeCnt           ; First, wait for oscillations
                    bsr       IsOsc
                    tsta
                    beq       Loop@@
          ;-------------------------------------- ; We have oscillations, now time duration
                    bsr       MeasureOnTime       ; D<--duration
                    bsr       OnSymbol            ; A<--symbol
                    cmpa      #START_BIT          ; We only want a start bit.
                    bne       Loop@@              ; Spurious bit...ignore it.
                    rts

;*******************************************************************************
                    end       Start
;*******************************************************************************
