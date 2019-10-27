;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; BLINK.ASM - Jonathan Hill - 11/7/2004
; Blink and Beep example for the 68HC11E9
; This is an example superloop application
; University of Hartford College of Engineering
;*******************************************************************************

;                   #ListOff
;                   #Uses     711e9.inc
;                   #ListOn

LED_MAX             def       122
SPK_MAX             def       24

PORTA               def       $1000
PORTB               def       $1004
TFLG2               def       $1025
RTIF                equ       $40
PACTL               def       $1026

;*******************************************************************************
                    #RAM                          ; the data section
;*******************************************************************************
                    org       0

led_count           rmb       1
led_val             rmb       1
spk_count           rmb       1
spk_val             rmb       1
spk_req             rmb       1

;*******************************************************************************
                    #ROM                          ; Program starting point
;*******************************************************************************
                    org       $0100

Start               proc
                    lds       #$41
          ;-------------------------------------- ; Initialize the variables
                    clr       led_val             ; clear store for LED
                    clr       spk_val             ; clear store for speaker
                    lda       #LED_MAX            ; Initialize LED count
                    sta       led_count           ; to maximim and
                    clr       spk_count           ; turn off the speaker
                    clr       spk_req             ; clear the req flag
          ;-------------------------------------- ; Initialize ports and real time counter
                    clr       PORTA               ; clear LED
                    clr       PORTB               ; clear speaker
                    lda       PACTL               ; load control value
                    anda      #$FC                ; set RTI rate
                    sta       PACTL               ; update the register
;                   bra       Top

;*******************************************************************************
; The main loop in the program is surprisingly simple, just a series of
; subroutine calls that repeat.  In limiting the dependencies between tasks,
; there is little danger in shutting down a task by simply commenting it out in
; the source code.
;*******************************************************************************

;*******************************************************************************
; The Main Loop
;*******************************************************************************

Top                 proc
Loop@@              bsr       LedTask             ; run the LED task
                    bsr       SpkTask             ; run the speaker task
                    bsr       Delay               ; wait for timeout
                    bra       Loop@@              ; back for more

;*******************************************************************************
; The task controlling the LED matches the flowchart in Figure 2.  In most cases
; when this code is called, the first bne instruction passes execution to the
; rts instruction.  The exclusive-or function is used to toggle the LED state.

;*******************************************************************************
; The LED Task
;*******************************************************************************

LedTask             proc
                    dec       led_count           ; decrement LED count
                    bne       Done@@              ; exit from work?

                    lda       #LED_MAX            ; Reset the LED count
                    sta       led_count           ; to maximum

                    lda       #$10                ; LED bit
                    eora      led_val             ; toggle the LED
                    sta       led_val             ; save LED value
                    sta       PORTA               ; update output

                    lda       #$FF                ; Value to make a
                    sta       spk_req             ; make a request
Done@@              rts                           ; done for now

;*******************************************************************************
; The task controlling the speaker matches the flowchart in Figure 4.  Notice
; that by inserting blank lines, to form three blocks of instructions helps us
; to read the code and see the correspondence with the flowchart.  Please use
; techniques like this to make your programs more readable.

;*******************************************************************************
; The Speaker Task - Beep when asked to
;*******************************************************************************

SpkTask             proc
                    tst       spk_count           ; test the count
                    bne       Skip@@              ; update speaker?
                    tst       spk_req             ; test the request
                    bne       Start@@             ; start new beeo?
                    clr       PORTB               ; clear the port
                    rts                           ; done for now

Skip@@              dec       spk_count           ; decrement count
                    lda       #%11                ; Speaker bits
                    eora      spk_val             ; toggle the speaker
                    bra       Save@@

Start@@             clr       spk_req             ; clear the request
                    lda       #SPK_MAX            ; restart the count
                    sta       spk_count           ; value and
                    lda       #1                  ; value to turn
Save@@              sta       spk_val             ; save new speaker value
                    sta       PORTB               ; update the speaker
                    rts                           ; done for now

;*******************************************************************************
; This example uses the real time clock to control the overall loop time.  The
; real time clock is really just a counter that sets a flag named RTIF, in a
; periodic fashion, whether or not the flag is cleared.  This program does not
; use interrupts, rather t

;*******************************************************************************
; The delay gate - wait for clock to timeout
;*******************************************************************************

Delay               proc
Loop@@              lda       TFLG2               ; get the flag register
                    anda      #RTIF               ; mask out the timer flag
                    beq       Loop@@              ; and wait if not done
                    lda       #RTIF               ; if done then clear the
                    sta       TFLG2               ; flag and restart
                    rts                           ; done for now
