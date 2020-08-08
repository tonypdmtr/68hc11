;*******************************************************************************
;* Module    : SERVO8.ASM
;* Programmer: Alan Kilian
;* Purpose   : HC11 RC Servo routines
;*           : This program demonstrates the use of a HC11 processor
;*           : to control 8 Model airplane type servos. The servos
;*           : are updated every 20 milliseconds and their individual
;*           : pulses do not jitter more than 1 microsecond.
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;* Status    : FREEWARE
;* Note(s)   : This program is written for the MC68HC811E2 processor running
;*           : in single-chip mode. It is designed for use with a processor
;*           : running with an 8 MHz crystal. If you are using a different
;*           : crystal frequency you will need to re-compute all of the
;*           : timing values in this code.
;*           : The structure, serial I/O, and command processor portions of
;*           : this program evolved from the program HEXLOB40 written by
;*           : Fred Martin and Randy Sargent and we thank them greatly.
;* History   : 93.02.11 v1.00 Created. Tested out one pulse.
;*           : 93.02.15       First cut of eight pulses.
;*           : 93.02.16       Getting eight good clean pulses.
;*           : 93.02.17       Added sort routine and tons of comments.
;*           :                Added user input routine.
;*           : 93.02.18       Debugged the whole thing and it was no fun at all.
;*           :                Removed all the printing routines and debug stuff.
;*           : 93.02.20       Removed FCB from RAM area. Don't do that.
;*******************************************************************************
;*
;* Use: Send ASCII characters over the serial line to control the servos.
;*
;*      The format is like this:
;*
;*      s0000    Sets servo 00 to pulse width $00 which is the minimum
;*               available pulse width.
;*      s0080    Sets servo 00 to pulse width $80 HEX which is the
;*               middle width pulse
;*      s00FF    Sets servo 00 to pulse width $FF HEX which is the
;*               largest available pulse width
;*      s0100    Servo one
;*      r        Resets all servos to a pulse width $80
;*
;*******************************************************************************

REGS                def       0
                    #ListOff
                    #Uses     811e2.inc
                    #ListOn

;-------------------------------------------------------------------------------
; Masks for serial port
;-------------------------------------------------------------------------------

PORTD_WOM           equ       $20                 ; Wire-OR mode
BAUD9600            equ       $B0                 ; 9600 Baud what else?

;*******************************************************************************
;* Zero page RAM definitions. Do not use FCB here. It will stomp EEBOOT20.
;*******************************************************************************

;*******************************************************************************
                    #RAM
;*******************************************************************************

values              rmb       8                   ; The place where the unsorted values live
servo_num           rmb       2                   ; A safe place for the servo number
smasks              rmb       8                   ; A safe place to put sorted masks
svalues             rmb       8                   ; A safe place to put sorted values
dvalues             rmb       8                   ; A safe place to put sorted delta values
newvals             rmb       1                   ; Alert the interrupt routine about new values

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    clr       XINIT
                    lds       #STACKTOP           ; Set stack at the top of ram

                    bclr      SPCR,PORTD_WOM      ; initialize serial port
                    lda       #BAUD9600           ; turn off wire-or mode
                    sta       BAUD                ; Set the port for 9600 baud
                    lda       #TE.|RE.            ; Transmit, Receive ENAble
                    sta       SCCR2               ; Enable the serial subsystem

                    lda       #1
                    sta       newvals             ; Set newvals the first time through

                    clr       servo_num           ; Zero out the high byte of servnum

                    clrx                          ; set the servos to the middle
                    lda       #128                ; of the range of pulse widths
Loop@@              sta       values,x
                    inx
                    cmpx      #7
                    bls       Loop@@
          ;-------------------------------------- ; Set up interrupt OC2 once every 20 msec
                    lda       #%01000000          ; Set up the OC2 interrupt to
                    sta       TCTL1               ; generate an interrupt once
                    sta       TFLG1               ; every 20 milliseconds
                    sta       TMSK1
                    cli                           ; Turn on interrupts
;                   bra       MainLoop

;*******************************************************************************

MainLoop            proc
MainLoop@@          lda       #CR                 ; return character
                    bsr       PutChar

                    lda       #LF                 ; linefeed character
                    bsr       PutChar

                    lda       #'>'                ; prompt character
                    bsr       PutChar

Loop@@              bsr       GetChar             ; Keep getting chars until you get one
                    cmpa      #'a'                ; that is alphabetic.
                    blo       Loop@@              ; If the char was less than 'a' ignore

                    cmpa      #'r'                ; the command is an 'r' reset values
                    beq       ResetServos

                    cmpa      #'s'                ; The command is an 's' set a new value
                    bne       MainLoop@@          ; Do it all over again
          ;-------------------------------------- ; set servo value
                    bsr       GetByte             ; Get the servo number
                    sta       servo_num+1

                    ldx       servo_num           ; And get it into the X register
                    bsr       GetByte             ; Get the pulse width
                    sta       values,x            ; And save it in the values list

                    lda       #1                  ; Alert the interrupt handler that there are
                    sta       newvals             ; new values in the values list

                    bra       MainLoop@@          ; Go back to the command loop

;*******************************************************************************

ResetServos         proc
                    clrx
Loop@@              lda       #128                ; Reset the servo values to
                    sta       values,x            ; a nice middle pulse width
                    inx
                    cmpx      #7
                    bls       Loop@@
                    bra       MainLoop            ; Go back to the command loop

;*******************************************************************************

GetByte             proc
                    bsr       GetChar             ; read 2 chars from the serial port and change
                    cmpa      #'A'                ; them into a one byte value in accumulator A
                    blo       Hi@@                ; This routine destroys accumulator B
                    suba      #'A'-10             ; so watch it.
Hi@@                asla:4
                    tab
                    bsr       GetChar             ; Get the second character which is the least
                    cmpa      #'A'                ; significant 4 bits of the value
                    blo       Lo@@
                    suba      #'A'-10
Lo@@                anda      #$0f
                    aba
                    rts

;*******************************************************************************

GetChar             proc
                    brclr     SCSR,RDRF.,*        ; Read a character from the serial port and put
                    lda       SCDR
                    anda      #$7f
                    rts

;*******************************************************************************

PutChar             proc
Loop@@              tst       SCSR                ; Send the character in accumulator A
                    bpl       Loop@@              ; out the serial port.
                    sta       SCDR
                    rts

;*******************************************************************************

OC2_Handler         proc
                    ldd       #BUS_KHZ*20         ; Once every 20 milliseconds
                    addd      TOC2                ; We need to generate an interrupt
                    std       TOC2
                    bclr      TFLG1,%10111111     ; clear OC2 for next compare
          ;--------------------------------------
          ; This section is timing critical. If you need to change anything
          ; in the oc2st loop you MUST make sure to get the delay in the oc2dn
          ; loop identical. There is a delay of 675 clocks to set the minimum
          ; pulse width. If you want to have a longer or shorter minimum pulse
          ; width you can change this value. The value 675 is derived as
          ; follows: The oc2st loop takes 39 clocks to start each pulse.
          ; Therefore it takes 7*39 clocks from the start of the first pulse
          ; until the loop completes. The oc2dn loop takes 32 clocks until it
          ; stops the first pulse if that servo's values array holds a zero.
          ; This means that if we want a 950 clock (475 microsecond) minimum
          ; pulse we need to delay 950 - (7*39) - 32 = 645 clocks between the
          ; end of oc2st and the beginning of oc2dn. A DECA/BNE loop takes 5
          ; clocks so we load A with 645/5 = 129. Now the lda  #129 adds 2
          ; clocks to the delay but do you REALLY care about a 2 clock
          ; difference in the desired minimum pulse width and the actual
          ; minimum pulse width? Do NOT replace it with an interrupt driven
          ; delay since this would introduce an unpredictable interrupt latency
          ; of as much as 41 clocks. This much change in the pulse width will
          ; almost surely cause the servos to jitter. Currently the oc2st loop
          ; takes 39 clocks as does the oc2dn loop. The pulse width changes 13
          ; clocks for every count in the values array. This gives you a
          ; 13*256*500nsec = 1664 usec change in pulse width from a zero to FF
          ; in the values array. If you add the 950 clock minimum pulse width
          ; to that you can produce pulses from 475 microseconds through 2139
          ; usec with a resolution of 6.5 usec. Pretty good huh? The numbers
          ; after the semicolon are the number of clocks the instruction takes
          ; to execute. Branches take the same time even if the branch is not
          ; taken. One clock is 500 nsec if you are using an 8 MHz crystal. If
          ; you are not using an 8 MHz crystal then all these timings are wrong.
          ; Too bad!
          ;--------------------------------------
                    clrx                          ; [3] Start at the shortest pulse-width
_1@@                ldb       smasks,x            ; [4] Figure out which servo it is
                    orb       PORTB               ; [3]
                    stb       PORTB               ; [3] Turn it on
                    nop                           ; [2]
                    lda       #3                  ; [2] We need to blow 15 clocks
_2@@                deca                          ; [2]
                    bne       _2@@                ; [3]
                    inx                           ; [3] Go to the next servo
                    cmpx      #7                  ; [4]
                    bls       _1@@                ; [3] Not done, do another one.

                    lda       #129                ; [2] Now, delay the minimum pulse-width
Delay@@             deca                          ; [2] Which is 645 clocks
                    bne       Delay@@             ; [3]

                    clrx                          ; [3] Now start turning off the servo pulses
_3@@                ldb       smasks,x            ; [4] Figure out which servo is first
                    eorb      PORTB               ; [3] Figure out what to store in PORTB
                    nop                           ; [2]
                    lda       dvalues,x           ; [4] Get this servos desired pulse width
_4@@                nop:2                         ; [4]
                    deca                          ; [2] And delay 13 clocks per unit
                    bne       _4@@                ; [3]
                    stb       PORTB               ; [3] Finally turn off the pulse
                    inx                           ; [3] Go to the next servo
                    cmpx      #7                  ; [4] Are we on the last servo?
                    bls       _3@@                ; [3] No, do another one

                    lda       newvals             ; Next see if there is a new values list
                    beq       Done@@              ; If not, we are done

                    clr       newvals             ; Zero out the new values indicator

                    clrx
                    ldb       #1
CopyOC2@@           lda       values,x            ; Copy the values array
                    sta       svalues,x           ; Into the svalues array
                    stb       smasks,x            ; (Save a proper mask for this port)
                    aslb
                    inx                           ; So that we can sort them without
                    cmpx      #7                  ; worrying about getting new user
                    bls       CopyOC2@@           ; values in the values array
          ;--------------------------------------
          ; Sort the svalues list so that the lowest pulse widths are at the
          ; beginning of the list. Also make sure to swap around the smasks
          ; list so that the proper masks stay with the values.
          ;--------------------------------------
                    clry                          ; Y is our "times through the list" counter
SortTop@@           clrx                          ; X is our pointer into the list of values
Sort@@              ldd       svalues,x           ; Get a value
                    cba                           ; Compare them
                    bls       NoSwap@@            ; The first one is lower. Do not swap them
                    sta       svalues+1,x         ; Swap the two values
                    stb       svalues,x
                    ldd       smasks,x            ; Also swap the bit masks
                    sta       smasks+1,x
                    stb       smasks,x
NoSwap@@            inx                           ; Go to the next entry in the list
                    cmpx      #6                  ; 6 is the second-to-last item and we are done
                    bls       Sort@@              ; Not done yet, do another pair of items
                    iny
                    cmpy      #7                  ; We only need to sort 7 times
                    blo       SortTop@@
                    clrx                          ; Now convert from pulse width values
                    lda       svalues,x
                    inca                          ; (Fix up for the delay loop)
                    sta       dvalues,x
Convert@@           lda       svalues+1,x         ; Into delta values so that we can simply
                    suba      svalues,x           ; delay the time left for each pulse
                    inca                          ; (Fix up for the delay loop)
                    sta       dvalues+1,x
                    inx
                    cmpx      #6
                    bls       Convert@@
Done@@              rti                           ; Set all unused vectors here

;*******************************************************************************
AnRTI               def       Done@@
COP_Failure         def       Start
CMF_Failure         def       Start
;*******************************************************************************

                    @vector   Vsci                ; $FFD6 ; SCI Serial System
                    @vector   Vspi                ; $FFD8 ; SPI Serial Transfer Complete
                    @vector   Vpai                ; $FFDA ; Pulse Accumulator Input Edge
                    @vector   Vpao                ; $FFDC ; Pulse Accumulator Overflow
                    @vector   Vrto                ; $FFDE ; Timer Overflow
                    @vector   Vtoc5               ; $FFE0 ; In Capture 4/Output Compare 5 (TI4O5)
                    @vector   Vtoc4               ; $FFE2 ; Timer Output Compare 4 (TOC4)
                    @vector   Vtoc3               ; $FFE4 ; Timer Output Compare 3 (TOC3)
                    @vector   Vtoc2,OC2_Handler   ; $FFE6 ; Timer Output Compare 2 (TOC2)
                    @vector   Vtoc1               ; $FFE8 ; Timer Output Compare 1 (TOC1)
                    @vector   Vtic3               ; $FFEA ; Timer Input Capture 3 (TIC3)
                    @vector   Vtic2               ; $FFEC ; Timer Input Capture 2 (TIC2)
                    @vector   Vtic1               ; $FFEE ; Timer Input Capture 1 (TIC1)
                    @vector   Vrti                ; $FFF0 ; Real Time Interrupt (RTI)
                    @vector   Virq                ; $FFF2 ; External Pin or Parallel I/O (IRQ)
                    @vector   Vxirq               ; $FFF4 ; Pseudo Non-Maskable Interrupt (XIRQ)
                    @vector   Vswi                ; $FFF6 ; Software Interrupt (SWI)
                    @vector   Villop              ; $FFF8 ; Illegal Opcode Trap ()
                    @vector   Vcop,COP_Failure    ; $FFFA ; COP Failure (Reset) ()
                    @vector   Vcmf,CMF_Failure    ; $FFFC ; Clock Monitor Fail (Reset) ()
                    @vector   Vreset,Start        ; $FFFE ; /RESET

                    end       Start
