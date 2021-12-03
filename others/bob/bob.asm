;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;* This is the program which allows a user to control up to 16 servos
;* from a terminal or from a higher level processor. Basically, it
;* includes the initialization code and the OC2_Handler interrupt routine
;* written by Jenny and partially by Erik.
;*******************************************************************************

;*******************************************************************************
;* ZERO PAGE EQUATES
;*******************************************************************************
;* A 152 entry table of 16-bit values is reserved at locations
;* $0000 through $012f (indexed as 0 through 151). Entry X is
;* found at location 2*x in the memory map. Each entry is a
;* turn-off bitmap with each bit (bit 15 to bit 0) representing
;* servo number 0 to 15 respectively. At a time given by 164.5us
;* plus the quantity [13.5us * (150-x)], the control pulses to
;* the servos specified by the bitmap at entry x are turned off.
;* TABLE_OFFSET is a pointer to the last table value. The table
;* is processed from highest to lowest index from entry $a7 to
;* $00 with the entry a7 being a null entry used to properly
;* align the control pulses. Attempts to write to the $97 index
;* will be redirected to the $96 index.
;*******************************************************************************

                    #ListOff
          #ifexists 711e9.inc
REGS                def       0
                    #Uses     711e9.inc
          #endif
                    #ListOn

TRENA               equ       $0C                 ; Transmit, Receive ENAble
RDRF                equ       $20                 ; Receive Data Register Full
ADON                equ       $80                 ; AD POWER UP

;*******************************************************************************
; Standard equates
;*******************************************************************************

          #ifndef REGS
RAM                 equ       $0000
ROM                 equ       $D000
VECTORS             equ       $FFD6               ; E9 VECTORS START AT $FFD6
STACKTOP            equ       $01FF

REGS                def       0                   ; WAS: $1000

?                   macro
~label~             set       REGS+~1~
                    endm

PORTA               @?        $00                 ; PORT A data register
PORTC               @?        $03                 ; PORT C data register
PORTB               @?        $04                 ; PORT B data register
PORTD               @?        $08                 ; PORT D data register
DDRC                @?        $07                 ; PORT C direction register
PORTE               @?        $0A                 ; PORT E data register
CFORC               @?        $0B                 ; Timer Compare Force
OC1M                @?        $0C                 ; Output Compare 1 Mask
TOC2                @?        $18                 ; Timer Output Compare register 2
TOC1                @?        $16                 ; Timer Output Compare register 1
TOC5                @?        $1E                 ; Timer Output Compare register 5
TCTL1               @?        $20                 ; Timer ConTroL register 1
TCTL2               @?        $21                 ; Timer ConTroL register 2
TMSK1               @?        $22                 ; Timer interrupt MaSK register 1
TFLG1               @?        $23                 ; Timer interrupt FLaG register 1
RTFLG1              @?        $23                 ; Relative to $1000 Timer interrupt FLaG register 1
TMSK2               @?        $24                 ; Timer interrupt MaSK register 2
TFLG2               @?        $25                 ; Timer interrupt FLaG register 2
PACTL               @?        $26                 ; PULSE ACCUMULATOR CONTROL
OC1D                @?        $0D                 ; OUTPUT COMPARE 1 DATA REGISTER
SPCR                @?        $28                 ; SPI Control Register
SPSR                @?        $29                 ; SPI Status Register
SPDR                @?        $2A                 ; SPI Data Register
BAUD                @?        $2B                 ; SCI Baud Rate Control Register
SCCR1               @?        $2C                 ; SCI Control Register 1
SCCR2               @?        $2D                 ; SCI Control Register 2
SCSR                @?        $2E                 ; SCI Status Register
SCDR                @?        $2F                 ; SCI Data Register
ADCTL               @?        $30                 ; A/D CONTROL Register
ADR1                @?        $31                 ; RESULT 1
ADR2                @?        $32                 ; RESULT 2
ADR3                @?        $33                 ; RESULT 3
ADR4                @?        $34                 ; RESULT 4
OPTION              @?        $39                 ; OPTION Register
TCNT                @?        $0E                 ; TCNT Register
          #endif

;*******************************************************************************
                    #RAM      RAM                 ; Register structures
;*******************************************************************************

?MyVars
TABLE               rmb       152*2               ; Table of 16-bit entry values
                    org       *+26                ; filler
TABLE_OFFSET        rmb       2                   ; Last record in turn-off table
CURRENT_OFF         rmb       16                  ; The current turn-off values
ONMASK              rmb       2                   ; The mask of selected servos
;                   #size     ?MyVars             ; original cleared vars up to here
STORAGE             rmb       2                   ; Temporary Storage
STORAGE2            rmb       2                   ; Temporary Storage 2
ONWAIT              rmb       2                   ; Signal Alignment Variable
COUNTER             rmb       2                   ; A 16-bit Counter for WAIT function
FLAGS               rmb       1                   ; HEADER FLAG
HOLD                rmb       2                   ; Temp register
                    #size     ?MyVars

;*******************************************************************************
                    #ROM      ROM                 ; Setup and initialization code
;*******************************************************************************

Start               proc
                    clr       XINIT
                    lds       #STACKTOP           ; Set stack at the top of ram

                    lda       #$30                ; Set baud to 9600 @2MHz bus
                    sta       BAUD                ; Set the port baud
                    clr       SCCR1               ; Set mode if indetermined to N81
                    lda       #TRENA              ; Load mask for Tx, Rx
                    sta       SCCR2               ; Enable the serial subsystem

                    clra                          ; Initialize outputs to
                    sta       PORTC               ; zero to prevent jerking
                    sta       PORTB               ; zero to prevent jerking
                    coma                          ; Set for output
                    sta       DDRC                ; All port C pins now output

                    lda       #ADON               ; Power on
                    sta       OPTION              ; A/D system

                    @ClrRange #?MyVars,#?MyVars+::?MyVars
          ;-------------------------------------- ; Set up interrupt for OC2
                    lda       #$40                ; Set up OC2 bitmap
                    sta       TFLG1               ; Clear the interrupt flag register
                    sta       TMSK1               ; Request hardware interrupt sequence
;                   bra       InitPACTL

;*******************************************************************************

InitPACTL           proc
                    bset      PACTL,$88           ; Set PA7 for output
                    bset      OC1M,$B0            ; Set OC1 to control OC1, OC3, OC4
                    clr       OC1D                ; All OCx pins go low on a compare of OC1
                    lda       #$AA                ; Set OC3-4 to go low and OC5, OC2 go low.
                    sta       TCTL1               ; "
          ;--------------------------------------
          ; Variable ONWAIT holds the iteration of the servo turn-on cycle
          ; at which the onmask is first asserted onto the servo ports
          ;--------------------------------------
                    ldx       #CURRENT_OFF+14     ; 15 loops desired before turn on
                    stx       ONWAIT              ; Store the wait value
                    clrd                          ; All servos are initially off
                    std       COUNTER             ; Clear 16-bit interrupt counter
                    std       ONMASK              ; This is crucial to make servos not go.
                    clr       FLAGS
                    cli                           ; Turn on interrupts
Loop@@              clr       FLAGS
                    ldx       #$8000              ; onmask flag
                    stx       HOLD
GetB@@              jsr       GetChar             ; Get the character from serial port
                    cmpa      #$BB                ; Looking for header character (terminal header)
                    bne       GetB@@              ; Terminal is communicating with processor
                    ldx       #CURRENT_OFF        ; X contains the positions table pointer
                    jsr       GetChar             ; Get servo number ($C0-$CF)
                    cmpa      #$BF
                    bls       Loop@@
                    anda      #$0F
                    tab
                    abx
                    clra
                    xgdy
                    iny
                    tst       FLAGS
                    bne       Loop@@

_1@@                dey
                    beq       _2@@
                    lsr       HOLD                ;|  REPLACES ORIGINAL:
                    ror       HOLD+1              ;| ldd HOLD, lsrd, std HOLD
                    bra       _1@@

_2@@                bsr       GetTime
                    cmpa      #$AA                ; Off directive?
                    beq       Off@@               ; Yes
                    tst       FLAGS               ; Test flags
                    bne       Loop@@              ; Flag was set
                    sta       ,x                  ; Put servo position in table
                    ldd       HOLD                ; Put onmask flag in D
                    ora       ONMASK              ; Turn respective servo on
                    orb       ONMASK+1
                    std       ONMASK
                    bra       Loop@@

Off@@               tst       FLAGS
                    bne       Loop@@
                    ldd       HOLD
                    comd                          ; Complement
                    anda      ONMASK              ; Turn off respective servo
                    andb      ONMASK+1
                    std       ONMASK
                    bra       Loop@@
          ;--------------------------------------
          ; The following code enables serial transmition by forcing OC2 low.
          ; It is assumed that the OC2 pin is wired to an enable line on the
          ; transmiting device or the CTS, DSR, and DCD pins of the console
          ; RS-232 cable. (Pins 5,6 and 8 respectively) the enable signal blocks
          ; serial communication during the interrupt routine and is active low
          ; complying with RS-232 standards. Note that the OC2 pin does not
          ; produce RS-232 level output and the signal should be fed into a
          ; MAX232 or MC145407P before being sent over a serial line.
          ;--------------------------------------
                    lda       #$80                ; prepare to force interrupt
                    sta       TCTL1               ; line back to low

                    lda       #$40                ; load bitmap for OC2
                    sta       CFORC               ; force line high

                    lda       #$C0                ; restore the go-high
                    sta       TCTL1               ; request code

                    cli                           ; Turn on interrupts
;                   bra       Clip

;*******************************************************************************
;* I/O AND TRANSLATION FUNCTIONS
;*******************************************************************************
;* ROUTINE CLIP: TAKES A BYTE AND SETS AT $69 IF BETWEEN $69 AND $E0
;* VALUES BETWEEN $E1 AND $FF ARE SET TO $00
;*******************************************************************************

Clip                proc
                    cmpa      #$AA                ; This is the "off" command
                    beq       Done@@              ; Done

                    cmpa      #$A5                ; $A5 is the maximum position
                    bls       Done@@              ; This number is larger

                    cmpa      #$E0                ; limit value
                    bhi       Zero@@              ; Greater than $e0

                    lda       #$A5                ; Limit value to $69
                    rts                           ; Return value in A

Zero@@              clra                          ; Underflow from subtract
Done@@              rts                           ; Return value in A

;*******************************************************************************
;* ROUTINE GETTIME: RETURNS A VALID TIMING VALUE FROM CONSOLE
;*******************************************************************************

GetTime             proc
                    pshb                          ; SAVE B REGISTER
                    bsr       GetChar             ; GET A CHARACTER
                    bsr       Clip                ; ASSURE RANGE IS APPROPRIATE
                    pulb                          ; RESTORE B REGISTER
                    rts

;*******************************************************************************
;* ROUTINE GETBYTE: CONSTRUCTS A BYTE VALUE FROM TWO ASCII INPUTS
;*******************************************************************************

GetByte             proc
                    pshb                          ; SAVE B REGISTER
                    bsr       GetChar             ; GET A CHARACTER
                    bsr       XLate               ; TRANSLATE TO NIBBLE
                    lsla:4                        ; TRANSFER TO HIGH NIBBLE
                    tab                           ; STORE IN B REGISTER
                    bsr       GetChar             ; GET THE SECOND HALF
                    bsr       XLate               ; TRANSLATE TO NIBBLE
                    aba                           ; CREATE FULL BYTE
                    pulb                          ; RESTORE B REGISTER
                    rts                           ; RETURN BYTE IN A

;*******************************************************************************
;* ROUTINE XLATE: TRANSLATES ASCII CHARACTER INTO NIBBLE
;*******************************************************************************

XLate               proc
                    cmpa      #'9'                ; IS IT A NUMBER?
                    bgt       Letter@@            ; TREAT AS LETTER
                    anda      #$0F                ; GET ABSOLUTE VALUE
                    rts                           ; FINISHED WITH NUMBER

Letter@@            anda      #$5F                ; MAKE UPPERCASE
                    suba      #55                 ; ADJUST TO HEX NUMBER
                    rts                           ; FAIRLY EASY

;*******************************************************************************
;* ROUTINE GETCHAR: GETS BYTE FROM SERIAL PORT AND ECHOS TO CONSOLE
;*******************************************************************************

GetChar             proc
Loop@@              lda       SCSR                ; CHECK RECEIVE REGISTER
                    anda      #RDRF               ; FOR INCOMING CHARACTER
                    beq       Loop@@              ; NOT THERE, KEEP TRYING
                    lda       SCDR                ; GET THE CHARACTER IN A
                    rts                           ; RETURN CHARACTER

;*******************************************************************************

OC2_Handler         proc
          ;-------------------------------------- ; First reset for the next interrupt
                    ldd       #BUS_KHZ*20         ; cycles for 20ms
                    addd      TOC2                ; Add directly to
                    std       TOC2                ; preserve timing accuracy

                    lda       #$40                ; prepare to clear the
                    sta       TFLG1               ; interrupt flag

                    ldd       COUNTER             ; get the current count
                    incd                          ; increment 16-bit value
                    std       COUNTER             ; store into the 20ms counter
          ;--------------------------------------
          ; Look at complete table flag
          ; Take positions from secondary table and put them in primary table
          ; Clear FLAGS and COMPLETE flags
          ;--------------------------------------
                    lda       #1
                    sta       FLAGS
          ;--------------------------------------
          ; Process the current servo list
          ; Now set the new turn-off values
          ;--------------------------------------
          ; The location storage holds a bitmap with a 16-bit value
          ; corresponding to the current servo being processed.
          ; For example, the bitmap $40000 is used to process servo 1.
          ; (0100... recall that indexing starts at zero) ONMASK holds
          ; the 16-bit bitmap of the servos currently dsired to be on.
          ; All servos that are on will have the bitmap in storage
          ; applied to the ($97-x) entry in the timing table where x
          ; is the timingvalue in the corresponding CURRENT_OFF register.
          ; Since more than one servo might have the same turn off timing
          ; value, the bitmap in storage is "OR-ed" with the previous value
          ; to prevent overwriting any other servo's information.
          ;--------------------------------------
                    ldd       #$8000              ; Set active bitmap to servo one
                    std       STORAGE             ; Save the bitmap

                    ldd       ONMASK              ; Find out what servos are on
                    std       STORAGE2            ; Working bitmap

                    ldy       #CURRENT_OFF-1      ; Get the first servo address

Loop@@              ldb       1,y                 ; Save the address
                    clra                          ; Zero for address usage
                    lsld                          ; Offset for 16-bit value
                    xgdx                          ; Get address of the servo time register
                    ldd       STORAGE2            ; Get active bitmap
                    lsld                          ; Get status in carry flag
                    bcs       ServoActive@@       ; Servo active
          ;--------------------------------------
          ; In the onmask indicates that the specified servo is on, it is
          ; processed at ServoActive@@ routine. Otherwise, the ServoOff routine
          ; will update the selection mask in storage and waste enough time so
          ; as to balance the ServoActive@@ routine
ServoOff@@;--------------------------------------
                    std       STORAGE2            ; Match delay of the other routine
                    ldd       STORAGE             ; Get the active bitmap
                    lsrd                          ; Shift for next channel
                    std       STORAGE             ; Store the active bitmap
                    tst:3     ,x                  ; Burn 18 cycles
                    bra       TOnCheck@@          ; Now ready to resume routine

ServoActive@@       std       STORAGE2            ; Store active bitmap
                    ldd       STORAGE             ; Restore the active bitmap
                    ora       ,x                  ; Inclusive Or to prevent
                    orb       1,X                 ; overwriting another servo's
                    std       ,x                  ; turn-off request.
                    ldd       STORAGE             ; Reload bitmap
                    lsrd                          ; Set bitmap for next servo channel
                    std       STORAGE             ; Save bitmap
          ;--------------------------------------
          ; At this point, check if the turn-on point of the channels has been
          ; reached. The control of the turn-on point is achieved by checking
          ; for a certain number of loops through the update routine. The number
          ; of loops before turn on is given in ONWAIT. As it is now, the time
          ; delay between turn on and the beginning of turn off is 0.484 ms
          ;--------------------------------------
TOnCheck@@          iny                           ; Go to next table entry
                    cpy       ONWAIT              ; See if X loops done
                    bne       NextServo@@         ; Bypass servo turn on
                    ldd       ONMASK              ; Find which servos are active
                    std       PORTC               ; and turn them on
NextServo@@         cpy       #CURRENT_OFF+15     ; Done if at this address
                    bne       Loop@@              ; Keep transfering table values
;                   bra       TurnOff

;*******************************************************************************
; Now update the LAST_OFF table. Since the tables are offset by
; exactly 16 bytes, there is no need for two indexes. Two values
; are updated at a time, so only 8 loops are required.
; The turn-off loop giving 13.5uS per loop at 8MHz
; This is the tightest possible way to execute the turn offs.
; The table must be processed backwards because comparing the index
; to a final value and branching conditionally takes more time than
; decrementing and branching on zero. The additional turnoff cycle
; is necessary after the OffLoop@@ because the branch on zero doesn't
; process the entry at index zero.

TurnOff             proc
                    ldx       #TABLE_OFFSET       ; GET TIMING TABLE ADDRESS
          ;--------------------------------------
          ; TIMED LOOP STARTS HERE: 27E'S = 13.5 uS PER LOOP
          ;--------------------------------------
OffLoop@@           ldd       ,x                  ; GET THE TIMING VALUE
                    eora      PORTC               ; XOR TO TAKE HIGH LINE
                    eorb      PORTB               ; LOW AND THEN
                    std       PORTC               ; UPDATE TO SERVO CHANNELS
                    dex:2                         ; GO TO NEXT 16 BIT TABLE VALUE
                    bne       OffLoop@@           ; IF NOT DONE CONTINUE TABLE
          ;-------------------------------------- ; TIMED LOOP ENDS HERE
                    ldd       ,x                  ; MUST TO FINAL TABLE VALUE
                    eora      PORTC               ; SINCE A COMPARISON TO ZERO
                    eorb      PORTB               ; WAS THE TIGHTEST LOOP
                    std       PORTC               ; POSSIBLE
          ;-------------------------------------- ; CLEAR TURN OFF TABLE
                    ldx       #CURRENT_OFF        ; Prepare to get servo turn off times
Clear@@             ldb       ,x                  ; Prepare to clear value
                    clra                          ; Zero high byte of address
                    lsld                          ; Adjust for 16 bit value
                    xgdy                          ; Get timing address in Y
                    clr       ,y                  ; Now clear location
                    clr       1,Y                 ; and the other half
                    inx                           ; Go to next location
                    cpx       #CURRENT_OFF+16     ; Have all locations been initialized
                    bne       Clear@@             ; Keep clearing
          ;--------------------------------------
          ; NOW FORCE THE OC2 PIN BACK LOW TO ALLOW SERIAL COMMUNCATION AND
          ; RESET THE TCTL1 CODE SO THAT THE NEXT INTERRUPT FORCES IT BACK HIGH.
          ;--------------------------------------
                    lda       #$80                ; prepare to force interrupt
                    sta       TCTL1               ; line back to low

                    lda       #$40                ; load bitmap for OC2
                    sta       CFORC               ; force line high

                    lda       #$C0                ; restore the go-high
                    sta       TCTL1               ; request code

AnRTI               rti                           ; ALL UNUSED VECTORS HERE

;*******************************************************************************
; INTERRUPT VECTORS
;*******************************************************************************

                    @vector   Vsci                ; SCI Serial System
                    @vector   Vspi                ; SPI Serial Transfer Complete
                    @vector   Vpai                ; Pulse Accumulator Input Edge
                    @vector   Vpao                ; Pulse Accumulator Overflow
                    @vector   Vrto                ; Timer Overflow
                    @vector   Vtoc5               ; In Capture 4/Output Compare 5 (TI4O5)
                    @vector   Vtoc4               ; Timer Output Compare 4 (TOC4)
                    @vector   Vtoc3               ; Timer Output Compare 3 (TOC3)
                    @vector   Vtoc2,OC2_Handler   ; Timer Output Compare 2 (TOC2)
                    @vector   Vtoc1               ; Timer Output Compare 1 (TOC1)
                    @vector   Vtic3               ; Timer Input Capture 3 (TIC3)
                    @vector   Vtic2               ; Timer Input Capture 2 (TIC2)
                    @vector   Vtic1               ; Timer Input Capture 1 (TIC1)
                    @vector   Vrti                ; Real Time Interrupt (RTI)
                    @vector   Virq                ; External Pin or Parallel I/O (IRQ)
                    @vector   Vxirq               ; Pseudo Non-Maskable Interrupt (XIRQ)
                    @vector   Vswi                ; Software Interrupt (SWI)
                    @vector   Villop,Start        ; Illegal Opcode Trap ()
                    @vector   Vcop,Start          ; COP Failure (Reset) ()
                    @vector   Vcmf,Start          ; COP Clock Monitor Fail (Reset) ()
                    @vector   Vreset,Start        ; /RESET

                    end       :s19crc
