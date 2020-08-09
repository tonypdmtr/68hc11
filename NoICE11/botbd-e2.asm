;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;
; 68HC11 Debug monitor for use with NOICE11 on Marvin Green's BOTBoard2
;
; Copyright (c) 2020 by John Hartman
;
; Modification History:
; 14-Jun-93 JLH release version
;  3-Aug-93 JLH improve I/O init documentation
; 24-Aug-93 JLH correct error in IN and OUT, stack init (v1.2)
; 12-May-94 JLH clarify TSTG paging info
;  7-Nov-94 JLH correct typos in comments
;  1-May-95 JLH correct error in RAMVEC usage (v1.3)
; 19-Aug-97 JLH correct bug in COP and Clock Monitor handling
; 23-Dec-97 JLH modify for use with BotBoard2 and HC11E2
; 25-Feb-98 JLH assemble with either Motorola or Dunfield
; 21-Jul-00 JLH change FN_MIN from F7 to F0
; 12-Mar-01 JLH V3.0: improve text about paging, formerly called 'mapping'
; 10-Nov-19 TGP Ported to ASM11 by Tony Papadimitriou, and optimized a bit
;===============================================================================
; This version of MONHC11 has been customized to work with Marvin Green's
; BOTBoard2 using a 68HC11E2 processor.
;
; It operates at 9600 baud using an 8 MHz crystal.  Faster baud
; rates are possible using a 7.3728 MHz crystal.  This would require
; changes to the baud rate calculations below.  This would also
; require a change to the special bootstrap mode's baud rate when
; using PCBUG.  Alternatively, this code could be downloaded using
; PCBUG with an 8 MHz crystal, and the crystal then replaced for
; use by MONHC11.
;
; The NoICE monitor resides in the E2's 2K of EEPROM, relocated to
; address B800-BFFF.  The BOTBoard must be set to come up in SPECIAL TEST
; mode, rather than the SPECIAL BOOTSTRAP mode used by PCBUG.  This
; is done by changing DIP switch B from ON to OFF.
;
; As supplied, this program sets PORTD bits 3 and 2 as outputs.
; When a reset occurs, a brief beep is generated on the piezo, using
; PD2 (you must jumper J3 appropriately).
; When the monitor is waiting for characters, it toggles the LED
; using PD3 as a heartbeat. (you must jumper J3 appropriately)
; If you do not want these features, or have another need for PORTD,
; such as SPI, you must modify this code as appropriate.
;
; Steps are:
;  1) Assemble this program
;  2) Set BOTBoard to SPECIAL BOOTSRAP MODE: DIPA=ON DIPB=ON
;  3) Use PCBUG to burn MONHC11.S19 into EEPROM on the BOTBOard:
; 3a) MS $103C $E5
; 3b) EPROM $103F
; 3c) MS $103F $B5
; 3d) EPROM $B800 $BFFF
; 3e) LOADS MONHC11.S19
;  4) Set BOTBoard to SPECIAL TEST MODE: DIPA=ON DIPB=OFF
;
; When the BOTBoard is next reset, MONHC11 will run.  User programs
; may be loaded using NoICE, into the RAM from $C000 to $FFFF.
; User interrupt vectors are $FFC0 to $FFFF.
; Start, XIRQ, Illegal-operation, COP-fail, Clock-fail, and SWI are
; reserved by MONHC11.
;
; Note that MONHC11, and hence the user program, are run in special
; test mode.  Thus, the 'protected' registers, which can normally
; be written only during the first 64 cycles after reset, may be
; written to at any time.  Since MONHC11 is running during the first
; 64 cycles after reset, this type of operations is intended to allow
; user programs to initialize the HC11 as desired.  The user must
; ensure that the 64 cycle limitation is respected or the program
; will not operate correctly when run without MONHC11.
;===============================================================================
; To customize for a given target, you must change code in the hardware equates,
; the string TSTG, and the routines Start and REWDT.
; You may or may not need to change GetChar, PutChar, depending on
; how peculiar your UART is.
;
; For more information, refer to the NoICE help file monitor.htm
;
; This file has been assembled with ASM11 by Tony Papadimitriou.
;
; To add banked or paged memory support:
; 1) Define page latch port PAGELATCH here
; 2) If PAGELATCH is write only, define or import the latch port's RAM
;    image PAGEIMAGE here (The application code must update PAGEIMAGE
;    before outputing to PAGELATCH)
; 3) Search for and modify PAGELATCH, PAGEIMAGE, and REG_PAGE usage below
; 4) In TSTG below edit 'LOW AND HIGH LIMIT OF PAGED MEM'
;    to appropriate range (typically 8000H to BFFFH for two-bit MMU)
;
; For more information, refer to the NoICE help file 2bitmmu.htm
;*******************************************************************************

BUS_KHZ             def       2000                ;default bus speed in KHz

;*******************************************************************************
; Macros
;*******************************************************************************

SetPage             macro     PageValue
          #ifdef PAGEIMAGE
                    lda       ~1~
                    sta       PAGEIMAGE           ;; set image
                    sta       PAGELATCH           ;; set hardware page
          #endif
                    endm

;*******************************************************************************

VERSION             equ       300                 ; version as x.xx

; Hardware definitions

CHIP_RAM            equ       $0000               ; START OF HC11 ON-CHIP RAM
REGS                equ       $1000               ; START OF HC11 ON-CHIP I/O
XINIT               equ       $103D               ; (out-of-reset location)
RAM_START           equ       $B000               ; START OF MONITOR RAM
ROM_START           equ       $BC00               ; START OF MONITOR CODE
HARD_VECT           equ       $BFD6               ; START OF HARDWARE VECTORS

USER_STACK          equ       $B000               ; TOP+1 OF USER STACK
USER_CODE           equ       $C000               ; START OF USER CODE SPACE
USER_VECT           equ       $FFD6               ; START OF USER VECTORS

                    #temp     REGS                ; Define HC11 I/O register locations (68HC11A8)

H11PORTA            next      :temp               ; X000 i/o port A
                    next      :temp               ; X001 reserved
H11PIOC             next      :temp               ; X002 i/o port C control
H11PORTC            next      :temp               ; X003 i/o port C

H11PORTB            next      :temp               ; X004 i/o port B
H11PORTCL           next      :temp               ; X005 i/o port CL
                    next      :temp               ; X006 reserved
H11DDRC             next      :temp               ; X007 data direction for port C

H11PORTD            next      :temp               ; X008 i/o port D
H11DDRD             next      :temp               ; X009 data direction for port D
H11PORTE            next      :temp               ; X00A input port E
H11CFORC            next      :temp               ; X00B compare force register

H11OC1M             next      :temp               ; X00C OC1 action mask register
H11OC1D             next      :temp               ; X00D OC1 action data register
H11TCNT             next      :temp,2             ; X00E timer counter register

H11TIC1             next      :temp,2             ; X010 input capture register 1
H11TIC2             next      :temp,2             ; X012 input capture register 2

H11TIC3             next      :temp,2             ; X014 input capture register 3
H11TOC1             next      :temp,2             ; X016 output compare register 1

H11TOC2             next      :temp,2             ; X018 output compare register 2
H11TOC3             next      :temp,2             ; X01A output compare register 3

H11TOC4             next      :temp,2             ; X01C output compare register 4
H11TOC5             next      :temp,2             ; X01E output compare register 5

H11TCTL1            next      :temp               ; X020 timer control register 1
H11TCTL2            next      :temp               ; X021 timer control register 2
H11TMSK1            next      :temp               ; X022 main timer interrupt mask 1
H11TFLG1            next      :temp               ; X023 main timer interrupt flag 1

H11TMSK2            next      :temp               ; X024 misc timer interrupt mask 2
H11TFLG2            next      :temp               ; X025 misc timer interrupt flag 2
H11PACTL            next      :temp               ; X026 pulse accumulator control register
H11PACNT            next      :temp               ; X027 pulse accumulator count register

H11SPCR             next      :temp               ; X028 SPI control register
H11SPSR             next      :temp               ; X029 SPI status register
H11SPDR             next      :temp               ; X02A SPI data in/out
H11BAUD             next      :temp               ; X02B SCI baud rate control

H11SCCR1            next      :temp               ; X02C SCI control register 1
H11SCCR2            next      :temp               ; X02D SCI control register 2
H11SCSR             next      :temp               ; X02E SCI status register
H11SCDR             next      :temp               ; X02F SCI data

H11ADCTL            next      :temp               ; X030 A to D control register
H11ADR1             next      :temp               ; X031 A to D result 1
H11ADR2             next      :temp               ; X032 A to D result 2
H11ADR3             next      :temp               ; X033 A to D result 3

H11ADR4             next      :temp               ; X034 A to D result 4
H11BPROT            next      :temp               ; X035 EEPROM block protect
                    next      :temp,2             ; X036 reserved

H11OPT2             next      :temp               ; X038 system configuration options 2
H11OPTION           next      :temp               ; X039 system configuration options
H11COPRST           next      :temp               ; X03A arm/reset COP timer circutry
H11PPROG            next      :temp               ; X03B EEPROM programming control

H11HPRIO            next      :temp               ; X03C highest priority I-bit and misc.
H11INIT             next      :temp               ; X03D ram/io mapping register
H11TEST1            next      :temp               ; X03E factory test control register
H11CONFIG           next      :temp               ; X03F COP, ROM, & EEPROM enables

                    next      :temp,16            ; X040 reserved
                    next      :temp,12            ; X050 reserved

H11CSSTRH           next      :temp               ; X05C Chip select clock stretch
H11CSCTL            next      :temp               ; X05D Chip select control
H11CSGADR           next      :temp               ; X05E General purpose CS address
H11CSGSIZ           next      :temp               ; X05F General purpose CS size

;*******************************************************************************
; HARDWARE PLATFORM CUSTOMIZATIONS
;*******************************************************************************

; UART equates

SER_STATUS          equ       H11SCSR             ; STATUS FROM SCI
SER_RXDATA          equ       H11SCDR             ; DATA FROM SCI
SER_TXDATA          equ       H11SCDR             ; DAT TO SCI
RXRDY               equ       $20
TXRDY               equ       $40                 ; TRANSMIT COMPLETE (FOR TURNOFF)

;*******************************************************************************
                    #RAM
;*******************************************************************************
                    org       USER_VECT           ; RAM interrupt vectors

RAMVEC              rmb       21*2

                    org       RAM_START           ; RAM definitions
          ;--------------------------------------
          ; Monitor stack
          ; (Calculated use is at most 7 bytes.  Leave plenty of spare)
          ; 68HC11 SP points at NEXT BYTE TO USE, rather than at last used byte
          ; like most processors.  Thus, init SP to TOP-1 of stack space
          ;--------------------------------------
                    rmb       15
MONSTACK            rmb       1
          ;--------------------------------------
          ; Target registers: Order must match that in TRGHC11.C
          ;--------------------------------------
TASK_REGS
REG_STATE           rmb       1
REG_PAGE            rmb       1
REG_SP              rmb       2
REG_Y               rmb       2
REG_X               rmb       2
REG_B               rmb       1                   ; B BEFORE A, SO D IS LEAST SIG. FIRST
REG_A               rmb       1
REG_CC              rmb       1
REG_PC              rmb       2
                    #size     TASK_REGS
          ;--------------------------------------
          ; Communications buffer
          ; (Must be at least as long as the longer of ::TASK_REGS or ::TSTG.
          ; At least 19 bytes recommended.  Larger values may improve speed of NoICE
          ; download and memory move commands.)
          ;--------------------------------------
COMBUF_SIZE         equ       128                 ; DATA SIZE FOR COMM BUFFER
COMBUF              rmb       2+COMBUF_SIZE+1     ; BUFFER ALSO HAS FN, LEN, AND CHECK
RAM_END             equ       *                   ; ADDRESS OF TOP+1 OF RAM

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       ROM_START

Start               proc                          ; Power on reset
          ;-------------------------------------- ; Set CPU mode to safe state
                    sei                           ; INTERRUPTS OFF (WE MAY JUMP HERE)
                    clrb                          ; STATE 0 = 'RESET'
                    bra       Initialize

;*******************************************************************************
; COP reset

COP_ENT             proc
                    ldb       #4                  ; STATE 4 = 'COP'
                    bra       Initialize

;*******************************************************************************
; Clock Fail reset

CLOCK_ENT           proc
                    ldb       #3                  ; STATE 3 = 'Clock fail'
;                   bra       Initialize

;*******************************************************************************
; Initialize HC11 hardware
; BE SURE THAT 'B' REMAINS INTACT UNTIL IT IS STORED TO REG_STATE BELOW!

Initialize          proc
; Monitor assumes operation in either Normal Expanded or Special Test mode
; The exact initialization required here will depend on which variant of
; the 68HC11 you have, and on your hardware layout.  The following is
; basic, and may not be sufficient for your case.
          ;--------------------------------------
          ; The following writes must occur within
          ; first 64 cycles after end of reset.
          ; CAUTION:
          ; Don't use I/O address equates until XINIT
          ; is written to set the I/O base to match our equates!
          ;--------------------------------------
                    lda       #]CHIP_RAM+{REGS/4096} ; On-chip RAM at CHIP_RAM (high nibble) & I/O regs at REGS (low nibble) location of RAM, I/O
                    sta       XINIT               ; Use the post-reset address!
          ;-------------------------------------- ; Save reset type (RESET, COP, or Clock Fail)
                    stb       REG_STATE           ; save state
          ;-------------------------------------- ; Now ok to use I/O address equates
                    lda       #$B5                ; No COP, enable EEPROM at Bxxx
                    sta       H11CONFIG

                    clra                          ; PRESCALE TO DIVIDE BY 1
                    sta       H11TMSK2

                    lda       #$13                ; IRQ LEVEL, OSC DELAY, LONG COP
                    sta       H11OPTION

                    lda       #$E5                ; EXPANDED MODE (ACCESS EXTERNAL BUS)
                    sta       H11HPRIO
          ;--------------------------------------
          ; Possible additional special initialization
          ; H11CSCTL          ;Chip select control
          ; H11CSGSIZ         ;General purpose CS size
          ; H11CSGADR         ;General purpose CS address
          ; H11CSSTRH         ;Chip select clock stretch
          ;
          ; H11BPROT          ;EEPROM block protect
          ; H11PPROG          ;EEPROM programming control
          ; Enable programming voltage to program CONFIG register
          ; Wait for voltage to stabilize before programming
          ;--------------------------------------
                    lda       #%00000010
                    sta       H11PORTD            ; PORT D: TX=1, LED OFF, PEIZO OFF
                    lda       #%00001110
                    sta       H11DDRD             ; PORT D: OUT: TX, LED, PIEZO
                    lda       #%00000000
                    sta       H11SPCR             ; DISABLE WIRE-OR, DISABLE SPI

                    lds       #MONSTACK           ; CLEAN STACK IS HAPPY STACK
          ;--------------------------------------
          ; Initialize your UART here
          ; (SCI at 9600 baud from 8 MHz crystal)
          ;--------------------------------------
                    lda       #%00110000          ; PRE-DIV BY 13; DIV BY 1
                    sta       H11BAUD
                    clra                          ; 8 BIT DATA
                    sta       H11SCCR1
                    lda       #$0C                ; TX AND RX ENABLED, NO INTS, NO WAKE
                    sta       H11SCCR2
          ;-------------------------------------- ; Initialize RAM interrupt vectors
                    ldy       #INT_ENTRY          ; ADDRESS OF DEFAULT HANDLER
                    ldx       #RAMVEC             ; POINTER TO RAM VECTORS
                    ldb       #::NVEC/2           ; NUMBER OF VECTORS
_1@@                sty       ,x                  ; SET VECTOR
                    inx:2
                    decb
                    bne       _1@@
          ;-------------------------------------- ; Initialize user registers
                    ldd       #USER_STACK
                    sta       REG_SP+1            ; INIT USER'S STACK POINTER MSB
                    stb       REG_SP              ; LSB
                    clrd
                    std       REG_PC
                    sta       REG_A
                    sta       REG_B
                    std       REG_X
                    std       REG_Y
          ;-------------------------------------- ; Initialize memory paging variables and hardware (if any)
                    sta       REG_PAGE            ; NO PAGE YET
                    @SetPage  #0
          ;-------------------------------------- ; Initialize non-zero registers
                    lda       #$50                ; disable interrupts in user program
                    sta       REG_CC
          ;--------------------------------------
          ; Beep to signal life (and occurance of a reset)
          ; 1 Khz = 500 usec half-period = 1000 cycles with E=2 MHz (8 MHz crystal)
          ;--------------------------------------
                              #Cycles
                    ldx       #DELAY@@
Loop@@              ldb       #200
                    lda       H11PORTD
                    eora      #$04                ; toggle beeper
                    sta       H11PORTD
                              #Cycles
_@@                 decb
                    bne       _@@
                              #temp :cycles*200
                    dex
                    bne       Loop@@
DELAY@@             equ       500*BUS_KHZ-:cycles-:ocycles/:temp+1
          ;--------------------------------------
          ; Set function code for 'GO'.  Then if we are here because of a reset
          ; (such as a COP timeout) after being told to GO, we will come
          ; back with registers so user can see the reset
          ;--------------------------------------
                    lda       #FN_RUN_TARG
                    sta       COMBUF
                    jmp       RETURN_REGS         ; DUMP REGS, ENTER MONITOR

;*******************************************************************************
; Get a character to A
; Return A=char, CY=0 if data received
;        CY=1 if timeout (0.5 seconds)
; Uses 6 bytes of stack including return address

GetChar             proc
                    lda       H11PORTD
                    eora      #$08                ; TOGGLE LED: WE ARE ALIVE...
                    sta       H11PORTD
                    pshx
                    clrx                          ; LONG TIMEOUT
Loop@@              bsr       REWDT               ; PREVENT WATCHDOG TIMEOUT
                    dex
                    sec                           ; Timeout:  return CY=1
                    beq       Done@@              ; EXIT IF TIMEOUT
          ;-------------------------------------- ; (Disable timeout in most cases...)
                    lda       SER_STATUS          ; READ DEVICE STATUS
                    anda      #RXRDY
                    beq       Loop@@              ; NOT READY YET.
          ;-------------------------------------- ; Data received:  return CY=0. data in A
                    lda       SER_RXDATA          ; READ DATA
                    clc                           ; CY=0
Done@@              pulx
                    rts

;*******************************************************************************
; Output character in A
; Uses 5 bytes of stack including return address

PutChar             proc
                    psha
Loop@@              bsr       REWDT               ; PREVENT WATCHDOG TIMEOUT
                    lda       SER_STATUS          ; CHECK TX STATUS
                    anda      #TXRDY              ; TX READY ?
                    beq       Loop@@
                    pula
                    sta       SER_TXDATA          ; TRANSMIT CHAR.
                    rts

;*******************************************************************************
; Reset watchdog timer.  Must be called at least once every little while
; or COP interrupt will occur
; Uses 2 bytes of stack including return address

REWDT               proc
                    lda       #$55
                    sta       H11COPRST
                    coma
                    sta       H11COPRST
                    rts

;*******************************************************************************
; Response string for GET TARGET STATUS request
; Reply describes target:

TSTG                fcb       3                   ;   2: PROCESSOR TYPE = 68HC11
                    fcb       COMBUF_SIZE         ;   3: SIZE OF COMMUNICATIONS BUFFER
                    fcb       0                   ;   4: NO TASKING SUPPORT
                    fcb       0,0                 ; 5,6: BOTTOM OF PAGED MEM (LSB FIRST)
                    fcb       0,0                 ; 7,8: TOP OF PAGED MEM (LSB FIRST)
                    fcb       ?Copyright-?SWI     ;   9: BREAKPOINT INSTR LENGTH
?SWI                swi                           ; 10+: BREAKPOINT INSTRUCTION
?Copyright          fcs       'NoICE 68HC11 monitor for BOTBoard 2 v{VERSION(2)}'
                    #size     TSTG                ; SIZE OF STRING

;*******************************************************************************
; HARDWARE PLATFORM INDEPENDENT EQUATES AND CODE
;*******************************************************************************

; Communications function codes

FN_GET_STAT         equ       $FF                 ; reply with device info
FN_READ_MEM         equ       $FE                 ; reply with data
FN_WRITE_M          equ       $FD                 ; reply with status (+/-)
FN_READ_RG          equ       $FC                 ; reply with registers
FN_WRITE_RG         equ       $FB                 ; reply with status
FN_RUN_TARG         equ       $FA                 ; reply (delayed) with registers
FN_SET_BYTE         equ       $F9                 ; reply with data (truncate if error)
FN_IN               equ       $F8                 ; input from port
FN_OUT              equ       $F7                 ; output to port

FN_MIN              equ       $F0                 ; MINIMUM RECOGNIZED FUNCTION CODE
FN_ERROR            equ       $F0                 ; error reply to unknown op-code

;*******************************************************************************
; Common handler for default interrupt handlers
; Enter with A=interrupt code = processor state
; All registers stacked, PC=next instruction

INT_ENTRY           proc
                    sta       REG_STATE           ; SAVE STATE
          ;--------------------------------------
          ; Save registers from stack to reg block for return to master
          ; Host wants least significant bytes first, so flip as necessary
          ;--------------------------------------
                    pula
                    sta       REG_CC              ; CONDITION CODES
                    pula
                    sta       REG_B
                    pula
                    sta       REG_A
                    pula
                    sta       REG_X+1             ; MSB
                    pula
                    sta       REG_X               ; LSB
                    pula
                    sta       REG_Y+1             ; MSB
                    pula
                    sta       REG_Y               ; LSB
          ;--------------------------------------
          ; If this is a breakpoint (state = 1), then back up PC to point at SWI
          ; (If SWI2, SWI3, or another instruction is used for breakpoint,
          ; then DEX multiple times to match ?Copyright-?SWI in TSTG
          ;--------------------------------------
                    pulx                          ; PC AFTER INTERRUPT
                    lda       REG_STATE
                    cmpa      #1
                    bne       NOTBP               ; branch if not a breakpoint
                    dex                           ; else back up to point at SWI location
NOTBP               xgdx                          ; transfer PC to D
                    sta       REG_PC+1            ; MSB
                    stb       REG_PC              ; LSB
                    tsx                           ; user stack pointer plus 1
                    dex                           ; make it just like the real SP
                    xgdx
                    stb       REG_SP              ; save user's stack pointer (LSB)
                    sta       REG_SP+1            ; MSB
          ;-------------------------------------- ; Change to our own stack
                    lds       #MONSTACK           ; AND USE OURS INSTEAD
          ;-------------------------------------- ; Save memory page
          #ifdef PAGEIMAGE
                    lda       PAGEIMAGE           ; GET CURRENT USER PAGE
          #else
                    clra                          ; ... OR ZERO IF UNPAGED TARGET
          #endif
                    sta       REG_PAGE            ; SAVE USER'S PAGE
                    jmp       RETURN_REGS         ; Return registers to master

;*******************************************************************************
; Main loop:  wait for command frame from master
; Uses 7 bytes of stack before jump to handlers

MAIN                proc
                    lds       #MONSTACK           ; CLEAN STACK IS HAPPY STACK
                    ldx       #COMBUF             ; BUILD MESSAGE HERE
          ;-------------------------------------- ; First byte is a function code
                    jsr       GetChar             ; GET A FUNCTION
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    cmpa      #FN_MIN
                    blo       MAIN                ; JIF BELOW MIN: ILLEGAL FUNCTION
                    sta       ,x                  ; SAVE FUNCTION CODE
                    inx
          ;-------------------------------------- ; Second byte is data byte count (may be zero)
                    jsr       GetChar             ; GET A LENGTH BYTE
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    cmpa      #COMBUF_SIZE
                    bhi       MAIN                ; JIF TOO LONG: ILLEGAL LENGTH
                    sta       ,x                  ; SAVE LENGTH
                    inx
                    tsta
                    beq       _2@@                ; SKIP DATA LOOP IF LENGTH = 0
          ;-------------------------------------- ; Loop for data
                    tab                           ; SAVE LENGTH FOR LOOP
_1@@                jsr       GetChar             ; GET A DATA BYTE
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    sta       ,x                  ; SAVE DATA BYTE
                    inx
                    decb
                    bne       _1@@
          ;-------------------------------------- ; Get the checksum
_2@@                jsr       GetChar             ; GET THE CHECKSUM
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    psha                          ; SAVE CHECKSUM
          ;--------------------------------------
          ; Compare received checksum to that calculated
          ; on received buffer (Sum should be 0)
          ;--------------------------------------
                    jsr       CHECKSUM
                    pulb
                    aba
                    bne       MAIN                ; JIF BAD CHECKSUM
          ;-------------------------------------- ; Process the message.
                    ldx       #COMBUF
                    ldd       ,x                  ; A=FUNCTION CODE, B=LENGTH
                    inx:2                         ; X POINTS AT DATA
                    cmpa      #FN_GET_STAT
                    beq       TARGET_STAT
                    cmpa      #FN_READ_MEM
                    beq       READ_MEM
                    cmpa      #FN_WRITE_M
                    beq       WRITE_MEM
                    cmpa      #FN_READ_RG
                    jeq       READ_REGS
                    cmpa      #FN_WRITE_RG
                    jeq       WRITE_REGS
                    cmpa      #FN_RUN_TARG
                    jeq       RUN_TARGET
                    cmpa      #FN_SET_BYTE
                    jeq       SET_BYTES
                    cmpa      #FN_IN
                    jeq       IN_PORT
                    cmpa      #FN_OUT
                    jeq       OUT_PORT
          ;-------------------------------------- ; Error: unknown function.  Complain
                    lda       #FN_ERROR
                    sta       COMBUF              ; SET FUNCTION AS 'ERROR'
                    lda       #1
                    jmp       SEND_STATUS         ; VALUE IS 'ERROR'

;*******************************************************************************
; Target Status:  FN, len
; Entry with A=function code, B=data size, X=COMBUF+2

TARGET_STAT         proc
                    ldx       #TSTG               ; DATA FOR REPLY
                    ldy       #COMBUF             ; POINTER TO RETURN BUFFER
                    ldb       #::TSTG             ; LENGTH OF REPLY
                    stb       1,y                 ; SET SIZE IN REPLY BUFFER
Loop@@              lda       ,x                  ; MOVE REPLY DATA TO BUFFER
                    sta       2,y
                    inx
                    iny
                    decb
                    bne       Loop@@
                    jmp       SEND                ; Compute checksum on buffer, and send to master, then return

;*******************************************************************************
; Read Memory:  FN, len, page, Alo, Ahi, Nbytes
; Entry with A=function code, B=data size, X=COMBUF+2

READ_MEM            proc
                    @SetPage  ,x
          ;-------------------------------------- ; Get address
                    lda       2,x                 ; MSB OF ADDRESS IN A
                    ldb       1,x                 ; LSB OF ADDRESS IN B
                    xgdy                          ; ADDRESS IN Y
          ;-------------------------------------- ; Prepare return buffer: FN (unchanged), LEN, DATA
                    ldb       3,x                 ; NUMBER OF BYTES TO RETURN
                    stb       COMBUF+1            ; RETURN LENGTH = REQUESTED DATA
                    beq       Done@@              ; JIF NO BYTES TO GET
          ;-------------------------------------- ; Read the requested bytes from local memory
Loop@@              lda       ,y                  ; GET BYTE
                    sta       ,x                  ; STORE TO RETURN BUFFER
                    inx
                    iny
                    decb
                    bne       Loop@@
Done@@              jmp       SEND                ; Compute checksum on buffer, and send to master, then return

;*******************************************************************************
; Write Memory:  FN, len, page, Alo, Ahi, (len-3 bytes of Data)
; Entry with A=function code, B=data size, X=COMBUF+2
; Uses 6 bytes of stack

WRITE_MEM           proc
                    @SetPage  ,x
          ;-------------------------------------- ; Get address
                    lda       2,x                 ; MSB OF ADDRESS IN A
                    ldb       1,x                 ; LSB OF ADDRESS IN B
                    xgdy                          ; ADDRESS IN Y
          ;-------------------------------------- ; Prepare return buffer: FN (unchanged), LEN, DATA
                    ldb       COMBUF+1            ; NUMBER OF BYTES TO RETURN
                    subb      #3                  ; MINUS PAGE AND ADDRESS
                    beq       Cont@@              ; JIF NO BYTES TO PUT
          ;-------------------------------------- ; Write the specified bytes to local memory
                    pshb
                    pshx
                    pshy
Loop@@              lda       3,x                 ; GET BYTE TO WRITE
                    sta       ,y                  ; STORE THE BYTE AT AAAA,y
                    inx
                    iny
                    decb
                    bne       Loop@@
          ;-------------------------------------- ; Compare to see if the write worked
                    puly
                    pulx
                    pulb
_1@@                lda       3,x                 ; GET BYTE JUST WRITTEN
                    cmpa      ,y
                    bne       Fail@@              ; BR IF WRITE FAILED
                    inx
                    iny
                    decb
                    bne       _1@@
          ;-------------------------------------- ; Write succeeded:  return status = 0
Cont@@              clra                          ; RETURN STATUS = 0
                    bra       Done@@
Fail@@              lda       #1                  ; Write failed:  return status = 1
Done@@              jmp       SEND_STATUS         ; Return status

;*******************************************************************************
; Read registers:  FN, len=0
; Entry with A=function code, B=data size, X=COMBUF+2

READ_REGS           proc
          ;--------------------------------------
          ; Enter here from SWI after 'RUN' and 'STEP'
          ; to return task registers.
          ; CAUTION: In this case, assume no registers!
          ;--------------------------------------
RETURN_REGS         ldy       #TASK_REGS          ; POINTER TO REGISTERS
                    ldb       #::TASK_REGS        ; NUMBER OF BYTES
                    stb       COMBUF+1            ; SAVE RETURN DATA LENGTH
          ;-------------------------------------- ; Copy the registers
                    ldx       #COMBUF+2           ; POINTER TO RETURN BUFFER
Loop@@              lda       ,y                  ; GET BYTE TO A
                    sta       ,x                  ; STORE TO RETURN BUFFER
                    inx
                    iny
                    decb
                    bne       Loop@@
                    jmp       SEND                ; Compute checksum on buffer, and send to master, then return

;*******************************************************************************
; Write registers:  FN, len, (register image)
; Entry with A=function code, B=data size, X=COMBUF+2

WRITE_REGS          proc
                    ldb       COMBUF+1            ; NUMBER OF BYTES
                    beq       Done@@              ; JIF NO REGISTERS
          ;-------------------------------------- ; Copy the registers
                    ldy       #TASK_REGS          ; POINTER TO REGISTERS
Loop@@              lda       ,x                  ; GET BYTE TO A
                    sta       ,y                  ; STORE TO REGISTER RAM
                    inx
                    iny
                    decb
                    bne       Loop@@
          ;-------------------------------------- ; Return OK status
Done@@              clra
                    bra       SEND_STATUS

;*******************************************************************************
; Run Target:  FN, len
; Entry with A=function code, B=data size, X=COMBUF+2

RUN_TARGET          proc
          ;-------------------------------------- ; Restore user's page
                    @SetPage  REG_PAGE            ; User's page
          ;-------------------------------------- ; Switch to user stack
                    ldb       REG_SP              ; Back to user stack
                    lda       REG_SP+1
                    xgdx                          ; to X
                    inx                           ; Pre-correct for TXS
                    txs                           ; SP = X-1
          ;-------------------------------------- ; Restore registers
                    lda       REG_PC              ; Save LSB user PC for RTI
                    psha
                    lda       REG_PC+1            ; Save MSB user PC for RTI
                    psha

                    lda       REG_Y
                    psha
                    lda       REG_Y+1
                    psha

                    lda       REG_X
                    psha
                    lda       REG_X+1
                    psha

                    lda       REG_A
                    psha
                    lda       REG_B
                    psha

                    lda       REG_CC              ; SAVE USER CONDITION CODES FOR RTI
                    psha
                    RTI                           ; Return to user

;*******************************************************************************
; Set target byte(s):  FN, len { (page, alow, ahigh, data), (...)... }
; Entry with A=function code, B=data size, X=COMBUF+2
; Return has FN, len, (data from memory locations)
; If error in insert (memory not writable), abort to return short data
; This function is used primarily to set and clear breakpoints
; Uses 3 bytes of stack

SET_BYTES           proc
                    ldy       #COMBUF+1           ; POINTER TO RETURN BUFFER
                    clra
                    sta       ,y                  ; SET RETURN COUNT AS ZERO
                    iny                           ; POINT AT FIRST RETURN DATA BYTE
                    lsrb:2                        ; LEN/4 = NUMBER OF BYTES TO SET
                    beq       Done@@              ; JIF NO BYTES (COMBUF+1 = 0)
          ;-------------------------------------- ; Loop on inserting bytes
Loop@@              pshb                          ; SAVE LOOP COUNTER
                    pshy                          ; SAVE RETURN BUFFER POINTER
          ;-------------------------------------- ; Get address
                    @SetPage  ,x
                    lda       2,x                 ; MSB OF ADDRESS IN A
                    ldb       1,x                 ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y
                    lda       ,y                  ; Read current data at byte location
          ;-------------------------------------- ; Insert new data at byte location
                    ldb       3,x                 ; GET BYTE TO STORE
                    stb       ,y                  ; WRITE TARGET MEMORY
          ;-------------------------------------- ; Verify write
                    cmpb      ,y                  ; READ TARGET MEMORY
                    puly                          ; RESTORE RETURN PTR (CC'S INTACT)
                    pulb                          ; RESTORE LOOP COUNTER (CC'S INTACT)
                    bne       Done@@              ; BR IF INSERT FAILED: ABORT
          ;-------------------------------------- ; Save target byte in return buffer
                    sta       ,y
                    iny                           ; ADVANCE TO NEXT RETURN BYTE
                    inc       COMBUF+1            ; COUNT ONE RETURN BYTE
          ;-------------------------------------- ; Loop for next byte
                    inx:4                         ; STEP TO NEXT BYTE SPECIFIER
                    cmpb      COMBUF+1
                    bne       Loop@@              ; LOOP FOR ALL BYTES
                                                  ; Return buffer with data from byte locations
Done@@              bra       SEND                ; Compute checksum on buffer, and send to master, then return

;*******************************************************************************
; Input from port:  FN, len, PortAddressLo, PAhi (=0)
; While the HC11 has no input or output instructions, we retain these
; to allow write-without-verify
; Entry with A=function code, B=data size, X=COMBUF+2

IN_PORT             proc
          ;-------------------------------------- ; Get port address
                    lda       1,x                 ; MSB OF ADDRESS IN A
                    ldb       ,x                  ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y
                    lda       ,y                  ; Read the requested byte from local memory
                    bra       SEND_STATUS         ; Return byte read as 'status'

;*******************************************************************************
; Output to port:  FN, len, PortAddressLo, PAhi (=0), data
; Entry with A=function code, B=data size, X=COMBUF+2

OUT_PORT            proc
          ;-------------------------------------- ; Get port address
                    lda       1,x                 ; MSB OF ADDRESS IN A
                    ldb       ,x                  ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y
                    lda       2,x                 ; Get data
                    sta       ,y                  ; Write value to port
          ;-------------------------------------- ; Do not read port to verify (some I/O devices don't like it)
                    clra                          ; Return status of OK
;                   bra       SEND_STATUS

;*******************************************************************************
; Build status return with value from 'A'

SEND_STATUS         proc
                    sta       COMBUF+2            ; SET STATUS
                    lda       #1
                    sta       COMBUF+1            ; SET LENGTH
;                   bra       SEND

;*******************************************************************************
; Append checksum to COMBUF and send to master

SEND                proc
                    bsr       CHECKSUM            ; GET A=CHECKSUM, X->checksum location
                    nega
                    sta       ,x                  ; STORE NEGATIVE OF CHECKSUM
          ;-------------------------------------- ; Send buffer to master
                    ldx       #COMBUF             ; POINTER TO DATA
                    ldb       1,x                 ; LENGTH OF DATA
                    addb      #3                  ; PLUS FUNCTION, LENGTH, CHECKSUM
Loop@@              lda       ,x
                    jsr       PutChar             ; SEND A BYTE
                    inx
                    decb
                    bne       Loop@@
                    jmp       MAIN                ; BACK TO MAIN LOOP

;*******************************************************************************
; Compute checksum on COMBUF.  COMBUF+1 has length of data,
; Also include function byte and length byte
; Returns: A = checksum
;          X = pointer to next byte in buffer (checksum location)
;          B is scratched
; Uses 2 bytes of stack including return address

CHECKSUM            proc
                    ldx       #COMBUF             ; pointer to buffer
                    ldb       1,x                 ; length of message
                    addb      #2                  ; plus function, length
                    clra                          ; init checksum to 0
Loop@@              adda      ,x
                    inx
                    decb
                    bne       Loop@@              ; loop for all
                    rts                           ; return with checksum in A

;*******************************************************************************
; Interrupt handlers to catch unused interrupts and traps
; Registers are stacked.  Jump through RAM vector using X, type in A
; This will affect only interrupt routines looking for register values!
; Our default handler uses the code in 'A' as the processor state to be
; passed back to the host.

?                   macro
                    #temp     :index-1
                    lda       #20-:temp           ; {:temp*2+$FFD6(h)}
                    ldx       RAMVEC+{:temp*2}
                    jmp       ,x
                    endm
;-------------------------------------------------------------------------------
SCI_ENT             @?
SPI_ENT             @?
PACE_ENT            @?
PACO_ENT            @?
TOV_ENT             @?
TCOMP5_ENT          @?
TCOMP4_ENT          @?
TCOMP3_ENT          @?
TCOMP2_ENT          @?
TCOMP1_ENT          @?
TCAP3_ENT           @?
TCAP2_ENT           @?
TCAP1_ENT           @?
RTC_ENT             @?
IRQ_ENT             @?
          ;--------------------------------------
          ; Non-RAM vectored
          ;--------------------------------------
SWI_ENTRY           lda       #1
                    jmp       INT_ENTRY

XIRQ_ENTRY          lda       #2
                    jmp       INT_ENTRY

ILLOP_ENT           lda       #5
                    jmp       INT_ENTRY

;*******************************************************************************
                    #VECTORS
;*******************************************************************************
                    org       HARD_VECT           ; INTERRUPT VECTORS

NVEC                dw        SCI_ENT             ; FFD6
                    dw        SPI_ENT             ; FFD8
                    dw        PACE_ENT            ; FFDA
                    dw        PACO_ENT            ; FFDC
                    dw        TOV_ENT             ; FFDE
                    dw        TCOMP5_ENT          ; FFE0
                    dw        TCOMP4_ENT          ; FFE2
                    dw        TCOMP3_ENT          ; FFE4
                    dw        TCOMP2_ENT          ; FFE6
                    dw        TCOMP1_ENT          ; FFE8
                    dw        TCAP3_ENT           ; FFEA
                    dw        TCAP2_ENT           ; FFEC
                    dw        TCAP1_ENT           ; FFEE
                    dw        RTC_ENT             ; FFF0
                    dw        IRQ_ENT             ; FFF2
                    #size     NVEC                ; number of vector bytes
          ;--------------------------------------
          ; The remaining interrupts are permanently trapped to the monitor
          ;--------------------------------------
                    dw        XIRQ_ENTRY          ; FFF4 (non-maskable interrupt)
                    dw        SWI_ENTRY           ; FFF6 SWI/breakpoint
                    dw        ILLOP_ENT           ; FFF8 illegal op-code
                    dw        COP_ENT             ; FFFA Watchdog timeout
                    dw        CLOCK_ENT           ; FFFC clock fail
                    dw        Start               ; FFFE reset

                    end       Start
