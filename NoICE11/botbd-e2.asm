;  68HC11 Debug monitor for use with NOICE11
;
;  Copyright (c) 1992, 1993, 1995, 1997 by John Hartman
;
;  Modification History:
;       14-Jun-93 JLH release version
;        3-Aug-93 JLH improve I/O init documentation
;       24-Aug-93 JLH correct error in IN and OUT, stack init (v1.2)
;       12-May-94 JLH clarify TSTG paging info
;        7-Nov-94 JLH correct typos in comments
;        1-May-95 JLH correct error in RAMVEC usage (v1.3)
;       19-Aug-97 JLH correct bug in COP and Clock Monitor handling
;       23-Dec-97 JLH modify for use with BotBoard2 and HC11E2
;
; *============================================================================
;  This version of MONHC11 has been customized to work with Marvin Green's
;  BOTBoard2 using a 68HC11E2 processor.
;
;  It operates at 9600 baud using an 8 Mhz crystal.  Faster baud
;  rates are possible using a 7.3728 Mhz crystal.  This would require
;  changes to the baud rate calculations below.  This would also
;  require a change to the special bootstrap mode's baud rate when
;  using PCBUG.  Alternatively, this code could be downloaded using
;  PCBUG with an 8 Mhz crystal, and the crystal then replaced for
;  use by MONHC11.
;
;  The NoICE monitor resides in the E2's 2K of EEPROM, relocated to
;  address B800-BFFF.  The BOTBoard must be set to come up in SPECIAL TEST
;  mode, rather than the SPECIAL BOOTSTRAP mode used by PCBUG.  This
;  is done by changing DIP switch B from ON to OFF.
;
;  As supplied, this program sets PORTD bits 3 and 2 as outputs.
;  When a reset occurs, a brief beep is generated on the piezo, using
;  PD2 (you must jumper J3 appropriately).
;  When the monitor is waiting for characters, it toggles the LED
;  using PD3 as a heartbeat. (you must jumper J3 appropriately)
;  If you do not want these features, or have another need for PORTD,
;  such as SPI, you must modify this code as appropriate.
;
;  Steps are
;  1) assemble this program
;  2) set BOTBoard to SPECIAL BOOTSRAP MODE: DIPA=ON DIPB=ON
;  3) use PCBUG to burn MONHC11.S19 into EEPROM on the BOTBOard:
;  3a) MS $103C $E5
;  3b) EPROM $103F
;  3c) MS $103F $B5
;  3d) EPROM $B800 $BFFF
;  3e) LOADS MONHC11.S19
;  4) Set BOTBoard to SPECIAL TEST MODE: DIPA=ON DIPB=OFF
;
;  When the BOTBoard is next reset, MONHC11 will run.  User programs
;  may be loaded using NoICE, into the RAM from $C000 to $FFFF.
;  User interrupt vectors are $FFC0 to $FFFF.
;  RESET, XIRQ, Illegal-operation, COP-fail, Clock-fail, and SWI are
;  reserved by MONHC11.
;
;  Note that MONHC11, and hence the user program, are run in special
;  test mode.  Thus, the "protected" registers, which can normally
;  be written only during the first 64 cycles after reset, may be
;  written to at any time.  Since MONHC11 is running during the first
;  64 cycles after reset, this type of operations is intended to allow
;  user programs to initialize the HC11 as desired.  The user must
;  ensure that the 64 cycle limitation is respected or the program
;  will not operate correctly when run without MONHC11.
;
; *============================================================================
;
;  To customize for a given target, you must change code in the
;  hardware equates, the string TSTG, and the routines RESET and REWDT.
;  You may or may not need to change GETCHAR, PUTCHAR, depending on
;  how peculiar your UART is.
;
;  This file has been assembled with the Motorola Freeware assembler
;  available from the Motorola Freeware BBS and elsewhere.
;
;  To add mapped memory support:
;       1) Define map port MAPREG here
;       2) Define or import map port RAM image MAPIMG here if MAPREG is
;          write only.  (The application code must update MAPIMG before
;          outputing to MAPREG)
;       3) Search for and modify MAPREG, MAPIMG, and REG_PAGE usage below
;       4) In TSTG below edit "LOW AND HIGH LIMIT OF MAPPED MEM"
;          to appropriate range (typically 4000H to 07FFFH for two-bit MMU)
;
; *============================================================================
;
;  Hardware definitions
CHIP_RAM            equ       $0000               ; START OF HC11 ON-CHIP RAM
IO_START            equ       $1000               ; START OF HC11 ON-CHIP I/O
RAM_START           equ       $B000               ; START OF MONITOR RAM
ROM_START           equ       $BC00               ; START OF MONITOR CODE
HARD_VECT           equ       $BFD6               ; START OF HARDWARE VECTORS

USER_STACK          equ       $B000               ; TOP+1 OF USER STACK
USER_CODE           equ       $C000               ; START OF USER CODE SPACE
USER_VECT           equ       $FFD6               ; START OF USER VECTORS
;
                    #Page
; *============================================================================
;  Define HC11 I/O register locations (68HC11A8)
                    org       IO_START
H11PORTA            rmb       1                   ; X000 i/o port A
                    rmb       1                   ; X001 reserved
H11PIOC             rmb       1                   ; X002 i/o port C control
H11PORTC            rmb       1                   ; X003 i/o port C

H11PORTB            rmb       1                   ; X004 i/o port B
H11PORTCL           rmb       1                   ; X005 i/o port CL
                    rmb       1                   ; X006 reserved
H11DDRC             rmb       1                   ; X007 data direction for port C

H11PORTD            rmb       1                   ; X008 i/o port D
H11DDRD             rmb       1                   ; X009 data direction for port D
H11PORTE            rmb       1                   ; X00A input port E
H11CFORC            rmb       1                   ; X00B compare force register

H11OC1M             rmb       1                   ; X00C OC1 action mask register
H11OC1D             rmb       1                   ; X00D OC1 action data register
H11TCNT             rmb       2                   ; X00E timer counter register

H11TIC1             rmb       2                   ; X010 input capture register 1
H11TIC2             rmb       2                   ; X012 input capture register 2

H11TIC3             rmb       2                   ; X014 input capture register 3
H11TOC1             rmb       2                   ; X016 output compare register 1

H11TOC2             rmb       2                   ; X018 output compare register 2
H11TOC3             rmb       2                   ; X01A output compare register 3

H11TOC4             rmb       2                   ; X01C output compare register 4
H11TOC5             rmb       2                   ; X01E output compare register 5

H11TCTL1            rmb       1                   ; X020 timer control register 1
H11TCTL2            rmb       1                   ; X021 timer control register 2
H11TMSK1            rmb       1                   ; X022 main timer interrupt mask 1
H11TFLG1            rmb       1                   ; X023 main timer interrupt flag 1

H11TMSK2            rmb       1                   ; X024 misc timer interrupt mask 2
H11TFLG2            rmb       1                   ; X025 misc timer interrupt flag 2
H11PACTL            rmb       1                   ; X026 pulse accumulator control register
H11PACNT            rmb       1                   ; X027 pulse accumulator count register

H11SPCR             rmb       1                   ; X028 SPI control register
H11SPSR             rmb       1                   ; X029 SPI status register
H11SPDR             rmb       1                   ; X02A SPI data in/out
H11BAUD             rmb       1                   ; X02B SCI baud rate control

H11SCCR1            rmb       1                   ; X02C SCI control register 1
H11SCCR2            rmb       1                   ; X02D SCI control register 2
H11SCSR             rmb       1                   ; X02E SCI status register
H11SCDR             rmb       1                   ; X02F SCI data

H11ADCTL            rmb       1                   ; X030 A to D control register
H11ADR1             rmb       1                   ; X031 A to D result 1
H11ADR2             rmb       1                   ; X032 A to D result 2
H11ADR3             rmb       1                   ; X033 A to D result 3

H11ADR4             rmb       1                   ; X034 A to D result 4
H11BPROT            rmb       1                   ; X035 EEPROM block protect
                    rmb       2                   ; X036 reserved

H11OPT2             rmb       1                   ; X038 system configuration options 2
H11OPTION           rmb       1                   ; X039 system configuration options
H11COPRST           rmb       1                   ; X03A arm/reset COP timer circutry
H11PPROG            rmb       1                   ; X03B EEPROM programming control

H11HPRIO            rmb       1                   ; X03C highest priority I-bit and misc.
H11INIT             rmb       1                   ; X03D ram/io mapping register
H11TEST1            rmb       1                   ; X03E factory test control register
H11CONFIG           rmb       1                   ; X03F COP, ROM, & EEPROM enables

                    rmb       16                  ; X040 reserved
                    rmb       12                  ; X050 reserved

H11CSSTRH           rmb       1                   ; X05C Chip select clock stretch
H11CSCTL            rmb       1                   ; X05D Chip select control
H11CSGADR           rmb       1                   ; X05E General purpose CS address
H11CSGSIZ           rmb       1                   ; X05F General purpose CS size
;
;
; *============================================================================
;  HARDWARE PLATFORM CUSTOMIZATIONS
; *============================================================================
;
;  Put you UART equates here
SER_STATUS          equ       H11SCSR             ; STATUS FROM SCI
SER_RXDATA          equ       H11SCDR             ; DATA FROM SCI
SER_TXDATA          equ       H11SCDR             ; DAT TO SCI
RXRDY               equ       $20
TXRDY               equ       $40                 ; TRANSMIT COMPLETE (FOR TURNOFF)
;
; *============================================================================
;  RAM interrupt vectors
                    org       USER_VECT
RAMVEC              rmb       2*21
;
;  RAM definitions
                    org       RAM_START
;
;  Monitor stack
;  (Calculated use is at most 7 bytes.  Leave plenty of spare)
;  68HC11 SP points at NEXT BYTE TO USE, rather than at last used byte
;  like most processors.  Thus, init SP to TOP-1 of stack space
                    rmb       15
MONSTACK            rmb       1
;
;  Target registers:  order must match that in TRGHC11.C
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
TASK_REG_SZ         equ       *-TASK_REGS
;
;  Communications buffer
;  (Must be at least as long as the longer of TASK_REG_SZ or TSTG_SIZE.
;  At least 19 bytes recommended.  Larger values may improve speed of NoICE
;  download and memory move commands.)
COMBUF_SIZE         equ       128                 ; DATA SIZE FOR COMM BUFFER
COMBUF              rmb       2+COMBUF_SIZE+1     ; BUFFER ALSO HAS FN, LEN, AND CHECK
;
RAM_END             equ       *                   ; ADDRESS OF TOP+1 OF RAM
;
; *===========================================================================
                    org       ROM_START
;
;  Power on reset
RESET
;
;  Set CPU mode to safe state
                    sei                           ; INTERRUPTS OFF (WE MAY JUMP HERE)
                    clrb                          ; STATE 0 = "RESET"
                    bra       RES10

;
; *------------------------------------------------
;  COP reset
COP_ENT
                    ldab      #4                  ; STATE 4 = "COP"
                    bra       RES10

;
; *------------------------------------------------
;  Clock Fail reset
CLOCK_ENT
                    ldab      #3                  ; STATE 3 = "Clock fail"
;
;  Initialize HC11 hardware
;
;  BE SURE THAT "B" REMAINS INTACT UNTIL IT IS STORED TO REG_STATE BELOW!
RES10
;
;  Monitor assumes operation in either Normal Expanded or Special Test mode
;  The exact initialization required here will depend on which variant of
;  the 68HC11 you have, and on your hardware layout.  The following is
;  basic, and may not be sufficient for your case.
;
; *----------------------------------------------------------------------------
;  The following writes must occur within first 64 cycles after end of reset
;
;  CAUTION: DON'T USE I/O ADDRESS EQUATES UNTIL H11INIT IS WRITTEN
;  TO SET THE I/O BASE TO MATCH OUR EQUATES!

;  (Freeware assembler does not support parenthesis.  Thus, compute
;  each nibble separately)
CRAMLOC             equ       CHIP_RAM/256        ; ON-CHIP RAM AT CHIP_RAM (HIGH NIBBLE)
IOLOC               equ       IO_START/4096       ; I/O REGS AT IO_START (LOW NIBBLE)
                    ldaa      #CRAMLOC+IOLOC      ; LOCATION OF RAM, I/O
;***     STAA    H11INIT
                    staa      $103D               ; USE THE POST-RESET ADDRESS!
;
;  Save reset type (RESET, COP, or Clock Fail)
                    stab      REG_STATE           ; SAVE STATE
;
;  NOW OK TO USE I/O ADDRESS EQUATES
                    ldaa      #$B5                ; NO COP, ENABLE EEPROM AT Bxxx
                    staa      H11CONFIG
;
                    ldaa      #$00                ; PRESCALE TO DIVIDE BY 1
                    staa      H11TMSK2
;
                    ldaa      #$13                ; IRQ LEVEL, OSC DELAY, LONG COP
                    staa      H11OPTION
;
                    ldaa      #$E5                ; EXPANDED MODE (ACCESS EXTERNAL BUS)
                    staa      H11HPRIO

; *----------------------------------------------------------------------------
;
;  Possible additional special initialization
;       H11CSCTL        ;Chip select control
;       H11CSGSIZ       ;General purpose CS size
;       H11CSGADR       ;General purpose CS address
;       H11CSSTRH       ;Chip select clock stretch
;
;       H11BPROT        ;EEPROM block protect
;       H11PPROG        ;EEPROM programming control
;               ;enable programming voltage to program CONFIG register
;               ;wait for voltage to stabilize before programming
;
; *----------------------------------------------------------------------------
                    ldaa      #%00000010
                    staa      H11PORTD            ; PORT D: TX=1, LED OFF, PEIZO OFF
                    ldaa      #%00001110
                    staa      H11DDRD             ; PORT D: OUT: TX, LED, PIEZO
                    ldaa      #%00000000
                    staa      H11SPCR             ; DISABLE WIRE-OR, DISABLE SPI
;
                    lds       #MONSTACK           ; CLEAN STACK IS HAPPY STACK
;
;  Initialize your UART here
;  (SCI at 9600 baud from 8 Mhz crystal)
                    ldaa      #%00110000          ; PRE-DIV BY 13; DIV BY 1
                    staa      H11BAUD
                    ldaa      #$00                ; 8 BIT DATA
                    staa      H11SCCR1
                    ldaa      #$0C                ; TX AND RX ENABLED, NO INTS, NO WAKE
                    staa      H11SCCR2
;
; *----------------------------------------------------------------------------
;
;  Initialize RAM interrupt vectors
                    ldy       #INT_ENTRY          ; ADDRESS OF DEFAULT HANDLER
                    ldx       #RAMVEC             ; POINTER TO RAM VECTORS
                    ldab      #NVEC/2             ; NUMBER OF VECTORS
RST10               sty       0,X                 ; SET VECTOR
                    inx
                    inx
                    decb
                    bne       RST10
;
;  Initialize user registers
                    ldd       #USER_STACK
                    staa      REG_SP+1            ; INIT USER'S STACK POINTER MSB
                    stab      REG_SP              ; LSB
                    ldd       #0
                    std       REG_PC
                    staa      REG_A
                    staa      REG_B
                    std       REG_X
                    std       REG_Y
;
;  Initialize memory paging variables and hardware (if any)
                    sta       REG_PAGE            ; NO PAGE YET
; *;*     STA     MAPIMG
; *;*     STA     MAPREG                  set hardware map
;
;  Initialize non-zero registers
                    ldaa      #$50                ; disable interrupts in user program
                    staa      REG_CC
;
;  Beep to signal life (and occurance of a reset)
;  1 Khz = 500 usec half-period = 1000 cycles with E=2 Mhz (8 Mhz crystal)
                    ldx       #1000
BEP10               ldab      #200
                    ldaa      H11PORTD
                    eora      #$04                ; TOGGLE BEEPER
                    staa      H11PORTD
BEP20               decb                          ; 2
                    bne       BEP20               ; 3 = 5 CYCLES. 200*5 = 1000
                    dex
                    bne       BEP10
;
;  Set function code for "GO".  Then if we are here because of a reset
;  (such as a COP timeout) after being told to GO, we will come
;  back with registers so user can see the reset
                    ldaa      #FN_RUN_TARG
                    staa      COMBUF
                    jmp       RETURN_REGS         ; DUMP REGS, ENTER MONITOR

;
; *===========================================================================
;  Get a character to A
;
;  Return A=char, CY=0 if data received
;         CY=1 if timeout (0.5 seconds)
;
;  Uses 6 bytes of stack including return address
;
GETCHAR
                    ldaa      H11PORTD
                    eora      #$08                ; TOGGLE LED: WE ARE ALIVE...
                    staa      H11PORTD
;
                    pshx
                    ldx       #0                  ; LONG TIMEOUT
GC10                bsr       REWDT               ; PREVENT WATCHDOG TIMEOUT
                    dex
                    beq       GC90                ; EXIT IF TIMEOUT
                    ldaa      SER_STATUS          ; READ DEVICE STATUS
                    anda      #RXRDY
                    beq       GC10                ; NOT READY YET.
;
;  Data received:  return CY=0. data in A
                    clc                           ; CY=0
                    ldaa      SER_RXDATA          ; READ DATA
                    pulx
                    rts

;
;  Timeout:  return CY=1
GC90                sec                           ; CY=1
                    pulx
                    rts

;
; *===========================================================================
;  Output character in A
;
;  Uses 5 bytes of stack including return address
;
PUTCHAR
                    psha
PC10                bsr       REWDT               ; PREVENT WATCHDOG TIMEOUT
                    ldaa      SER_STATUS          ; CHECK TX STATUS
                    anda      #TXRDY              ; TX READY ?
                    beq       PC10
                    pula
                    staa      SER_TXDATA          ; TRANSMIT CHAR.
                    rts

;
; *======================================================================
;  Reset watchdog timer.  Must be called at least once every little while
;  or COP interrupt will occur
;
;  Uses 2 bytes of stack including return address
;
REWDT               ldaa      #$55
                    staa      H11COPRST
                    ldaa      #$AA
                    staa      H11COPRST
                    rts

;
; *======================================================================
;  Response string for GET TARGET STATUS request
;  Reply describes target:
TSTG                fcb       3                   ; 2: PROCESSOR TYPE = 68HC11
                    fcb       COMBUF_SIZE         ; 3: SIZE OF COMMUNICATIONS BUFFER
                    fcb       0                   ; 4: NO TASKING SUPPORT
                    fcb       0,0                 ; 5,6: BOTTOM OF MAPPED MEM (LSB FIRST)
                    fcb       0,0                 ; 7,8: TOP OF MAPPED MEM (LSB FIRST)
                    fcb       B1-B0               ; 9 BREAKPOINT INSTR LENGTH
B0                  swi                           ; 10+ BREKAPOINT INSTRUCTION
B1                  fcc       'NoICE 68HC11 monitor for BOTBoard 2.  V2.0'  ; DESCRIPTION, ZERO
                    fcb       0
TSTG_SIZE           equ       *-TSTG              ; SIZE OF STRING
;
; *======================================================================
;  HARDWARE PLATFORM INDEPENDENT EQUATES AND CODE
;
;  Communications function codes.
FN_GET_STAT         equ       $FF                 ; reply with device info
FN_READ_MEM         equ       $FE                 ; reply with data
FN_WRITE_M          equ       $FD                 ; reply with status (+/-)
FN_READ_RG          equ       $FC                 ; reply with registers
FN_WRITE_RG         equ       $FB                 ; reply with status
FN_RUN_TARG         equ       $FA                 ; reply (delayed) with registers
FN_SET_BYTE         equ       $F9                 ; reply with data (truncate if error)
FN_IN               equ       $F8                 ; input from port
FN_OUT              equ       $F7                 ; output to port
;
FN_MIN              equ       $F7                 ; MINIMUM RECOGNIZED FUNCTION CODE
FN_ERROR            equ       $F0                 ; error reply to unknown op-code
;
; *===========================================================================
;  Common handler for default interrupt handlers
;  Enter with A=interrupt code = processor state
;  All registers stacked, PC=next instruction
INT_ENTRY
                    staa      REG_STATE           ; SAVE STATE
;
;  Save registers from stack to reg block for return to master
;  Host wants least significant bytes first, so flip as necessary
                    pula
                    staa      REG_CC              ; CONDITION CODES
                    pula
                    staa      REG_B
                    pula
                    staa      REG_A
                    pula
                    staa      REG_X+1             ; MSB
                    pula
                    staa      REG_X               ; LSB
                    pula
                    staa      REG_Y+1             ; MSB
                    pula
                    staa      REG_Y               ; LSB
;
;  If this is a breakpoint (state = 1), then back up PC to point at SWI
;  (If SWI2, SWI3, or another instruction is used for breakpoint,
;  then DEX multiple times to match B1-B0 in TSTG
                    pulx                          ; PC AFTER INTERRUPT
                    ldaa      REG_STATE
                    cmpa      #1
                    bne       NOTBP               ; BR IF NOT A BREAKPOINT
                    dex                           ; ELSE BACK UP TO POINT AT SWI LOCATION
NOTBP               xgdx                          ; TRANSFER PC TO D
                    staa      REG_PC+1            ; MSB
                    stab      REG_PC              ; LSB
                    tsx                           ; USER STACK POINTER PLUS 1
                    dex                           ; MAKE IT JUST LIKE THE REAL SP
                    xgdx
                    stab      REG_SP              ; SAVE USER'S STACK POINTER (LSB)
                    staa      REG_SP+1            ; MSB
;
;  Change to our own stack
                    lds       #MONSTACK           ; AND USE OURS INSTEAD
;
;  Save memory page
; *;*     LDAA    MAPIMG          GET CURRENT USER MAP
                    ldaa      #0                  ; ... OR ZERO IF UNMAPPED TARGET
                    staa      REG_PAGE            ; SAVE USER'S PAGE
;
;  Return registers to master
                    jmp       RETURN_REGS

;
; *===========================================================================
;  Main loop:  wait for command frame from master
;
;  Uses 7 bytes of stack before jump to handlers
;
MAIN                lds       #MONSTACK           ; CLEAN STACK IS HAPPY STACK
                    ldx       #COMBUF             ; BUILD MESSAGE HERE
;
;  First byte is a function code
                    jsr       GETCHAR             ; GET A FUNCTION
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    cmpa      #FN_MIN
                    blo       MAIN                ; JIF BELOW MIN: ILLEGAL FUNCTION
                    staa      0,X                 ; SAVE FUNCTION CODE
                    inx
;
;  Second byte is data byte count (may be zero)
                    jsr       GETCHAR             ; GET A LENGTH BYTE
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    cmpa      #COMBUF_SIZE
                    bhi       MAIN                ; JIF TOO LONG: ILLEGAL LENGTH
                    staa      0,X                 ; SAVE LENGTH
                    inx
                    cmpa      #0
                    beq       MA80                ; SKIP DATA LOOP IF LENGTH = 0
;
;  Loop for data
                    tab                           ; SAVE LENGTH FOR LOOP
MA10                jsr       GETCHAR             ; GET A DATA BYTE
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    staa      0,X                 ; SAVE DATA BYTE
                    inx
                    decb
                    bne       MA10
;
;  Get the checksum
MA80                jsr       GETCHAR             ; GET THE CHECKSUM
                    bcs       MAIN                ; JIF TIMEOUT: RESYNC
                    psha                          ; SAVE CHECKSUM
;
;  Compare received checksum to that calculated on received buffer
;  (Sum should be 0)
                    jsr       CHECKSUM
                    pulb
                    aba
                    bne       MAIN                ; JIF BAD CHECKSUM
;
;  Process the message.
                    ldx       #COMBUF
                    ldaa      0,X                 ; GET THE FUNCTION CODE
                    ldab      1,X                 ; GET THE LENGTH
                    inx
                    inx                           ; X POINTS AT DATA
                    cmpa      #FN_GET_STAT
                    beq       TARGET_STAT
                    cmpa      #FN_READ_MEM
                    beq       JREAD_MEM
                    cmpa      #FN_WRITE_M
                    beq       JWRITE_MEM
                    cmpa      #FN_READ_RG
                    beq       JREAD_REGS
                    cmpa      #FN_WRITE_RG
                    beq       JWRITE_REGS
                    cmpa      #FN_RUN_TARG
                    beq       JRUN_TARGET
                    cmpa      #FN_SET_BYTE
                    beq       JSET_BYTES
                    cmpa      #FN_IN
                    beq       JIN_PORT
                    cmpa      #FN_OUT
                    beq       JOUT_PORT
;
;  Error: unknown function.  Complain
                    ldaa      #FN_ERROR
                    staa      COMBUF              ; SET FUNCTION AS "ERROR"
                    ldaa      #1
                    jmp       SEND_STATUS         ; VALUE IS "ERROR"

;
;  long jumps to handlers
JREAD_MEM           bra       READ_MEM

JWRITE_MEM          bra       WRITE_MEM

JREAD_REGS          bra       READ_REGS

JWRITE_REGS         jmp       WRITE_REGS

JRUN_TARGET         jmp       RUN_TARGET

JSET_BYTES          jmp       SET_BYTES

JIN_PORT            jmp       IN_PORT

JOUT_PORT           jmp       OUT_PORT

; *===========================================================================
;
;  Target Status:  FN, len
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
TARGET_STAT
                    ldx       #TSTG               ; DATA FOR REPLY
                    ldy       #COMBUF             ; POINTER TO RETURN BUFFER
                    ldab      #TSTG_SIZE          ; LENGTH OF REPLY
                    stab      1,Y                 ; SET SIZE IN REPLY BUFFER
TS10                ldaa      0,X                 ; MOVE REPLY DATA TO BUFFER
                    staa      2,Y
                    inx
                    iny
                    decb
                    bne       TS10
;
;  Compute checksum on buffer, and send to master, then return
                    jmp       SEND

; *===========================================================================
;
;  Read Memory:  FN, len, page, Alo, Ahi, Nbytes
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
READ_MEM
;
;  Set map
; *;      LDAA    0,X
; *;      STAA    MAPIMG
; *;      STAA    MAPREG
;
;  Get address
                    ldaa      2,X                 ; MSB OF ADDRESS IN A
                    ldab      1,X                 ; LSB OF ADDRESS IN B
                    xgdy                          ; ADDRESS IN Y
;
;  Prepare return buffer: FN (unchanged), LEN, DATA
                    ldab      3,X                 ; NUMBER OF BYTES TO RETURN
                    stab      COMBUF+1            ; RETURN LENGTH = REQUESTED DATA
                    beq       GLP90               ; JIF NO BYTES TO GET
;
;  Read the requested bytes from local memory
GLP                 ldaa      0,Y                 ; GET BYTE
                    staa      0,X                 ; STORE TO RETURN BUFFER
                    inx
                    iny
                    decb
                    bne       GLP
;
;  Compute checksum on buffer, and send to master, then return
GLP90               jmp       SEND

; *===========================================================================
;
;  Write Memory:  FN, len, page, Alo, Ahi, (len-3 bytes of Data)
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
;  Uses 6 bytes of stack
;
WRITE_MEM
;
;  Set map
; *;      LDAA    0,X
; *;      STAA    MAPIMG
; *;      STAA    MAPREG
;
;  Get address
                    ldaa      2,X                 ; MSB OF ADDRESS IN A
                    ldab      1,X                 ; LSB OF ADDRESS IN B
                    xgdy                          ; ADDRESS IN Y
;
;  Prepare return buffer: FN (unchanged), LEN, DATA
                    ldab      COMBUF+1            ; NUMBER OF BYTES TO RETURN
                    subb      #3                  ; MINUS PAGE AND ADDRESS
                    beq       WLP50               ; JIF NO BYTES TO PUT
;
;  Write the specified bytes to local memory
                    pshb
                    pshx
                    pshy
WLP                 ldaa      3,X                 ; GET BYTE TO WRITE
                    staa      0,Y                 ; STORE THE BYTE AT AAAA,Y
                    inx
                    iny
                    decb
                    bne       WLP
;
;  Compare to see if the write worked
                    puly
                    pulx
                    pulb
WLP20               ldaa      3,X                 ; GET BYTE JUST WRITTEN
                    cmpa      0,Y
                    bne       WLP80               ; BR IF WRITE FAILED
                    inx
                    iny
                    decb
                    bne       WLP20
;
;  Write succeeded:  return status = 0
WLP50               ldaa      #0                  ; RETURN STATUS = 0
                    bra       WLP90

;
;  Write failed:  return status = 1
WLP80               ldaa      #1
;
;  Return OK status
WLP90               jmp       SEND_STATUS

; *===========================================================================
;
;  Read registers:  FN, len=0
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
READ_REGS
;
;  Enter here from SWI after "RUN" and "STEP" to return task registers
;  CAUTION:  in this case, assume no registers!
RETURN_REGS
                    ldy       #TASK_REGS          ; POINTER TO REGISTERS
                    ldab      #TASK_REG_SZ        ; NUMBER OF BYTES
                    stab      COMBUF+1            ; SAVE RETURN DATA LENGTH
;
;  Copy the registers
                    ldx       #COMBUF+2           ; POINTER TO RETURN BUFFER
GRLP                ldaa      0,Y                 ; GET BYTE TO A
                    staa      0,X                 ; STORE TO RETURN BUFFER
                    inx
                    iny
                    decb
                    bne       GRLP
;
;  Compute checksum on buffer, and send to master, then return
                    jmp       SEND

; *===========================================================================
;
;  Write registers:  FN, len, (register image)
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
WRITE_REGS
;
                    ldab      COMBUF+1            ; NUMBER OF BYTES
                    beq       WRR80               ; JIF NO REGISTERS
;
;  Copy the registers
                    ldy       #TASK_REGS          ; POINTER TO REGISTERS
WRRLP               ldaa      0,X                 ; GET BYTE TO A
                    staa      0,Y                 ; STORE TO REGISTER RAM
                    inx
                    iny
                    decb
                    bne       WRRLP
;
;  Return OK status
WRR80               ldaa      #0
                    jmp       SEND_STATUS

; *===========================================================================
;
;  Run Target:  FN, len
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
RUN_TARGET
;
;  Restore user's map
; *;      LDAA    REG_PAGE                USER'S PAGE
; *;      STAA    MAPIMG                  SET IMAGE
; *;      STAA    MAPREG                  SET MAPPING REGISTER
;
;  Switch to user stack
                    ldab      REG_SP              ; BACK TO USER STACK
                    ldaa      REG_SP+1
                    xgdx                          ; TO X
                    inx                           ; PRE-CORRECT FOR TXS
                    txs                           ; SP = X-1
;
;  Restore registers
                    ldaa      REG_PC              ; SAVE LS USER PC FOR RTI
                    psha
                    ldaa      REG_PC+1            ; SAVE MS USER PC FOR RTI
                    psha
;
                    ldaa      REG_Y
                    psha
                    ldaa      REG_Y+1
                    psha
;
                    ldaa      REG_X
                    psha
                    ldaa      REG_X+1
                    psha
;
                    ldaa      REG_A
                    psha
                    ldaa      REG_B
                    psha
;
                    ldaa      REG_CC              ; SAVE USER CONDITION CODES FOR RTI
                    psha
;
;  Return to user
                    rti

;
; *===========================================================================
;
;  Set target byte(s):  FN, len { (page, alow, ahigh, data), (...)... }
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
;  Return has FN, len, (data from memory locations)
;
;  If error in insert (memory not writable), abort to return short data
;
;  This function is used primarily to set and clear breakpoints
;
;  Uses 3 bytes of stack
;
SET_BYTES
                    ldy       #COMBUF+1           ; POINTER TO RETURN BUFFER
                    ldaa      #0
                    staa      0,Y                 ; SET RETURN COUNT AS ZERO
                    iny                           ; POINT AT FIRST RETURN DATA BYTE
                    lsrb
                    lsrb                          ; LEN/4 = NUMBER OF BYTES TO SET
                    beq       SB99                ; JIF NO BYTES (COMBUF+1 = 0)
;
;  Loop on inserting bytes
SB10                pshb                          ; SAVE LOOP COUNTER
                    pshy                          ; SAVE RETURN BUFFER POINTER
;
;  Set map
; *;      LDAA    0,X
; *;      STAA    MAPIMG
; *;      STAA    MAPREG
;
;  Get address
                    ldaa      2,X                 ; MSB OF ADDRESS IN A
                    ldab      1,X                 ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y
;
;  Read current data at byte location
                    ldaa      0,Y
;
;  Insert new data at byte location
                    ldab      3,X                 ; GET BYTE TO STORE
                    stab      0,Y                 ; WRITE TARGET MEMORY
;
;  Verify write
                    cmpb      0,Y                 ; READ TARGET MEMORY
                    puly                          ; RESTORE RETURN PTR (CC'S INTACT)
                    pulb                          ; RESTORE LOOP COUNTER (CC'S INTACT)
                    bne       SB90                ; BR IF INSERT FAILED: ABORT
;
;  Save target byte in return buffer
                    staa      0,Y
                    iny                           ; ADVANCE TO NEXT RETURN BYTE
                    inc       COMBUF+1            ; COUNT ONE RETURN BYTE
;
;  Loop for next byte
                    inx                           ; STEP TO NEXT BYTE SPECIFIER
                    inx
                    inx
                    inx
                    cmpb      COMBUF+1
                    bne       SB10                ; LOOP FOR ALL BYTES
;
;  Return buffer with data from byte locations
SB90
;
;  Compute checksum on buffer, and send to master, then return
SB99                bra       SEND

; *===========================================================================
;
;  Input from port:  FN, len, PortAddressLo, PAhi (=0)
;
;  While the HC11 has no input or output instructions, we retain these
;  to allow write-without-verify
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
IN_PORT
;
;  Get port address
                    ldaa      1,X                 ; MSB OF ADDRESS IN A
                    ldab      0,X                 ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y
;
;  Read the requested byte from local memory
                    ldaa      0,Y
;
;  Return byte read as "status"
                    bra       SEND_STATUS

; *===========================================================================
;
;  Output to port:  FN, len, PortAddressLo, PAhi (=0), data
;
;  Entry with A=function code, B=data size, X=COMBUF+2
;
OUT_PORT
;
;  Get port address
                    ldaa      1,X                 ; MSB OF ADDRESS IN A
                    ldab      0,X                 ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y
;
;  Get data
                    ldaa      2,X
;
;  Write value to port
                    staa      0,Y
;
;  Do not read port to verify (some I/O devices don't like it)
;
;  Return status of OK
                    ldaa      #0
;                   bra       SEND_STATUS

; *===========================================================================
;  Build status return with value from "A"
;
SEND_STATUS
                    staa      COMBUF+2            ; SET STATUS
                    ldaa      #1
                    staa      COMBUF+1            ; SET LENGTH
;                   bra       SEND

; *===========================================================================
;  Append checksum to COMBUF and send to master
;
SEND                bsr       CHECKSUM            ; GET A=CHECKSUM, X->checksum location
                    nega
                    staa      0,X                 ; STORE NEGATIVE OF CHECKSUM
;
;  Send buffer to master
                    ldx       #COMBUF             ; POINTER TO DATA
                    ldab      1,X                 ; LENGTH OF DATA
                    addb      #3                  ; PLUS FUNCTION, LENGTH, CHECKSUM
SND10               ldaa      0,X
                    jsr       PUTCHAR             ; SEND A BYTE
                    inx
                    decb
                    bne       SND10
;
                    jmp       MAIN                ; BACK TO MAIN LOOP

; *===========================================================================
;  Compute checksum on COMBUF.  COMBUF+1 has length of data,
;  Also include function byte and length byte
;
;  Returns:
;       A = checksum
;       X = pointer to next byte in buffer (checksum location)
;       B is scratched
;
;  Uses 2 bytes of stack including return address
;
CHECKSUM
                    ldx       #COMBUF             ; pointer to buffer
                    ldab      1,X                 ; length of message
                    addb      #2                  ; plus function, length
                    ldaa      #0                  ; init checksum to 0
CHK10               adda      0,X
                    inx
                    decb
                    bne       CHK10               ; loop for all
                    rts                           ; return with checksum in A

;***********************************************************************
;
;  Interrupt handlers to catch unused interrupts and traps
;  Registers are stacked.  Jump through RAM vector using X, type in A
;
;  This will affect only interrupt routines looking for register values!
;
;  Our default handler uses the code in "A" as the processor state to be
;  passed back to the host.
;
SCI_ENT             ldaa      #20                 ; ffd6
                    ldx       RAMVEC+0
                    jmp       0,X

;
SPI_ENT             ldaa      #19                 ; ffd8
                    ldx       RAMVEC+2
                    jmp       0,X

;
PACE_ENT            ldaa      #18                 ; ffda
                    ldx       RAMVEC+4
                    jmp       0,X

;
PACO_ENT            ldaa      #17                 ; ffdc
                    ldx       RAMVEC+6
                    jmp       0,X

;
TOV_ENT             ldaa      #16                 ; ffde
                    ldx       RAMVEC+8
                    jmp       0,X

;
TCOMP5_ENT          ldaa      #15                 ; ffe0
                    ldx       RAMVEC+10
                    jmp       0,X

;
TCOMP4_ENT          ldaa      #14                 ; ffe2
                    ldx       RAMVEC+12
                    jmp       0,X

;
TCOMP3_ENT          ldaa      #13                 ; ffe4
                    ldx       RAMVEC+14
                    jmp       0,X

;
TCOMP2_ENT          ldaa      #12                 ; ffe6
                    ldx       RAMVEC+16
                    jmp       0,X

;
TCOMP1_ENT          ldaa      #11                 ; ffe8
                    ldx       RAMVEC+18
                    jmp       0,X

;
TCAP3_ENT           ldaa      #10                 ; ffea
                    ldx       RAMVEC+20
                    jmp       0,X

;
TCAP2_ENT           ldaa      #9                  ; ffec
                    ldx       RAMVEC+22
                    jmp       0,X

;
TCAP1_ENT           ldaa      #8                  ; ffee
                    ldx       RAMVEC+24
                    jmp       0,X

;
RTC_ENT             ldaa      #7                  ; fff0
                    ldx       RAMVEC+26
                    jmp       0,X

;
IRQ_ENT             ldaa      #6                  ; fff2
                    ldx       RAMVEC+28
                    jmp       0,X

;
;  Non-RAM vectored
SWI_ENTRY           ldaa      #1
                    jmp       INT_ENTRY

XIRQ_ENTRY          ldaa      #2
                    jmp       INT_ENTRY

ILLOP_ENT           ldaa      #5
                    jmp       INT_ENTRY

;
;  INTERRUPT VECTORS
                    org       HARD_VECT
;
;  VECTORS THROUGH RAM
VEC0                fdb       SCI_ENT             ; ffd6
                    fdb       SPI_ENT             ; ffd8
                    fdb       PACE_ENT            ; ffda
                    fdb       PACO_ENT            ; ffdc
                    fdb       TOV_ENT             ; ffde
                    fdb       TCOMP5_ENT          ; ffe0
                    fdb       TCOMP4_ENT          ; ffe2
                    fdb       TCOMP3_ENT          ; ffe4
                    fdb       TCOMP2_ENT          ; ffe6
                    fdb       TCOMP1_ENT          ; ffe8
                    fdb       TCAP3_ENT           ; ffea
                    fdb       TCAP2_ENT           ; ffec
                    fdb       TCAP1_ENT           ; ffee
                    fdb       RTC_ENT             ; fff0
                    fdb       IRQ_ENT             ; fff2
NVEC                equ       *-VEC0              ; number of vector bytes
;
;  The remaining interrupts are permanently trapped to the monitor
                    fdb       XIRQ_ENTRY          ; fff4 (non-maskable interrupt)
                    fdb       SWI_ENTRY           ; fff6 SWI/breakpoint
                    fdb       ILLOP_ENT           ; fff8 illegal op-code
                    fdb       COP_ENT             ; fffa Watchdog timeout
                    fdb       CLOCK_ENT           ; fffc clock fail
                    fdb       RESET               ; fffe reset
;
                    end       RESET
