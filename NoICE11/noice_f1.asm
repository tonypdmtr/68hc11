;*******************************************************************************
; Program   : NOICE_F1.ASM
; Programmer: John Hardman (JLH)       Original version
;           : Tony Papadimitriou (TGP) Modified version
; Purpose   : 68HC11 Debug monitor for use with NOICE11
;           : and the ASPiSYS F1 Board or compatible hardware
; Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
; Status    : FREEWARE
; History   : 93.06.14 JLH release version
;           : 93.08.03 JLH improve I/O init documentation
;           : 93.08.24 JLH correct error in IN and OUT, stack init (v1.2)
;           : 94.05.12 JLH clarify TSTG paging info
;           : 94.11.07 JLH correct typos in comments
;           : 95.05.01 JLH correct error in RAMVEC usage (v1.3)
;           : 97.08.19 JLH correct bug in COP and Clock Monitor handling
;           : 98.02.25 JLH assemble with either Motorola or Dunfield
;           : -----------------------------------------------------------
;           : 99.11.27 TGP Modify for ASM11 and ASPiSYS F1 Board
;           : 00.10.16 TGP Improved source code
;           : 00.10.22 TGP Allowed non-SWI breakpoints (TEST)
;           : 00.10.26 TGP Disallowed loading to non-user memory
;*******************************************************************************
; Copyright (c) 2019 by John Hartman (JLH)
; Modified for ASM11 by Tony G Papadimitriou <tonyp@acm.org>
; For use with the ASPiSYS F1 Board or compatible hardware.
;===============================================================================
; To customize for a given target, you must change code in the
; hardware equates, the string TSTG, and the routines RESET and KickCOP.
; You may or may not need to change GetChar, PutChar, depending on
; how peculiar your UART is.
;
; To add mapped memory support:
; 1) Define map port MAPREG here
; 2) Define or import map port RAM image MAPIMG here if MAPREG is
; write only.  (The application code must update MAPIMG before
; outputing to MAPREG)
; 3) Search for and modify MAPREG, MAPIMG, and REG_PAGE usage below
; 4) In TSTG below edit "LOW AND HIGH LIMIT OF MAPPED MEM"
; to appropriate range (typically $4000 to $7FFF for two-bit MMU)
;*******************************************************************************

                    #CaseOn                       ; Case insensitive labels
                    #ExtraOn                      ; Allow extra mnemonics
                    #OptRelOn                     ; Show optimization warnings for JMP/JSR
                    #OptRtsOn                     ; Show optimization warnings for JSR/BSR, RTS
                    #SpacesOff                    ; Do not allow spaces within expressions
 #ifdef ?
   #Hint     旼컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
   #Hint     � Available conditionals (for use with -Dx option)
   #Hint     쳐컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
   #Hint     � SWI...: Allows user usage of SWI instruction
   #Hint     � OS....: When SWI above is used allows usage of OS
   #Hint     � MAPPED: Allow memory mapping (w/ MMU hardware)
   #Hint     � ROM:nn: Move ROM to address nn
   #Hint     읕컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴
   #Fatal    Run ASM11 -Dx (where x is any of the above)
#endif
          #ifdef    OS
SWI                 def       0                   ;OS implies SWI
          #endif
_PC_                equ       7                   ;stack frame PC offset
_X_                 equ       3                   ;stack frame X offset
_B_                 equ       1                   ;stack frame B offset
          #ifdef MAPPED
                    #Message  Mapped Memory enabled (MAPREG/MAPIMG values OK?)
MAPREG              equ       $4000               ;/Address of MAP Register
MAPIMG              equ       $4001               ;\Modify according to hardware
          #endif
          ;-------------------------------------- ;Hardware definitions
CHIP_RAM            equ       $0000               ;START OF HC11 ON-CHIP RAM
IO_START            equ       $1000               ;START OF HC11 ON-CHIP I/O
RAM_START           equ       $0400               ;START OF MONITOR RAM (external)

ROM                 def       $FC00               ;START OF MONITOR CODE
          #if ROM < $8000
                    #Fatal    ROM cannot be less than $8000
          #endif
VECTORS             equ       $FFD6               ;START OF HARDWARE VECTORS
          #if VECTORS&$0FFF <> $0FD6
                    #Warning  VECTORS is incorrect ({VECTORS(h)}, not $xFD6)
          #endif
                    #Memory   ROM VECTORS-23
                    #Memory   VECTORS VECTORS+41
;-------------------------------------------------------------------------------
; Define HC11 I/O register locations (68HC11A8)
;-------------------------------------------------------------------------------
                    #SEG1
                    org       IO_START

                    rmb       1                   ;X000 i/o port A
                    rmb       1                   ;X001 reserved
                    rmb       1                   ;X002 i/o port C control
                    rmb       1                   ;X003 i/o port C

                    rmb       1                   ;X004 i/o port B
                    rmb       1                   ;X005 i/o port CL
                    rmb       1                   ;X006 reserved
                    rmb       1                   ;X007 data direction for port C

                    rmb       1                   ;X008 i/o port D
                    rmb       1                   ;X009 data direction for port D
                    rmb       1                   ;X00A input port E
                    rmb       1                   ;X00B compare force register

                    rmb       1                   ;X00C OC1 action mask register
                    rmb       1                   ;X00D OC1 action data register
                    rmb       2                   ;X00E timer counter register

                    rmb       2                   ;X010 input capture register 1
                    rmb       2                   ;X012 input capture register 2

                    rmb       2                   ;X014 input capture register 3
                    rmb       2                   ;X016 output compare register 1

                    rmb       2                   ;X018 output compare register 2
                    rmb       2                   ;X01A output compare register 3

                    rmb       2                   ;X01C output compare register 4
                    rmb       2                   ;X01E output compare register 5

                    rmb       1                   ;X020 timer control register 1
                    rmb       1                   ;X021 timer control register 2
                    rmb       1                   ;X022 main timer interrupt mask 1
                    rmb       1                   ;X023 main timer interrupt flag 1

H11TMSK2            rmb       1                   ;X024 misc timer interrupt mask 2
                    rmb       1                   ;X025 misc timer interrupt flag 2
                    rmb       1                   ;X026 pulse accumulator control register
                    rmb       1                   ;X027 pulse accumulator count register

                    rmb       1                   ;X028 SPI control register
                    rmb       1                   ;X029 SPI status register
                    rmb       1                   ;X02A SPI data in/out
H11BAUD             rmb       1                   ;X02B SCI baud rate control

H11SCCR1            rmb       1                   ;X02C SCI control register 1
H11SCCR2            rmb       1                   ;X02D SCI control register 2
H11SCSR             rmb       1                   ;X02E SCI status register
H11SCDR             rmb       1                   ;X02F SCI data

                    rmb       1                   ;X030 A to D control register
                    rmb       1                   ;X031 A to D result 1
                    rmb       1                   ;X032 A to D result 2
                    rmb       1                   ;X033 A to D result 3

                    rmb       1                   ;X034 A to D result 4
H11BPROT            rmb       1                   ;X035 EEPROM block protect
                    rmb       2                   ;X036 reserved

                    rmb       1                   ;X038 system configuration options 2
H11OPTION           rmb       1                   ;X039 system configuration options
H11COPRST           rmb       1                   ;X03A arm/reset COP timer circutry
                    rmb       1                   ;X03B EEPROM programming control

                    rmb       1                   ;X03C highest priority I-bit and misc.
                    rmb       1                   ;X03D ram/io mapping register
XINIT               equ       $103D               ;Post-reset INIT location
                    rmb       1                   ;X03E factory test control register
                    rmb       1                   ;X03F COP, ROM, & EEPROM enables

                    rmb       16                  ;X040 reserved
                    rmb       12                  ;X050 reserved

H11CSSTRH           rmb       1                   ;X05C Chip select clock stretch
H11CSCTL            rmb       1                   ;X05D Chip select control
H11CSGADR           rmb       1                   ;X05E General purpose CS address
H11CSGSIZ           rmb       1                   ;X05F General purpose CS size

;===============================================================================
; HARDWARE PLATFORM CUSTOMIZATIONS
;===============================================================================

; Put your UART equates here

SER_STATUS          equ       H11SCSR             ;STATUS FROM SCI
SER_RXDATA          equ       H11SCDR             ;DATA FROM SCI
SER_TXDATA          equ       H11SCDR             ;DAT TO SCI
RXRDY               equ       $20
TXRDY               equ       $40                 ;TRANSMIT COMPLETE (FOR TURNOFF)

;*******************************************************************************
                    #RAM                          ;RAM definitions
;*******************************************************************************
                    org       RAM_START
          ;--------------------------------------
          ; RAM interrupt vectors (first in SEG for easy
          ; addressing, else move to their own SEG)
          ;--------------------------------------
RAMVEC              rmb       21*2                ;All vectors even if not used
          ;-------------------------------------- ;Initial user stack
          ; (Size and location is user option - at least 9 bytes
          ; to accept an SWI!)
          ; 68HC11 SP points at NEXT BYTE TO USE, rather than at
          ; last used byte like most processors.  Thus, init SP
          ; to TOP-1 of stack space
          ;--------------------------------------
                    org       *+64
INITSTACK           equ       *-1
          ;-------------------------------------- ;Monitor stack
          ; (Calculated use is at most 7 bytes.  Leave plenty of spare)
          ; 68HC11 SP points at NEXT BYTE TO USE, rather than at last used byte
          ; like most processors.  Thus, init SP to TOP-1 of stack space
          ;--------------------------------------
                    org       *+16
MONSTACK            equ       *-1
          ;--------------------------------------
          ; Target registers: Order must match that in TRGHC11.C
          ;--------------------------------------
TASK_REGS
REG_STATE           rmb       1
REG_PAGE            rmb       1
REG_SP              rmb       2
REG_Y               rmb       2
REG_X               rmb       2
REG_B               rmb       1                   ;B BEFORE A, SO D IS LEAST SIG. FIRST
REG_A               rmb       1
REG_CC              rmb       1
REG_PC              rmb       2
TASK_REG_SZ         equ       *-TASK_REGS
          ;-------------------------------------- ;Communications buffer
          ; (Must be at least as long as the longer of TASK_REG_SZ
          ; or ::TSTG. At least 19 bytes recommended.  Larger values
          ; may improve speed of NoICE download and memory move commands.)
          ;--------------------------------------
COMBUF_SIZE         equ       128                 ;DATA SIZE FOR COMM BUFFER
COMBUF              rmb       2+COMBUF_SIZE+1     ;BUFFER ALSO HAS FN, LEN, AND CHECK

RAM_END             equ       *                   ;ADDRESS OF TOP+1 OF RAM

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       ROM

Start               proc                          ;Power on reset
                                                  ;Set CPU mode to safe state
                    sei                           ;INTERRUPTS OFF (WE MAY JUMP HERE)
                    clrb                          ;STATE 0 = "RESET"
                    bra       Restore

;*******************************************************************************
; Purpose: COP handler

COP_Handler         proc
                    ldb       #4                  ;STATE 4 = "COP"
                    bra       Restore

;*******************************************************************************
; Purpose: Clock Fail handler

Clock_Handler       proc
                    ldb       #3                  ;STATE 3 = "Clock fail"
;                   bra       Restore

;*******************************************************************************
; Purpose: Initialize HC11 hardware
; Note(s): BE SURE THAT "B" REMAINS INTACT UNTIL IT IS STORED TO REG_STATE BELOW!

Restore             proc
          ;--------------------------------------
          ; The following is for 32K RAM @ 0, 32K (EEP)ROM @ $8000
          ; as is the case with the ASPiSYS F1 Board
          ; CSGEN is used to select RAM, CSPROG to select (EEP)ROM
          ;--------------------------------------
                    clr       H11CSSTRH           ;No clock stretch
                    clr       H11CSGADR           ;RAM at $0000
                    lda       #1                  ;32K RAM
                    sta       H11CSGSIZ
                    lda       #%101               ;32K ROM at $8000
                    sta       H11CSCTL
          ;--------------------------------------
          ; Monitor assumes operation in either Normal Expanded or
          ; Special Test mode.
          ; The exact initialization required here will depend
          ; on which variant of the 68HC11 you have, and on your
          ; hardware layout.  The following is basic, and may not
          ; be sufficient for your case.
          ;--------------------------------------
          ; The following writes must occur within first 64 cycles
          ; after end of reset.
          ; CAUTION: DON'T USE I/O ADDRESS EQUATES UNTIL H11INIT IS WRITTEN
          ; TO SET THE I/O BASE TO MATCH OUR EQUATES!
          ;--------------------------------------
                    lda       #]CHIP_RAM+{IO_START/4096} ;LOCATION OF RAM, I/O
                    sta       XINIT               ;USE THE POST-RESET ADDRESS!
          ;-------------------------------------- ;Save reset type (RESET, COP, or Clock Fail)
                    stb       REG_STATE           ;SAVE STATE
          ;-------------------------------------- ;NOW OK To usE I/O ADDRESS EQUATES
                    clr       H11TMSK2            ;PRESCALE TO DIVIDE BY 1

                    lda       #$13                ;IRQ LEVEL, OSC DELAY, LONG COP
                    sta       H11OPTION
;-------------------------------------------------------------------------------
#ifdef                                            ;Possible additional special initialization
H11CONFIG                                         ;COP, ROM, & EEPROM enables
                                                  ;(read only except in special test mode.  May need to
                                                  ;delay vefore programming in order to allow EEPROM
                                                  ;charge pump to come up to voltage)
H11HPRIO                                          ;highest priority I-bit and misc - set mode
                                                  ;(writable only in special test mode)

H11BPROT                                          ;EEPROM block protect
H11PPROG                                          ;EEPROM programming control
                                                  ;enable programming voltage to program CONFIG register
                                                  ;wait for voltage to stabilize before programming
#endif
;-------------------------------------------------------------------------------
                    lds       #MONSTACK           ;CLEAN STACK IS HAPPY STACK
          ;--------------------------------------
          ; Initialize your UART here
          ; (SCI = 9600 bps @ 8 MHz crystal, 19200 bps @ 16 MHz)
          ;--------------------------------------
                    lda       #$30                ;9600@8MHz/19200@16MHz
                    sta       H11BAUD
                    clr       H11SCCR1            ;8 BIT DATA
                    lda       #%1100              ;TX AND RX ENABLED, NO INTS, NO WAKE
                    sta       H11SCCR2
          ;-------------------------------------- ;Initialize RAM interrupt vectors
                    ldy       #CommonHandler      ;ADDRESS OF DEFAULT HANDLER
                    ldx       #RAMVEC             ;POINTER TO RAM VECTORS
                    ldb       #NVEC/2             ;NUMBER OF VECTORS
RST10               sty       ,x                  ;SET VECTOR
                    inx:2
                    decb
                    bne       RST10
          ;--------------------------------------
          ; Initialize user registers
          ;--------------------------------------
                    ldd       #~INITSTACK         ;Reverse high/low order
                    std       REG_SP              ;INIT USER'S STACK POINTER
                    clrd
                    std       REG_PC
                    sta       REG_A
                    sta       REG_B
                    std       REG_X
                    std       REG_Y
          ;--------------------------------------
          ; Initialize memory paging variables and hardware (if any)
          ;--------------------------------------
                    sta       REG_PAGE            ;NO PAGE YET
          #ifdef MAPPED
                    sta       MAPIMG
                    sta       MAPREG              ;set hardware map
          #endif
          ;-------------------------------------- ;Initialize non-zero registers
                    lda       #$50                ;disable interrupts in user program
                    sta       REG_CC
          ;--------------------------------------
          ; Set function code for "GO".  Then if we are here
          ; because of a reset (such as a COP timeout) after
          ; being told to GO, we will come back with registers
          ; so user can see the reset
          ;--------------------------------------
                    lda       #FN_RUN_TARG
                    sta       COMBUF
                    jmp       ReturnRegs          ;DUMP REGS, ENTER MONITOR

;*******************************************************************************
; Purpose: Get a character to A
; Input  : None
; Output : A = char
;        : CCR[C] = 0 if data received
;        : CCR[C] = 1 if timeout (0.5 seconds)
; Note(s): Uses 6 bytes of stack including return address

GetChar             proc
                    pshx
                    clrx                          ;LONG TIMEOUT
Loop@@              bsr       KickCOP             ;PREVENT WATCHDOG TIMEOUT
                    dex
;                   beq       Fail@@              ;EXIT IF TIMEOUT
          ;-------------------------------------- ;(Disable timeout in most cases...)
                    lda       SER_STATUS          ;READ DEVICE STATUS
                    anda      #RXRDY
                    beq       Loop@@              ;NOT READY YET.
          ;-------------------------------------- ;Data received:  return CY=0. data in A
                    clc                           ;CY=0
                    lda       SER_RXDATA          ;READ DATA
                    pulx
                    rts
          ;-------------------------------------- ;Timeout:  return CY=1
Fail@@              sec                           ;CY=1
                    pulx
                    rts

;*******************************************************************************
; Purpose: Output character in A
; Input  : A = character
; Output : None
; Note(s): Uses 5 bytes of stack including return address

PutChar             proc
                    psha
Loop@@              bsr       KickCOP             ;PREVENT WATCHDOG TIMEOUT
                    lda       SER_STATUS          ;CHECK TX STATUS
                    anda      #TXRDY              ;TX READY ?
                    beq       Loop@@
                    pula
                    sta       SER_TXDATA          ;TRANSMIT CHAR.
                    rts

;*******************************************************************************
; Purpose: Reset watchdog timer.  Must be called at least once every little while
;        : or COP interrupt will occur
; Note(s): Uses 2 bytes of stack including return address

KickCOP             proc
                    lda       #$55
                    sta       H11COPRST
                    coma
                    sta       H11COPRST
                    rts

;-------------------------------------------------------------------------------
; Response string for GET TARGET STATUS request
; Reply describes target:
;-------------------------------------------------------------------------------

TSTG                fcb       3                   ;2: PROCESSOR TYPE = 68HC11
                    fcb       COMBUF_SIZE         ;3: SIZE OF COMMUNICATIONS BUFFER
                    fcb       0                   ;4: NO TASKING SUPPORT
          #ifdef MAPPED
                    fdb       ~0                  ;5,6: BOTTOM OF MAPPED MEM (LSB FIRST)
                    fdb       ~0                  ;7,8: TOP OF MAPPED MEM (LSB FIRST)
          #else
                    fdb       ~0                  ;5,6: BOTTOM OF MAPPED MEM (LSB FIRST)
                    fdb       ~0                  ;7,8: TOP OF MAPPED MEM (LSB FIRST)
          #endif
                    fcb       B1-B0               ;9 BREAKPOINT INSTR LENGTH
          #ifdef SWI
B0                  fcb       0                   ;10+ BREAKPOINT INSTRUCTION
            #ifdef OS
                    #Message  OS support enabled
                    fcb       0                   ;2nd byte
            #endif
          #else
B0                  swi                           ;10+ BREAKPOINT INSTRUCTION
          #endif
B1                  fcs       '68HC11 monitor V1.3 (for ASPiSYS F1 Board)'
                    #size     TSTG                ;SIZE OF STRING

;-------------------------------------------------------------------------------
; HARDWARE PLATFORM INDEPENDENT EQUATES AND CODE
;-------------------------------------------------------------------------------
          ;--------------------------------------
          ; Communications function codes
          ;--------------------------------------
FN_GET_STAT         equ       $FF                 ;reply with device info
FN_READ_MEM         equ       $FE                 ;reply with data
FN_WRITE_M          equ       $FD                 ;reply with status (+/-)
FN_READ_RG          equ       $FC                 ;reply with registers
FN_WRITE_RG         equ       $FB                 ;reply with status
FN_RUN_TARG         equ       $FA                 ;reply (delayed) with registers
FN_SET_BYTE         equ       $F9                 ;reply with data (truncate if error)
FN_IN               equ       $F8                 ;input from port
FN_OUT              equ       $F7                 ;output to port

FN_MIN              equ       $F7                 ;MINIMUM RECOGNIZED FUNCTION CODE
FN_ERROR            equ       $F0                 ;error reply to unknown op-code

;*******************************************************************************
; Purpose: Common handler for default interrupt handlers
; Input  : A = interrupt code = processor state
; Note(s): All registers stacked, PC -> next instruction

CommonHandler       proc
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
          ; then DEX multiple times to match B1-B0 in TSTG
          ;--------------------------------------
                    pulx                          ; PC AFTER INTERRUPT
                    lda       REG_STATE
                    cmpa      #1
                    bne       NotBreak@@          ; BR IF NOT A BREAKPOINT
                    dex                           ; ELSE BACK UP TO POINT AT SWI LOCATION
NotBreak@@          xgdx                          ; TRANSFER PC TO D
                    sta       REG_PC+1            ; MSB
                    stb       REG_PC              ; LSB
                    tsx                           ; USER STACK POINTER PLUS 1
                    dex                           ; MAKE IT JUST LIKE THE REAL SP
                    xgdx
                    stb       REG_SP              ; SAVE USER'S STACK POINTER (LSB)
                    sta       REG_SP+1            ; MSB
          ;-------------------------------------- ; Change to our own stack
                    lds       #MONSTACK           ; AND USE OURS INSTEAD
          ;-------------------------------------- ; Save memory page
;                   lda       MAPIMG              ; GET CURRENT USER MAP
                    clr       REG_PAGE            ; ... OR ZERO IF UNMAPPED TARGET
          ;-------------------------------------- ; Return registers to master
                    jmp       ReturnRegs

;*******************************************************************************
; Purpose: Main loop waits for command frame from master
; Note(s): Uses 7 bytes of stack before jump to handlers

MainLoop            proc
                    lds       #MONSTACK           ; CLEAN STACK IS HAPPY STACK
                    ldx       #COMBUF             ; BUILD MESSAGE HERE
          ;-------------------------------------- ; First byte is a function code
                    jsr       GetChar             ; GET A FUNCTION
                    bcs       MainLoop            ; JIF TIMEOUT: RESYNC
                    cmpa      #FN_MIN
                    blo       MainLoop            ; JIF BELOW MIN: ILLEGAL FUNCTION
                    sta       ,x                  ; SAVE FUNCTION CODE
                    inx
          ;-------------------------------------- ; Second byte is data byte count (may be zero)
                    jsr       GetChar             ; GET A LENGTH BYTE
                    bcs       MainLoop            ; JIF TIMEOUT: RESYNC
                    cmpa      #COMBUF_SIZE
                    bhi       MainLoop            ; JIF TOO LONG: ILLEGAL LENGTH
                    sta       ,x                  ; SAVE LENGTH
                    inx
                    tsta
                    beq       CheckSum@@          ; SKIP DATA LOOP IF LENGTH = 0
          ;-------------------------------------- ; Loop for data
                    tab                           ; SAVE LENGTH FOR LOOP
Loop@@              jsr       GetChar             ; GET A DATA BYTE
                    bcs       MainLoop            ; JIF TIMEOUT: RESYNC
                    sta       ,x                  ; SAVE DATA BYTE
                    inx
                    decb
                    bne       Loop@@
          ;-------------------------------------- ; Get the checksum
CheckSum@@          jsr       GetChar             ; GET THE CHECKSUM
                    bcs       MainLoop            ; JIF TIMEOUT: RESYNC
                    psha                          ; SAVE CHECKSUM
          ;--------------------------------------
          ; Compare received checksum to that calculated
          ; on received buffer (Sum should be 0)
          ;--------------------------------------
                    jsr       CheckSum
                    pulb
                    aba
                    bne       MainLoop            ; JIF BAD CHECKSUM
          ;-------------------------------------- ; Process the message.
                    ldx       #COMBUF
                    ldd       ,x                  ; GET THE FUNCTION CODE AND THE LENGTH
                    inx:2                         ; X POINTS AT DATA
                    cmpa      #FN_GET_STAT
                    beq       TargetStatus
                    cmpa      #FN_READ_MEM
                    beq       ReadMem
                    cmpa      #FN_WRITE_M
                    beq       WriteMem
                    cmpa      #FN_READ_RG
                    jeq       ReadRegs
                    cmpa      #FN_WRITE_RG
                    jeq       WriteRegs
                    cmpa      #FN_RUN_TARG
                    jeq       RunTarget
                    cmpa      #FN_SET_BYTE
                    jeq       SetBytes
                    cmpa      #FN_IN
                    jeq       InPort
                    cmpa      #FN_OUT
                    jeq       OutPort
          ;-------------------------------------- ; Error: unknown function.  Complain
                    lda       #FN_ERROR
                    sta       COMBUF              ; SET FUNCTION AS "ERROR"
                    lda       #1
                    jmp       SendStatus          ; VALUE IS "ERROR"

;*******************************************************************************
; Purpose: Target Status FN, len
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

TargetStatus        proc
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
          ;--------------------------------------
          ; Compute checksum on buffer, and send to master, then return
          ;--------------------------------------
                    jmp       Send

;*******************************************************************************
; Purpose: Read Memory FN, len, page, Alo, Ahi, Nbytes
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

ReadMem             proc
          ;-------------------------------------- ; Set map
          #ifdef MAPPED
                    lda       ,x
                    sta       MAPIMG
                    sta       MAPREG
          #endif
          ;-------------------------------------- ; Get address
                    lda       2,x                 ; MSB OF ADDRESS IN A
                    ldb       1,x                 ; LSB OF ADDRESS IN B
                    xgdy                          ; ADDRESS IN Y
          ;--------------------------------------
          ; Prepare return buffer: FN (unchanged), LEN, DATA
          ;--------------------------------------
                    ldb       3,x                 ; NUMBER OF BYTES TO RETURN
                    stb       COMBUF+1            ; RETURN LENGTH = REQUESTED DATA
                    beq       Done@@              ; JIF NO BYTES TO GET
          ;--------------------------------------
          ; Read the requested bytes from local memory
          ;--------------------------------------
Loop@@              lda       ,y                  ; GET BYTE
                    sta       ,x                  ; STORE TO RETURN BUFFER
                    inx
                    iny
                    decb
                    bne       Loop@@
          ;--------------------------------------
          ; Compute checksum on buffer, and send
          ; to master, then return
          ;--------------------------------------
Done@@              jmp       Send

;*******************************************************************************
; Purpose: Write Memory FN, len, page, Alo, Ahi, (len-3 bytes of Data)
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2
; Note(s): Uses 6 bytes of stack

WriteMem            proc
          ;-------------------------------------- ; Set map
          #ifdef MAPPED
                    lda       ,x
                    sta       MAPIMG
                    sta       MAPREG
          #endif
          ;-------------------------------------- ; Get address
                    lda       2,x                 ; MSB OF ADDRESS IN A
                    ldb       1,x                 ; LSB OF ADDRESS IN B
                    xgdy                          ; ADDRESS IN Y
          ;--------------------------------------
          ; Prepare return buffer: FN (unchanged), LEN, DATA
          ;--------------------------------------
                    ldb       COMBUF+1            ; NUMBER OF BYTES TO RETURN
                    subb      #3                  ; MINUS PAGE AND ADDRESS
                    beq       Success@@           ; JIF NO BYTES TO PUT
          ;--------------------------------------
          ; Write the specified bytes to local memory
          ;--------------------------------------
                    pshb
                    pshx
                    pshy
Loop@@              lda       3,x                 ; GET BYTE TO WRITE
                    bsr       CheckY
                    bcs       SkipWrite@@
                    sta       ,y                  ; STORE THE BYTE AT AAAA,y
SkipWrite@@         inx
                    iny
                    decb
                    bne       Loop@@
          ;--------------------------------------
          ; Compare to see if the write worked
          ;--------------------------------------
                    puly
                    pulx
                    pulb
CLoop@@             lda       3,x                 ; GET BYTE JUST WRITTEN
                    bsr       CheckY
                    bcs       Fail@@
                    cmpa      ,y
                    bne       Fail@@              ; BR IF WRITE FAILED
                    inx
                    iny
                    decb
                    bne       CLoop@@
          ;--------------------------------------
Success@@           clra                          ; Write succeeded:  return status = 0
                    bra       Done@@
          ;--------------------------------------
Fail@@              lda       #1                  ; Write failed: return status = 1
Done@@              jmp       SendStatus          ; Return OK status

;*******************************************************************************
; Purpose: Check Y against valid ASPiSYS F1 Board addresses
;        : Valid user ranges are 0-$3FF, $1060-$7FFF, RAMVEC-(RAMVEC+42)

CheckY              proc
                    cmpy      #$7FFF
                    bhi       Fail@@
                    cmpy      #RAM_START+42       ; $400 + RAMVECTORS
                    blo       Done@@
                    cmpy      #$1060
                    blo       Fail@@
Done@@              clc
                    rts

Fail@@              sec
                    rts

;*******************************************************************************
; Purpose: Read registers FN, len=0
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

ReadRegs            proc
          ;--------------------------------------
          ; Enter here from SWI after "RUN" and "STEP" to return task registers
          ; CAUTION:  in this case, assume no registers!
          ;--------------------------------------
;                   bra       ReturnRegs

;*******************************************************************************

ReturnRegs          proc
                    ldy       #TASK_REGS          ; POINTER TO REGISTERS
                    ldb       #TASK_REG_SZ        ; NUMBER OF BYTES
                    stb       COMBUF+1            ; SAVE RETURN DATA LENGTH
          ;-------------------------------------- ; Copy the registers
                    ldx       #COMBUF+2           ; POINTER TO RETURN BUFFER
Loop@@              lda       ,y                  ; GET BYTE TO A
                    sta       ,x                  ; STORE TO RETURN BUFFER
                    inx
                    iny
                    decb
                    bne       Loop@@
          ;--------------------------------------
          ; Compute checksum on buffer, and send to master, then return
          ;--------------------------------------
                    jmp       Send

;*******************************************************************************
; Write registers:  FN, len, (register image)
; Entry with A=function code, B=data size, X=COMBUF+2

WriteRegs           proc
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
          ;--------------------------------------
Done@@              clra                          ; Return OK status
          #ifdef MAPPED
                    jmp       SendStatus
          #else
                    bra       SendStatus
          #endif

;*******************************************************************************
; Purpose: Run Target FN, len
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

RunTarget           proc
          ;-------------------------------------- ; Restore user's map
          #ifdef MAPPED
                    lda       REG_PAGE            ; USER'S PAGE
                    sta       MAPIMG              ; SET IMAGE
                    sta       MAPREG              ; SET MAPPING REGISTER
          #endif
          ;-------------------------------------- ; Switch to user stack
                    ldb       REG_SP              ; BACK TO USER STACK
                    lda       REG_SP+1
                    xgdx                          ; TO X
                    inx                           ; PRE-CORRECT FOR TXS
                    txs                           ; SP = X-1
          ;-------------------------------------- ; Restore registers
                    lda       REG_PC              ; SAVE LS USER PC FOR RTI
                    psha
                    lda       REG_PC+1            ; SAVE MS USER PC FOR RTI
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

                    rti                           ; Return to user

;*******************************************************************************
; Purpose: Set target byte(s) FN, len { (page, alow, ahigh, data), (...)... }
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2
; Output : Return has FN, len, (data from memory locations)
; Note(s): If error in insert (memory not writable), abort to return short data
;        : This function is used primarily to set and clear breakpoints
;        : Uses 3 bytes of stack

SetBytes            proc
                    ldy       #COMBUF+1           ; POINTER TO RETURN BUFFER
                    clra
                    sta       ,y                  ; SET RETURN COUNT AS ZERO
                    iny                           ; POINT AT FIRST RETURN DATA BYTE
                    lsrb:2                        ; LEN/4 = NUMBER OF BYTES TO SET
                    beq       Done@@              ; JIF NO BYTES (COMBUF+1 = 0)
          ;-------------------------------------- ; Loop on inserting bytes
Loop@@              pshb                          ; SAVE LOOP COUNTER
                    pshy                          ; SAVE RETURN BUFFER POINTER
          ;-------------------------------------- ; Set map
          #ifdef MAPPED
                    lda       ,x
                    sta       MAPIMG
                    sta       MAPREG
          #endif
          ;-------------------------------------- ; Get address
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

                    sta       ,y                  ; Save target byte in return buffer
                    iny                           ; ADVANCE TO NEXT RETURN BYTE
                    inc       COMBUF+1            ; COUNT ONE RETURN BYTE
          ;-------------------------------------- ; Loop for next byte
                    inx:4                         ; STEP TO NEXT BYTE SPECIFIER
                    cmpb      COMBUF+1
                    bne       Loop@@              ; LOOP FOR ALL BYTES
          ;--------------------------------------
          ; Return buffer with data from byte locations
          ; Compute checksum on buffer, and send to
          ; master, then return
          ;--------------------------------------
Done@@              bra       Send

;*******************************************************************************
; Purpose: Input from port FN, len, PortAddressLo, PAhi (=0)
;        : While the HC11 has no input or output instructions, we retain these
;        : to allow write-without-verify
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

InPort              proc
          ;-------------------------------------- ; Get port address
                    lda       1,x                 ; MSB OF ADDRESS IN A
                    ldb       ,x                  ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y
          ;--------------------------------------
                    lda       ,y                  ; Read the requested byte from local memory
                    bra       SendStatus          ; Return byte read as "status"

;*******************************************************************************
; Purpose: Output to port FN, len, PortAddressLo, PAhi (=0), data
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

OutPort             proc
          ;-------------------------------------- ; Get port address
                    lda       1,x                 ; MSB OF ADDRESS IN A
                    ldb       ,x                  ; LSB OF ADDRESS IN B
                    xgdy                          ; MEMORY ADDRESS IN Y

                    lda       2,x                 ; Get data
                    sta       ,y                  ; Write value to port
          ;--------------------------------------
          ; Do not read port to verify (some I/O devices don't like it)
          ;--------------------------------------
                    clra                          ; Return status of OK
;                   bra       SendStatus

;*******************************************************************************
; Purpose: Build status return with value from "A"

SendStatus          proc
                    sta       COMBUF+2            ; SET STATUS
                    lda       #1
                    sta       COMBUF+1            ; SET LENGTH
;                   bra       SEND

;*******************************************************************************
; Purpose: Append checksum to COMBUF and send to master

Send                proc
                    bsr       CheckSum            ; GET A=CHECKSUM, X->checksum location
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
                    jmp       MainLoop            ; BACK TO MAIN LOOP

;*******************************************************************************
; Purpose: Compute checksum on COMBUF.  COMBUF+1 has length of data,
;        : Also include function byte and length byte
; Input  : None
; Output : A = checksum
;        : X = pointer to next byte in buffer (checksum location)
;        : B is scratched
; Note(s): Uses 2 bytes of stack including return address

CheckSum            proc
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
; Our default handler uses the code in "A" as the processor state to be
; passed back to the host.

SCI_Handler         proc
                    lda       #20                 ; ffd6
                    ldx       RAMVEC+0
                    jmp       ,x

;*******************************************************************************

SPI_Handler         proc
                    lda       #19                 ; ffd8
                    ldx       RAMVEC+2
                    jmp       ,x

;*******************************************************************************

PACE_Handler        proc
                    lda       #18                 ; ffda
                    ldx       RAMVEC+4
                    jmp       ,x

;*******************************************************************************

PACO_Handler        proc
                    lda       #17                 ; ffdc
                    ldx       RAMVEC+6
                    jmp       ,x

;*******************************************************************************

TOV_Handler         proc
                    lda       #16                 ; ffde
                    ldx       RAMVEC+8
                    jmp       ,x

;*******************************************************************************

TC5_Handler         proc
                    lda       #15                 ; ffe0
                    ldx       RAMVEC+10
                    jmp       ,x

;*******************************************************************************

TC4_Handler         proc
                    lda       #14                 ; ffe2
                    ldx       RAMVEC+12
                    jmp       ,x

;*******************************************************************************

TC3_Handler         proc
                    lda       #13                 ; ffe4
                    ldx       RAMVEC+14
                    jmp       ,x

;*******************************************************************************

TC2_Handler         proc
                    lda       #12                 ; ffe6
                    ldx       RAMVEC+16
                    jmp       ,x

;*******************************************************************************

TCOMP_Handler       proc
                    lda       #11                 ; ffe8
                    ldx       RAMVEC+18
                    jmp       ,x

;*******************************************************************************

TCAP3_Handler       proc
                    lda       #10                 ; ffea
                    ldx       RAMVEC+20
                    jmp       ,x

;*******************************************************************************

TCAP2_Handler       proc
                    lda       #9                  ; ffec
                    ldx       RAMVEC+22
                    jmp       ,x

;*******************************************************************************

TCAP1_Handler       proc
                    lda       #8                  ; ffee
                    ldx       RAMVEC+24
                    jmp       ,x

;*******************************************************************************

RTC_Handler         proc
                    lda       #7                  ; fff0
                    ldx       RAMVEC+26
                    jmp       ,x

;*******************************************************************************

IRQ_Handler         proc
                    lda       #6                  ; fff2
                    ldx       RAMVEC+28
                    jmp       ,x

;*******************************************************************************

XIRQ_Handler        proc
                    lda       #2
                    ldx       RAMVEC+30           ; fff4
                    jmp       ,x

;*******************************************************************************

SWI_Handler         proc
          #ifdef SWI
                    #Message  SWI is for user code only ($00 for debugger)
                    ldx       RAMVEC+32           ; fff6
                    pshx                          ; save SWI handler vector
                    tsx
                    ldx       _X_+2,x             ; restore original X
                    rts                           ; call vectored SWI handler
          #else
                    lda       #1
                    #Message  SWI not allowed
                    jmp       CommonHandler
          #endif
;*******************************************************************************

Illop_Handler       proc
          #ifdef SWI                              ; Non-RAM vectored
                    tsx
                    ldx       _PC_,x
                    lda       ,x                  ; get opcode
                    tsx
                    cmpa      B0                  ; is it the breakpoint?
                    bne       Real@@              ; no, it's a real ILLEGAL opcode
                    ldd       _PC_,x              ; increment PC (as with SWI)
                    incd
                    std       _PC_,x
                    ldb       _B_,x
                    lda       #1                  ; Code=1 indicates breakpoint
                    bra       Break@@

Real@@              lda       #5                  ; indicates illegal opcode
Break@@             ldx       _X_,x               ; restore original X
          #else
                    lda       #5
          #endif
                    jmp       CommonHandler

;*******************************************************************************
                    #VECTORS
;*******************************************************************************
                    org       VECTORS
          ;-------------------------------------- ; VECTORS THROUGH RAM
VEC0                dw        SCI_Handler         ; ffd6
                    dw        SPI_Handler         ; ffd8
                    dw        PACE_Handler        ; ffda
                    dw        PACO_Handler        ; ffdc
                    dw        TOV_Handler         ; ffde
                    dw        TC5_Handler         ; ffe0
                    dw        TC4_Handler         ; ffe2
                    dw        TC3_Handler         ; ffe4
                    dw        TC2_Handler         ; ffe6
                    dw        TCOMP_Handler       ; ffe8
                    dw        TCAP3_Handler       ; ffea
                    dw        TCAP2_Handler       ; ffec
                    dw        TCAP1_Handler       ; ffee
                    dw        RTC_Handler         ; fff0
                    dw        IRQ_Handler         ; fff2
                    dw        XIRQ_Handler        ; fff4 (non-maskable interrupt)
          #ifdef SWI
                    dw        SWI_Handler         ; fff6 SWI/breakpoint
NVEC                equ       *-VEC0              ; number of vector bytes
          #else
NVEC                equ       *-VEC0              ; number of vector bytes
                    dw        SWI_Handler         ; fff6 SWI/breakpoint
          #endif
          ;--------------------------------------
          ; The remaining interrupts are permanently trapped to the monitor
          ;--------------------------------------
                    dw        Illop_Handler       ; fff8 illegal op-code
                    dw        COP_Handler         ; fffa Watchdog timeout
                    dw        Clock_Handler       ; fffc clock fail
                    dw        Start               ; fffe reset

                    end       Start
