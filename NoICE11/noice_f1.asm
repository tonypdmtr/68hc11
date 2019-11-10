;*******************************************************************************
;* Program   : NOICE_F1.ASM
;* Programmer: John Hardman (JLH)                 Original version
;*           : Tony Papadimitriou (TGP)           Modified version
;* Purpose   : 68HC11 Debug Monitor for use with NOICE11
;*           : and the ASPiSYS F1 Board or compatible hardware
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;* Status    : FREEWARE
;* History   : 93.06.14 JLH release version
;*           : 93.08.03 JLH improve I/O init documentation
;*           : 93.08.24 JLH correct error in IN and OUT, stack init (v1.2)
;*           : 94.05.12 JLH clarify TSTG paging info
;*           : 94.11.07 JLH correct typos in comments
;*           : 95.05.01 JLH correct error in RAMVEC usage (v1.3)
;*           : 97.08.19 JLH correct bug in COP and Clock Monitor handling
;*           : 98.02.25 JLH assemble with either Motorola or Dunfield
;*           : -----------------------------------------------------------
;*           : 99.11.27 TGP Modify for ASM11 and ASPiSYS F1 Board
;*           : 00.10.16 TGP Improved source code
;*           : 00.10.22 TGP Allowed non-SWI breakpoints (TEST)
;*           : 00.10.26 TGP Disallowed loading to non-user memory
;*           : 19.11.10 TGP Minor optimizations and cosmetic changes
;*******************************************************************************

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
;   1) Define map port MAPREG here
;   2) Define or import map port RAM image MAPIMG here if MAPREG is
;      write only.  (The application code must update MAPIMG before
;      outputing to MAPREG)
;   3) Search for and modify MAPREG, MAPIMG, and REG_PAGE usage below
;   4) In TSTG below edit 'LOW AND HIGH LIMIT OF MAPPED MEM'
;      to appropriate range (typically $4000 to $7FFF for two-bit MMU)
;*******************************************************************************

VERSION             equ       130                 ;version as x.xx

                    #CaseOn                       ;Case insensitive labels
                    #ExtraOn                      ;Allow extra mnemonics
                    #OptRelOn                     ;Show optimization warnings for JMP/JSR
                    #OptRtsOn                     ;Show optimization warnings for JSR/BSR, RTS
                    #SpacesOff                    ;Do not allow spaces within expressions
#ifdef ?
  #Hint +===================================================
  #Hint | Available conditionals (for use with -Dx option)
  #Hint +===================================================
  #Hint | SWI: Allows user usage of SWI instruction
  #Hint | OS: When SWI above is used allows usage of OS
  #Hint | MAPPED: Allow memory mapping (w/ MMU hardware)
  #Hint | ROM:nn: Move ROM to address nn
  #Hint +===================================================
  #Fatal Run ASM11 -Dx (where x is any of the above)
#endif
          #ifdef OS
SWI                 def       0                   ;OS implies SWI
          #endif
_PC_                equ       7                   ;stack frame PC offset
_X_                 equ       3                   ;stack frame X offset
_B_                 equ       1                   ;stack frame B offset
          #ifdef MAPPED
                    #Message  Mapped Memory enabled (MAPREG/MAPIMG values OK?)
MAPREG              equ       $4000               ;Address of MAP Register
MAPIMG              equ       $4001               ;Modify according to hardware
          #endif
          ;--------------------------------------
          ; Hardware definitions
          ;--------------------------------------
CHIP_RAM            equ       $0000               ;START OF HC11 ON-CHIP RAM
REGS                equ       $1000               ;START OF HC11 ON-CHIP I/O
XINIT               equ       $103D               ;Post-reset INIT location
RAM_START           equ       $0400               ;START OF MONITOR RAM (external)
ROM                 def       $FC00               ;START OF MONITOR CODE

          #if ROM < $8000
                    #Fatal    ROM cannot be less than $8000
          #endif
VECTORS             equ       $FFD6               ;START OF HARDWARE VECTORS
          #if VECTORS&$0FFF <> $0FD6
                    #Warning  VECTORS is incorrect ({VECTORS(h)}, not $xFD6)
          #endif
                    #MEMORY   ROM       VECTORS-23
                    #MEMORY   VECTORS   VECTORS+41
          ;--------------------------------------
          ; Define HC11 I/O register locations (MC68HC11F1)
          ;--------------------------------------
                    #temp     REGS

                    next      :temp               ;X000 i/o port A
                    next      :temp               ;X001 reserved
                    next      :temp               ;X002 i/o port C control
                    next      :temp               ;X003 i/o port C

                    next      :temp               ;X004 i/o port B
                    next      :temp               ;X005 i/o port CL
                    next      :temp               ;X006 reserved
                    next      :temp               ;X007 data direction for port C

                    next      :temp               ;X008 i/o port D
                    next      :temp               ;X009 data direction for port D
                    next      :temp               ;X00A input port E
                    next      :temp               ;X00B compare force register

                    next      :temp               ;X00C OC1 action mask register
                    next      :temp               ;X00D OC1 action data register
                    next      :temp,2             ;X00E timer counter register

                    next      :temp,2             ;X010 input capture register 1
                    next      :temp,2             ;X012 input capture register 2

                    next      :temp,2             ;X014 input capture register 3
                    next      :temp,2             ;X016 output compare register 1

                    next      :temp,2             ;X018 output compare register 2
                    next      :temp,2             ;X01A output compare register 3

                    next      :temp,2             ;X01C output compare register 4
                    next      :temp,2             ;X01E output compare register 5

                    next      :temp               ;X020 timer control register 1
                    next      :temp               ;X021 timer control register 2
                    next      :temp               ;X022 main timer interrupt mask 1
                    next      :temp               ;X023 main timer interrupt flag 1

H11TMSK2            next      :temp               ;X024 misc timer interrupt mask 2
                    next      :temp               ;X025 misc timer interrupt flag 2
                    next      :temp               ;X026 pulse accumulator control register
                    next      :temp               ;X027 pulse accumulator count register

                    next      :temp               ;X028 SPI control register
                    next      :temp               ;X029 SPI status register
                    next      :temp               ;X02A SPI data in/out
H11BAUD             next      :temp               ;X02B SCI baud rate control

H11SCCR1            next      :temp               ;X02C SCI control register 1
H11SCCR2            next      :temp               ;X02D SCI control register 2
H11SCSR             next      :temp               ;X02E SCI status register
H11SCDR             next      :temp               ;X02F SCI data

                    next      :temp               ;X030 A to D control register
                    next      :temp               ;X031 A to D result 1
                    next      :temp               ;X032 A to D result 2
                    next      :temp               ;X033 A to D result 3

                    next      :temp               ;X034 A to D result 4
H11BPROT            next      :temp               ;X035 EEPROM block protect
                    next      :temp,2             ;X036 reserved

                    next      :temp               ;X038 system configuration options 2
H11OPTION           next      :temp               ;X039 system configuration options
H11COPRST           next      :temp               ;X03A arm/reset COP timer circutry
                    next      :temp               ;X03B EEPROM programming control

                    next      :temp               ;X03C highest priority I-bit and misc.
                    next      :temp               ;X03D ram/io mapping register
                    next      :temp               ;X03E factory test control register
                    next      :temp               ;X03F COP, ROM, & EEPROM enables

                    next      :temp,16            ;X040 reserved
                    next      :temp,12            ;X050 reserved

H11CSSTRH           next      :temp               ;X05C Chip select clock stretch
H11CSCTL            next      :temp               ;X05D Chip select control
H11CSGADR           next      :temp               ;X05E General purpose CS address
H11CSGSIZ           next      :temp               ;X05F General purpose CS size

;*******************************************************************************
; HARDWARE PLATFORM CUSTOMIZATIONS
;*******************************************************************************

; UART equates

SER_STATUS          equ       H11SCSR             ;STATUS FROM SCI
SER_RXDATA          equ       H11SCDR             ;DATA FROM SCI
SER_TXDATA          equ       H11SCDR             ;DAT TO SCI
RXRDY               equ       $20
TXRDY               equ       $40                 ;TRANSMIT COMPLETE (FOR TURNOFF)

;*******************************************************************************
                    #RAM
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
                    rmb       64
INITSTACK           equ       *-1
          ;-------------------------------------- ; Monitor stack
          ; (Calculated use is at most 7 bytes.  Leave plenty of spare)
          ; 68HC11 SP points at NEXT BYTE TO USE, rather than at last used byte
          ; like most processors.  Thus, init SP to TOP-1 of stack space
          ;--------------------------------------
                    rmb       16
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
REG_B               rmb       1                   ;B before A, so D is LSB first
REG_A               rmb       1
REG_CC              rmb       1
REG_PC              rmb       2
                    #size     TASK_REGS
          ;--------------------------------------
          ; Communications buffer
          ; (Must be at least as long as the longer of ::TASK_REGS
          ; or ::TSTG. At least 19 bytes recommended.  Larger values
          ; may improve speed of NoICE download and memory move commands.)
          ;--------------------------------------
COMBUF_SIZE         equ       128                 ;data size for comm buffer
COMBUF              rmb       2+COMBUF_SIZE+1     ;buffer also has FN, LEN, and CHECK

RAM_END             equ       *                   ;address of TOP+1 of RAM

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       ROM

Start               proc                          ;Power on reset
          ;-------------------------------------- ;Set CPU mode to safe state
                    sei                           ;interrupts off (we may jump here)
                    clrb                          ;STATE 0 = 'RESET'
                    bra       Initialize

;*******************************************************************************

COP_Handler         proc                          ;COP reset
                    ldb       #4                  ;STATE 4 = 'COP'
                    bra       Initialize

;*******************************************************************************

Clock_Handler       proc                          ;Clock Fail reset
                    ldb       #3                  ;STATE 3 = 'Clock fail'
;                   bra       Initialize

;*******************************************************************************
; Initialize HC11 hardware
; Be sure that 'B' remains intact until it is stored to reg_state below!

Initialize          proc
          ;--------------------------------------
          ; The following is for 32K RAM @ 0, 32K (EEP)ROM @ $8000
          ; as is the case with the ASPiSYS F1 Board
          ; CSGEN is used to select RAM, CSPROG to select (EEP)ROM
          ;--------------------------------------
                    clr       H11CSSTRH           ;No clock stretch
                    clr       H11CSGADR           ;RAM at $0000
                    lda       #1                  ;32K RAM
                    sta       H11CSGSIZ
                    lda       #%01000101          ;32K ROM at $8000
                    sta       H11CSCTL
          ;--------------------------------------
          ; Monitor assumes operation in either Normal Expanded or Special Test mode
          ; The exact initialization required here will depend on which variant of
          ; the 68HC11 you have, and on your hardware layout.  The following is
          ; basic, and may not be sufficient for your case.
          ;--------------------------------------
          ; The following writes must occur within first 64 cycles after end of reset
          ; CAUTION: Don't use I/O address equates until XINIT is written
          ; to set the I/O base to match our equates!
          ;--------------------------------------
                    lda       #]CHIP_RAM+{REGS>12};On-chip RAM at CHIP_RAM (high nibble), I/O regs at REGS (low nibble), Location of RAM, I/O
                    sta       XINIT               ;use the post-reset address!
          ;-------------------------------------- ;Save reset type (RESET, COP, or Clock Fail)
                    stb       REG_STATE           ;save state
          ;-------------------------------------- ;now OK to use I/O address equates
                    clr       H11TMSK2            ;Prescale to divide by 1

                    lda       #$13                ;IRQ level, OSC delay, Long COP
                    sta       H11OPTION
          ;--------------------------------------
          ; Possible additional special initialization
          ;
          ; H11CONFIG ;COP, ROM, & EEPROM enables
          ;           ;(read only except in special test mode.  May need to
          ;           ;delay vefore programming in order to allow EEPROM
          ;           ;charge pump to come up to voltage)
          ; H11HPRIO  ;highest priority I-bit and misc - set mode
          ;           ;(writable only in special test mode)
          ;
          ; H11BPROT  ;EEPROM block protect
          ; H11PPROG  ;EEPROM programming control
          ; Enable programming voltage to program CONFIG register
          ; Wait for voltage to stb ilize before programming
          ;--------------------------------------
                    lds       #MONSTACK           ;clean stack is happy stack
          ;--------------------------------------
          ; Initialize UART
          ; (SCI = 9600 bps @8MHz crystal, 19200 bps @16MHz)
          ;--------------------------------------
                    lda       #$30                ;9600bps @8MHz/19200bps @16MHz
                    sta       H11BAUD
                    clr       H11SCCR1            ;8 bit data
                    lda       #%1100              ;TX and RX enabled, no ints, no wake
                    sta       H11SCCR2
          ;--------------------------------------
          ; Initialize RAM interrupt vectors
          ;--------------------------------------
                    ldy       #CommonHandler      ;address of default handler
                    ldx       #RAMVEC             ;pointer to ram vectors
                    ldb       #::NVEC/2           ;number of vectors
Loop@@              sty       ,x                  ;set vector
                    inx:2
                    decb
                    bne       Loop@@
          ;--------------------------------------
          ; Initialize user registers
          ;--------------------------------------
                    ldd       #~INITSTACK         ;Reverse high/low order
                    std       REG_SP              ;Init user's stack pointer
                    clrd
                    std       REG_PC
                    sta       REG_A
                    sta       REG_B
                    std       REG_X
                    std       REG_Y
          ;--------------------------------------
          ; Initialize memory paging variables and hardware (if any)
          ;--------------------------------------
                    sta       REG_PAGE            ;no page yet
          #ifdef MAPPED
                    sta       MAPIMG
                    sta       MAPREG              ;set hardware map
          #endif
          ;--------------------------------------
          ; Initialize non-zero registers
          ;--------------------------------------
                    lda       #$50                ;disable interrupts in user program
                    sta       REG_CC
          ;--------------------------------------
          ; Set function code for 'GO'.  Then if we are here because of a reset
          ; (such as a COP timeout) after being told to GO, we will come
          ; back with registers so user can see the reset
          ;--------------------------------------
                    lda       #FN_RUN_TARG
                    sta       COMBUF
                    jmp       ReturnRegs          ;dump regs, enter monitor

;*******************************************************************************
; Purpose: Get a character to A
; Output : A=char, CCR[C]=0 if data received
;        : CCR[C]=1 if timeout (0.5 seconds)
; Note(s): Uses 6 bytes of stack including return address

GetChar             proc
                    pshx
                    clrx                          ;Long timeout
Loop@@              bsr       KickCOP             ;Prevent watchdog timeout
                    dex
                    sec                           ;Timeout: return CCR[C]=1
;                   beq       Done@@              ;exit if timeout
          ;-------------------------------------- ;(Disable timeout in most cases...)
                    lda       SER_STATUS          ;read device status
                    anda      #RXRDY
                    beq       Loop@@              ;not ready yet
          ;-------------------------------------- ;Data received:  return CY=0. data in A
                    clc
                    lda       SER_RXDATA          ;read data
Done@@              pulx
                    rts

;*******************************************************************************
; Purpose: Output character in A
; Note(s): Uses 5 bytes of stack including return address

PutChar             proc
                    psha
Loop@@              bsr       KickCOP             ;prevent watchdog timeout
                    lda       SER_STATUS          ;check TX status
                    anda      #TXRDY              ;TX ready?
                    beq       Loop@@
                    pula
                    sta       SER_TXDATA          ;transmit char
                    rts

;*******************************************************************************
; Purpose: Reset watchdog timer.  Must be called at least once every little
;        : while or COP interrupt will occur
; Note(s): Uses 2 bytes of stack including return address

KickCOP             proc
                    lda       #$55
                    sta       H11COPRST
                    coma
                    sta       H11COPRST
                    rts

;*******************************************************************************
; Response string for GET TARGET STATUS request
; Reply describes target:

TSTG                fcb       3                   ;  2: processor type = 68HC11
                    fcb       COMBUF_SIZE         ;  3: size of communications buffer
                    fcb       0                   ;  4: no tasking support
          #ifdef MAPPED
                    dw        ~0                  ;5,6: bottom of mapped mem (LSB first)
                    dw        ~0                  ;7,8: top of mapped mem (LSB first)
          #else
                    dw        ~0                  ;5,6: bottom of mapped mem (LSB first)
                    dw        ~0                  ;7,8: top of mapped mem (LSB first)
          #endif
                    fcb       ?Copyright-?SWI     ;  9: breakpoint instruction length
          #ifdef SWI
?SWI                fcb       0                   ;10+: breakpoint instruction
            #ifdef OS
                    #Message  OS support enabled
                    fcb       0                   ;2nd byte
            #endif
          #else
?SWI                swi                           ;10+: breakpoint instruction
          #endif
?Copyright          fcs       'NoICE HC11 monitor v{VERSION(2)} (ASPiSYS F1 Board)'
                    #size     TSTG                ;SIZE OF STRING

;*******************************************************************************
; HARDWARE PLATFORM INDEPENDENT EQUATES AND CODE
;*******************************************************************************

; Communications function codes.

FN_GET_STAT         equ       $FF                 ;reply with device info
FN_READ_MEM         equ       $FE                 ;reply with data
FN_WRITE_M          equ       $FD                 ;reply with status (+/-)
FN_READ_RG          equ       $FC                 ;reply with registers
FN_WRITE_RG         equ       $FB                 ;reply with status
FN_RUN_TARG         equ       $FA                 ;reply (delayed) with registers
FN_SET_BYTE         equ       $F9                 ;reply with data (truncate if error)
FN_IN               equ       $F8                 ;input from port
FN_OUT              equ       $F7                 ;output to port

FN_MIN              equ       $F7                 ;minimum recognized function code
FN_ERROR            equ       $F0                 ;error reply to unknown op-code

;*******************************************************************************
; Purpose: Common handler for default interrupt handlers
; Input  : A = interrupt code = processor state
; Output :
; Note(s): All registers stacked, PC = next instruction

CommonHandler       proc
                    sta       REG_STATE           ;save state
          ;--------------------------------------
          ; Save registers from stack to reg block for return to master
          ; Host wants least significant bytes first, so flip as necessary
          ;--------------------------------------
                    pula
                    sta       REG_CC              ;condition codes

                    pula
                    sta       REG_B

                    pula
                    sta       REG_A

                    pula
                    sta       REG_X+1             ;MSB

                    pula
                    sta       REG_X               ;LSB

                    pula
                    sta       REG_Y+1             ;MSB

                    pula
                    sta       REG_Y               ;LSB
          ;--------------------------------------
          ; If this is a breakpoint (state = 1), then back up PC to point at SWI
          ; (If SWI2, SWI3, or another instruction is used for breakpoint,
          ; then DEX multiple times to match ?Copyright-?SWI in TSTG
          ;--------------------------------------
                    pulx                          ;PC after interrupt
                    lda       REG_STATE
                    cmpa      #1
                    bne       _1@@                ;Branch if not a breakpoint
                    dex                           ;Else back up to point at SWI location
_1@@                xgdx                          ;Transfer PC to D
                    sta       REG_PC+1            ;MSB
                    stb       REG_PC              ;LSB
                    tsx                           ;User stack pointer plus 1
                    dex                           ;Make it just like the real SP
                    xgdx
                    stb       REG_SP              ;Save user's stack pointer (LSB)
                    sta       REG_SP+1            ;MSB
                    lds       #MONSTACK           ;Change to our own stack
          ;-------------------------------------- ;Save memory page
          #ifdef MAPIMG
                    lda       MAPIMG              ;get current user map
          #endif
                    clr       REG_PAGE            ;... or zero if unmapped target
                    jmp       ReturnRegs          ;Return registers to master

;*******************************************************************************
; Purpose: Main loop:  wait for command frame from master
; Note(s): Uses 7 bytes of stack before jump to handlers

MainLoop            proc
                    lds       #MONSTACK           ;clean stack is happy stack
                    ldx       #COMBUF             ;build message here
          ;-------------------------------------- ;First byte is a function code
                    jsr       GetChar             ;Get a function
                    bcs       MainLoop            ;JIF timeout: RESYNC
                    cmpa      #FN_MIN
                    blo       MainLoop            ;JIF below min: illegal function
                    sta       ,x                  ;save function code
                    inx
          ;-------------------------------------- ;Second byte is data byte count (may be zero)
                    jsr       GetChar             ;Get a length byte
                    bcs       MainLoop            ;JIF timeout: RESYNC
                    cmpa      #COMBUF_SIZE
                    bhi       MainLoop            ;JIF too long: Illegal length
                    sta       ,x                  ;save length
                    inx
                    tsta
                    beq       CheckSum@@          ;skip data loop if length = 0
          ;-------------------------------------- ;Loop for data
                    tab                           ;Save length for loop
GetData@@           jsr       GetChar             ;Get a data byte
                    bcs       MainLoop            ;JIF timeout: RESYNC
                    sta       ,x                  ;save data byte
                    inx
                    decb
                    bne       GetData@@
          ;-------------------------------------- ;Get the checksum
CheckSum@@          jsr       GetChar             ;Get the checksum
                    bcs       MainLoop            ;JIF timeout: RESYNC
                    psha                          ;save checksum
          ;--------------------------------------
          ; Compare received checksum to that calculated
          ; on received buffer (Sum should be 0)
          ;--------------------------------------
                    jsr       CHECKSUM
                    pulb
                    aba
                    bne       MainLoop            ;JIF bad checksum
          ;-------------------------------------- ;Process the message
                    ldx       #COMBUF
                    ldd       ,x                  ;Get the function code and the length
                    inx:2                         ;X points at data

                    cmpa      #FN_GET_STAT
                    beq       TargetStat

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
          ;--------------------------------------
          ; Error: unknown function.  Complain
          ;--------------------------------------
                    lda       #FN_ERROR
                    sta       COMBUF              ;set function as 'ERROR'
                    lda       #1
                    jmp       SendStatus          ;value is 'ERROR'

;*******************************************************************************
; Purpose: Target Status:  FN, len
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

TargetStat          proc
                    ldx       #TSTG               ;data for reply
                    ldy       #COMBUF             ;pointer to return buffer
                    ldb       #::TSTG             ;length of reply
                    stb       1,y                 ;set size in reply buffer
Loop@@              lda       ,x                  ;move reply data to buffer
                    sta       2,y
                    inx
                    iny
                    decb
                    bne       Loop@@
                    jmp       Send                ;Compute checksum on buffer, and send to master, then return

;*******************************************************************************

SetMap              macro
          #ifdef MAPPED
                    lda       ,x
                    sta       MAPIMG
                    sta       MAPREG
          #endif
                    endm

;*******************************************************************************
; Purpose: Read Memory:  FN, len, page, Alo, Ahi, Nbytes
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

ReadMem             proc
                    @SetMap
          ;-------------------------------------- ;Get address
                    lda       2,x                 ;MSB OF ADDRESS IN A
                    ldb       1,x                 ;LSB OF ADDRESS IN B
                    xgdy                          ;ADDRESS IN Y
          ;-------------------------------------- ;Prepare return buffer: FN (unchanged), LEN, DATA
                    ldb       3,x                 ;number of bytes to return
                    stb       COMBUF+1            ;return length = requested data
                    beq       Done@@              ;JIF no bytes to get
          ;-------------------------------------- ;Read the requested bytes from local memory
Loop@@              lda       ,y                  ;get byte
                    sta       ,x                  ;store to return buffer
                    inx
                    iny
                    decb
                    bne       Loop@@
Done@@              jmp       Send                ;Compute checksum on buffer, and send to master, then return

;*******************************************************************************
; Purpose: Write Memory:  FN, len, page, Alo, Ahi, (len-3 bytes of Data)
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2
; Note(s): Uses 6 bytes of stack

WriteMem            proc
                    @SetMap
          ;-------------------------------------- ;Get address
                    lda       2,x                 ;MSB OF ADDRESS IN A
                    ldb       1,x                 ;LSB OF ADDRESS IN B
                    xgdy                          ;ADDRESS IN Y
          ;-------------------------------------- ;Prepare return buffer: FN (unchanged), LEN, DATA
                    ldb       COMBUF+1            ;NUMBER OF BYTES TO RETURN
                    subb      #3                  ;MINUS PAGE AND ADDRESS
                    beq       Loop50@@            ;JIF no bytes to put
          ;-------------------------------------- ;Write the specified bytes to local memory
                    pshb
                    pshx
                    pshy
Loop@@              lda       3,x                 ;get byte to write
                    bsr       CheckY
                    bcs       Skip@@
                    sta       ,y                  ;store the byte at AAAA,y
Skip@@              inx
                    iny
                    decb
                    bne       Loop@@
          ;-------------------------------------- ;Compare to see if the write worked
                    puly
                    pulx
                    pulb
Loop20@@            lda       3,x                 ;get byte just written
                    bsr       CheckY
                    bcs       Fail@@
                    cmpa      ,y
                    bne       Fail@@              ;branch if write failed
                    inx
                    iny
                    decb
                    bne       Loop20@@
          ;-------------------------------------- ;Write succeeded:  return status = 0
Loop50@@            clra                          ;return status = 0
                    bra       Done@@
Fail@@              lda       #1                  ;Write failed:  return status = 1
Done@@              jmp       SendStatus          ;Return OK status

;*******************************************************************************
; Purpose: Check Y against valid ASPiSYS F1 Board addresses
; Note(s): Valid user ranges are 0-$3FF, $1060-$7FFF, RAMVEC-(RAMVEC+42)

CheckY              proc
                    cmpy      #$7FFF
                    bhi       Fail@@

                    cmpy      #RAM_START+42       ;$400 + RAMVECTORS
                    blo       OK@@

                    cmpy      #$1060
                    blo       Fail@@

OK@@                clc
                    rts

Fail@@              sec
                    rts

;*******************************************************************************
; Purpose: Read registers:  FN, len=0
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

ReadRegs            proc
;                   bra       ReturnRegs

;*******************************************************************************
; Enter here from SWI after 'RUN' and 'STEP' to return task registers
; CAUTION: In this case, assume no registers!

ReturnRegs          proc
                    ldy       #TASK_REGS          ;pointer to registers
                    ldb       #::TASK_REGS        ;number of bytes
                    stb       COMBUF+1            ;save return data length
          ;-------------------------------------- ;Copy the registers
                    ldx       #COMBUF+2           ;pointer to return buffer
Loop@@              lda       ,y                  ;get byte to a
                    sta       ,x                  ;store to return buffer
                    inx
                    iny
                    decb
                    bne       Loop@@
                    jmp       Send                ;Compute checksum on buffer, and send to master, then return

;*******************************************************************************
; Purpose: Write registers:  FN, len, (register image)
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

WriteRegs           proc
                    ldb       COMBUF+1            ;number of bytes
                    beq       Done@@              ;JIF no registers
          ;-------------------------------------- ;Copy the registers
                    ldy       #TASK_REGS          ;pointer to registers
Loop@@              lda       ,x                  ;get byte to A
                    sta       ,y                  ;store to register RAM
                    inx
                    iny
                    decb
                    bne       Loop@@

Done@@              clra                          ;Return OK status
                    !jmp      SendStatus

;*******************************************************************************
; Purpose: Run Target: FN, len
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

RunTarget           proc
          #ifdef MAPPED                           ;Restore user's map
                    lda       REG_PAGE            ;User's page
                    sta       MAPIMG              ;Set image
                    sta       MAPREG              ;Set mapping register
          #endif
          ;-------------------------------------- ;Switch to user stack
                    ldb       REG_SP              ;back to user stack
                    lda       REG_SP+1
                    xgdx                          ;to X
                    inx                           ;pre-correct for TXS
                    txs                           ;SP = X-1
          ;-------------------------------------- ;Restore registers
                    lda       REG_PC              ;save LSB user PC for RTI
                    psha
                    lda       REG_PC+1            ;save MSB user PC for RTI
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

                    lda       REG_CC              ;save user condition codes for RTI
                    psha
                    RTI                           ;return to user

;*******************************************************************************
; Purpose: Set target byte(s): FN, len { (page, alow, ahigh, data), (...)... }
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2
; Output : Return has FN, len, (data from memory locations)
; Note(s): If error in insert (memory not writable), abort to return short data
;        : This function is used primarily to set and clear breakpoints
;        : Uses 3 bytes of stack

SetBytes            proc
                    ldy       #COMBUF+1           ;pointer to return buffer
                    clra
                    sta       ,y                  ;set return count as zero
                    iny                           ;point at first return data byte
                    lsrb:2                        ;len/4 = number of bytes to set
                    beq       Done@@              ;JIF no bytes (COMBUF+1 = 0)
          ;-------------------------------------- ;Loop on inserting bytes
Loop@@              pshb                          ;save loop counter
                    pshy                          ;save return buffer pointer

                    @SetMap
          ;-------------------------------------- ;Get address
                    lda       2,x                 ;MSB of address in A
                    ldb       1,x                 ;LSB of address in B
                    xgdy                          ;memory address in Y

                    lda       ,y                  ;Read current data at byte location
          ;-------------------------------------- ;Insert new data at byte location
                    ldb       3,x                 ;get byte to store
                    stb       ,y                  ;write target memory
          ;-------------------------------------- ;Verify write
                    cmpb      ,y                  ;read target memory
                    puly                          ;restore return ptr (CC's intact)
                    pulb                          ;restore loop counter (CC's intact)
                    bne       Abort@@             ;branch if insert failed: Abort

                    sta       ,y                  ;Save target byte in return buffer
                    iny                           ;advance to next return byte
                    inc       COMBUF+1            ;count one return byte
          ;-------------------------------------- ;Loop for next byte
                    inx:4                         ;step to next byte specifier
                    cmpb      COMBUF+1
                    bne       Loop@@              ;loop for all bytes
Abort@@   ; Return buffer with data from byte locations
          ; Compute checksum on buffer, and send to master, then return
Done@@              bra       Send

;*******************************************************************************
; Purpose: Input from port:  FN, len, PortAddressLo, PAhi (=0)
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2
; Note(s): While the HC11 has no input or output instructions, we retain these
;        : to allow write-without-verify

InPort              proc
          ;-------------------------------------- ;Get port address
                    lda       1,x                 ;MSB of address in A
                    ldb       ,x                  ;LSB of address in B
                    xgdy                          ;memory address in Y

                    lda       ,y                  ;Read the requested byte from local memory
                    bra       SendStatus          ;Return byte read as 'status'

;*******************************************************************************
; Purpose: Output to port:  FN, len, PortAddressLo, PAhi (=0), data
; Input  : A = function code
;        : B = data size
;        : X = COMBUF+2

OutPort             proc
          ;-------------------------------------- ;Get port address
                    lda       1,x                 ;MSB of address in A
                    ldb       ,x                  ;LSB of address in B
                    xgdy                          ;memory address in Y

                    lda       2,x                 ;Get data
                    sta       ,y                  ;Write value to port
          ;--------------------------------------
          ; Do not read port to verify (some I/O devices don't like it)
          ;--------------------------------------
                    clra                          ;Return status of OK
;                   bra       SendStatus

;*******************************************************************************
; Build status return with value from 'A'

SendStatus          proc
                    sta       COMBUF+2            ;set status
                    lda       #1
                    sta       COMBUF+1            ;set length
;                   bra       Send

;*******************************************************************************
; Append checksum to COMBUF and send to master

Send                proc
                    bsr       CHECKSUM            ;A=checksum, X->checksum location
                    nega
                    sta       ,x                  ;store negative of checksum
          ;-------------------------------------- ;Send buffer to master
                    ldx       #COMBUF             ;pointer to data
                    ldb       1,x                 ;length of data
                    addb      #3                  ;plus function, length, checksum
Loop@@              lda       ,x
                    jsr       PutChar             ;Send a byte
                    inx
                    decb
                    bne       Loop@@
                    jmp       MainLoop

;*******************************************************************************
; Purpose: Compute checksum on COMBUF.  COMBUF+1 has length of data,
;        : Also include function byte and length byte
; Input  : None
; Output : A = checksum
;        : X -> next byte in buffer (checksum location)
;        : B is scratched
; Note(s): Uses 2 bytes of stack including return address

CHECKSUM            proc
                    ldx       #COMBUF             ;pointer to buffer
                    ldb       1,x                 ;length of message
                    addb      #2                  ;plus function, length
                    clra                          ;init checksum to 0
Loop@@              adda      ,x
                    inx
                    decb
                    bne       Loop@@              ;loop for all
                    rts                           ;return with checksum in A

;*******************************************************************************
; Purpose: Interrupt handlers to catch unused interrupts and traps
; Note(s): Registers are stacked.  Jump through RAM vector using X, type in A
;        : This will affect only interrupt routines looking for register values!
;        : Our default handler uses the code in 'A' as the processor state to be
;        : passed back to the host.

?                   macro     [VectorNumber],[VectorOffset]
                    mdef      1,{:mindex-1}
                    mdef      2,{~1~*2}
                    lda       #20-~1~             ;{$FFD6+{~1~*2}(h)}
                    ldx       RAMVEC+~2~
                    jmp       ,x
                    endm

SCI_Handler         @?
SPI_Handler         @?
PACE_Handler        @?
PACO_Handler        @?
TOV_Handler         @?
TCOMP5_Handler      @?
TCOMP4_Handler      @?
TCOMP3_Handler      @?
TCOMP2_Handler      @?
TCOMP1_Handler      @?
TCAP3_Handler       @?
TCAP2_Handler       @?
TCAP1_Handler       @?
RTC_Handler         @?
IRQ_Handler         @?
XIRQ_ENTRY          @?        18,30

;*******************************************************************************

SWI_Handler         proc
          #ifdef SWI
                    #Message  SWI is for user code only ($00 for debugger)
                    ldx       RAMVEC+32           ;$FFF6
                    pshx                          ;save SWI handler vector
                    tsx
                    ldx       _X_+2,x             ;restore original X
                    rts                           ;call vectored SWI handler
          #else
                    #Message  SWI not allowed
                    lda       #1
                    jmp       CommonHandler
          #endif

;*******************************************************************************
; Non-RAM vectored

ILLOP_Handler       proc
          #ifdef SWI
                    tsx
                    ldx       _PC_,x
                    lda       ,x                  ;get opcode
                    tsx
                    cmpa      ?SWI                ;is it the breakpoint?
                    bne       Real@@              ;no, it's a real ILLEGAL opcode
                    ldd       _PC_,x              ;increment PC (as with SWI)
                    incd
                    std       _PC_,x
                    ldb       _B_,x
                    lda       #1                  ;Code=1 indicates breakpoint
                    bra       BreakPoint@@
Real@@              lda       #5                  ;indicates illegal opcode
BreakPoint@@        ldx       _X_,x               ;restore original X
          #else
                    lda       #5
          #endif
                    jmp       CommonHandler

;*******************************************************************************
                    #VECTORS
;*******************************************************************************
                    org       VECTORS             ;Vectors through RAM

NVEC                dw        SCI_Handler         ;$FFD6
                    dw        SPI_Handler         ;$FFD8
                    dw        PACE_Handler        ;$FFDA
                    dw        PACO_Handler        ;$FFDC
                    dw        TOV_Handler         ;$FFDE
                    dw        TCOMP5_Handler      ;$FFE0
                    dw        TCOMP4_Handler      ;$FFE2
                    dw        TCOMP3_Handler      ;$FFE4
                    dw        TCOMP2_Handler      ;$FFE6
                    dw        TCOMP1_Handler      ;$FFE8
                    dw        TCAP3_Handler       ;$FFEA
                    dw        TCAP2_Handler       ;$FFEC
                    dw        TCAP1_Handler       ;$FFEE
                    dw        RTC_Handler         ;$FFF0
                    dw        IRQ_Handler         ;$FFF2
                    dw        XIRQ_ENTRY          ;$FFF4 (non-maskable interrupt)
          #ifdef SWI
                    dw        SWI_Handler         ;$FFF6 SWI/breakpoint
                    #size     NVEC                ;number of vector bytes
          #else
                    #size     NVEC                ;number of vector bytes
                    dw        SWI_Handler         ;$FFF6 SWI/breakpoint
          #endif
          ;--------------------------------------
          ; The remaining interrupts are permanently trapped to the monitor
          ;--------------------------------------
                    dw        ILLOP_Handler       ;$FFF8 illegal op-code
                    dw        COP_Handler         ;$FFFA Watchdog timeout
                    dw        Clock_Handler       ;$FFFC clock fail
                    dw        Start               ;$FFFE reset

                    end       :s19crc
