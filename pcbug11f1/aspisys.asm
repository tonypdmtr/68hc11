;*******************************************************************************
;*                   MC68HC11F1 PCBUG11 BOOTLOADER                             *
;* Disassembled from the 711E9 talker and modified for ASPiSYS F1 Board        *
;*               by Tony Papadimitriou <tonyp@acm.org>                         *
;*******************************************************************************
;*            SDP_Off was made optional with conditional SDP                   *
;*******************************************************************************

                    #ExtraOn
                    #CaseOn

REGS                def       $1000
SCSR                equ       REGS+$2E
SCDR                equ       REGS+$2F
BAUD                equ       REGS+$2B
SCCR1               equ       REGS+$2C
SCCR2               equ       REGS+$2D

BPROT               equ       REGS+$35

CSSTRH              equ       REGS+$5C            ;Chip Select Clock Stretch
CSCTL               equ       REGS+$5D            ;Chip Select Program Control
CSGADR              equ       REGS+$5E            ;Chip Select General Address
CSGSIZ              equ       REGS+$5F            ;Chip Select General Address Size
          #ifdef SDP
?$5555              equ       $D555               ;change to 5555 if using lower 32K
?$2AAA              equ       $AAAA               ;change to 2AAA if using lower 32K
          #endif
RAM                 equ       0
ROM                 equ       $FE70               ;beginning of program
STACKTOP            equ       $3FF                ;top of stack

relocate_buf        exp       $0100               ;Breakpoint relocation address for user code

xirq_ujmp           exp       $00F2               ;Talker code address of user XIRQ server address
xirq_jmp            exp       xirq_ujmp           ;XIRQ vector
swi_jmp             exp       $00F5               ;SWI vector
cme_jmp             exp       $00FB               ;COP clock monitor vector

PPROG               exp       $103B               ;address of PPROG register
EPROG               exp       PPROG               ;address of EPROG register (found only on 711E20)

RAMVECTORS          equ       $00C4               ;beginning of RAM copy of vectors (WAS: $BF)

;*******************************************************************************
                    #ROM
                    org       ROM
;*******************************************************************************

talker_start        exp       *                   ;Talker code start address

;*******************************************************************************

CopyByte            proc
                    lda       ,y
                    sta       ,x
                    inx
                    iny
                    rts

;*******************************************************************************

user_start          exp       *                   ;User reset entry into talker code

Start               proc
                    lds       #STACKTOP           ;Setup stack area
                    ldx       #REGS               ;Point to Control Register

          ;the following code segment sets up the F1 Board's chip selects

                    clr       [CSSTRH,x           ;No clock stretch
                    clr       [CSGADR,x           ;RAM at $0000
                    lda       #%1                 ;32K RAM
                    sta       [CSGSIZ,x
                    lda       #%01000101          ;CSIO1 Active High, 32K ROM at $8000
                    sta       [CSCTL,x

          ;enable writes to EEPROM (but not CONFIG)
                    bclr      [BPROT,x,%00001111
          #ifdef SDP
          ;the following section copies the SDP_Off routine in RAM
          ;and executes it from there
                    getx      #SDP_Off.End-SDP_Off
                    ldy       #SDP_Off
                    pshx
Loop@@              bsr       CopyByte
                    cmpy      #SDP_Off.End
                    blo       Loop@@
                    pulx
                    jsr       ,x                  ;turn off Software Data Protection
                    givex     #SDP_Off.End-SDP_Off
          #endif
                    ldx       #RAMVECTORS
                    ldy       #VECTORS
CopyVectors@@       bsr       CopyByte
                    cmpx      #$100
                    blo       CopyVectors@@

                    ldx       #REGS

                    clr       [SCCR1,x            ;Set Start-8-Stop mode
                    ldd       #$302C              ;BAUD and SCI mode
                    sta       [BAUD,x             ;Set BAUD rate
                    stb       [SCCR2,x            ;and SCI mode
                    lda       #$40                ;Setup Condition Codes
                    tap
;                   bra       talker_idle

;*******************************************************************************

talker_idle         exp       *                   ;Talker code idle loop address
                    bra       *                   ;Wait here forever

;*******************************************************************************

IsChar              proc
Loop@@              lda       SCSR                ;Check if there's a character
                    anda      #$20
                    beq       Loop@@              ;Wait until there's a character
                    lda       SCDR                ;Get Data Byte
                    coma
                    bsr       PutSCICh            ;Echo the received byte's complement
                    bpl       L0073
                    bsr       GetSCICh
                    xgdx
                    bsr       GetSCICh
                    tba
                    bsr       GetSCICh
                    xgdx
                    cmpa      #$FE
                    bne       ChkCmd
;                   bra       IsChar001

;*******************************************************************************

read_jmp            exp       *                   ;address of BSR to read memory
IsChar001           proc
Loop@@              bsr       GetChar
                    bsr       PutSCICh
                    tba
                    bsr       GetSCICh
                    tab
                    inx
                    decb
                    bne       Loop@@
                    rti

;*******************************************************************************

GetChar             proc
                    lda       ,x
                    rts

;*******************************************************************************

ChkCmd              proc
                    cmpa      #$BE
                    bne       null_srv
                    tba
;                   bra       ChkCmd001

;*******************************************************************************

ChkCmd001           proc
Loop@@              bsr       GetSCICh
prog_jmp            exp       *                   ;address of BSR to write to memory
                    bsr       PutChar
                    ldb       ,x
                    stb       SCDR                ;Write data to SCI port
                    inx
                    deca
                    bne       Loop@@

xirq_srv            exp       *                   ;Talker XIRQ service address
null_srv            exp       *                   ;Talker RTI
                    rti

;*******************************************************************************

PutChar             proc
                    stb       ,x
                    rts

;*******************************************************************************

GetSCICh            proc
Loop@@              ldb       SCSR                ;Read SCI status register
                    bitb      #$0A                ;Overrun or framing error?
                    bne       Start               ;If yes, restart program
                    andb      #$20                ;Is there a character?
                    beq       Loop@@              ;No, keep looking
                    ldb       SCDR                ;Yes, get character in B
                    rts

;*******************************************************************************

PutSCICh            proc
Loop@@              tst       SCSR                ;Test SCI status register
                    bpl       Loop@@              ;Wait for empty transmit buffer
                    sta       SCDR                ;Put character to transmit buffer
                    rts

;*******************************************************************************

L0073               proc
                    cmpa      #$7E
                    bne       L0083
L0077               tsx
                    xgdx
                    bsr       PutSCICh
                    tba
                    bsr       PutSCICh
                    tsx
                    ldb       #$09
                    bra       IsChar001

L0083               cmpa      #$3E
                    bne       L0099
                    bsr       GetSCICh
                    tba
                    bsr       GetSCICh
                    xgdx
                    txs
                    lda       #$09
                    bra       ChkCmd001

;*******************************************************************************

swi_srv             exp       *                   ;Talker SWI service address for breakpoints
                    lda       #$4A
                    bsr       PutSCICh
;                   bra       swi_idle

;*******************************************************************************

swi_idle            exp       *                   ;Talker SWI idle loop
                    wait                          ;cli
                    bra       swi_idle

;*******************************************************************************

L0099               cmpa      #$4A
                    bne       null_srv
                    tsx
                    ldb       #$09
                    abx
                    txs
                    ldd       7,x
                    bsr       PutSCICh
                    tba
                    bsr       PutSCICh
                    ldd       #swi_idle
                    std       7,x
                    bra       L0077

;*******************************************************************************

#ifdef SDP
SDP_Off             proc
                    lda       #$AA                ;LOAD $AA TO $5555
                    sta       ?$5555
                    coma                          ;LOAD $55 TO $2AAA
                    sta       ?$2AAA
                    lda       #$80                ;Load $80 to $5AAA
                    sta       ?$5555
                    lda       #$AA                ;Load $AA to $5555
                    sta       ?$5555
                    coma                          ;Load $55 to $2AAA
                    sta       ?$2AAA
                    lda       #$20                ;Load $20 to $5AAA
                    sta       ?$5555

;*******************************************************************************

Delay10             proc
                    pshx
                    ldx       #333*30             ;make it 30 msec (fast compatible)
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts
SDP_Off.End         equ       *
#endif

VECTORS             equ       *                   ;20 + 1 vectors
vSCI                !jmp      IsChar              ;SCI action interrupt
vSPI                !jmp      null_srv            ;SPI transfer complete
vPACC               !jmp      null_srv            ;Pulse Accumulator Input Edge
vPAOV               !jmp      null_srv            ;Pulse Accumulator Overflow
vTOV                !jmp      null_srv            ;Timer Overflow
vTIC4               !jmp      null_srv            ;Timer Input Capture 4/Output Compare 5
vTOC4               !jmp      null_srv            ;Timer Output Compare 4
vTOC3               !jmp      null_srv            ;Timer Output Compare 3
vTOC2               !jmp      null_srv            ;Timer Output Compare 2
vTOC1               !jmp      null_srv            ;Timer Output Compare 1
vTIC3               !jmp      null_srv            ;Timer Input Capture 3
vTIC2               !jmp      null_srv            ;Timer Input Capture 2
vTIC1               !jmp      null_srv            ;Timer Input Capture 1
vRTI                !jmp      null_srv            ;Real-Time Interrupt
vIRQ                !jmp      null_srv            ;IRQ
vXIRQ               !jmp      null_srv            ;XIRQ
vSWI                !jmp      swi_srv             ;SWI
vILLOP              !jmp      Start               ;Illegal Opcode
vCOP                !jmp      null_srv            ;COP Failure
vCMF                !jmp      null_srv            ;Clock Monitor Fail
;xirq_ujmp          exp       *
                    !jmp      Start               ;Reset

;*******************************************************************************
                    #VECTORS
                    org       $FFD6
;*******************************************************************************

?                   macro
                    dw        RAMVECTORS-VECTORS+~1~
                    endm

                    @?        vSCI
                    @?        vSPI
                    @?        vPACC
                    @?        vPAOV
                    @?        vTOV
                    @?        vTIC4
                    @?        vTOC4
                    @?        vTOC3
                    @?        vTOC2
                    @?        vTOC1
                    @?        vTIC3
                    @?        vTIC2
                    @?        vTIC1
                    @?        vRTI
                    @?        vIRQ
                    @?        vXIRQ
                    @?        vSWI
                    @?        vILLOP
                    @?        vCOP
                    @?        vCMF
                    dw        Start

                    end       Start
