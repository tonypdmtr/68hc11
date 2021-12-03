;*******************************************************************************
; SWTBUG VERSION 1.00 - REPLACEMENT FOR MIKBUG ROM
; FOR SWTPC 6800 COMPUTER SYSTEM
; COPYRIGHT 1977
; SOUTHWEST TECHNICAL PROD. CORP.
; AUGUST, 1977
;*******************************************************************************
;
; Revised for the 68HC11
; 15 December 2002
; John E. Kent

                    #RAM      $A000

IRQ                 rmb       2                   ; IRQ POINTER
BEGA                rmb       2                   ; BEGINNING ADDR PNCH
ENDA                rmb       2                   ; ENDING ADDR PNCH
NMI                 rmb       2                   ; NMI INTERRUPT VECTOR
SP                  rmb       1                   ; S HIGH
                    rmb       1                   ; S LOW
PORADD              rmb       2                   ; PORT ADDRESS
PORECH              rmb       1                   ; ECHO ON/OFF FLAG
XHI                 rmb       1                   ; XREG HIGH
XLOW                rmb       1                   ; XREG LOW
CKSM                rmb       1                   ; CHECKSUM
XTEMP               rmb       2                   ; X-REG TEMP STGE
SWIJMP              rmb       2                   ; SWI JUMP VECTOR
TW                  equ       $A044               ; TEMPORARY STORAGE
TEMP                equ       $A046               ; TEMPORARY STORAGE
BYTECT              equ       $A047               ; BYTECT AND MCONT TEMP.
CTLPOR              equ       $8004               ; CONTROL PORT ADDRESS
PROM                equ       $C000               ; JUMP TO PROM ADDRESS
BKPT                rmb       2                   ; BREAKPOINT ADDRESS
BKLST               rmb       1                   ; BREAKPOINT DATA
;
; Two more bytes added to the stack
; for the IY register
;
STACK               equ       $A040               ; SWTBUG STACK

                    #ROM      $E000
;
; I/O INTERRUPT SEQUENCE
IRQV                ldx       IRQ
                    jmp       0,x

;
; JUMP TO USER PROGRAM
JUMP                bsr       BADDR
                    jmp       0,x

;
CURSOR              fcb       $10,$16,4           ; CT-1024 CURSOR CONTROL
;
; ASCII LOADING ROUTINE
LOAD                jsr       RDON                ; READER ON, DIS ECHO, GET P#
LOAD3               bsr       INCH
                    cmpa      #'S'
                    bne       LOAD3               ; 1ST CHAR NOT S
                    bsr       INCH                ; READ CHAR
                    cmpa      #'9'
                    beq       LOAD21
                    cmpa      #'1'
                    bne       LOAD3               ; 2ND CHAR NOT 1
                    clr       CKSM                ; ZERO CHECKSUM
                    bsr       BYTE                ; READ BYTE
                    suba      #2
                    sta       BYTECT              ; BYTE COUNT
; BUILD ADDRESS
                    bsr       BADDR
; STORE DATA
LOAD11              bsr       BYTE
                    dec       BYTECT
                    beq       LOAD15              ; ZERO BYTE COUNT
                    sta       0,x                 ; STORE DATA
                    cmpa      0,x                 ; DATA STORED?
                    bne       LOAD19
                    inx
                    bra       LOAD11

LOAD15              inc       CKSM
                    beq       LOAD3
LOAD19              lda       #'?'
                    bsr       OUTCH
LOAD21              jmp       RDOFF1

;
; BUILD ADDRESS
BADDR               bsr       BYTE                ; READ 2 FRAMES
                    sta       XHI
                    bsr       BYTE
                    sta       XLOW
                    ldx       XHI                 ; LOAD IXR WITH NUMBER
                    rts

;
; INPUT BYTE (TWO FRAMES)
BYTE                bsr       INHEX               ; GET HEX CHAR
BYTE1               asla
                    asla
                    asla
                    asla
                    tab
                    bsr       INHEX
                    aba
                    tab
                    addb      CKSM
                    stb       CKSM
                    rts

;
OUTHL               lsra                          ; OUT HEX LEFT BCD DIGIT
                    lsra
                    lsra
                    lsra
OUTHR               anda      #$F                 ; OUT HEX RIGHT BCD DIGIT
                    adda      #$30
                    cmpa      #$39
                    bls       OUTCH
                    adda      #$7
;
; OUTPUT ONE CHAR
OUTCH               jmp       OUTEEE

INCH                jmp       INEEE

;
; PRINT DATA POINTED TO BY X REG
PDATA2              bsr       OUTCH
                    inx
PDATA1              lda       0,x
                    cmpa      #4
                    bne       PDATA2
                    rts                           ; STOP ON HEX 04

;
C1                  jmp       SWTCTL

;
; MEMORY EXAMINE AND CHANGE
CHANGE              bsr       BADDR
CHA51               ldx       #MCL
                    bsr       PDATA1              ; C/R L/F
                    ldx       #XHI
                    bsr       OUT4HS              ; PRINT ADDRESS
                    ldx       XHI
                    bsr       OUT2HS              ; PRINT OLD DATA
                    bsr       OUTS                ; OUTPUT SPACE
ANOTH               bsr       INCH                ; INPUT CHAR
                    cmpa      #$20
                    beq       ANOTH
                    cmpa      #$D
                    beq       C1
                    cmpa      #'^'                ; UP ARROW?
                    bra       AL3                 ; BRANCH FOR ADJUSTMENT

                    nop
;
; INPUT HEX CHARACTER
INHEX               bsr       INCH
INHEX1              suba      #$30
                    bmi       C3
                    cmpa      #$9
                    ble       IN1HG
                    cmpa      #$11
                    bmi       C3                  ; NOT HEX
                    cmpa      #$16
                    bgt       C3                  ; NOT HEX
                    suba      #7
IN1HG               rts

;
OUT2H               lda       0,x                 ; OUTPUT 2 HEX CHAR
OUT2HA              bsr       OUTHL               ; OUT LEFT HEX CHAR
                    lda       0,x
                    inx
                    bra       OUTHR               ; OUTPUT RIGHT HEX CHAR

;
OUT4HS              bsr       OUT2H               ; OUTPUT 4 HEX CHAR + SPACE
OUT2HS              bsr       OUT2H               ; OUTPUT 2 HEX CHAR + SPACE
;
OUTS                lda       #$20                ; SPACE
                    bra       OUTCH               ; (BSR & TRS)

;
; ENTER POWER ON SEQUENCE
START               lds       #STACK
                    bra       AL1                 ; BRANCH FOR ADDRESS COMPATIBIL

;
;********************************************
; PART OF MEMORY EXAMINE AND CHANGE
AL3                 bne       SK1
                    dex
                    dex
                    stx       XHI
                    bra       CHA51

SK1                 stx       XHI
                    bra       AL4

;
EOE3                bra       CONTRL              ; BRANCH FOR MIKBUG EQUIV. CONT

;
AL4                 cmpa      #$30
                    bcs       CHA51
                    cmpa      #$46
                    bhi       CHA51
                    bsr       INHEX1
                    jsr       BYTE1
                    dex
                    sta       0,x                 ; CHANGE MEMORY
                    cmpa      0,x
                    beq       CHA51               ; DID CHANGE
                    jmp       LOAD19              ; DIDN'T CHANGE

C3                  lds       SP
                    bra       SWTCTL

;**************************************************
;
; CONTINUE POWER UP SEQUENCE
AL1                 sts       SP                  ; INIT TARGET STACK PTR.
                    lda       #$FF
                    jsr       SWISET
; CONFIGURE FOR PIA AND SEE IF OK
                    ldx       #CTLPOR
                    jsr       PIAINI              ; INIT PIA
                    lda       0,x
                    cmpa      2,x
                    bra       AL2

;
                    bra       PRINT               ; BRA FOR BILOAD

;
AL2                 bne       CONTRL
;
; INITIALIZE AS ACIA
                    lda       #3                  ; ACIA MASTER RESET
                    sta       0,x
                    lda       #$11
                    sta       0,x
                    bra       CONTRL

;
; ENTER FROM SOFTWARE INTERRUPT
SF0                 nop
SFE1                sts       SP                  ; SAVE TARGETS STACK POINTER
; DECREMENT P COUNTER
                    tsx
                    tst       6,x
                    bne       *+4
                    dec       5,x
                    dec       6,x
; PRINT CONTENTS OF STACK.
PRINT               ldx       #MCL
                    jsr       PDATA1
                    ldx       SP
                    inx
                    bsr       OUT2HS              ; COND CODES
                    bsr       OUT2HS              ; ACC B
                    bsr       OUT2HS              ; ACC A
                    jmp       PATCH1

                    nop
; BSR OUT4HS IXR
; BSR OUT4HS PGM COUNTER
PATCH2              ldx       #SP
                    jsr       OUT4HS              ; STACK POINTER
SWTCTL              ldx       SWIJMP
                    cpx       #SF0
                    beq       CONTR1
;
CONTRL              lds       #STACK              ; SET CONTRL STACK POINTER
                    ldx       #CTLPOR             ; RESET TO CONTROL PORT
                    stx       PORADD
                    clr       PORECH              ; TURN ECHO ON
                    bsr       SAVGET              ; GET PORT # AND TYPE
                    beq       POF1
                    jsr       PIAECH              ; SET PIA ECHO ON IF MP-C INTER
POF1                jsr       PNCHOF              ; TURN PUNCH OFF
                    jsr       RDOFF               ; TURN READER OFF
CONTR1              ldx       #MCLOFF
                    jsr       PDATA1              ; PRINT DATA STRING
                    bsr       INEEE               ; READ COMMAND CHARACTER
;
; COMMAND LOOKUP ROUTINE
LOOK                ldx       #TABLE
OVER                cmpa      0,x
                    bne       SK3
                    jsr       OUTS                ; SKIP SPACE
                    ldx       1,x
                    jmp       0,x

SK3                 inx
                    inx
                    inx
                    cpx       #TABEND+3
                    bne       OVER
SWTL1               bra       SWTCTL

;
; SOFTWARE INTERRUPT ENTRY POINT
SFE                 ldx       SWIJMP              ; JUMP TO VECTORED SOFTWARE INT
                    jmp       0,x

;
S9                  fcb       'S9',4              ; END OF TAPE
;
;***************************************************
MTAPE1              fcb       $D,$A,$15,0,0,0,'S1',4  ; PUNCH FORMAT
;
MCLOFF              fcb       $13                 ; READER OFF
MCL                 fcb       $D,$A,$15,0,0,0,'$',4
;
EIA5                bra       BILD                ; BINARY LOADER INPUT

;***************************************************
;
;
; NMI SEQUENCE
NMIV                ldx       NMI                 ; GET NMI VECTOR
                    jmp       0,x

;
INEEE               bra       INEEE1

;
; BYTE SEARCH ROUTINE
SEARCH              jsr       BADDR               ; GET TOP ADDRESS
                    stx       ENDA
                    jsr       BADDR               ; GET BOTTOM ADDRESS
                    jsr       BYTE                ; GET BYTE TO SEARCH FOR
                    tab
OVE                 lda       0,x
                    stx       XHI
                    cba
                    beq       PNT
                    bra       INCR1

PNT                 ldx       #MCL
                    jsr       PDATA1
                    ldx       #XHI
                    bra       SKP0

;***************************************************
;
; GO TO USER PROGRAM ROUTINE
GOTO                rti

OUTEEE              bra       OUTEE1

;
;
;
; SAVE IXR AND LOAD IXR WITH CORRECT
; PORT NUMBER AND TEST FOR TYPE
SAVGET              stx       XTEMP               ; STORE INDEX REGISTER
GETPT1              ldx       PORADD
ISACIA              pshb
                    ldb       1,x
                    cmpb      3,x
                    pulb
                    rts

;***************************************************
;
; CONTINUATION OF SEARCH ROUTINE
SKP0                jsr       OUT4HS
                    ldx       XHI
INCR1               cpx       ENDA
                    beq       SWTL1
                    inx
                    bra       OVE

;
INEEE1              bsr       INCH8               ; INPUT 8 BIT CHARACTER
                    anda      #%01111111          ; GET RID OF PARITY BIT
                    rts

;
BILD                ins                           ; FIX UP STACK WHEN USING
                    ins                           ; BINARY LOADER ON SWTPC TAPES
                    ins
;
; INPUT ONE CHAR INTO ACC B
INCH8               pshb                          ; SAVE ACC B
                    bsr       SAVGET              ; SAVE IXR, GET PORT# AND TYPE
                    bne       IN1                 ; INPUT FROM PIA IF NOT
                    lda       #$15                ; RECONFIG FOR 8 BIT, 1 SB
                    sta       0,x
ACIAIN              lda       0,x
                    asra
                    bcc       ACIAIN              ; NOT READY
                    lda       1,x                 ; LOAD CHAR
                    ldb       PORECH
                    beq       ACIOUT              ; ECHO
                    bra       RES                 ; DON'T ECHO

;
; OUTPUT ONE CHARACTER
OUTEE1              pshb                          ; SAVE ACC B
                    bsr       SAVGET
                    bne       IOUT
;
ACIOUT              ldb       #$11
                    stb       0,x
ACIOU1              ldb       0,x
                    asrb
                    asrb
                    bcc       ACIOU1              ; ACIA NOT READY
                    sta       1,x                 ; OUTPUT CHARACTER
RES                 pulb                          ; RESTORE ACC B
                    ldx       XTEMP
                    rts

;
; PIA INPUT ROUTINE
IN1                 lda       0,x                 ; LOOK FOR START BIT
                    bmi       IN1
                    bsr       DDL                 ; DELAY HALF BIT TIME
                    ldb       #4                  ; SET DEL FOR FULL BIT TIME
                    stb       2,x
                    aslb                          ; SET UP CNTR WITH 8
IN3                 bsr       DEL                 ; WAIT ONE CHAR TIME
                    sec
                    rol       0,x
                    rora
                    decb
                    bne       IN3
                    bsr       DEL                 ; WAIT FOR STOP BIT
                    ldb       PORECH              ; IS ECHO DESIRED?
                    beq       IOUT2               ; ECHO
                    bra       RES                 ; RESTORE IXR,ACCB

; PIA OUTPUT ROUTINE
IOUT                bsr       DDL1                ; DELAY ONE HALF BIT TIME
                    ldb       #$A                 ; SET UP COUNTER
                    dec       0,x                 ; SET START BIT
                    bsr       DE                  ; START TIMER
OUT1                bsr       DEL                 ; DELAY ONE BIT TIME
                    sta       0,x                 ; PUT OUT ONE DATA BIT
                    sec
                    rora                          ; SHIFT IN NEXT BIT
                    decb                          ; DECREMENT COUNTER
                    bne       OUT1                ; TEST FOR 0
IOUT2               ldb       2,x                 ; TEST FOR STOP BITS
                    aslb                          ; SHIFT BIT TO SIGN
                    bpl       RES                 ; BRA FOR 1 STOP BIT
                    bsr       DEL                 ; DELAY FOR STOP BITS
                    bra       RES

DEL                 tst       2,x                 ; IS TIME UP
                    bpl       DEL
DE                  inc       2,x                 ; RESET TIMER
                    dec       2,x
                    rts

;
DDL                 clr       2,x                 ; HALF BIT DELAY
DDL1                bsr       DE
                    bra       DEL

;
;
; OPTIONAL PORT ROUTINE
OPTL                bsr       INEEE1
                    tab
                    clr       PORADD+1            ; SET I/O ADDRESS FOR $8000
                    ldx       PORADD
                    bsr       PIAINI              ; INITIALIZE PIA
                    bsr       PIAECH              ; SET ECHO
                    ldx       #TABLE1             ; P, L OR E
                    tba
                    jmp       OVER                ; LOOK AT TABLE FOR E, L OR P

;
PIAECH              lda       #$34                ; SET DDR
                    sta       3,x
                    sta       2,x
NOOPT               rts

;
; PIA INITIALIZATION ROUTINE
PIAINI              inc       0,x                 ; SET DDR
                    lda       #$7
                    sta       1,x
                    inc       0,x
                    sta       2,x
                    rts

;
; MINIFLOPPY DISK BOOT
DISK                clr       $8014
                    bsr       DELAY
                    ldb       #$0B
                    bsr       RETT2
LOOP1               ldb       4,x
                    bitb      #1
                    bne       LOOP1
                    clr       6,x
                    bsr       RETURN
                    ldb       #$9C
                    bsr       RETT2
                    ldx       #$2400
LOOP2               bitb      #2
                    beq       LOOP3
                    lda       $801B
                    sta       0,x
                    inx
LOOP3               ldb       $8018
                    bitb      #1
                    bne       LOOP2
                    jmp       $2400

RETT2               stb       4,x
RETURN              bsr       RETT1
RETT1               rts

;
; GENERAL PURPOSE DELAY LOOP
DELAY               ldx       #$FFFF
DELAY1              dex
                    cpx       #$8014              ; STOP AT 8014
DUM                 bne       DELAY1
                    rts

;
;
; CLRAR SCREEN FOR CT-1024 TYPE TERMINALS
CLEAR               ldx       #CURSOR
                    jsr       PDATA1
                    bsr       DELAY1              ; DELAY
RDOFF1              bsr       RDOFF
                    bra       C4

;
; BREAKPOINT ENTERING ROUTINE
BREAK               ldx       #SF0
                    cpx       SWIJMP              ; BREAKPOINTS ALREADY IN USE?
                    beq       INUSE
                    inx
BREAK0              bsr       STO1
                    jsr       BADDR
                    stx       BKPT
                    lda       0,x
                    sta       BKLST
                    lda       #$3F
                    sta       0,x
                    ldx       #SF0
                    bsr       STO1
                    jmp       CONTR1

INUSE               ldx       BKPT
                    lda       BKLST
                    sta       0,x
                    ldx       #SFE1
                    bra       BREAK0

;
SWISET              sta       STACK+1             ; FIX POWER UP INTERRUPT
                    ldx       SWIJMP
                    cpx       #SF0
                    beq       STORTN
STO                 ldx       #SFE1
STO1                stx       SWIJMP
STORTN              rts

;
PUNCH1              bsr       PUNCH
                    bra       POFC4

;
; FORMAT END OF TAPE WITH PGM. CTR. AND S9
PNCHS9              ldx       #$A049
                    stx       ENDA
                    dex
                    bsr       PUNCH2
                    ldx       #S9
PDAT                jsr       PDATA1
POFC4               bsr       PNCHOF
                    bsr       DELAY
C4                  jmp       CONTRL

;
RDON                com       PORECH              ; DISABLE ECHO FOR ACIA
                    lda       #$11                ; RON CHAR.
                    ldb       #$20                ; STROBE CHAR
                    bsr       STROBE
                    jsr       ISACIA              ; CHECK TO SEE IF PIA
                    beq       RTNN
                    lda       #$3C                ; DISABLE PIA ECHO IF PIA
                    sta       3,x
RTNN                rts

;
RDOFF               lda       #$13                ; TURN READER OFF
                    ldb       #$10
                    bra       STROBE

;
PNCHON              lda       #$12
                    ldb       #4
                    bra       STROBE

;
PNCHOF              lda       #$14
                    ldb       #$8
;
; PIA STROBING ROUTINE FOR PUNCH/READ ON/OFF
STROBE              jsr       OUTCH
                    jsr       GETPT1
;                   beq       RTN1
                    bra       RTN1

;
; New code for stack dump
;
PATCH1              jsr       OUT4HS              ; IXR
                    jsr       OUT4HS              ; IYR
                    jsr       OUT4HS              ; PGM COUNTER
                    jmp       PATCH2

;                   lda       #2
;                   orb       #1
;                   bsr       STR2
;                   bsr       STR1
;                   lda       #2
;                   ldb       #1
                    stb       0,x
                    bsr       STR2
STR1                lda       #6
STR2                sta       1,x
                    stb       0,x
RTN1                rts

; PUNCH FROM BEGINNING ADDRESS (BEGA) THRU
; ENDING ADDRESS (ENDA)
PUNCH               ldx       BEGA
PUNCH2              stx       TW
                    bsr       PNCHON
PUN11               lda       ENDA+1
                    suba      TW+1
                    ldb       ENDA
                    sbcb      TW
                    bne       PUN22
                    cmpa      #16
                    bcs       PUN23
PUN22               lda       #15
PUN23               adda      #4
                    sta       BYTECT
                    suba      #3
                    sta       TEMP
; PUNCH C/R L/F NULLS S1
                    ldx       #MTAPE1
                    jsr       PDATA1
                    clrb
; PUNCH FRAME COUNT
                    ldx       #BYTECT
                    bsr       PUNT2               ; PUNCH 2 HEX CHARACTERS
; PUNCH ADDRESS
                    ldx       #TW
                    bsr       PUNT2
                    bsr       PUNT2
; PUNCH DATA
                    ldx       TW
PUN32               bsr       PUNT2               ; PUNCH ONE BYTE
                    dec       TEMP
                    bne       PUN32
                    stx       TW
                    comb
                    pshb
                    tsx
                    bsr       PUNT2               ; PUNCH CHECKSUM
                    pulb                          ; RESTORE STACK
                    ldx       TW
                    dex
                    cpx       ENDA
                    bne       PUN11
RTN5                rts

; PUNCH 2 HEX CHAR, UPDATE CHECKSUM
PUNT2               addb      0,x
                    jmp       OUT2H               ; OUTPUT 2 HEX CHAR AND RTS

; COMMAND TABLE
TABLE               fcb       'G'                 ; GOTO
                    fdb       GOTO
                    fcb       'Z'                 ; GOTO PROM
                    fdb       PROM
                    fcb       'M'                 ; MEMORY EXAM AND CHANGE
                    fdb       CHANGE
                    fcb       'F'                 ; BYTE SEARCH
                    fdb       SEARCH
                    fcb       'R'                 ; REGISTER DUMP
                    fdb       PRINT
                    fcb       'J'                 ; JUMP
                    fdb       JUMP
                    fcb       'C'                 ; CLEAR SCREEN
                    fdb       CLEAR
                    fcb       'D'                 ; DISK BOOT
                    fdb       DISK
                    fcb       'B'                 ; BREAKPOINT
                    fdb       BREAK
                    fcb       'O'                 ; OPTIONAL PORT
                    fdb       OPTL
TABLE1              fcb       'P'                 ; ASCII PUNCH
                    fdb       PUNCH1
                    fcb       'L'                 ; ASCII LOAD
                    fdb       LOAD
TABEND              fcb       'E'                 ; END OF TAPE
                    fdb       PNCHS9

                    #VECTORS  $E3F8
                    fdb       IRQV                ; IRQ VECTOR
                    fdb       SFE                 ; SOFTWARE INTERRUPT
                    fdb       NMIV                ; NMI VECTOR
                    fdb       START               ; RESTART VECTOR

                    #VECTORS  $A048
                    fdb       START
                    end
