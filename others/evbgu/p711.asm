;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; "  P711 - TEST   PROGRAM   for   68HC711K4              1.1.97"
; Ver 1.3b
; Optimized a bit by Tony Papadimitriou (99.08.07)

                    #Macro
                    #CaseOn

PORTD               equ       $08                 ; Port D data
DDRD                equ       $09                 ; Data Direction Register for Port D
TCNT                equ       $0E                 ; Timer Counter
TOC1                equ       $16                 ; : Timer Output Compare
TFLG1               equ       $23                 ; Timer interrupt Flag 1
ADCTL               equ       $30                 ; A/C
ADR1                equ       $31                 ; RESULT A/D REG 1
ADR2                equ       $32                 ; RESULT A/D REG 2
ADR3                equ       $33                 ; RESULT A/D REG 3
ADR4                equ       $34                 ; RESULT A/D REG 4
OPTION              equ       $39
CSCTL               equ       $5B                 ; CSCTL
GPCS1A              equ       $5C                 ; CSGPA1
GPCS1C              equ       $5D
GPCS2A              equ       $5E                 ; CSGPA2
GPCS2C              equ       $5F
BAUD                equ       $70                 ; SCI Baud Rate Control
SCCR1               equ       $72                 ; SCI Control Register 1
SCCR2               equ       $73                 ; SCI Control Register 2
SCSR1               equ       $74                 ; SCI Status Register
SCDAT               equ       $77                 ; SCI Data (Read RDR, Write TDR)
PORTH               equ       $7C                 ; Port H data
DDRH                equ       $7D                 ; Data Direction Register for Port H
CODE                equ       $80                 ; CODE FOR PROGRAM OPERATION (FROM IBM-PC)
TABLE               equ       $82                 ; DATA TABLE TO USE WHITE IBM-PC
VIRQ                equ       $0365               ; INT. VECTOR - IRQ (from BUFFALO JUMP VECTOR)
EPRM                equ       $1000               ; EPROM ADDRESS
RAM                 equ       $6000               ; RAM ADDRES
DtoA                equ       $8000               ; D to A ADDRES
DUART               equ       $8100               ; DUART ADDRES
DISPL               equ       $8200               ; DISPLAY PORT
DISPL1              equ       $8201               ; DISPLAY PORT

;*******************************************************************************
; Macros

Delay               macro     parm
                    lda       ~1~
                    jsr       DELAY
                    endm

;-------------------------------------------------------------------------------

cbeqa               macro     Value,Address
          #ifparm ~2~ = *
                    mset      2,{*}
          #endif
                    cmpa      ~1~
                    beq       ~2~
                    endm

;-------------------------------------------------------------------------------

cbnea               macro     Value,Address
          #ifparm ~2~ = *
                    mset      2,{*}
          #endif
                    cmpa      ~1~
                    bne       ~2~
                    endm

;-------------------------------------------------------------------------------

cjeqa               macro     Value,Address
          #ifparm ~2~ = *
                    mset      2,{*}
          #endif
                    cmpa      ~1~
                    jeq       ~2~
                    endm

;*******************************************************************************
                    #ROM      $A000
;*******************************************************************************

Start               proc
                    lds       #$0300              ; INIT. STACK
                    ldd       #$0B00              ; INITIAL CHIP SELECTS
                    sta       CSCTL               ; 0B
                    stb       GPCS1A              ; /CS equ $CSGP1
                    ldd       #$0510              ; RANGE 0000-7FFFH
                    sta       GPCS1C              ; /CS equ $CSGP2
                    stb       GPCS2A              ; RANGE 8000H-87FF
                    lda       #1
                    sta       GPCS2C
                    inca
                    sta       DDRD                ; PD2 - PD5 =INPUT; PD1 =OUT, PD0 =IN
                    ldd       #$FFF
                    sta       DDRH                ; PORTH
                    stb       PORTH               ; ALL LED ARE OFF
                    sei
                    lda       #$7E                ; JMP CODE
                    sta       VIRQ
                    ldd       #intkey             ; INTERRUPT VECTOR (EXTERNAL)
                    std       VIRQ+1

          ; RECEIVE DATA in A reg.  FROM IBM PC        : INIT  SCI
          ; 8 DATA BITS, NO PARITY, 16MHz CRYSTAL CLOCK.

                    ldd       #$1A                ; BAUD REG. equ $9600
                    std       BAUD
                    ldd       #$0C
                    std       SCCR1               ; RECEIVE 8 DATA BITS, 1 START BIT, 1 STOP BIT

                    ldy       #4                  ; INIT. LCD
D0                  lda       #$38                ; Function Set : 8 bit, 2line, 5x7 dots
                    sta       DISPL
                    ldx       #5000
D1                  nop
                    dex
                    bne       D1
                    dey
                    bne       D0

                    delay     #8                  ; Function
                    delay     #1
                    delay     #6
                    delay     #$0F                ; Function
STAR                delay     #1                  ; Clear Display (Cursor at HOME)
                    inca                          ; Cursor At Home
                    jsr       DELAY

                    ldx       #T1ST               ; PRINT TEST HEADER
                    bsr       TYP
                    delay     #$C0                ; NEXT LINE OF LCD
                    ldx       #VER                ; PRINT VER
                    bsr       TYP

; RECEIVE the NUMBER of PROGRAM to RUN

START               bsr       GETNUM
                    sta       CODE

                    cbeqa     #1,SCI
                    cjeqa     #2,DURT
                    cjeqa     #3,ECHO
                    cjeqa     #4,EPROM
                    cjeqa     #5,SRAM
                    cjeqa     #6,AtoD
                    cjeqa     #7,LEDH
                    cjeqa     #8,D2A
                    cbnea     #9,XA

                    delay     #1                  ; Clear Display (Cursor at HOME)
                    inca                          ; Cursor At Home
                    jsr       DELAY
                    bra       START

XA                  cmpa      #$0A
                    jeq       DISP
                    cmpa      #$0B
                    bne       START
                    jmp       KEYBO

DELY                proc
                    pshx
                    ldx       #30000              ; DELAY
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

;*******************************************************************************

TYP                 lda       ,x                  ; TYPE .TEXT
                    cmpa      #'$'                ; END OF STRING
                    beq       :AnRTS
                    jsr       DELAY1
                    inx
                    bra       TYP

; =================    SCI      =========================

SCI                 ldx       #TABLE              ; DATA TABLE ADDRES
NEXT                bsr       GETNUM
                    sta       ,x
                    inx                           ; NEXT ADDRES
                    tsta                          ; 0 - END OF DATA TABLE
                    bne       NEXT                ; READ NEXT DATA
                    sta       ,x                  ; 0 AT THE END OF TABLE
                    bra       TRANS

GETNUM
GET                 lda       SCSR1               ; READ STATUS REG.
                    anda      #$20
                    beq       GET                 ; JUMP IF RDRF equ $0
                    lda       SCDAT               ; READ DATA REG.
                    cmpa      #' '                ; SPACE SEND BY BASIC
                    beq       GET
                    anda      #$0F                ; CONVERT TO BINARY
                    rts

; TRANS                                            ; TRANSMIT  A  reg. to PC  by  SCI

TRANS               ldx       #TABLE              ; SEND to IBM_PC.
ZERO                ldb       ,x
                    jeq       STAR
                    orb       #$30
TRN                 lda       SCSR1               ; LOOP TILL TDRE=1
                    bpl       TRN
                    stb       SCDAT
                    inx
                    bra       ZERO

; ===================    ONUART     =====================

ECHO                ldx       #DUART
                    ldd       #$2238              ; RSET RECEIVER
                    sta       2,x                 ; $8102 equ $CRA REG
                    stb       2,x                 ; RESET TRANSMIT CRA
                    ldd       #$4010
                    sta       2,x                 ; RESET ERROR STATUS CRA
                    stb       2,x                 ; RESET POINTER
                    clrd
                    std       DUART+4             ; CLOCK SOURCE ACR REG, INTURUPT MUSK IMR
                    ldd       #$13C7              ; C7 equ $REMOTE LOOP
                    sta       ,x                  ; 8 DATA, NO PERITY MR1A
                    stb       ,x                  ; 1 STOP BIT MR2A
                    lda       #$BB                ; BAUD RATE equ $9600
                    sta       1,x                 ; TX and RCV BOUD RATE CSRA
                    lda       #05                 ; ENABLE TX and RCV
                    sta       2,x
ST                  jmp       START

; ==================   DUART    =========================

DURT                ldx       #DUART
                    ldd       #$2238              ; RSET RECEIVER
                    sta       2,x                 ; $8102 equ $CRA REG
                    stb       2,x                 ; RESET TRANSMIT CRA
                    ldd       #$4010
                    sta       2,x                 ; RESET ERROR STATUS CRA
                    stb       2,x                 ; RESET POINTER
                    clrd
                    std       DUART+4             ; CLOCK SOURCE ACR REG, INTURUPT MUSK IMR
                    ldd       #$1307
                    sta       ,x                  ; 8 DATA, NO PERITY MR1A
                    stb       ,x                  ; 1 STOP BIT MR2A
                    lda       #$BB                ; BOUD RATE equ $9600
                    sta       1,x                 ; TX and RCV BOUD RATE CSRA
                    lda       #05                 ; ENABLE TX and RCV
                    sta       2,x

                    ldx       #TABLE

; INUART (PORT) - CHECK DUART for ANY INPUT

INUART              ldy       #DUART
                    lda       1,Y                 ; READ STATUS (SRA REG)
                    anda      #01                 ; CHECK RXRDY
                    beq       INURT0              ; JUMPING NO DATA
                    lda       3,Y                 ; READ DATA - RBA
                    anda      #$7F                ; MASK PARITY
INURT0              tsta
                    beq       INUART
                    cmpa      #$20
                    beq       INUART
                    anda      #$0F
                    sta       ,x                  ; CHECK IF THER WAS AN INPUT
                    inx
                    tsta       EXIT IF GET 0 FROM P.C.
                    bne       INUART
                    sta       ,x                  ; 0 AT THE END OF TABLE

                    ldx       #TABLE              ; TRANSMIT THE DATA TO THE PC.
LD2                 lda       ,x
                    beq       ST
                    ora       #$30
                    bsr       OUTURT
                    inx
                    bra       LD2

; OUTUART (PORT) -     OUTPUT the CHARACTER IN A.

OUTURT              pshx
                    ldx       #DUART
OUTU2               ldb       1,x                 ; CHECK STATUS
                    andb      #4
                    beq       OUTU2               ; LOOP UNTIL TDRE =1
                    anda      #$7F                ; MASK PERITY
                    sta       3,x                 ; SEND CHARACTER
                    pulx
                    rts

; ==================   RAM  TEST   ======================

SRAM                ldx       #RAM
SRM                 ldd       #$55AA
                    sta       ,x
                    stb       TABLE+1             ; DELAY FOR REMOVING PREVIOUS DATA.
                    cmpa      ,x
                    bne       EROR
                    stb       ,x
                    sta       TABLE+1             ; DELAY FOR REMOVING PREVIOUS DATA.
                    cmpb      ,x
                    bne       EROR
                    inx
                    cpx       #$8000
                    bne       SRM
                    lda       #1
                    bra       OK

; ================    EPROM  TEST    ====================

EPROM               ldx       #EPRM
EROM                ldd       #$55AA
                    sta       ,x
                    jsr       D10MS               ; DELAY OF 10 Msec
                    cmpa      ,x
                    bne       EROR
                    stb       ,x
                    jsr       D10MS               ; DELAY OF 10 Msec
                    cmpb      ,x
                    bne       EROR
                    ldb       #$20
                    abx
                    cpx       #$5FD0
                    bls       EROM
                    lda       #1
                    bra       OK

EROR                lda       #2
OK                  sta       TABLE
                    clr       TABLE+1
                    jmp       TRANS

; ==================    A to D    =======================

AtoD                lda       #$90                ; INIT A to D
                    sta       OPTION
                    ldy       #10
SAMP                clr       ADCTL               ; start of conversion.
WAIT                lda       ADCTL
                    bpl       WAIT                ; CCF equ $1 ; Conversion Complete Flag.
                    ldb       ADR1

          ; TRANSMIT  A  reg. to PC  by  SCI

                    andb      #$0F                ; LOW BYTE
                    cmpb      #9
                    ble       NUM
                    subb      #9
                    orb       #$40
                    bra       TRAL

NUM                 orb       #$30
TRAL                lda       SCSR1               ; LOOP TILL TDRE=1
                    bpl       TRAL
                    stb       SCDAT               ; send
                    ldb       ADR1
                    lsrb:4                        ; HIGH BYTE
                    cmpb      #9
                    ble       NUML
                    subb      #9
                    orb       #$40
                    bra       TRAH

NUML                orb       #$30
TRAH                lda       SCSR1               ; LOOP TILL TDRE=1
                    bpl       TRAH
                    stb       SCDAT               ; send

                    ldx       #50
DL                  bsr       D10MS
                    dex
                    bne       DL
                    dey
                    bne       SAMP
                    lda       #$10                ; STOP A to D
                    sta       OPTION
                    jmp       START

; ==================    D to A    =======================

D2A                 lda       #$FF
                    ldy       #4
DD2                 sta       DtoA
                    ldx       #200
DDL                 bsr       D10MS               ; DELAY
                    dex
                    bne       DDL
                    suba      #$40
                    dey
                    bne       DD2
                    clr       DtoA
                    jmp       START

; ==================   LED PORT H    ====================

LEDH                ldd       #$FFE               ; 0 - 3 -> OUT ; 4 - 7 <- IN
                    sta       DDRH
                    stb       PORTH               ; LED PH0 -> ON
                    ldx       #100
PH0                 bsr       D10MS
                    dex
                    bne       PH0
                    lda       #$FD                ; LED PH1 -> ON
                    sta       PORTH
                    ldx       #100
PH1                 bsr       D10MS
                    dex
                    bne       PH1
                    lda       #$FB                ; LED PH2 -> ON
                    sta       PORTH
                    ldx       #100
PH2                 bsr       D10MS
                    dex
                    bne       PH2
                    lda       #$F7                ; LED PH3 -> ON
                    sta       PORTH
                    ldx       #100
PH3                 bsr       D10MS
                    dex
                    bne       PH3
                    ldd       #$FF00              ; ALL LED ARE OFF
                    sta       PORTH
                    stb       TABLE+1
                    jmp       TRANS

; DELAY 10 Msec
D10MS               pshd
                    lda       #$80
                    sta       TFLG1
                    ldd       TCNT
                    addd      #20000              ; D+ 2000 -> D
                    std       TOC1                ; D -> TOC1
DE10                lda       TFLG1
                    bpl       DE10
                    sta       TFLG1
                    puld
                    rts

; ================   LCD PC1602-ARS-PSD   ===============

; CLEAR LCD DISPLAY

DISP                lda       #1                  ; Clear Display (Cursor at HOME)
                    bsr       DELAY
                    inca                          ; Cursor At Home
                    bsr       DELAY
                    ldx       #ABC                ; A to P
                    jsr       TYP
                    lda       #$C0                ; NEXT LINE
                    bsr       DELAY
                    inx
                    jsr       TYP                 ; Q to Z , 1 to 6
                    jmp       START

DELAY               proc
                    sta       DISPL
                    pshx
                    ldx       #3280               ; 1.64 mS DELAY
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

DELAY1              proc
                    sta       DISPL1
                    pshx
                    ldx       #0080               ; 200 uS DELAY
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

; ===================   KEYBORD     =====================

KEYBO               lda       #1                  ; Clear Display (Cursor at HOME)
                    bsr       DELAY
                    inca                          ; Cursor At Home
                    bsr       DELAY

                    ldx       #KEYB               ; TYPE '  KEYBOARD'
                    jsr       TYP
                    clr       TABLE+1             ; COUNTER OF PRESSING KEY
LL                  cli
                    ldd       #$31FE              ; PH0 equ $0 :1, 4, 7, A
                    stb       PORTH
                    sta       TABLE               ; '1'
                    bsr       IDEL
                    ldd       #$32FD              ; PH1 equ $0 :2, 5, 8, 0
                    stb       PORTH
                    sta       TABLE               ; '2'
                    bsr       IDEL
                    ldd       #$33FB              ; PH2 equ $0 :3, 6, 9, B
                    stb       PORTH
                    sta       TABLE               ; '3'
                    bsr       IDEL
                    ldd       #$43F7              ; PH4 equ $0 :C, D, E, F
                    stb       PORTH
                    sta       TABLE               ; 'C'
                    bsr       IDEL
                    sei
                    lda       TABLE+1
                    cmpa      #8
                    ble       LL
                    ldd       #$FF00              ; ALL LED ARE OFF
                    sta       PORTH
                    stb       TABLE+1
                    jmp       TRANS

IDEL                sei
                    pshx
                    ldx       #20000              ; DELAY
LLX                 dex
                    bne       LLX
                    pulx
                    cli
                    rts

T1ST                fcc       ' TEST 68HC711K4$'
VER                 fcc       ' Ver 1.3b $'
ABC                 fcc       'ABCDEFGHIJKLMNOP$QRSTUVWXYZ123456$'
KEYB                fcc       '  KEYBOARD TEST$'

; "  K711 - PROGRAM TO BE OPERATED BY INTERRUPT           25.11.96"
; VER 1.3b
                    org       $A600

intkey              sei
                    lda       PORTD               ; SAMPLE PORTD D
                    clrb
                    anda      #$3C                ; 00111100B ; PD2 - PD5
                    cmpa      #$38                ; PD2 = 0
                    beq       SOF
H1                  cmpa      #$34                ; PD3 = 0
                    bne       H2
                    ldb       TABLE
                    cmpb      #$43                ; COLUMN OF "C"
                    bne       H12
                    lda       #'D'
                    bra       SFF

H12                 ldb       #3
                    bra       SOF

H2                  cmpa      #$2C                ; PD4 = 0
                    bne       H3
                    ldb       TABLE
                    cmpb      #$43                ; COLUMN OF "C"
                    bne       H22
                    lda       #'E'
                    bra       SFF

H22                 ldb       #6
                    bra       SOF

H3                  cmpa      #$1C                ; PD5 = 0
                    bne       ERR
                    ldb       TABLE
                    cmpb      #$31                ; FIRST COLUMN
                    bne       N2
                    lda       #'A'                ; "A"
                    bra       SFF

N2                  cmpb      #$32                ; SECOND COLUMN
                    bne       N3
                    lda       #'0'                ; "0"
                    bra       SFF

N3                  cmpb      #$33                ; THIRED COLUMN
                    bne       N4
                    lda       #'B'                ; "B"
                    bra       SFF

N4                  cmpb      #$43                ; FOURTH COLUMN
                    bne       SOF
                    lda       #'F'
                    bra       SFF

ERR                 lda       #'J'                ; JUNK
                    bra       SFF

SOF                 lda       TABLE
                    aba
SFF                 sta       TABLE

                    inc       TABLE+1             ; = COUNTER
                    lda       #02
                    sta       DISPL
                    jsr       DELY
                    lda       #$20
                    sta       DISPL1
                    jsr       DELY
                    lda       #02
                    sta       DISPL
                    jsr       DELY
                    lda       TABLE
                    sta       DISPL1
                    jsr       DELY
                    rti

                    end       :s19crc
