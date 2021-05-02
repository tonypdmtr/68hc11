;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; "  P711 - TEST   PROGRAM   for   68HC711K4              1.1.97"
; Ver 1.3b - LCD PC1602-ARS-PSD
; Optimized a bit by Tony Papadimitriou (99.08.07)

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
                    #ROM
;*******************************************************************************
                    org       $A000

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
_0@@                lda       #$38                ; Function Set : 8 bit, 2line, 5x7 dots
                    sta       DISPL
                    ldx       #5000
_1@@                nop
                    dex
                    bne       _1@@
                    dey
                    bne       _0@@

                    @delay    #8                  ; Function
                    @delay    #1
                    @delay    #6
                    @delay    #$0F                ; Function

Star                @delay    #1                  ; Clear Display (Cursor at HOME)
                    inca                          ; Cursor At Home
                    jsr       DELAY

                    ldx       #T1ST               ; PRINT TEST HEADER
                    bsr       TYP
                    @delay    #$C0                ; NEXT LINE OF LCD
                    ldx       #VER                ; PRINT VER
                    bsr       TYP
;                   bra       MainLoop

;*******************************************************************************
; RECEIVE the NUMBER of PROGRAM to RUN

MainLoop            proc
                    bsr       GETNUM
                    sta       CODE

                    @cbeqa    #1,SCI
                    @cjeqa    #2,DURT
                    @cjeqa    #3,ECHO
                    @cjeqa    #4,EPROM
                    @cjeqa    #5,SRAM
                    @cjeqa    #6,AtoD
                    @cjeqa    #7,LEDH
                    @cjeqa    #8,D2A
                    @cbnea    #9,_@@

                    @delay    #1                  ; Clear Display (Cursor at HOME)
                    inca                          ; Cursor At Home
                    jsr       DELAY
                    bra       MainLoop

_@@                 cmpa      #$0A
                    jeq       ClearLCD
                    cmpa      #$0B
                    bne       MainLoop
                    jmp       KEYBO

;*******************************************************************************

Delay               proc
                    pshx
                    ldx       #30000              ; DELAY
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

;*******************************************************************************

TYP                 proc
Loop@@              lda       ,x                  ; TYPE .TEXT
                    cmpa      #'$'                ; END OF STRING
                    beq       Done@@
                    jsr       DELAY1
                    inx
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************

SCI                 proc
                    ldx       #TABLE              ; DATA TABLE ADDRES
Loop@@              bsr       GETNUM
                    sta       ,x
                    inx                           ; NEXT ADDRES
                    tsta                          ; 0 - END OF DATA TABLE
                    bne       Loop@@              ; READ NEXT DATA
                    sta       ,x                  ; 0 AT THE END OF TABLE
                    bra       TRANS

;*******************************************************************************

GETNUM              proc
Loop@@              lda       SCSR1               ; READ STATUS REG.
                    anda      #$20
                    beq       Loop@@              ; JUMP IF RDRF equ $0
                    lda       SCDAT               ; READ DATA REG.
                    cmpa      #' '                ; SPACE SEND BY BASIC
                    beq       Loop@@
                    anda      #$0F                ; CONVERT TO BINARY
                    rts

;*******************************************************************************
; TRANSMIT RegA to PC by SCI

TRANS               proc
                    ldx       #TABLE              ; SEND to IBM_PC.
Loop@@              ldb       ,x
                    jeq       Star
                    jsr       PutChar
                    inx
                    bra       Loop@@

;*******************************************************************************
; ONUART

ECHO                proc
                    ldx       #DUART
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
                    lda       #5                  ; ENABLE TX and RCV
                    sta       2,x
ST                  jmp       MainLoop

;*******************************************************************************
; DUART

DURT                proc
                    ldx       #DUART
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
                    lda       #5                  ; ENABLE TX and RCV
                    sta       2,x

                    ldx       #TABLE
;                   bra       INUART

;*******************************************************************************
; INUART (PORT) - CHECK DUART for ANY INPUT

INUART              proc
                    ldy       #DUART
                    lda       1,y                 ; READ STATUS (SRA REG)
                    anda      #1                  ; CHECK RXRDY
                    beq       Go@@                ; JUMPING NO DATA
                    lda       3,y                 ; READ DATA - RBA
                    anda      #$7F                ; MASK PARITY
Go@@                tsta
                    beq       INUART
                    cmpa      #' '
                    beq       INUART
                    anda      #$0F
                    sta       ,x                  ; CHECK IF THER WAS AN INPUT
                    inx
                    tsta       EXIT IF GET 0 FROM P.C.
                    bne       INUART
                    sta       ,x                  ; 0 AT THE END OF TABLE

                    ldx       #TABLE              ; TRANSMIT THE DATA TO THE PC.
Loop@@              lda       ,x
                    beq       ST
                    ora       #$30
                    bsr       OUTURT
                    inx
                    bra       Loop@@

;*******************************************************************************
; OUTUART (PORT) - OUTPUT THE CHARACTER IN A.

OUTURT              proc
                    pshx
                    ldx       #DUART
Loop@@              ldb       1,x                 ; CHECK STATUS
                    andb      #4
                    beq       Loop@@              ; LOOP UNTIL TDRE =1
                    anda      #$7F                ; MASK PERITY
                    sta       3,x                 ; SEND CHARACTER
                    pulx
                    rts

;*******************************************************************************
; RAM TEST

SRAM                proc
                    ldx       #RAM
Loop@@              ldd       #$55AA
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
                    bne       Loop@@
                    lda       #1
                    bra       OK

;*******************************************************************************
; EPROM TEST

EPROM               proc
                    ldx       #EPRM
Loop@@              ldd       #$55AA
                    sta       ,x
                    jsr       Delay10msec
                    cmpa      ,x
                    bne       EROR
                    stb       ,x
                    jsr       Delay10msec
                    cmpb      ,x
                    bne       EROR
                    ldb       #$20
                    abx
                    cpx       #$5FD0
                    bls       Loop@@
                    lda       #1
                    bra       OK

EROR                lda       #2
OK                  sta       TABLE
                    clr       TABLE+1
                    jmp       TRANS

;*******************************************************************************

AtoD                proc
                    lda       #$90                ; INIT A to D
                    sta       OPTION
                    ldy       #10
Sample@@            clr       ADCTL               ; start of conversion.
_@@                 lda       ADCTL
                    bpl       _@@                 ; CCF equ $1 ; Conversion Complete Flag.
                    ldb       ADR1
          ;-------------------------------------- ; TRANSMIT  A  reg. to PC  by  SCI
                    andb      #$0F                ; LOW BYTE
                    cmpb      #9
                    ble       NUM
                    subb      #9
                    orb       #$40
                    bsr       WaitTDRE
                    bra       ReadAD@@
          ;--------------------------------------
NUM                 bsr       PutChar
ReadAD@@            ldb       ADR1
                    lsrb:4                        ; HIGH BYTE
                    cmpb      #9
                    ble       NUML
                    subb      #9
                    orb       #$40
                    bsr       WaitTDRE
                    bra       Delay@@

NUML                bsr       PutChar

Delay@@             ldx       #50
DLoop@@             bsr       Delay10msec
                    dex
                    bne       DLoop@@
                    dey
                    bne       Sample@@
                    lda       #$10                ; STOP A to D
                    sta       OPTION
                    jmp       MainLoop

;*******************************************************************************

PutChar             proc
                    orb       #$30
Loop@@              lda       SCSR1               ; LOOP TILL TDRE=1
                    bpl       Loop@@
                    stb       SCDAT               ; send
                    rts

WaitTDRE            equ       Loop@@

;*******************************************************************************

D2A                 proc
                    lda       #$FF
                    ldy       #4
Loop@@              sta       DtoA
                    ldx       #200
_@@                 bsr       Delay10msec
                    dex
                    bne       _@@
                    suba      #64
                    dey
                    bne       Loop@@
                    clr       DtoA
                    jmp       MainLoop

;*******************************************************************************
; LED PORT H

LEDH                proc
                    ldd       #$FFE               ; 0 - 3 -> OUT ; 4 - 7 <- IN
                    sta       DDRH
                    stb       PORTH               ; LED PH0 -> ON
                    ldx       #100
_0@@                bsr       Delay10msec
                    dex
                    bne       _0@@
                    lda       #$FD                ; LED PH1 -> ON
                    sta       PORTH
                    ldx       #100
_1@@                bsr       Delay10msec
                    dex
                    bne       _1@@
                    lda       #$FB                ; LED PH2 -> ON
                    sta       PORTH
                    ldx       #100
_2@@                bsr       Delay10msec
                    dex
                    bne       _2@@
                    lda       #$F7                ; LED PH3 -> ON
                    sta       PORTH
                    ldx       #100
_3@@                bsr       Delay10msec
                    dex
                    bne       _3@@
                    ldd       #$FF00              ; ALL LED ARE OFF
                    sta       PORTH
                    stb       TABLE+1
                    jmp       TRANS

;*******************************************************************************

Delay10msec         proc
                    pshd
                    lda       #$80
                    sta       TFLG1
                    ldd       TCNT
                    addd      #20000              ; D+ 2000 -> D
                    std       TOC1                ; D -> TOC1
Loop@@              lda       TFLG1
                    bpl       Loop@@
                    sta       TFLG1
                    puld
                    rts

;*******************************************************************************

ClearLCD            proc
                    lda       #1                  ; Clear Display (Cursor at HOME)
                    bsr       DELAY
                    inca                          ; Cursor At Home
                    bsr       DELAY
                    ldx       #ABC                ; A to P
                    jsr       TYP
                    lda       #$C0                ; NEXT LINE
                    bsr       DELAY
                    inx
                    jsr       TYP                 ; Q to Z , 1 to 6
                    jmp       MainLoop

;*******************************************************************************

DELAY               proc
                    sta       DISPL
                    pshx
                    ldx       #DELAY@@
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

DELAY@@             equ       3280                ; 1.64 mS DELAY

;*******************************************************************************

DELAY1              proc
                    sta       DISPL1
                    pshx
                    ldx       #80                 ; 200 uS DELAY
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

;*******************************************************************************
; KEYBORD

KEYBO               proc
                    lda       #1                  ; Clear Display (Cursor at HOME)
                    bsr       DELAY
                    inca                          ; Cursor At Home
                    bsr       DELAY

                    ldx       #KEYB               ; TYPE '  KEYBOARD'
                    jsr       TYP
                    clr       TABLE+1             ; COUNTER OF PRESSING KEY
Loop@@              cli
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
                    ble       Loop@@
                    ldd       #$FF00              ; ALL LED ARE OFF
                    sta       PORTH
                    stb       TABLE+1
                    jmp       TRANS

;*******************************************************************************

IDEL                proc
                    sei
                    pshx
                    ldx       #DELAY@@
Loop@@              dex
                    bne       Loop@@
                    pulx
                    cli
                    rts

DELAY@@             equ       20000

;*******************************************************************************

T1ST                fcc       ' TEST 68HC711K4$'
VER                 fcc       ' Ver 1.3b $'
ABC                 fcc       'ABCDEFGHIJKLMNOP$QRSTUVWXYZ123456$'
KEYB                fcc       '  KEYBOARD TEST$'

;*******************************************************************************
; "  K711 - PROGRAM TO BE OPERATED BY INTERRUPT           25.11.96"
; VER 1.3b
                    org       $A600

intkey              proc
                    sei
                    lda       PORTD               ; SAMPLE PORTD D
                    clrb
                    anda      #$3C                ; 00111100B ; PD2 - PD5
                    cmpa      #$38                ; PD2 = 0
                    beq       _8@@
                    cmpa      #$34                ; PD3 = 0
                    bne       _2@@
                    ldb       TABLE
                    cmpb      #$43                ; COLUMN OF "C"
                    bne       _1@@
                    lda       #'D'
                    bra       _9@@

_1@@                ldb       #3
                    bra       _8@@

_2@@                cmpa      #$2C                ; PD4 = 0
                    bne       _4@@
                    ldb       TABLE
                    cmpb      #$43                ; COLUMN OF "C"
                    bne       _3@@
                    lda       #'E'
                    bra       _9@@

_3@@                ldb       #6
                    bra       _8@@

_4@@                cmpa      #$1C                ; PD5 = 0
                    bne       Fail@@
                    ldb       TABLE
                    cmpb      #$31                ; FIRST COLUMN
                    bne       _5@@
                    lda       #'A'                ; "A"
                    bra       _9@@

_5@@                cmpb      #$32                ; SECOND COLUMN
                    bne       _6@@
                    lda       #'0'                ; "0"
                    bra       _9@@

_6@@                cmpb      #$33                ; THIRED COLUMN
                    bne       _7@@
                    lda       #'B'                ; "B"
                    bra       _9@@

_7@@                cmpb      #$43                ; FOURTH COLUMN
                    bne       _8@@
                    lda       #'F'
                    bra       _9@@

Fail@@              lda       #'J'                ; JUNK
                    bra       _9@@

_8@@                lda       TABLE
                    aba
_9@@                sta       TABLE

                    inc       TABLE+1             ; = COUNTER
                    lda       #2
                    sta       DISPL
                    jsr       Delay
                    lda       #' '
                    sta       DISPL1
                    jsr       Delay
                    lda       #2
                    sta       DISPL
                    jsr       Delay
                    lda       TABLE
                    sta       DISPL1
                    jsr       Delay
                    rti
