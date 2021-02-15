;*******************************************************************************
;                      MC68HC11E32 RDS multiband radio
;
;                 Used with RDSE.S11, FNCE.S11 & RDRAME.S11
;
;               P. Topping                      3rd April '94
;*******************************************************************************

?                   macro
          #ifb ~1~
                    mexit
          #endif
~1~                 def       *
                    mdel      1
                    mtop
                    endm

                    @?        SDATA,TINTB,INITD,MOD,CLOCK,MJDAT,WAIT,CLREON,CBCD,PROC,TFCC,ADB,IFO
                    #Export    DCON2,NEW,CLTR,SHAFT
; LIB       RDRAME.S11

PORTA               equ       $00                 ; PORT A ADDRESS
PORTB               equ       $04                 ; "    B  "
PORTC               equ       $03                 ; "    C  "
PORTD               equ       $08                 ; "    D  "
PORTE               equ       $0A                 ; "    E  "
PORTCD              equ       $07                 ; PORT C DATA DIRECTION REG.
PORTDD              equ       $09                 ; "    D  " "        "
TMSK2               equ       $24
PACTL               equ       $26
OPTION              equ       $39
INIT                equ       $3D

RBO                 equ       $1000               ; REGISTER BLOCK OFFSET
PPROG               equ       $3B                 ; EEPROM CONTROL REGISTER
ND                  equ       9                   ; No. DIGITS

;*******************************************************************************
                    #RAM      $0000               ;SECTION.S .RAM1,COMM
;*******************************************************************************

BMJD                rmb       3                   ; BINARY MJD
Q                   rmb       9                   ; WORKING NUMBER 1 - RDS
TMQ                 rmb       9                   ; SCRATCH
                    rmb       9                   ; WORKING NUMBER 2 - RDS
TMP                 rmb       9                   ; MULT. OVER. OR DIV. REMAINDER
                    rmb       9                   ; WORKING NUMBER 3 - RDS
MJD                 rmb       9                   ; MODIFIED JULIAN DAY NUMBER
YR                  rmb       9                   ; YEAR
MNTH                rmb       2                   ; MONTH
DOM                 rmb       2                   ; DATE
DOW                 rmb       1                   ; DAY OF WEEK

;*******************************************************************************
; RAM allocation, RDS & radio
;*******************************************************************************

DIST                rmb       1                   ; TRANSIENT DISPLAY, TIMEOUT,COUNTER
SLEPT               rmb       1                   ; SLEEP TIMER MINUTES COUNTER
RDSTO               rmb       1                   ; RDS TIMEOUT COUNTER
PSNP                rmb       1                   ; PS DISPLAY POINTER
DAT                 rmb       4                   ; SERIAL DATA BUFFER
TMPGRP              rmb       8                   ; TEMPORARY GROUP DATA
GROUP               rmb       8                   ; COMPLETE GROUP DATA
PTY                 rmb       1                   ; PROGRAM-TYPE CODE (CURRENT)
PTYCMP              rmb       1                   ; PROGRAM TYPE CODE (PTY SCAN)
PI                  rmb       2                   ; PROGRAM IDENTIFICATION CODE
PION                rmb       2                   ; PROGRAM IDENTIFICATION CODE (EON)
PIN                 rmb       2                   ; PROGRAM ITEM NUMBER
LEV                 rmb       1                   ; VALID BLOCK LEVEL
BIT                 rmb       1                   ; BIT LEVEL
ITMP1               rmb       1                   ; TEMP BYTE FOR USE IN IRQ
SYN                 rmb       2                   ; SYNDROME
CONF                rmb       1                   ; SYNDROME CONFIDENCE
TH32                rmb       1                   ; TICS (SECONDS/32)
TH8                 rmb       1                   ; EIGHTHS OF SECONDS
SEC                 rmb       1                   ; SECONDS
MIN                 rmb       1                   ; MINUTES
OUR                 rmb       1                   ; HOURS
AMIN                rmb       1                   ; ALARM MINUTES
AOUR                rmb       1                   ; ALARM HOURS
DISP1               rmb       1                   ; RT DISPLAY POINTER #1
DISP2               rmb       1                   ; RT DISPLAY POINTER #2
                                                  ; 6
RQ                  rmb       6                   ; WORKING BCD NUMBER 1 RADIO
RP                  rmb       6                   ; "        " "    2  "
RR                  rmb       2                   ; "        " "    3  "
W1                  rmb       2                   ; W
W2                  rmb       2                   ; O
W3                  rmb       2                   ; R
W4                  rmb       2                   ; K
W5                  rmb       2                   ; I
W6                  rmb       2                   ; N
W7                  rmb       2                   ; G
KEY                 rmb       1                   ; CODE OF PRESSED KEY
KOUNT               rmb       1                   ; KEYBOARD COUNTER
DIG2                rmb       1                   ; 2nd DIGIT TIMEOUT COUNTER
CARRY               rmb       1                   ; BCD CARRY
COUNT               rmb       1                   ; LOOP COUNTER
NUM1                rmb       2                   ; 1ST No. POINTER (ADD & SUBTRACT)
NUM2                rmb       2                   ; 2ND No. POINTER (ADD & SUBTRACT)
LED                 rmb       1                   ; STATION NUMBER
SMEM                rmb       2                   ; CURRENT FREQUENCY
REARET              rmb       1                   ; LAST TA REASON FOR RETURN
RTDIS               rmb       1                   ; RDS DISPLAY TYPE
DI                  rmb       1                   ; DECODER IDENTIFICATION
SCHAN               rmb       1                   ; SCAN CHANNEL

;*******************************************************************************
; Flags, & pages 1-2
;*******************************************************************************

STAT                rmb       1                   ; 0: MODE 1: STATION, 0: FREQ
;                                               1: STEP 1: 50KHz, 0: 10KHz
;                                               2: CLRQ 1: CLEAR IF NO. KEYED
;                                               3: TIMER MS BIT TOGGLE (64 Hz)
;                                               4: RDS DATA CLEARING ENABLE
;                                               5: KEY FUNCTION PERFORMED
;                                               6: KEY REPEATING
;                                               7: NOT JUST POWERED UP
STAT2               rmb       1                   ; 0: VALID SYNDROME
;                                               1: VALID GROUP
;                                               2: RT DISPLAY
;                                               3: UPDATE DISPLAY
;                                               4: CLEAR DISPLAY
;                                               5: SPACE FLAG
;                                               6: NOT ON PROGRAM (AM)
;                                               7: TA RETUNE DONE
STAT3               rmb       1                   ; 0: NOT ON PROGRAM (FM)
;                                               1: TEXTA/TEXTB BIT (RT)
;                                               2: TA FLAG
;                                               3: TP FLAG
;                                               4: SHAFT DIRECTION
;                                               5: SHAFT ROTATION
;                                               6: UPDATE DATE
;                                               7: SHAFT INTERRUPTS
STAT4               rmb       1                   ; 0: DISPLAY (OR TA SWITCH) TRANSIENT
;                                               1: SLEEP TIMER RUNNING
;                                               2: TRAFFIC ENABLED
;                                               3: ALARM DISPLAY
;                                               4: ALARM ARMED
;                                               5: ALARM SET-UP
;                                               6: ALARM HOURS (SET-UP)
;                                               7: VALID GROUP 14B RECEIVED
STAT5               rmb       1                   ; 0: BAND CHANGE TIMEOUT
;                                               1: RDS DISPLAYS
;                                               2: SLEEP DISPLAY
;                                               3: M/S 0: M, 1: S
;                                                  4: RETUNE FLAG (FREQUENCY MODE)
;                                                  5: TA INHIBIT FLAG (NVM)
;                                                  6: STORE MODE
;                                                  7: WEEKDAY ONLY ALARM

STAT6               rmb       1                   ; BAND/BANK (,MW STEP,COLON, ,A1,A0,,E6)
BCTO                rmb       1                   ; BAND CHANGE TIMEOUT
SCNT                rmb       1                   ; SHAFT DETENT COUNTER

                    #RAM      $0100               ; SECTION .RAM2,COMM

EON                 rmb       256

                    #RAM      $0200               ; SECTION .RAM3,COMM EON DATA (16 NETWORKS)

DISP                rmb       16                  ; LCD MODULE BUFFER
DISPP               rmb       16                  ; CURRENT LCD MODULE CONTENTS
PSN                 rmb       8
RT                  rmb       69                  ; RADIOTEXT

                    #ROM      $D000               ; SECTION .ROM1

STRST               !jmp      Start               ; RESET VECTOR
TMRB                !jmp      TINTB               ; RTI
IRQ                 !jmp      SDATA               ; IRQ

;*******************************************************************************
; Reset routine - set-up ports etc
;*******************************************************************************

Start               proc
                    lda       #$01
                    sta       INIT                ; REGISTERS AT $1000
                    lda       #$10                ; ENABLE EEPROM WRITE (NOT CONFIG)
                    sta       $1035

                    lda       #$30                ; IRQ EDGE SENSITIVE
                    sta       $1039
                    lda       #$03                ; 32Hz RTI (8.388MHz XTAL)
                    sta       $1026               ; PORTA, BITS 3 & 7 INPUTS
                    lda       #$40                ; ENABLE REAL TIME INTERRUPTS
                    sta       $1024
                    lda       #$00                ; DWOM = 0, PORTD PUSH-PULL
                    sta       $1028

                    lds       #$02FF              ; INITIALISE STACK POINTER

                    ldy       #$1000              ; 0,1: BAND INPUTS (FM, FM, MW, SW), 2: FM IF
                    lda       #$10                ; 3: IRQ CONTROL, 4: MUTE, 5: TA=TP=1
                    sta       PORTA,Y             ; 6: M/S=1, 7: 8/16 LCD MUX

                    lda       #0                  ; 0,1: SERIAL CLOCK/DATA, 5,6,7: LCD CONTROL
                    sta       PORTB,Y             ; 2,3,4: LATCH SIGNALS (VFD, 5157 & 5170)

                    clr       PORTC,Y
                    lda       #$FF                ; 0-7: LCD PARALLEL BUS
                    sta       PORTCD,Y

                    clr       PORTD,Y             ; 0,1: SCI (DEBUG)
                    lda       #$3C                ; 2-4: KEYBOARD OUTPUTS
                    sta       PORTDD,Y            ; 5: STANDBY

; PORTE 0-3:   KEYBOARD INPUTS, 4: SHAFT INPUT (XIRQ)
;     "   5:     RDS/SHAFT INPUT, 6: SW BANK, 7: LCD/VFD

;*******************************************************************************
; INITIALISE LCD AND RAM
;*******************************************************************************

                    jsr       DBOUNC              ; WAIT 15ms
                    lda       #$30
                    bsr       CLOCK               ; INITIALISE LCD
                    jsr       DBOUNC              ; WAIT 15ms
                    lda       #$30
                    bsr       CLOCK               ; INITIALISE LCD

                    ldx       #BMJD               ; INITIALISE PAGE 0 RAM
CLoop@@             clr       ,x
                    inx
                    cpx       #SCNT+1             ; MORE ?
                    bne       CLoop@@
                    bset      STAT4,$04           ; ENABLE TRAFFIC SWITCHING - DEFAULT ?
                    bset      STAT,$01            ; STATION MODE

                    lda       #$30
                    bsr       CLOCK               ; INITIALISE LCD
                    bsr       WAIT
                    lda       #$30                ; /8 DISPLAY
                    brclr     PORTA,Y,$80,M8@@
                    lda       #$38                ; /16 DISPLAY
M8@@                bsr       CLOCK               ; LATCH IT
                    bsr       WAIT
                    lda       #$08                ; SWITCH DISPLAY OFF
                    bsr       CLOCK               ; LATCH IT
                    bsr       WAIT
                    lda       #$01                ; CLEAR DISPLAY
                    jsr       CLOCK               ; LATCH IT
                    jsr       INITD               ; INITIALISE RDS DATA & DISPLAY
                    jsr       CLREON              ; AND EON DATA
          ;-------------------------------------- ; Initialise interrupt JMPs
JRTI                equ       $00EB               ; E32 BUFFALO RAM JUMP TABLE
JIRQ                equ       $00EE               ; "    " "    " "
JXIRQ               equ       $00F1               ; "    " "    " "
          ;--------------------------------------
                    lda       #$7E
                    sta       JRTI
                    sta       JIRQ
                    sta       JXIRQ
                    ldd       #TINTB
                    std       JRTI+1              ; RTI
                    ldd       #SDATA
                    std       JIRQ+1              ; IRQ
                    ldd       #SHAFTX
                    std       JXIRQ+1             ; XIRQ

                    clra                          ; ENABLE IRQ & XIRQ
                    tap
;                   bra       IDLE

;*******************************************************************************
; Idle loop

IDLE                proc
                    ldy       #$1000
                    brset     STAT,$08,TBH
                    brset     $0E,Y,$80,*
                    bset      STAT,$08
                    bra       NO2D

TBH                 brclr     $0E,Y,$80,*
                    bclr      STAT,$80

NO2D                brclr     STAT4,$01,NOPS      ; DISPLAY TRANSIENT ?
                    lda       DIST
                    bne       NOPS                ; YES, TIMED OUT ?
                    jsr       CLTR

NOPS                brclr     STAT2,$08,NDU       ; DISPLAY UPDATE REQUIRED ?
                    jsr       MOD                 ; YES, DO IT
                    bclr      STAT2,$08           ; AND CLEAR FLAG

NDU                 brclr     PORTD,Y,$20,FULON   ; STANDBY ?

NOTSNZ              brset     STAT4,$10,NNT2      ; STANDBY, ALARM ARMED ?
NT2J                jmp       NT2

NNT2                brclr     STAT5,$80,NWA       ; YES, WEEKDAY ALARM ONLY ?
                    lda       DOW                 ; YES
                    cmpa      #4                  ; SATURDAY OR
                    bhi       NT2J                ; SUNDAY ?
NWA                 lda       AOUR                ; NO, COMPARE ALARM HOURS
                    cmpa      OUR                 ; WITH TIME
                    bne       NT2J                ; SAME ?
                    lda       AMIN                ; YES, COMPARE ALARM MINUTES
                    cmpa      MIN                 ; WITH TIME
                    bne       NT2J                ; SAME ?
                    lda       SEC                 ; ONLY ALLOW WAKE-UP IN FIRST SECOND
                    bne       NT2                 ; TO PREVENT SWITCH-OFF LOCKOUT
ONAG                bclr      PORTD,Y,$20         ; YES, SWITCH ON,
                    jsr       DEL500              ; WAIT 500ms,
                    bclr      PORTA,Y,$10         ; DEMUTE
                    jsr       P5170               ; AND TUNE (5170 & 5157)

FULON               brclr     STAT4,$02,FLN       ; SLEEP TIMER RUNNING ?
                    lda       SLEPT               ; YES
                    bne       FLN                 ; TIME TO FINISH ?
                    bclr      STAT4,$02           ; YES, CLEAR FLAG,
                    bset      PORTD,Y,$20         ; SWITCH OFF
                    bset      PORTA,Y,$10         ; AND MUTE

;*******************************************************************************
; Idle loop (cont.)
;*******************************************************************************

FLN                 proc
                    brclr     STAT4,$80,NT1       ; 14B FLAG HIGH ?
                    brset     STAT2,$80,NT2       ; YES, BIT AGREES ?
                    bset      STAT2,$80           ; NO, SET BIT
                    clr       REARET
                    lda       #25                 ; LOCK OUT RETURN
                    sta       DIST                ; FOR 3 SECONDS
                    bset      STAT4,$01           ; SET DISPLAY TRANSIENT FLAG
                    bset      PORTA,Y,$10         ; MUTE
                    jsr       DBNC                ; WAIT 150 ms
                    jsr       RETUNE2             ; AND RETUNE
                    brclr     STAT4,$80,NWWS      ; PI CODE NOT IN EON LIST ?
                    jsr       DEL500              ; WAIT 500ms
;                   brclr     PORTE,Y,$10,SOK     ;SIGNAL OK ?
;                   lda       #2
;                   sta       REARET
;                   bra       NT1
SOK                 jsr       DEL500              ; WAIT 500ms
                    brset     STAT3,$08,TPOK      ; TP OK?
                    lda       #5
                    sta       REARET
                    bra       NT1

TPOK                lda       PI                  ; YES, CHECK PI CODE
                    cmpa      PION
                    bne       PINOK1
                    lda       PI+1
                    cmpa      PION+1              ; AGAINST PI (EON)
                    beq       NT2                 ; IF OK STAY SWITCHED
PINOK1              lda       #3
                    sta       REARET

NT1                 brclr     STAT2,$80,NT2       ; 14B FLAG LOW, BIT AGREES ?
                    bclr      STAT4,$80           ; MAKE SURE 14B CANCELLED
                    bset      PORTA,Y,$10         ; MUTE
                    jsr       DBNC                ; WAIT 150 ms
NWWS                bclr      STAT2,$80           ; CLEAR FLAG
                    lda       LED                 ; SELECTED PROG.
                    jsr       RETUNE2             ; AND RETURN TO ORIGINAL PROGRAM

NT2                 jsr       KBD                 ; READ KEYBOARD
                    jsr       KEYP                ; EXECUTE KEY
                    brclr     STAT3,$20,NSRO      ; SHAFT ROTATION PENDING ?
                    bclr      STAT3,$20           ; YES, CLEAR FLAG
                    brset     STAT3,$10,ANTI      ; DIRECTION ?
                    jsr       PINC2               ; CLOCKWISE, INCREMENT
                    bra       NSRO

ANTI                jsr       PDEC2               ; ANTI-CLOCKWIRE, DECREMENT
NSRO                brclr     STAT3,$40,NRDSP     ; UPDATE DATE ?
                    jsr       MJDAT               ; YES, CONVERT FROM MJD

;*******************************************************************************
; Idle loop (cont.)
; Retune if band or SW bank inputs changed
;*******************************************************************************

NRDSP               proc
                    ldy       #$1000
                    brclr     STAT,$80,BTO        ; JUST POWERED UP ?
                    brclr     PORTA,Y,$01,L5      ; NO, A0 LOW ?
                    brset     STAT6,$04,CG6       ; NO, HIGH, BIT AGREES ?
                    bset      STAT6,$04           ; NO, MAKE IT HIGH
                    brset     STAT6,$08,BTO       ; BAND ONE ?
                    bset      STAT3,$80           ; YES, SHAFT INTERRUPTS
                    bra       CHE                 ; AND NOTHING ELSE TO DO

L5                  brclr     STAT6,$04,CG6       ; YES, A0 LOW, BUT AGREES ?
                    bclr      STAT6,$04           ; NO, MAKE IT LOW
                    brset     STAT6,$08,BTO       ; BAND ZERO ?
                    bclr      STAT3,$80           ; YES, RDS INTERRUPTS
                    bra       CHE                 ; AND NOTHING ELSE TO DO

CG6                 brclr     PORTA,Y,$02,L6      ; A1 LOW ?
                    brset     STAT6,$08,CHE       ; NO, HIGH, BIT AGREES ?
                    bset      STAT6,$08           ; NO, MAKE IT HIGH
                    bra       BTO

L6                  brclr     STAT6,$08,CHE       ; YES, A1 LOW, BIT AGREES ?
                    bclr      STAT6,$08           ; NO, MAKE IT LOW
                    brset     STAT6,$04,BTO       ; BAND ZERO ?
                    bclr      STAT3,$80           ; YES, RDS INTERRUPTS
                    bra       BTO

CHE                 brset     STAT6,$0C,BD3       ; BAND 3 ?
                    bra       OK6

BD3
CE6                 brclr     PORTE,Y,$40,E6L     ; NO, E6 LOW ?
                    brset     STAT6,$01,OK6       ; NO, HIGH, BIT AGREES ?
                    bset      STAT6,$01           ; NO, MAKE IT HIGH
                    bra       BTO

E6L                 brclr     STAT6,$01,OK6       ; YES, E6 LOW, BIT AGREES ?
                    bclr      STAT6,$01           ; NO, MAKE IT LOW

BTO                 bset      STAT,$80            ; SET POWER-UP FLAG,
                    lda       #10
                    sta       BCTO                ; INITIALISE
                    bset      STAT5,$01           ; AND START BAND-CHANGE TIMEOUT

;*******************************************************************************
; Idle loop (cont.)
;*******************************************************************************

OK6                 brclr     STAT5,$01,ARI       ; TIMEOUT RUNNING?
                    dec       BCTO                ; YES, DECREMENT COUNT
                    bne       ARI                 ; FINISHED ?
                    bclr      STAT5,$01           ; YES, CLEAR FLAG
                    bsr       RCLP                ; AND RECALL LAST USED PROG. No.
                    brclr     STAT6,$0C,ARI       ; BAND 0 ?
                    bset      STAT3,$80           ; NO, SHAFT INTERUPTS

ARI                 brset     STAT3,$0C,TATP      ; TA=TP=1 ?
                    bset      PORTA,Y,$20
                    bra       IOOK

TATP                bclr      PORTA,Y,$20         ; YES, A5 LOW

IOOK                brset     STAT5,$08,MSH       ; M/S=1 ?
                    bset      PORTA,Y,$40
                    bra       IDLJ

MSH                 bclr      PORTA,Y,$40         ; YES, A6 LOW

IDLJ                jmp       IDLE

RCLP                bset      PORTA,Y,$10         ; MUTE
                    ldb       #120
                    jsr       READ1               ; GET STORED PROG. No.
                    sta       LED
                    jmp       RETUNE2             ; PROGRAM 145170/57

;*******************************************************************************
; Shaft rotation interrupts
;*******************************************************************************

SHAFT               proc
                    brset     PORTE,Y,$20,SEM     ; IRQ,SHAFT I/O HIGH (E5) ?
                    bclr      STAT3,$10           ; NO, CLEAR DIRECTION BIT
                    bra       TEM

SEM                 bset      STAT3,$10           ; YES, SET DIRECTION BIT
TEM                 bset      STAT3,$20           ; SET FLAG TO INDICATE ROTATION
                    rti

SHAFTX              brset     PORTE,Y,$10,XEM     ; XIRQ, SHAFT I/O HIGH (E4) ?
                    bclr      STAT3,$10           ; NO, CLEAR DIRECTION BIT
                    bra       YEM

XEM                 bset      STAT3,$10           ; YES, SET DIRECTION BIT
YEM                 bset      STAT3,$20           ; SET FLAG TO INDICATE ROTATION
                    rti

;*******************************************************************************
; Keyboard routine
;*******************************************************************************

KBD                 proc
                    clr       W1
                    ldy       #$1000
                    ldx       #7
Loop@@              ldb       W1
                    addb      #$04                ; SELECT COLUMN
                    stb       W1
                    ldb       PORTD,Y
                    andb      #$20                ; PRESERVE OTHER PORTD DATA
                    addb      W1
                    stb       PORTD,Y
                    lda       PORTE,Y             ; READ KEYBOARD
                    bita      #$0F                ; ANY INPUT LINE HIGH ?
                    bne       L1
                    dex                           ; NO, TRY NEXT COLUMN
                    bne       Loop@@              ; LAST COLUMN ?
                    clr       KEY                 ; YES, NO KEY PRESSED
                    bra       EXIT

L1                  ldb       W1
                    lslb:2
                    lda       PORTE,Y             ; READ KEYBOARD
                    anda      #$0F
                    aba
                    cmpa      KEY                 ; SAME AS LAST TIME ?
                    beq       EXIT
                    sta       KEY                 ; NO, SAVE THIS KEY
                    clr       KOUNT
EXIT                inc       KOUNT               ; YES, THE SAME
                    lda       KOUNT
                    brclr     STAT,$40,NRML       ; REPEATING ?
                    ldb       PSNP                ; YES
                    beq       NOTCH               ; CHARACTER CHANGE ?
                    cmpa      #8                  ; YES, REPEAT AT 8 Hz
                    bra       GON2

NOTCH               cmpa      #16                 ; NO, REPEAT AT 4 Hz
                    bra       GON2

NRML                cmpa      #3                  ; NO, 3 THE SAME ?
                    blo       KCLC                ; IF NOT DO NOTHING
                    beq       GOON                ; IF 3 THEN PERFORM KEY FUNCTION
                    cmpa      #47                 ; MORE THAN 3, MORE THAN 47 (750mS) ?
GON2                bhi       GOON2               ; TIME TO DO SOMETHING ?
                    lda       KEY                 ; NO
                    beq       RKEY                ; KEY PRESSED ?
                    clc
                    rts                           ; YES BUT DO NOTHING

GOON2               lda       KEY
                    cmpa      #$54                ; DEC. PROG.
                    beq       GOON3
                    cmpa      #$58                ; INC.PROG.
                    beq       GOON3
                    cmpa      #$52                ; SLEEP
                    bne       DNT2                ; IF NOT A REPEAT KEY, DO NOTHING
GOON3               bset      STAT,$40            ; SET REPEAT FLAG
                    clr       KOUNT
GOON                lda       KEY
                    beq       RKEY                ; SOMETHING TO DO ?
                    sec                           ; YES, SET C
                    rts

RKEY                bclr      STAT,$20            ; NO, CLEAR DONE FLAG
DNT2                bclr      STAT,$40            ; CLEAR REPEAT FLAG
                    clr       KOUNT               ; CLEAR COUNTER
KCLC                clc
DNT                 rts

;*******************************************************************************
; Execute key
;*******************************************************************************

KEYP                proc
                    bcc       DNT                 ; ANYTHING TO DO ?
                    lda       KEY                 ; YES, GET KEY
                    cmpa      #$54                ; DEC. PROG. (M)
                    beq       RPT
                    cmpa      #$58                ; INC. PROG. (S)
                    beq       RPT
                    cmpa      #$52                ; SLEEP
                    beq       RPT
                    brset     STAT,$20,DNT        ; NOT A REPEAT KEY, FLAG SET ?

RPT                 clrb
RJ                  ldx       #CTAB
                    abx
                    lda       ,x                  ; FETCH KEYCODE
                    cmpa      KEY                 ; THIS ONE ?
                    beq       PJ                  ; YES
                    cmpa      LAST                ; NO, LAST CHANCE ?
                    beq       DNT                 ; YES, ABORT
                    addb      #4                  ; NO TRY THE NEXT KEY
                    bra       RJ

PJ                  bset      STAT,$20
                    jsr       1,X
                    jmp       P5170

;*******************************************************************************
; Keyboard jump table
;*******************************************************************************

?                   macro
                    fcb       ~1~
                    !jmp      ~2~
                    endm

CTAB                @?        $11,DIGIT           ; 0
                    @?        $21,DIGIT           ; 1
                    @?        $22,DIGIT           ; 2
                    @?        $24,DIGIT           ; 3
                    @?        $31,DIGIT           ; 4
                    @?        $32,DIGIT           ; 5
                    @?        $34,DIGIT           ; 6
                    @?        $41,DIGIT           ; 7
                    @?        $42,DIGIT           ; 8
                    @?        $44,DIGIT           ; 9
                    @?        $48,ALARM           ; ALARM
                    @?        $38,SAVE            ; STORE/SET
                    @?        $18,ONOFF           ; ON/OFF
                    @?        $14,CLEAR           ; CLEAR/STEP
                    @?        $12,MODE            ; MODE (PROG./FREQ.)
                    @?        $52,SLEEP           ; SLEEP TIMER START
                    @?        $54,PDEC            ; DEC. PROG./FREQ./CHAR.
                    @?        $58,PINC            ; INC. PROG./FREQ./CHAR.
                    @?        $61,RTDSP           ; RDS DISPLAYS
                    @?        $62,TPEN            ; TRAFFIC ENABLE (TOGGLE)
                    @?        $64,T910            ; MW STEP 9/10KHz (TOGGLE)
                    @?        $51,TFCC            ; COLON CONTROL
LAST                @?        $68,TEST            ; TA TEST

;*******************************************************************************
; Alarm key
;*******************************************************************************

ALARM               proc
                    brclr     STAT4,$08,ADON      ; ALARM DISPLAY ON ?
                    brclr     STAT4,$10,ALOF      ; YES, ALARM ON ?
                    bclr      STAT4,$10           ; YES, SWITCH OFF
                    bra       UDCNT

ALOF                bset      STAT4,$10           ; NO, SWITCH ON
                    bra       UDCNT

ADON                jsr       CLTR                ; NO, ENABLE ALARM DISPLAY
                    bset      STAT4,$08           ; ALARM DISPLAY FLAG
UDCNT               bclr      STAT4,$20           ; CANCEL SET-UP
                    lda       #25                 ; 3 SECONDS TIMEOUT
                    sta       DIST
                    bset      STAT4,$01           ; SET DISPLAY TRANSIENT FLAG
ABOA                rts

;*******************************************************************************
; On/off key
;*******************************************************************************

ONOFF               proc
                    jsr       CLTR                ; CLEAR DISPLAY TRANSIENTS
                    bclr      STAT4,$82           ; CANCELL SLEEP TIMER & TA SWITCH FLAG
                    bclr      STAT5,$40           ; CANCEL STORE MODE

                    brclr     PORTD,Y,$20,ALRON   ; ON ?
SODM                bclr      PORTD,Y,$20         ; NO, SWITCH ON
                    jsr       DEL500              ; WAIT 500ms
                    bclr      PORTA,Y,$10         ; AND DEMUTE
                    rts

ALRON               bset      PORTD,Y,$20         ; YES, SWITCH OFF
                    bset      PORTA,Y,$10         ; AND MUTE
                    rts

;*******************************************************************************
; PS name clear
;*******************************************************************************

PSC                 proc
                    ldx       #PSN
                    lda       #$FF
Loop@@              sta       ,x
                    inx
                    cpx       #PSN+8
                    bne       Loop@@
                    rts

;*******************************************************************************
; TP
;*******************************************************************************

TPEN                proc
                    brset     PORTD,Y,$20,HIGH    ; STANDBY ?
                    brset     STAT,$01,NS1        ; NO, NORMAL MODE ?
                    brset     STAT5,$20,TAEH      ; NO, FREQ. MODE, NVM DISABLE FLAG SET ?
                    bset      STAT5,$20           ; NO, SET IT
                    rts

TAEH                bclr      STAT5,$20           ; YES, CLEAR IT
HIGH                rts

NS1                 brclr     STAT4,$04,TPOF      ; NORMAL MODE, TRAFFIC ON ?
                    bclr      STAT4,$04           ; YES, DISABLE
                    rts

TPOF                bset      STAT4,$04           ; NO, ENABLE
                    rts

;*******************************************************************************
; Sleep timer
;*******************************************************************************

SLEEP               proc
                    brset     STAT5,$04,DECS      ; ALREADY SLEEP DISPLAY ?
                    brset     STAT4,$02,STR       ; NO, SLEEP TIMER ALREADY RUNNING ?
INSLP               lda       #60                 ; NO, INITIALISE SLEEP TIMER
SLEP                sta       SLEPT
                    bset      STAT4,$02           ; START SLEEP TIMER
STR                 jsr       CLTR                ; YES, CLEAR DISPLAY TRANSIENTS
                    bset      STAT5,$04           ; SLEEP DISPLAY
                    bra       SLPTOK              ; NO DECREMENT IF FIRST TIME

DECS                lda       SLEPT               ; DECREMENT SLEEP TIMER
                    suba      #5
                    sta       SLEPT
                    bmi       INSLP

SLPTOK              lda       #25
                    sta       DIST
                    bset      STAT4,$01           ; START DISPLAY TRANSIENT
                    brset     PORTD,Y,$20,SODM    ; ALREADY ON ?
                    bclr      PORTA,Y,$10         ; YES, JUST DEMUTE
                    rts

;*******************************************************************************
; Number entry routine
;*******************************************************************************

DIGIT               proc
                    brset     PORTD,Y,$20,ABO3    ; STANDBY ?
                    jsr       CLTR                ; NO, CLEAR DISPLAY TRANSIENTS
                    lsrb:2
                    brset     STAT,$01,SKP        ; STATION MODE ?
                    brset     STAT5,$40,SKP       ; NO, STORE MODE ?
                    bset      STAT5,$10           ; NO, SET RETUNE FLAG (FREQUENCY MODE)
                    bclr      STAT5,$20           ; AND CLEAR TA INHIBIT BIT (NVM)
                    stb       W3
                    brclr     STAT,$04,SHIFT      ; CLEAR Q ?
                    bclr      STAT,$04            ; YES, CLEAR FLAG
                    jsr       CLQ                 ; AND CLEAR Q
SHIFT               bsr       DR1                 ; W1: MSD, W2: LSD
                    ldx       W1
AGS                 lda       1,X                 ; MOVE ALL DIGITS
                    sta       1,X                 ; UP ONE PLACE
                    inx
                    cpx       W2
                    bne       AGS                 ; DONE ?
                    lda       W3                  ; YES, RECOVER NEW DIGIT
                    sta       ,x                  ; AND PUT IT IN LSD
                    rts

SKP                 bset      PORTA,Y,$10         ; MUTE
                    tba
                    sta       LED
                    jmp       RETUNE

;*******************************************************************************
; Save pointers & 500ms delay
;*******************************************************************************

DR1                 proc
                    ldx       #RQ                 ; STORE POINTERS
                    stx       W1
                    ldb       #5
                    abx
                    stx       W2
ABO3                rts

DEL500              ldx       #255
                    jsr       SKDB
                    ldx       #255
                    jmp       SKDB

;*******************************************************************************
; Increment key (& knob)
;*******************************************************************************

PINC2               proc
                    brset     STAT4,$20,ALSU1     ; ALARM SET-UP ?
                    brset     STAT4,$08,TOG57J    ; NO, ALARM DISPLAY ?
                    brset     PORTD,Y,$20,DMI     ; NO,STANDBY ?
                    ldb       PSNP
                    bne       PSN0                ; NO,PS EDIT MODE ?
                    jmp       UP                  ; NO, STEP UP

PINC                brset     STAT4,$20,ALSU1     ; ALARM SET-UP ?
TOG57J              brset     STAT4,$08,TOG57     ; NO, ALARM DISPLAY ?

                    brset     PORTD,Y,$20,DMI     ; NO, STANDBY ?
                    brset     STAT,$01,NACS       ; NO, FREQ. MODE ?
                    jmp       UP                  ; YES, STEP UP

;*******************************************************************************
; Alarm inc. (hours/minutes)
;*******************************************************************************

ALSU1               proc
                    brset     STAT4,$40,IHR       ; YES, SET-UP HOURS ?
                    lda       AMIN                ; NO, MINUTES
                    cmpa      #59
                    bhs       TOOH
                    inc       AMIN
                    bra       T5S

TOOH                clr       AMIN
                    bra       T5S

IHR                 lda       AOUR
                    cmpa      #23
                    blo       HTOH
                    clr       AOUR
                    bra       T5S

HTOH                inc       AOUR
T5S                 lda       #80                 ; 10 SECOND TIMEOUT
                    sta       DIST
                    bset      STAT4,$01           ; SET DISPLAY TRANSIENT FLAG
                    bclr      PORTA,Y,$10         ; DEMUTE
DMI                 rts

NACS                ldb       PSNP
                    beq       CONTI               ; NO, PS EDIT MODE ?

;*******************************************************************************
; P-S Edit inc. (ASCII) and 5/7 day toggle
;*******************************************************************************

PSN0                proc
                    ldx       #PSN-1
                    abx
                    lda       ,x                  ; YES
                    inca                          ; INCREMENT ASCII VALUE
                    cmpa      #$20                ; SPACE
                    bls       MAK20               ; LESS OR EQUAL ?
                    cmpa      #$2E                ; NO, .
                    bls       MAK2E               ; LESS OR EQUAL ?
                    cmpa      #$30                ; NO, 0
                    blo       MAK30               ; LESS ?
                    cmpa      #$39                ; NO, 9
                    bls       CNTB                ; LESS OR EQUAL ?
                    cmpa      #$41                ; NO, A
                    blo       MAK41               ; LESS ?
                    cmpa      #$5A                ; NO, Z
                    bls       CNTB                ; LESS OR EQUAL ?
                    cmpa      #$61                ; NO, a
                    blo       MAK61               ; LESS ?
                    cmpa      #$7A                ; NO, z
                    bls       CNTB                ; LESS OR EQUAL ?
MAK20               lda       #$20                ; MAKE SPACE
                    bra       CNTB

MAK2E               lda       #$2E                ; MAKE .
                    bra       CNTB

MAK30               lda       #$30                ; MAKE 0
                    bra       CNTB

MAK41               lda       #$41                ; MAKE A
                    bra       CNTB

MAK61               lda       #$61                ; MAKE a
CNTB                sta       ,x
                    lda       #80
                    jmp       OUTCH

TOG57               brclr     STAT4,$10,DMI       ; ALARM ARMED ?
                    brclr     STAT5,$80,A7        ; YES, 7-DAY ALARM ?
                    bclr      STAT5,$80           ; NO, MAKE IT 7 DAY
                    bra       T5S

A7                  bset      STAT5,$80           ; YES, MAKE IT 5 DAY
                    bra       T5S

;*******************************************************************************
; Program number increment
;*******************************************************************************

CONTI               proc
                    bset      PORTA,Y,$10         ; MUTE
                    bset      STAT2,$08           ; PROG. No. INCREMENT, UPDATE DISPLAY
                    lda       LED
                    brset     STAT2,$80,IOK       ; IF SWITCHED TO TA DON'T INCREMENT
                    inca                          ; NEXT PROG.
                    cmpa      #9                  ; TOO HIGH ?
                    bls       IOK
                    clra                          ; YES, BACK TO ZERO
IOK                 sta       LED
                    jmp       RETUNE

;*******************************************************************************
; Decrement key (& knob)
;*******************************************************************************

PDEC2               proc
                    brset     STAT4,$20,ALSU2     ; ALARM SET-UP ?
                    brset     STAT4,$08,TOG57     ; NO, ALARM DISPLAY ?
                    brset     PORTD,Y,$20,DMD     ; NO, STANDBY ?
                    ldb       PSNP
                    bne       PSN1                ; NO, PS EDIT MODE ?
                    jmp       DOWN                ; NO, STEP DOWN

PDEC                brset     STAT4,$20,ALSU2     ; ALARM SET-UP ?
                    brset     STAT4,$08,TOG57     ; NO, ALARM DISPLAY ?
                    brset     PORTD,Y,$20,DMD     ; NO, STANDBY ?
                    brset     STAT,$01,NACS2      ; NO, FREQ. MODE ?
                    jmp       DOWN                ; YES, STEP DOWN

;*******************************************************************************
; Alarm dec. (hours/minutes)
;*******************************************************************************

ALSU2               proc
                    brset     STAT4,$40,IHRD      ; YES, SET-UP HOURS ?
                    tst       AMIN                ; NO, MINUTES
                    beq       MZ
                    dec       AMIN
                    bra       T5SD

MZ                  lda       #59
                    sta       AMIN
                    bra       T5SD

IHRD                tst       AOUR
                    bne       HZ
                    lda       #24
                    sta       AOUR
HZ                  dec       AOUR
T5SD                lda       #80                 ; 10 SECOND TIMEOUT
                    sta       DIST
                    bset      STAT4,$01           ; SET DISPLAY TRANSIENT FLAG
                    bclr      PORTA,Y,$10         ; DEMUTE
DMD                 rts

NACS2               ldb       PSNP
                    beq       CONTD               ; PS EDIT CHARACTER CHANGE ?

;*******************************************************************************
; P-S Edit dec. (ASCII)
;*******************************************************************************

PSN1                proc
                    ldx       #PSN-1
                    abx
                    lda       ,x                  ; YES
                    deca                          ; DECREMENT ASCII VALUE
                    cmpa      #$20                ; SPACE
                    bls       MKE7A               ; LESS OR EQUAL ?
                    cmpa      #$2E                ; NO, .
                    bls       MKE20               ; LESS OR EQUAL ?
                    cmpa      #$30                ; NO, 0
                    blo       MKE2E               ; LESS ?
                    cmpa      #$39                ; NO, 9
                    bls       CNTS                ; LESS OR EQUAL ?
                    cmpa      #$41                ; NO, A
                    blo       MKE39               ; LESS ?
                    cmpa      #$5A                ; NO, Z
                    bls       CNTS                ; LESS OR EQUAL ?
                    cmpa      #$61                ; NO, a
                    blo       MKE5A               ; LESS ?
                    cmpa      #$7A                ; NO, z
                    bls       CNTS                ; LESS OR EQUAL ?
MKE20               lda       #$20                ; MAKE SPACE
                    bra       CNTS

MKE2E               lda       #$2E                ; MAKE .
                    bra       CNTS

MKE5A               lda       #$5A                ; MAKE Z
                    bra       CNTS

MKE7A               lda       #$7A                ; MAKE z
                    bra       CNTS

MKE39               lda       #$39                ; MAKE A
CNTS                sta       ,x
                    lda       #80

OUTCH               sta       DIST
                    bset      STAT4,$01           ; SET DISPLAY TRANSIENT FLAG
                    bclr      STAT4,$08           ; NOT ALARM DISPLAY MODE
                    rts

;*******************************************************************************
; Program number decrement
;*******************************************************************************

CONTD               proc
                    bset      PORTA,Y,$10         ; MUTE
                    lda       LED                 ; PROG. No. DECREMENT
                    brset     STAT2,$80,RETUNE    ; IF SWITCHED TO TA DON'T DECREMENT
PNM1                deca                          ; DECREMENT PROGRAM NUMBER
                    bpl       SK2P                ; TOO FAR ?
                    lda       #9
SK2P                sta       LED                 ; SAVE NEW PROGRAM NUMBER
RETUNE              psha
                    ldb       #120                ; CHANGE PROGRAM NUMBER IN NVM
                    jsr       WRITE1
                    pula
                    brclr     STAT4,$80,RETUNE2   ; TA SWITCHED ?
                    bclr      STAT4,$80           ; YES, MANUAL RETURN FROM TA
                    lda       #9
                    sta       REARET
                    rts

RETUNE2             bsr       DOIT                ; NEW PROGRAM
                    jsr       P5170
                    ldx       #64                 ; WAIT 100ms
                    jsr       SKDB
                    bclr      PORTA,Y,$10         ; DEMUTE
                    bclr      STAT2,$02           ; KILL ANY PENDING RDS GROUP
                    bclr      STAT3,$01           ; AND INHIBIT FM PS-NAME CLEARING
                    bclr      STAT,$10            ; RE-ENABLE RDS DATA CLEARING
                    rts

FOK                 ldb       #10
                    mul
                    addb      #$5C
                    stb       SMEM
                    adca      #$26
                    sta       SMEM+1
                    jmp       NEW

;*******************************************************************************
; Tune to TA (using EEPROM data)
;*******************************************************************************

TASW                proc
                    clrb
TPIC                addb      #10
                    jsr       READ1               ; FIND PI
                    incb
                    cmpa      PION                ; MSB OK ?
                    bne       TNP
                    decb
                    jsr       READ1
                    cmpa      PION+1              ; LSB OK ?
                    bne       TNP
                    subb      #12                 ; YES, FOUND IT
                    jsr       READ1
                    psha
                    anda      #$80                ; NVM INHIBIT FLAG SET ?
                    beq       TASOK
                    lda       #8                  ; NVM INHIBIT MESSAGE
                    bra       ABTA

TASOK               pula
                    sta       SMEM+1
                    jsr       NEWSUB2
                    jmp       NEW

TNP                 cmpb      #252                ; TRY NEXT RECORD
                    blo       TPIC
                    psha
                    lda       #7
ABTA                sta       REARET
                    pula
                    bclr      STAT4,$80           ; PI MATCH NOT FOUND, FORGET IT
                    rts

;*******************************************************************************
; Program store/recall
;*******************************************************************************

DOIT                proc
                    brset     STAT2,$80,TASW
                    ldb       #12
                    mul
                    brset     STAT5,$40,STORE
                    jmp       RECALL

;*******************************************************************************
; NVW write, sub-address in X
;*******************************************************************************

STORE               proc
                    bclr      SMEM+1,$80
                    brclr     STAT5,$20,SKTA      ; TA NVM INHIBIT FLAG SET ?
                    bset      SMEM+1,$80
SKTA                lda       SMEM+1              ; BINARY FREQUENCY MSB
                    jsr       WRITE1
                    lda       SMEM                ; BINARY FREQUENCY LSB
                    jsr       WRITE1
                    lda       PSN
                    cmpa      #$A0                ; PS NAME OK ?
                    beq       PSNOK
                    cmpa      #$FF                ; PERHAPS, TRY FF
                    bne       PSOK
PSNOK               lda       #$FF
                    jsr       WRITE1
                    lda       DISP+10
                    jsr       WRITE1
                    lda       DISP+11
                    jsr       WRITE1
                    lda       DISP+12
                    jsr       WRITE1
                    lda       DISP+13
                    jsr       WRITE1
                    lda       DISP+14
                    jsr       WRITE1
                    lda       DISP+15
                    jsr       WRITE1
                    lda       #$20
                    jsr       WRITE1
                    lda       #$00                ; DUMMY PI CODE
                    jsr       WRITE1
                    lda       #$00
                    bra       FINST

PSOK                bsr       WRITE1
                    lda       PSN+1
                    bsr       WRITE1
                    lda       PSN+2
                    bsr       WRITE1
                    lda       PSN+3
                    bsr       WRITE1
                    lda       PSN+4
                    bsr       WRITE1
                    lda       PSN+5
                    bsr       WRITE1
                    lda       PSN+6
                    bsr       WRITE1
                    lda       PSN+7
                    bsr       WRITE1
                    lda       PI                  ; PI CODE
                    bsr       WRITE1
                    lda       PI+1
FINST               bsr       WRITE1
                    bclr      STAT5,$40           ; CLEAR STORE MODE
                    rts

;*******************************************************************************
; NVW read, sub-address in X
;*******************************************************************************

RECALL              proc
                    bsr       NEWSUB
                    jmp       NEW

NEWSUB              bsr       READ1
                    sta       SMEM+1
                    bclr      STAT5,$20
                    brclr     SMEM+1,$80,NEWSUB2
                    bset      STAT5,$20
NEWSUB2             bsr       READ1
                    cmpa      #$FF
                    bne       NOTFF2
                    lda       #$26                ; $04
                    sta       SMEM+1
                    lda       #$5C                ; $2E
NOTFF2              sta       SMEM
                    bsr       READ1
                    sta       PSN
                    bsr       READ1
                    sta       PSN+1
                    bsr       READ1
                    sta       PSN+2
                    bsr       READ1
                    sta       PSN+3
                    bsr       READ1
                    sta       PSN+4
                    bsr       READ1
                    sta       PSN+5
                    bsr       READ1
                    sta       PSN+6
                    bsr       READ1
                    sta       PSN+7
                    rts

;*******************************************************************************
; NVW read & write one byte
;*******************************************************************************

READ1               proc
                    bsr       GETAD
                    lda       ,x
                    incb
                    rts

;*******************************************************************************

WRITE1              proc
                    ldy       #$1000
                    bset      PPROG,Y,$16         ; SET EELAT, ERASE & BYTE ERASE BITS
                    bsr       WBYTE               ; ERASE BYTE
                    jsr       DBOUNC              ; WAIT 15 ms
                    bset      PPROG,Y,$02         ; SET EELAT TO WRITE BYTE
                    decb

WBYTE               bsr       GETAD
                    sta       ,x                  ; LATCH DATA
                    bset      PPROG,Y,$01         ; SET EEPGM BIT TO START PROGRAMMING
                    jsr       DBOUNC              ; WAIT 15 ms
                    clr       PPROG,Y             ; STOP
                    incb
                    rts

;*******************************************************************************

GETAD               proc
                    pshd
                    jsr       BAND                ; GET BAND
                    ldx       #$B618              ; EEPROM START ADDRESS
                    tba
                    cmpa      #1                  ; FM ?
                    bls       Done@@
                    ldb       #122                ; NO, AM
                    abx
                    cmpa      #2                  ; MW ?
                    beq       Done@@
                    abx                           ; NO, SW
                    brclr     PORTE,Y,$40,_1@@    ; SECOND BANK ?
                    abx                           ; YES
_1@@               ;brclr     PORTE,Y,$80,Done@@  ; SECOND PAIR OF BANKS ?
;                   abx:2                         ; YES
Done@@              puld
                    abx
                    rts

;*******************************************************************************
; RDS displays
;*******************************************************************************

RTDSP               proc
                    brset     PORTD,Y,$20,SRT     ; STANDBY ?
                    brset     STAT5,$02,NOTRT     ; ALREADY RDS DISPLAY ?
                    brclr     STAT2,$04,NORT      ; ALREADY RT DISPLAY ?

NOTRT               bset      STAT5,$02           ; SET RDS DISPLAY FLAG
                    lda       RTDIS               ; YES, MOVE ON
                    inca
                    cmpa      #26
                    beq       NORT
                    sta       RTDIS
                    lda       #100
                    sta       DIST
                    bset      STAT4,$01           ; RE-START TRANSIENT TIMEOUT
                    rts

NORT                jsr       CLTR
                    bset      STAT2,$04           ; SET RT DISPLAY FLAG
                    lda       #9
                    sta       DISP1
                    lda       #1
                    sta       DISP2
                    rts

;*******************************************************************************
; Increment and decrement routines.
;*******************************************************************************

UP                  proc
                    bsr       LDXR
IF                  inc       SMEM                ; NO, INCREMENT LSB
                    bne       TT1                 ; DID IT WRAP ROUND
                    inc       SMEM+1              ; YES, INCREMENT MSB
TT1                 decb
                    bne       IF                  ; ALL DONE ?
                    bra       NEWJ

;*******************************************************************************

DOWN                proc
                    bsr       LDXR
DF                  tst       SMEM                ; NO, IS LSB ZERO ?
                    bne       TT2                 ; IF NOT LEAVE MSD
                    dec       SMEM+1              ; DECREMENT MSB
TT2                 dec       SMEM                ; DECREMENT LSB
                    decb
                    bne       DF                  ; ALL DONE ?
NEWJ                jsr       NEW
                    jsr       P5170
                    bclr      PORTA,Y,$10         ; DEMUTE
                    rts

;*******************************************************************************

LDXR                proc
                    brclr     STAT6,$08,LDXR2     ; AM ?
                    bset      STAT2,$40           ; YES, CLEAR PS NAME
                    bra       NFMB

LDXR2               bset      STAT3,$01           ; NO, FM, ENABLE PS NAME CLEARING
NFMB                jsr       BAND                ; GET BAND
                    tba
                    ldb       #1                  ; SINGLE STEP (1,5,10 KHz FOR MW,SW,FM)
                    brclr     STAT,$02,SRT        ; LARGE STEPS SELECTED ?
                    cmpa      #3                  ; YES, BAND 3 (SW) ?
                    beq       SRT
                    ldb       #5                  ; NO, x5 STEP (50 KHz FOR FM)
                    cmpa      #2                  ; MW ?
                    bne       SRT
                    ldb       #9                  ; YES, 9KHz
                    brclr     STAT6,$40,SRT       ; OR SHOULD IT BE 10KHz
                    incb                          ; YES
SRT                 rts

;*******************************************************************************
; TA test
;*******************************************************************************

TEST                proc
                    brset     PORTD,Y,$20,AOB     ; STANDBY ?
                    ldd       #$C5B1              ; CLYDE 1
                    std       PION
                    brset     STAT4,$04,NABT      ; TA SWITCHING ENABLED ?
                    lda       #1                  ; NO, SET RETURN REASON
                    sta       REARET
AOB                 rts

NABT                bset      STAT4,$80           ; YES, DO IT
                    rts

;*******************************************************************************
; Store key
;*******************************************************************************

SAVE                proc
                    brclr     STAT4,$08,NAME      ; ALARM DISPAY ?
                    brclr     STAT4,$10,NTB2      ; YES, ALARM ARMED ?
                    brset     STAT4,$20,AISM      ; YES, ALREADY SET-UP MODE ?
                    bset      STAT4,$60           ; NO, ENTER SET-UP MODE, HOURS
A5SD                lda       #80
                    bra       SDT

AISM                brset     STAT4,$40,MSM       ; YES, SET-UP HOURS ?
                    bclr      STAT4,$20           ; NO, CANCEL SET-UP
                    bra       A5SD

MSM                 bclr      STAT4,$40           ; YES, MAKE IT MINUTES
                    bra       A5SD

NAME                brset     PORTD,Y,$20,NTB2    ; STANDBY ?
                    brset     STAT,$01,NFM        ; NO, FREQUENCY MODE ?
                    brset     STAT5,$40,ASM       ; YES, STORE MODE ?
                    bset      STAT5,$40           ; NO, ENTER STORE MODE
                    rts

ASM                 lda       LED
                    jmp       DOIT                ; SAVE

NFM                 lda       PSNP                ; NOT FREQUENCY MODE
                    bne       SKPCLR              ; SET
                    jsr       CLTR                ; UP
SKPCLR              inc       PSNP                ; PS-NAME CHANGE MODE
                    lda       PSNP
                    cmpa      #8
                    bls       NTB3
                    clr       PSNP
NTB3                lda       #80
SDT                 sta       DIST
                    bset      STAT4,$01           ; SET DISPLAY TRANSIENT FLAG
NTB2                rts

;*******************************************************************************
; PROG, the displayed number is added to the IF offset, converted to binary and
; stored in SMEM & SMEM+1.

; NEW takes binary working frequency in SMEM & SMEM+1 converts it to BCD and
; subtracts the IF offset.
;*******************************************************************************

PROG                proc
                    brset     STAT,$01,NEW        ; STATION MODE ?
                    jsr       IFO                 ; P < IF OFFSET
                    jsr       ADB                 ; Q < FREQ + IF

                    jsr       BAND
                    bne       ONE                 ; BAND 3 (SW) ?
                    jsr       ADD                 ; YES, DIVIDE BY 5, Q < 2 X (FREQ + IF)
                    ldx       #5
LPP                 lda       RQ-1,X              ; MOVE ALL DIGITS
                    sta       RQ,X                ; IN Q DOWN ONE
                    dex                           ; PLACE TO DEVIDE
                    bne       LPP                 ; BY 10 (Q < Q/5)

ONE                 jsr       BCON                ; CONVERT Q TO BINARY

NEW                 exp       *
                    jsr       DCON                ; CONVERT TO BCD IN Q

                    bsr       BAND
                    bne       STIF                ; BAND 3 (SW) ?
                    stx       NUM1                ; YES
                    ldx       #RP
                    jsr       ADD                 ; P < 2Q
                    ldx       #RP
                    stx       NUM1
                    ldx       #RQ
                    jsr       ADD                 ; Q < 3Q
                    ldx       #RQ
                    jsr       ADD                 ; Q < 5Q

STIF                jsr       IFO                 ; P < IF OFFSET
                    bset      STAT,$04
                    jmp       SUB                 ; Q < (RATIO X STEP) -IF

;*******************************************************************************
; The IF offset is selected according to the required band and placed in "RP."
;*******************************************************************************

IPO                 proc
                    bsr       BAND                ; FIND BAND
                    brset     PORTA,Y,$04,NOTN    ; NEGATIVE FM IF ?
                    cmpb      #1                  ; YES
                    bhi       NOTN                ; BUT IS IT FM ?
                    ldb       #4                  ; YES, FIFTH IS FROM TABLE
NOTN                lda       #6
                    mul                           ; TIMES 6
                    ldx       #IFS
                    abx
                    ldy       #RP
LP6                 lda       ,x                  ; TRANSFER
                    sta       ,y                  ; INTO RP
                    inx
                    iny
                    cpy       #RP+6
                    blo       LP6                 ; DONE ?
                    ldy       #$1000              ; RE-INITIALISE Y
                    ldx       #RP                 ; SET-UP POINTERS
                    stx       NUM2
                    ldx       #RQ
                    stx       NUM1
                    rts

IFS                 fcb       0,0,1,0,7,0         ; 10.70 MHz FM OSC HIGH
                    fcb       0,0,1,0,7,0         ; 10.70 MHz FM OSC HIGH
                    fcb       0,0,0,4,5,5         ; 455 KHz SW/MW
                    fcb       0,1,0,7,0,0         ; 10.70 MHz SW (EXT/5 FOR 5157)
                    fcb       9,9,8,9,3,0         ; �10.70 MHz FM OSC LOW

BAND                proc
                    ldb       PORTA,Y             ; GET BAND
                    andb      #$03
                    ldx       #RQ
                    stx       NUM2
                    cmpb      #3                  ; BAND 3 (SW, /5) ?
                    rts

;*******************************************************************************
; Mode change & clear routines

MODE                proc
                    brset     PORTD,Y,$20,Done@@  ; STANDBY ?
                    bsr       CLTR
                    jsr       PROG                ; SEND DISPLAYED FREQUENCY
                    brclr     STAT,$01,Skip@@     ; FREQUENCY MODE ?
                    bclr      STAT,$01            ; NO, SET TO FREQUENCY MODE
                    rts

Skip@@              bclr      STAT5,$40           ; FREQ. MODE, CLEAR STORE MODE
                    brclr     STAT5,$10,NNTR      ; NEW FREQUENCY ENTERED ?
                    bset      PORTA,Y,$10         ; YES, MUTE
                    jsr       DBNC                ; WAIT 15ms
                    jsr       P5170
                    ldx       #64
                    jsr       SKDB                ; WAIT 100ms
                    bclr      PORTA,Y,$10         ; DE-MUTE
                    bclr      STAT2,$02           ; AND KILL ANY PENDING RDS GROUP
SKSM                bclr      STAT5,$10           ; CLEAR RETUNE FLAG
                    rts

NNTR                bset      STAT,$01            ; NO, RETURN TO STATION MODE
                    bclr      STAT5,$40           ; CANCEL STORE MODE
                    rts

CLEAR               brset     PORTD,Y,$20,Done@@  ; STANDBY ?
                    brset     STAT,$01,SM         ; NO, STATION MODE ?
                    bset      STAT5,$10           ; FREQUENCY CHANGED
CLAL                bsr       CLQ                 ; NO, CLEAR Q
SM                  lda       PSNP
                    beq       SPCC
                    jsr       PSC
SPCC                bsr       CLTR                ; CLEAR DISPLAY TRANSIENTS
                    brset     STAT,$02,KHz@@
                    bset      STAT,$02            ; 9 (MW), 50 (FM) KHz STEPS
                    rts

KHz@@               bclr      STAT,$02            ; 1 (MW), 10 (FM) KHz STEPS
Done@@              rts

;*******************************************************************************

CLQ                 proc
                    ldx       #RQ                 ; CLEAR RQ
CLRAS               lda       #6                  ; CLEAR 6 BYTES
                    sta       COUNT               ; STARTING AT X
Loop@@              clr       ,x
                    inx
                    dec       COUNT
                    bne       Loop@@              ; DONE ?
                    rts

;*******************************************************************************

CLTR                proc
                    bclr      STAT4,$01           ; CLEAR DISPLAY TRANSIENT FLAG
                    bclr      STAT2,$04           ; CANCEL RT DISPLAY
                    clr       RTDIS
                    bclr      STAT4,$28           ; NOT ALARM (DISPLAY OR SET-UP)
                    bclr      STAT5,$06           ; NOT RT OR SLEEP DISPLAY
                    clr       PSNP                ; NOT PS-EDIT
                    rts

;*******************************************************************************
; BCD to binary conversion. No, in "RQ" is converted to binary in SMEM & SMEM+1

BCON                proc
                    clr       SMEM                ; CLEAR WORKING
                    clr       SMEM+1              ; FREQUENCY LOCATIONS
                    clrx
Loop@@              lda       SMEM                ; LS BYTE
                    lsla                          ; 2xLSB
                    sta       W1                  ; SAVE 2xLSB
                    rol       SMEM+1              ; 2xMS BYTE
                    lda       SMEM+1
                    sta       W2                  ; SAVE 2xMSB
                    lda       W1                  ; 2xLSB
                    lsla                          ; 4xLSB
                    rol       SMEM+1              ; 4xMSB
                    lsla                          ; 8xLSB
                    rol       SMEM+1              ; 8xMSB
                    adda      W1                  ; 10xLSB
                    sta       SMEM
                    lda       SMEM+1
                    adca      W2
                    sta       SMEM+1
                    adca      W2                  ; 10xMSB
                    sta       SMEM+1
                    inx                           ; FETCH
                    lda       RQ,X                ; NEXT
                    adda      SMEM                ; DIGIT
                    sta       SMEM                ; AND
                    lda       #0                  ; (CLRA CLEARS THE C BIT)
                    adca      SMEM+1              ; ADD IT TO WORKING
                    sta       SMEM+1              ; FREQUENCY
                    cpx       #5                  ; DONE ?
                    bne       Loop@@
                    rts

;*******************************************************************************
; Clear NVM - not used

CLRNVW              proc
                    clr       COUNT
Loop@@              lda       #$FF
                    ldb       COUNT
                    jsr       WRITE1
                    inc       COUNT
                    bne       Loop@@
                    ldd       #120                ; CLEAR MAX. PROG. No.
                    jmp       WRITE1

;*******************************************************************************
; Addition and subtraction of BCD numbers
;*******************************************************************************

SUB                 proc
                    stx       W5                  ; ANSWER POINTER
COM2                ldx       NUM2                ; 9S COMPLIMENT
COMP                ldb       #$06                ; SECOND NUMBER
LOOP3               lda       #$09
                    suba      5,x                 ; SUBTRACT FROM 9
                    sta       5,x                 ; AND PUT IT BACK
                    dex
                    decb
                    bne       LOOP3
                    clr       CARRY               ; SET CARRY TO ONE
                    inc       CARRY               ; BEFORE ADDING
                    bra       AD                  ; ADD FIRST NUMBER

ADD                 clr       CARRY
                    stx       W5                  ; ANSWER POINTER
AD                  ldb       #$06
                    ldx       NUM1                ; 1st No. POINTER
                    stx       W3
                    ldx       NUM2                ; 2nd No. POINTER
                    stx       W4
LOOP                ldx       W3
                    lda       5,X
                    dex
                    stx       W3
                    ldx       W4
                    adda      5,X                 ; ADD
                    dex
                    stx       W4
                    adda      CARRY               ; SET ON ADDITION OVERFLOW
                    clr       CARRY               ; OR POS. RESULT SUBTRACTION
                    bsr       ADJ                 ; DECIMAL ADJUST
                    ldx       W5
                    sta       5,X                 ; SAVE ANSWER
                    dex
                    stx       W5
                    decb
                    bne       LOOP                ; DONE ?
                    rts

AJ                  suba      #10                 ; YES, SUBTRACT 10
                    inc       CARRY               ; AND RECORD CARRY
ADJ                 cmpa      #10
                    bhs       AJ                  ; 10 OR MORE ?
                    rts                           ; NO

;*******************************************************************************
; Current binary divide ratio in SMEM & SMEM+1 is converted to decimal in RQ
;*******************************************************************************

DCON                proc
                    lda       SMEM+1              ; TRANSFER CURRENT
                    sta       W2                  ; FREQUENCY DIVIDE
                    lda       SMEM                ; RATIO INTO
                    sta       W1                  ; WORKING AREA
DCON2               exp       *
                    ldx       #RR                 ; CLEAR
                    stx       NUM1
                    jsr       CLRAS               ; RR
                    inc       RR+5                ; RR <- 1
                    jsr       CLQ                 ; CLEAR RQ
                    lda       #14                 ; 14 BITS TO CONVERT
                    sta       W6
LOOP2               lsr       W2                  ; MOVE OUT
                    ror       W1                  ; FIRST (LS) BIT
                    bcc       NXT                 ; ZERO
                    ldx       #RQ                 ; ONE, ADD
                    stx       NUM2                ; CURRENT VALUE
                    bsr       ADD                 ; OF RR
NXT                 ldx       #RR                 ; ADD RR
                    stx       NUM2                ; TO
                    bsr       ADD                 ; ITSELF
                    dec       W6                  ; ALL
                    bne       LOOP2               ; DONE ?
                    rts

;*******************************************************************************
; Delay (X x 1.5mS)
;*******************************************************************************

DBNC                proc
                    ldx       #100                ; 150mS
                    bra       SKDB

DBOUNC              ldx       #10                 ; APPROX 15mS WITH A 8.388 MHz XTAL
SKDB                stx       W6                  ; X x 1.5mS
DLP                 ldx       #$FF                ; PAUSE
DLOOP               brn       *                   ; 256X12
                    brn       *                   ; CYCLES
                    dex
                    bne       DLOOP
                    dec       W6+1
                    bne       DLP
ABO                 rts

;*******************************************************************************
; Serial output routine to the MC145170
;*******************************************************************************

P5170               proc
                    bclr      PORTB,Y,$01         ; CLOCK LOW
                    bclr      PORTB,Y,$10         ; LE LOW
                    clra                          ; CLEAR
                    bsr       SQU8I               ; CONTROL REGISTER
                    bset      PORTB,Y,$10         ; LATCH IT
                    bclr      PORTB,Y,$10         ; LE LOW
                    lda       SMEM+1
                    anda      #$7F
                    bsr       SQU8I               ; SEND MSBYTE
                    lda       SMEM                ; AND LSBYTE OF
                    bsr       SQU8I               ; NEW FREQUENCY
                    bset      PORTB,Y,$10         ; LATCH IT

                    bclr      PORTB,Y,$10         ; LE LOW
                    lda       #$03                ; SEND
                    bsr       SQU7I               ; REFERENCE
                    lda       #$20                ; DIVIDE RATIO
                    bsr       SQU8I               ; 800 = 8MHz/10KHz
                    bset      PORTB,Y,$10         ; LATCH IT

;*******************************************************************************
; Serial output routine to the MC145157
;*******************************************************************************

P5157               proc
                    lda       SMEM                ; TRANSFER SMEM AND
                    lsla                          ; MEM+1 TO TEMPORARY
                    sta       W4                  ; LOCATIONS AND MOVE
                    lda       SMEM+1              ; UP ONE BIT TO INCLUDE
                    rola                          ; THE 5157 CONTROL BIT.
                    bsr       SQU7                ; SEND MSBYTE (7 BITS)
                    lda       W4                  ; AND LSBYTE OF
                    bsr       SQU8                ; NEW FREQUENCY
                    bset      PORTB,Y,$08         ; LATCH
                    bclr      PORTB,Y,$08         ; IT
                    lda       #$4E                ; SEND 15 BIT (14+1)
                    bsr       SQU7                ; REFERENCE
                    lda       #$21                ; DIVIDE RATIO
                    bsr       SQU8
                    bset      PORTB,Y,$08         ; LATCH IT
                    bclr      PORTB,Y,$08         ; ALL LOW (5157/70 SWITCHED OFF)
                    rts

;*******************************************************************************
; Subroutines for the MC145157/170
;*******************************************************************************

SQU8I               proc
                    ldb       #8                  ; SEND 8 BITS
                    bra       S1I

SQU7I               lsla                          ; MOVE OUT MS BIT
                    ldb       #7                  ; AND SEND OTHER 7
S1I                 lsla                          ; MOVE I BIT INTO "C"
                    bcc       S2I                 ; ZERO ?
                    bset      PORTB,Y,$02         ; NO
S2I                 bset      PORTB,Y,$01         ; CLOCK
                    bclr      PORTB,Y,$01         ; IT
                    bclr      PORTB,Y,$02
                    decb
                    bne       S1I                 ; ANY MORE ?
                    rts

SQU8                ldb       #8                  ; SEND 8 BITS
                    bra       S1

SQU7                lsla                          ; MOVE OUT MS BIT
                    ldb       #7                  ; AND SEND OTHER 7
S1                  lsla                          ; MOVE 1 BIT INTO "C"
                    bcc       S2                  ; ZERO ?
                    bset      PORTB,Y,$02         ; NO
S2                  bclr      PORTB,Y,$01         ; CLOCK
                    bset      PORTB,Y,$01         ; IT
                    bclr      PORTB,Y,$02
                    decb
                    bne       S1                  ; ANY MORE ?
                    rts

;*******************************************************************************
; Toggle 9/10 KHz step (MW)
;*******************************************************************************

T910                proc
                    brset     STAT6,$40,CBH
                    bset      STAT6,$40
                    rts

CBH                 bclr      STAT6,$40
                    rts

;*******************************************************************************
;
; LINK batch files (RLE.BAT & RDE.LD) and PCBUG11 Vectors.
;
; ILD11 RADE.O FNCE.O RDSE.O -MKUF E32.MAP -G RDE -O RDE.OUT
; IHEX RDE.OUT -O RDE.0
; TYPE E32.MAP
;
; section .RAM1 BSS origin 0x0000
; section .RAM2 BSS origin 0x0100
; section .RAM3 BSS origin 0x0200           E32
; section .ROM1 origin 0xD000             $9000
; section .ROM2 origin 0xE000             $9c00
; section .ROM3 origin 0xF000             $A000
; section .VECT origin 0xBFC1               -
; section .VECT2 origin 0xFFD6            ($FFD6)
;
;*******************************************************************************

;                   #VECTORS  $BFC1               ; SECTION .VECT
;
;                   jmp       START               ; SCI
;                   jmp       START               ; SPI
;                   jmp       START               ; PULSE ACCUMULATOR EDGE
;                   jmp       START               ;   "      "        OVER
;                   jmp       START               ; TIMER OVER
;                   jmp       START               ; "     IC4/OC5
;                   jmp       START               ; "     OC4
;                   jmp       START               ; "     OC3
;                   jmp       START               ; "     OC2
;                   jmp       START               ; "     OC1
;                   jmp       START               ; "     IC3
;                   jmp       START               ; "     IC2
;                   jmp       START               ; "     IC1
;                   jmp       TINTB               ; RTI
;                   jmp       SDATA               ; IRQ
;                   jmp       SHAFTX              ; NOT USED, XIRQ USED BY PCbug11
;                   jmp       START               ; SWI
;                   jmp       START               ; ILLEGAL OP CODE
;                   jmp       START               ; COP
;                   jmp       START               ; CLOCK MONITOR
;                   jmp       START               ; RESET

;*******************************************************************************
                    #VECTORS  $FFD6               ; MC68HC11E32 Vectors
;*******************************************************************************

                    dw        Start               ; SCI
                    dw        Start               ; SPI
                    dw        Start               ; PULSE ACCUMULATOR EDGE
                    dw        Start               ; "      " OVER
                    dw        Start               ; TIMER OVER
                    dw        Start               ; "     IC4/OC5
                    dw        Start               ; "     OC4
                    dw        Start               ; "     OC3
                    dw        Start               ; "     OC2
                    dw        Start               ; "     OC1
                    dw        Start               ; "     IC3
                    dw        Start               ; "     IC2
                    dw        Start               ; "     IC1
                    dw        TINTB               ; RTI
                    dw        SDATA               ; IRQ
                    dw        SHAFTX              ; XIRQ
                    dw        Start               ; SWI
                    dw        Start               ; ILLEGAL OP CODE
                    dw        Start               ; COP
                    dw        Start               ; CLOCK MONITOR
                    dw        Start               ; RESET
