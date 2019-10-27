;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;         MC68HC11K4/E2 Longwave RD Clock
;
;              P. Topping 21st August '95
;                       AN1597
;
; (Typed from AN1597.PDF and corrected syntax errors
; by Tony Papadimitriou to assemble with ASM11)
;*******************************************************************************

HC11                def       2                   ; 2 FOR E2, 4 FOR K4

REGS                equ       $1000
PORTA               equ       $00                 ; PORT A ADDRESS
PORTB               equ       $04                 ; B
PORTC               equ       $03                 ; C
PORTD               equ       $08                 ; D
PORTE               equ       $0A                 ; E

PORTCD              equ       $07                 ; PORT C DATA DIRECTION REG.
PORTDD              equ       $09                 ; D

TCNT                equ       $0E
TMSK2               equ       $24
PACTL               equ       $26
SPCR                equ       $28
ADCTL               equ       $30
ADR1                equ       $31
ADR2                equ       $32
ADR4                equ       $34
OPTION              equ       $39
INIT                equ       $3D

RBO                 equ       $1000               ; REGISTER BLOCK OFFSET

;*******************************************************************************
;                   RAM allocation  status flags
;*******************************************************************************

                    org       $0000

stat1               rmb       1                   ; 0: VALID CRC
                                                  ; 3: UPDATE DISPLAY
                                                  ; 6: DATE VALID
                                                  ; 7: LWRD DATA BIT
stat2               rmb       1                   ; 0: DISPLAY TRANSIENT
                                                  ; 1: SLEEP TIMER RUNNING
                                                  ; 2: SLEEP DISPLAY
                                                  ; 3: ALARM DISPLAY
                                                  ; 4: ALARM ARMED
                                                  ; 5: ALARM SETUP
                                                  ; 6: ALARM HOURS (SETUP)
                                                  ; 7: ALTERNATIVE DISPLAY

;*******************************************************************************
;                        RAM allocation
;*******************************************************************************

DOY                 rmb       2                   ; DAY OF YEAR
MNTH                rmb       1                   ; MONTH
DOM                 rmb       2                   ; DAY OF MONTH
DOW                 rmb       2                   ; DAY OF WEEK
WEEK                rmb       1                   ; WEEK NUMBER
SDAY                rmb       2                   ; YEAR START DAY
TYPE                rmb       1                   ; YEAR TYPE (LEAP)
OFSET               rmb       1                   ; LOCAL OFFET
DAT                 rmb       7                   ; SERIAL DATA BUFFER
BLOCK               rmb       4                   ; BLOCK DATA
BIT                 rmb       1                   ; BIT LEVEL
CONF                rmb       1                   ; CRC CONFIDENCE
QSEC                rmb       1                   ; QUARTER SECONDS
TH8                 rmb       1                   ; EIGHTHS OF SECONDS
M9                  rmb       1                   ; 9 MINUTE COUNTER
CODE                rmb       1                   ; BLOCK CODE
MIN                 rmb       1                   ; MINUTES
OUR                 rmb       1                   ; HOURS
DMIN                rmb       1                   ; DISPLAYED MINUTES
DOUR                rmb       1                   ; DISPLAYED HOURS
DDOW                rmb       2                   ; DISPLAYED DAY OF WEEK
DWEEK               rmb       1                   ; DISPLAYED WEEK NUMBER
TMIN                rmb       1                   ; TEMPORARY MINUTES
TOUR                rmb       1                   ; TEMPORARY HOURS
TDOW                rmb       1                   ; TEMPORARY DAY OF WEEK
TWEEK               rmb       1                   ; TEMPORARY WEEK NUMBER
SUP                 rmb       1                   ; VDD (A/D RESULT)
RSSI                rmb       1                   ; SIGNAL LEVEL (A/D RESULT)
RSS4                rmb       1                   ; AVERAGED SIGNAL LEVEL
DISP                rmb       16                  ; LCD MODULE BUFFER
DSPOLD              rmb       16                  ; LCD MODULE PREVIOUS DATA
W1                  rmb       2                   ; \
W2                  rmb       2                   ; > USED IN INTERRUPT
W3                  rmb       2                   ; /
TEMP                rmb       3
DIST                rmb       1                   ; TRANSIENT DISPLAY TIMEOUT COUNTER
SLEPT               rmb       1                   ; SLEEP TIMER MINUTES COUNTER
AMIN                rmb       1                   ; ALARM MINUTES
AOUR                rmb       1                   ; ALARM HOURS
KEY                 rmb       1                   ; CODE OF PRESSED KEY
KOUNT               rmb       1                   ; KEYBOARD COUNTER
ADIS                rmb       1                   ; ALTERNATIVE DISPLAY TYPE

                    org       $F800               ; .ROM1

;*******************************************************************************
;                     Reset & initialisation
;*******************************************************************************

Start               proc
                    lds       #$00FF              ; INITIALISE STACK POINTER
                    lda       #$40                ; ENABLE REAL TIME INTERRUPTS
                    sta       TMSK2+RBO
                    lda       #$B0                ; IRQ EDGE SENSETIVE, A/D ON
                    sta       OPTION+RBO
                    lda       #$0B                ; 133072 us WITH A 2.0 MHz XTAL
                    sta       PACTL+RBO
                    lda       #$34                ; ENABLE CONTINUOUS A/D
                    sta       ADCTL+RBO
                    ldy       #$1000

                    ldd       #$003C              ; 0,1: SCI (PCBUG11), 24: not used
                    std       PORTD,y             ; 5: CONTROL OUTPUT
                                                  ; PORTE 4: BATTERY (A/D), 5: TUNING (A/D)
                                                  ; PORTE 7: KEYBOARD INHIBIT,0,1,2,3,6: not used
LCDB                equ       PORTC
LCDBDD              equ       PORTCD

                    lda       #$FF
                    sta       LCDBDD,y
LCDC                equ       PORTB
                    clr       LCDC,y              ; LCD CONTROL BITS: 5(RS), 6(R/W),7(E)
KEYP                equ       PORTA               ; 0,1: KEY INS, 3,4: KEY OUTS
R1                  equ       $08                 ; ROW 1 BIT 3
R2                  equ       $10                 ; ROW 2 BIT 4
KINS                equ       $03

                    jsr       CLOCK3              ; INITIALISE LCD
                    ldb       #5
Loop@@              jsr       CLRAM               ; CLEAR RAM
                    decb                          ; 5 TIMES TO PROVIDE A 5mS DELAY
                    bne       Loop@@              ; FOR LCD INITIALISATION
                    jsr       CLOCK3              ; INITIALISE LCD
                    jsr       CLRAM               ; 1mS DELAY FOR LCD
                    jsr       NEXTD               ; START WITH CONFIDENCE

                    jsr       CLOCK3              ; INITIALISE LCD
                    jsr       WAIT
                    lda       #$0C                ; SWITCH DISPLAY ON
                    jsr       CLOCK               ; LATCH IT
                    cli                           ; ENABLE IRQ
;                   bra       IDLE

;*******************************************************************************
;                        Idle loop
;*******************************************************************************

IDLE                proc
                    brclr     TCNT,y,$1F,_1@@     ; 64 Hz
                    bra       IDLE

_1@@                brclr     stat2,$01,_2@@      ; DISPLAY TRANSIENT?
                    lda       DIST
                    bne       _2@@                ; YES, TIMED OUT?
                    jsr       CLTR

_2@@                brclr     stat1,$08,Scan@@    ; DISPLAY UPDATE REQUIRED?
                    jsr       MOD                 ; YES, DO IT (4 Hz)
                    bclr      stat1,$08           ; AND CLEAR FLAG

Scan@@              brclr     stat2,$10,CHSLP@@   ; ALARM ARMED?
                    ldd       DMIN                ; YES, COMPARE TIME WITH ALARM TIME
                    inca                          ; ADD A MINUTE
                    cmpa      #60                 ; NEXT HOUR?
                    bne       ITOK@@
                    clra                          ; YES, CLEAR MINUTES
                    incb                          ; AND INCREMENT HOURS
                    cmpb      #24                 ; NEXT DAY?
                    bne       ITOK@@
                    clrb                          ; YES, CLEAR HOURS
ITOK@@              cpd       AMIN                ; ALARM TIME
                    bne       CHSLP@@             ; SAME?

                    lda       QSEC                ; WAKEUP TWO SECONDS EARLY
                    cmpa      #218
                    bne       CHSLP@@             ; TO PREVENT SWITCHOFF LOCKOUT
                    bclr      PORTD,y,$20         ; YES, SWITCH ON
                    jsr       INSLP               ; START SLEEP TIMER
                    inc       SLEPT               ; 61 TO COMPENSATE FOR IMMEDIATE DECREMENT
CHSLP@@             brclr     stat2,$02,FLN@@     ; SLEEP TIMER RUNNING?
                    lda       SLEPT               ; YES
                    bne       FLN@@               ; TIME TO FINISH?
                    bclr      stat2,$02           ; YES, CLEAR FLAG
                    bset      PORTD,y,$20         ; AND SWITCH OFF
FLN@@               brclr     ADR4,y,$80,SKBD@@   ; KEYBOARD ENABLED?
                    bsr       KBD                 ; YES, READ KEYBOARD
SKBD@@              bra       IDLE

;*******************************************************************************
;                      Keyboard routine
;*******************************************************************************

KBD                 proc
                    bset      KEYP,y,R1           ; ROW 1
                    bclr      KEYP,y,R2
                    lda       KEYP,y              ; READ KEYBOARD
                    bita      #KINS               ; ANY INPUT LINE HIGH?
                    bne       _1@@
                    bset      KEYP,y,R2           ; ROW 2
                    bclr      KEYP,y,R1
                    lda       KEYP,y              ; READ KEYBOARD
                    bita      #KINS               ; ANY INPUT LINE HIGH?
                    bne       _1@@
                    clr       KEY                 ; NO KEY PRESSED
                    rts

_1@@                anda      #$1B
                    cmpa      KEY                 ; SAME AS LAST TIME?
                    beq       _2@@
                    sta       KEY                 ; NO, SAVE THIS KEY
                    clr       KOUNT
_2@@                inc       KOUNT               ; YES, THE SAME
                    lda       KOUNT
                    cmpa      #3                  ; 3 THE SAME?
                    bne       Done@@              ; IF 3 THEN PERFORM KEY FUNCTION

                    lda       KEY
                    suba      #9                  ; SLEEP ($09)?
                    beq       SLEEP
                    deca                          ; NO, DISPLAY CONTROL($0A)?
                    beq       DCK
                    suba      #7                  ; NO, ALARM ($11)?
                    beq       ALARM
                    deca                          ; NO, ON/OFF ($12)?
                    bne       Done@@              ; IF NOT DO NOTHING
;                   bra       ONOFF
Done@@              equ       :AnRTS

;*******************************************************************************
;                         On/off key
;*******************************************************************************

ONOFF               proc
                    brclr     stat2,$08,NOTALR    ; ALARM DISPLAY?
                    brclr     stat2,$10,NOTALR    ; YES, ALARM ARMED?
                    brset     stat2,$20,InSetup@@ ; YES, ALREADY SETUP MODE?
                    bset      stat2,$60           ; NO, ENTER SETUP MODE WITH HOURS

A5SD                lda       #40                 ; 10 SECOND TIMEOUT
                    bra       TRAN

InSetup@@           brset     stat2,$40,Min@@     ; SETUP HOURS?
                    bclr      stat2,$20           ; NO, CANCELL SETUP
                    bra       A5SD

Min@@               bclr      stat2,$40           ; YES, MAKE IT MINUTES
                    bra       A5SD

;*******************************************************************************

NOTALR              proc
                    bsr       CLTR
                    bclr      stat2,$02           ; CANCEL SLEEP TIMER
                    brclr     PORTD,y,$20,On@@    ; ON?
SODM                bclr      PORTD,y,$20         ; NO, SWITCH ON
                    rts

On@@                bset      PORTD,y,$20         ; YES, SWITCH OFF
                    rts

;*******************************************************************************

CLTR                proc
                    bclr      stat2,$AD           ; CLEAR DISPLAY FLAGS (TRANSIENT, ALARM, ALT. DISPLAY, SLEEP)
                    clr       ADIS
                    rts

;*******************************************************************************
;                      Alarm key
;*******************************************************************************

ALARM               proc
                    brclr     stat2,$08,On@@      ; ALARM DISPLAY?
                    brclr     stat2,$10,Off@@     ; YES, ALARM ON?
                    bclr      stat2,$10           ; YES, SWITCH OFF
                    bra       Update@@

Off@@               bset      stat2,$10           ; NO, SWITCH ON
                    bra       Update@@

On@@                bsr       CLTR                ; NO,START ALARM DISPLAY
                    bset      stat2,$08           ; ALARM DISPLAY FLAG
Update@@            bclr      stat2,$20           ; CANCEL SETUP
;                   bra       T25

;*******************************************************************************

T25                 proc
                    lda       #12                 ; 3 SECONDS TIMEOUT
TRAN                sta       DIST
                    bset      stat2,$01           ; SET DISPLAY TRANSIENT FLAG
                    rts

;*******************************************************************************
;                   Alternative displays key
;*******************************************************************************

DCK                 proc
                    brset     stat2,$20,PINC      ; ALARM SETUP?
;                   bra       NEXTD

;*******************************************************************************

NEXTD               proc
                    bset      stat2,$80           ; NO, SET ALTERNATIVE DISPLAY FLAG AND
                    bclr      stat2,$2D           ; CLEAR OTHER DISPLAY FLAGS
                    inc       ADIS                ; INCREMENT DISPLAY TYPE
                    lda       ADIS
                    cmpa      #4                  ; TOO FAR?
                    beq       CLTR                ; IF SO BACK TO ZERO
                    rts

;*******************************************************************************
;                        Sleep key
;*******************************************************************************

SLEEP               proc
                    brset     stat2,$20,PDEC      ; ALARM SETUP?

                    brset     stat2,$04,?DECS     ; NO, ALREADY SLEEP DISPLAY?
                    brset     stat2,$02,?STR2     ; NO, SLEEP TIMER ALREADY RUNNING?
;                   bra       INSLP

;*******************************************************************************

INSLP               proc
Loop@@              lda       #60                 ; NO, INITIALISE SLEEP TIMER
                    sta       SLEPT
                    bset      stat2,$02           ; START SLEEP TIMER
?STR2               bsr       CLTR                ; YES, CLEAR DISPLAY TRANSIENTS
                    bset      stat2,$04           ; SLEEP DISPLAY
                    bra       SLPTOK@@            ; NO DECREMENT IF FIRST TIME

?DECS               lda       SLEPT               ; DECREMENT SLEEP TIMER
                    suba      #5
                    sta       SLEPT
                    bmi       Loop@@              ; IF UNDERFLOW, WRAP ROUND TO 60
SLPTOK@@            bsr       T25
                    bra       SODM

;*******************************************************************************
;                     Increment alarm time
;*******************************************************************************

PINC                proc
                    brset     stat2,$40,_1@@      ; SETUP HOURS?
                    lda       AMIN
                    inca
                    cmpa      #59
                    ble       MinOK@@
                    clra
MinOK@@             sta       AMIN
                    bra       T5S                 ; 10 SECOND TIMEOUT

_1@@                lda       AOUR
                    inca
                    cmpa      #23
                    ble       _2@@
                    clra
_2@@                sta       AOUR
T5S                 jmp       A5SD                ; 10 SECOND TIMEOUT

;*******************************************************************************
;                   Decrement alarm time
;*******************************************************************************

PDEC                proc
                    brset     stat2,$40,_1@@      ; SETUP HOURS?
                    dec       AMIN
                    bpl       T5S
                    lda       #59
                    sta       AMIN
                    bra       T5S
_1@@                dec       AOUR
                    bpl       T5S
                    lda       #23
                    sta       AOUR
                    bra       T5S

;*******************************************************************************
;                 Timer interrupt routine
;*******************************************************************************

TINTB               proc
                    ldy       #REGS
                    bclr      $25,y,$BF           ; CLEAR RTI INTERRUPT FLAG
                    inc       TH8                 ; EIGTHS OF SECONDS
                    lda       TH8
                    cmpa      #2                  ; QUARTER SECOND?
                    beq       Quart@@
Done@@              rti

Quart@@             clr       TH8
                    dec       DIST                ; DECREMENT TRANSIENT DISPLAY TIMER
                    bset      stat1,$08           ; UPDATE DISPLAY
          ;--------------------------------------
          ; Update clock
          ;--------------------------------------
                    inc       QSEC                ; UPDATE QUARTER SECONDS
                    ldb       QSEC
                    lda       M9                  ; 9 MINUTE COUNTER
                    bne       _1@@                ; TIME TO COMPENSATE FOR 2.000 MHz CRYSTAL?
                    cmpb      #228                ; YES, 228 QUARTER SECONDS A MINUTE
                    bra       _2@@

_1@@                cmpb      #229                ; NO, 229 (DIVIDE RATIO=2x228.888)
_2@@                bne       Done@@              ; IE 457.778, 457.778x131.072=60.00185 sec/min)
                    clr       QSEC                ; IF 228 OR 229 THEN CLEAR SECONDS
                    inc       MIN                 ; AND UPDATE MINUTES
                    dec       SLEPT               ; AND SLEEP TIMER
                    lda       M9                  ; AND 9 MINUTE COUNTER
                    inca
                    cmpa      #9
                    bne       _3@@                ; TENTH MINUTE FINISHED?
                    clra                          ; YES, START AGAIN
_3@@                sta       M9

                    lda       MIN
                    cmpa      #60
                    bne       NOTC                ; PAST 59?
                    clr       MIN                 ; YES, CLEAR
                    inc       OUR                 ; UPDATE HOURS
                    lda       OUR
                    cmpa      #24
                    bne       NOTC                ; PAST 23?
                    clr       OUR                 ; YES CLEAR
;                   bra       TEST1

;*******************************************************************************
;                        Update date
;*******************************************************************************

TEST1               proc
                    inc       DOW+1               ; NEXT DAY
                    lda       DOW+1
                    cmpa      #7                  ; PAST SUNDAY?
                    bls       NOTC
                    ldb       #1                  ; YES, BACK TO MONDAY
                    stb       DOW+1
TEST2               inc       WEEK                ; INCREMENT WEEK NUMBER
                    lda       WEEK

                    ldb       SDAY+1
                    cmpb      #4                  ; 1st JANUARY WAS A THURSDAY?
                    beq       W53                 ; IF SO, 53 WEEKS
                    cmpb      #3                  ; WEDNESDAY?
                    bne       W52                 ; NEITHER WED NOR THU SO 52 WEEKS
                    tst       TYPE                ; WED., BUT IS IT LEAP?
                    bne       W52                 ; IF NOT THEN ONLY 52
W53                 cmpa      #53                 ; (THU.) OR (WED. & LEAP) SO 53 WEEKS
                    bra       TWN

W52                 cmpa      #52                 ; ELSE, 52 WEEKS
TWN                 bls       NOTC                ; TOO BIG?
                    ldb       #1                  ; YES, BACK TO 1

                    stb       WEEK
                    inc       SDAY+1
                    lda       TYPE                ; IF LEAP THEN START DAY INCREASES BY 2
                    bne       CSD
                    inc       SDAY+1              ; SO INCREMENT AGAIN
CSD                 ldd       SDAY
                    cmpb      #7                  ; UPDATED START DAY TO BIG?
                    bls       NOV2
                    subb      #7                  ; YES, CORRECT
                    std       SDAY
NOV2                lda       TYPE                ; YEAR TYPE
                    inca
                    anda      #$03                ; IF 4, BACK TO 0
                    sta       TYPE

NOTC                jsr       CDATE
                    rti

;*******************************************************************************
;                   LW RD input interrupt (IRQ).
;                     Get bits from this edge.
;*******************************************************************************

B5                  fcb       $F5,$1C,$1F,$05,$3E,$0A
B4                  long      $7C140D14,$EF142B15,$A316B311,$931FD303
B3                  long      $A6074C0F,$981EC501,$8A031407,$280E501C
B2                  long      $5504AA08,$54115D1E,$4F009E00,$3C017802
B1                  long      $F004E009,$C013751B,$1F0A3E14,$8914E715

QBP                 equ       20                  ; MSB COUNTS FOR 10 ms (BIT PERIOD/4)

SDATA               proc
                    ldd       TCNT+RBO            ; READ TIMER
                    std       W1                  ; SAVE IT
                    subd      W2                  ; SUBTRACT PREVIOUS
                    std       W3                  ; AND SAVE DELTA
                    cmpa      #2*QBP              ; OVER 20 ms?
                    blo       LT20
                    ldd       W1                  ; YES, UPDATE PREVIOUS WITH CURRENT TIME
                    std       W2
                    ldd       W3                  ; RELOAD DELTA
                    cmpa      #5*QBP              ; 2 HALF BITS?
                    bhs       NT2HB
INBIT               bsr       BITIN               ; YES, REPEAT LAST BIT
LT20                rti

NT2HB               cmpa      #7*QBP              ; 3 HALF BITS?
                    bhs       NOT3HB
                    brset     stat1,$80,WAS1      ; YES, LAST BIT A 1?
IN1                 bsr       BITIN1              ; NO, MAKE THIS ONE A 1
                    rti

WAS1                bclr      stat1,$80           ; YES, ENTER TWO 0s
                    bsr       BITIN
                    bra       INBIT

NOT3HB              cmpa      #10*QBP             ; 4 HALF BITS?
                    bhs       ILL                 ; NO, TOO BIG
                    brclr     stat1,$80,ILL       ; YES, BUT WAS LAST BIT A 0?
                    bclr      stat1,$80           ; NO, ENTER A 0 AND A 1
                    bsr       BITIN
                    bra       IN1

ILL                 brclr     stat1,$80,B0        ; ILLEGAL, TRY INVERTING CURRENT BIT
                    bclr      stat1,$80
                    bra       FINV

B0                  bset      stat1,$80
FINV                bclr      stat1,$01           ; AND FORCE RESYNC
                    rti

;*******************************************************************************
;                   Shift in bit and calculate CRC
;*******************************************************************************

MULT                proc
                    sta       TEMP
                    lda       #8
                    sta       TEMP+1
                    lda       TEMP+2
Loop@@              ror       TEMP
                    bcc       Cont@@
                    eora      ,x
                    eorb      1,x
Cont@@              inx:2
                    dec       TEMP+1
                    bne       Loop@@
                    sta       TEMP+2
                    rts

;*******************************************************************************

BITIN1              proc
                    bset      stat1,$80           ; FORCE TO 1
;                   bra       BITIN

;*******************************************************************************

BITIN               proc
                    lda       stat1
                    rola                          ; PUT stat1 $80 BIT INTO C BIT
                    rol       DAT+6               ; MOVE ALL (50) BITS UP
                    jsr       SHFT
                    brclr     stat1,$01,TRY2      ; BIT BY BIT CHECK?
                    dec       BIT                 ; NO, WAIT FOR BIT 50
                    bne       Done@@              ; THIS TIME?
TRY1                lda       #50                 ; YES, RELOAD BIT COUNTER
                    sta       BIT
TRY2                brclr     DAT,$02,NOTV        ; PREBIT SHOULD BE A 1
                    lda       DAT+6               ; LSB
                    ldb       DAT+5               ; MSB (5 BITS)
                    andb      #$1F
                    sta       TEMP+2
                    lda       DAT+5
                    anda      #$E0
                    ldx       #$B510              ; OFFSET FOR MISSING MATRIX ENTRIES (WAS: B510)
                    bsr       MULT
                    lda       DAT+4
                    bsr       MULT
                    lda       DAT+3
                    bsr       MULT
                    lda       DAT+2
                    bsr       MULT
                    lda       DAT+1
                    bsr       MULT
                    brclr     DAT+0,$01,FIN
                    eora      #$3B
                    eorb      #$17
;                   bra       FIN
Done@@              equ       :AnRTS

;*******************************************************************************
;                 CRC check and confidence handling
;*******************************************************************************

FIN                 proc
                    cpd       #0
                    beq       VALID
NOTV                lda       CONF
                    cmpa      #$0F                ; CONFIDENCE 15?
                    beq       DecConf@@
                    bclr      stat1,$01           ; NO, BIT BY BIT CRC CHECK
                    tsta
                    beq       Done@@              ; CONFIDENCE ZERO?
                    dec       BIT
                    bne       Done@@              ; USE BIT COUNTER TO SLOW CONFIDENCE
                    lda       #15                 ; DROP DURING BIT BY BIT ATTEMPT TO
                    sta       BIT                 ; RESYNCRONISE
DecConf@@           dec       CONF
Done@@              rts

;*******************************************************************************

VALID               proc
                    bset      stat1,$01
                    lda       CONF
                    cmpa      #14
                    bhi       _1@@
                    inca
                    sta       CONF
_1@@                lda       #50
                    sta       BIT
                    bsr       SHFT
                    bsr       SHFT
                    bsr       SHFT
                    ldd       DAT+1
                    std       BLOCK
                    ldd       DAT+3
                    std       BLOCK+2
                    lda       DAT
                    anda      #$0F
                    sta       CODE
                    bne       Done@@              ; BLOCK 0?
                    brclr     BLOCK,$80,TIME      ; BLOCK 0, TIME?
Done@@              rts

;*******************************************************************************

SHFT                proc
                    rol       DAT+5
                    rol       DAT+4
                    rol       DAT+3
                    rol       DAT+2
                    rol       DAT+1
                    rol       DAT
                    rts

;*******************************************************************************
;                      Process time block
;*******************************************************************************

TIME                proc
                    ldd       BLOCK+2
                    lsld:2
                    anda      #$3F
                    cmpa      #59
                    bhi       Done@@              ; OVER 59?
                    sta       TMIN                ; NO, MINUTES OK
                    ldd       BLOCK+1
                    lsrd
                    lsrb:3
                    cmpb      #23
                    bhi       Done@@              ; OVER 23?
                    stb       TOUR                ; NO, HOURS OK
                    lda       BLOCK+1
                    rora
                    anda      #$07
                    beq       Done@@              ; ZERO?
                    sta       TDOW                ; NO, DAYOFWEEK OK
                    ldd       BLOCK
                    lsrd:2
                    lsrb:2
                    beq       Done@@              ; ZERO?
                    cmpb      #53                 ; NO, OVER 53?
                    bhi       Done@@              ; NO, WEEK NUMBER OK
                    stb       TWEEK
                    anda      #$07
                    beq       Done@@              ; YEAR START DAY ZERO?
                    sta       SDAY+1              ; NO, OK
                    ldb       BLOCK
                    clra
                    lsld:3
                    sta       TYPE                ; YEAR TYPE (LEAP)
                    lda       BLOCK+3
                    anda      #$3F
                    sta       OFSET               ; LOCAL TIM OFFSET
                    clr       QSEC
                    clr       TH8
                    ldd       TDOW                ; UPDATE DOW & WEEK
                    std       DOW+1
                    ldd       TMIN                ; UPDATE MINUTE & HOUR
                    std       MIN
                    brset     stat1,$40,CDATE     ; DATE ALREADY VALID?
                    jsr       NOTALR              ; NO, FIRST TIME, STANDBY
                    bset      stat1,$40           ; AND SET FLAG
;                   bra       CDATE
Done@@              equ       :AnRTS

;*******************************************************************************
;                      Calculate offset
;*******************************************************************************

CDATE               proc
                    ldd       MIN                 ; XFER MINUTES AND HOURS
                    std       DMIN
                    lda       WEEK                ; XFER WEEK NUMBER
                    std       DWEEK
                    ldd       DOW                 ; XFER DAYOFWEEK
                    std       DDOW
;                   bra       LOCAL

;*******************************************************************************
;              Local time difference adjustment (neg.)
;*******************************************************************************

LOCAL               proc
                    ldb       OFSET               ; CHECK FOR OFFSET
                    brclr     OFSET,$20,POS       ; POSITIVE?
                    negb                          ; NO, NEGATIVE HOURS IN B
                    lsrb
                    orb       #$F0                ; MS BITS TO 1s
                    bsr       HALF                ; HALF HOUR ADJUSTMENT
                    addb      DOUR                ; HOUR OFFSET, MINUS UTC HOURS
                    bcs       ZOM@@               ; OVERFLOW?
                    addb      #24                 ; NO, ADD 24 HOURS
                    lda       DDOW+1
                    deca                          ; AND GO BACK A DAY
                    bne       Ok@@                ; WAS MONDAY?
                    dec       DWEEK               ; YES, LAST WEEK
                    lda       #7                  ; SUNDAY
Ok@@                sta       DDOW+1
ZOM@@               andb      #$1F
                    bra       TFIN

;*******************************************************************************

HALF                proc
                    bcc       Done@@              ; 1/2 HOUR?
                    lda       DMIN                ; YES
                    adda      #30                 ; ADD 30 MINUTES
                    cmpa      #59
                    bls       Save@@              ; OVERFLOW?
                    suba      #60                 ; YES, SUBTRACT 60 MINUTES
                    inc       DOUR                ; AND ADD 1 HOUR
Save@@              sta       DMIN
Done@@              rts

;*******************************************************************************
;                Local time difference adjustment (pos)
;*******************************************************************************

POS                 proc
                    lsrb                          ; HOURS IN B
                    bsr       HALF                ; HALF HOUR ADJUSTMENT
                    addb      DOUR                ; HOUR OFFSET, ADD UTC HOURS
                    cmpb      #23
                    bls       TFIN                ; OVERFLOW?
                    subb      #24                 ; YES, SUBTRACT 24 HOURS
                    lda       DDOW+1
                    inca                          ; AND INCREMENT DAYOFWEEK
                    cmpa      #7
                    ble       Ok@@                ; WAS SUNDAY?
                    inc       DWEEK               ; YES, NEXT WEEK
                    lda       #1                  ; MONDAY
Ok@@                sta       DDOW+1
;                   bra       TFIN

;*******************************************************************************

TFIN                proc
                    stb       DOUR
;                   bra       DATE

;*******************************************************************************
;                Calculate date (month & dayofmonth)
;*******************************************************************************

DATE                proc
                    lda       DWEEK               ; WEEK NUMBER ADJUSTED FOR LOCAL OFFSET
                    ldb       SDAY+1              ; CAN BE 0 OR 54 (53 IN A 52 WEEK YEAR)
                    cmpb      #5                  ; IF 1st JAN/31st DEC RANSITION CAUSED
                    blo       WNOK@@              ; BY OFFSET
                    inca                          ; ADJUST WEEK FOR YEARSTARTDAYOFWEEK
WNOK@@              ldb       #7                  ; AND MULTIPLY BY 7 TO GET DAYOFYEAR
                    mul
                    addd      DDOW                ; ADD CURRENT DAYOFWEEK
                    addd      #55                 ; START AT 1st NOV (PREVIOUS YEAR)
                    subd      SDAY                ; SUBTRACT YEAR START DAYOFWEEK
                    std       DOY                 ; SAVE DAYOFYEAR (DEBUG)
                    brclr     stat1,$40,Done@@    ; DATE VALID?
                    clr       MNTH                ; MONTH=0: NOVEMBER (PREVIOUS YEAR)
                    ldx       #Table@@
                    tst       TYPE
                    bne       Loop@@              ; LEAP YEAR?
                    ldx       #Table@@+24         ; YES, USE SECOND TABLE
Loop@@              inc       MNTH
                    subd      ,x
                    inx
                    inx
                    cpd       ,x
                    bhi       Loop@@
                    std       DOM
Done@@              rts

Table@@             fdb       30,31               ; NOVEMBER, DECEMBER
                    fdb       31,28,31,30,31,30,31,31,30,31,30,31  ; JANUARYDECEMBER
                    fdb       31,29,31,30,31,30,31,31,30,31,30,31  ; JANUARYDECEMBER (LEAP)
                    fdb       31                  ; JANUARY

;*******************************************************************************
;                      Display type selection
;*******************************************************************************

MOD                 proc
                    brset     stat2,$04,Sleep@@   ; SLEEP DISPLAY?
                    brset     stat2,$08,Alarm@@   ; NO, ALARM DISPLAY?

                    brclr     stat2,$80,Normal@@  ; NO,ALTERNATIVE DISPLAYS?
                    lda       ADIS
                    deca
                    bne       _2@@
                    jsr       ALTD1               ; DATA & TIME DISPLAY
                    bra       Row@@

_2@@                deca
                    bne       _3@@
                    jsr       ALTD2               ; YEAR & WEEK DISPLAY
                    bra       Row@@

_3@@                deca
                    bne       Sleep@@
                    jsr       ALTD3               ; VDD & TUNING DISPLAY
                    bra       Row@@

Sleep@@             jsr       SLEEPD              ; SLEEP TIMER DISPLAY?
                    bra       Row@@

Normal@@            brset     PORTD,y,$20,StandBy@@ ; STANDBY?
                    bsr       NORMD               ; NORMAL DISPLAY
                    bra       Row@@

StandBy@@           bsr       STBYD               ; STANDBY DISPLAY
                    bra       Row@@

Alarm@@             jsr       ALRMD               ; ALARM DISPLAY

Row@@               ldx       #DISP
Loop@@              lda       ,x
                    cmpa      16,x                ; HAS CHARACTER CHANGED?
                    bne       DIFF
                    inx
                    cpx       #DISP+16
                    bne       Loop@@
                    rts

;*******************************************************************************

DIFF                proc
                    jsr       WAIT
                    lda       #$80                ; ADDRESS DISPLAY RAM
                    jsr       CLOCK               ; LATCH IT
                    ldx       #DISP
Loop@@              jsr       WAIT
                    bset      LCDC,y,$20          ; WRITE DATA
                    lda       ,x                  ; GET A BYTE
                    sta       16,x
                    jsr       CLOCK               ; SEND IT TO MODULE
                    inx
                    cpx       #DISP+16            ; DONE?
                    bne       Loop@@
                    rts                           ; REMOVE FOR /16 DISPLAY

;*******************************************************************************
;                   Additional code for /16 LCD modules
;                    (also change CLOCK3 to lda  #$38)
;*******************************************************************************
#ifdef
LCD16               proc
                    jsr       WAIT
                    lda       #$A8                ;ADDRESS 40
                    jsr       CLOCK               ;SEND IT TO MODULE
                    ldx       #DISP
Loop@@              jsr       WAIT
         #if HC11 = 2                             ;E2
                    bset      LCDC,y,$20          ;WRITE DATA
         #else                                    ;K4
                    bset      LCDC,y,$04          ;WRITE DATA
         #endif
                    lda       8,x                 ;GET A BYTE
                    jsr       CLOCK               ;SEND IT TO MODULE
                    inx
                    cpx       #DISP+8             ;DONE?
                    bne       Loop@@
                    rts
#endif
;*******************************************************************************
;               Normal and Standby (alarm armed) displays
;*******************************************************************************

NORMD               proc
                    bsr       STIME
                    jmp       DSUB1

;*******************************************************************************

ALRMA               proc
                    ldx       #ALARMS
                    jsr       XFER16
                    lda       AOUR                ; GET ALARM HOURS
                    jsr       CBCD
                    std       DISP
                    lda       AMIN
                    jsr       CBCD
                    std       DISP+2
;                   bra       STIME

;*******************************************************************************

STIME               proc
                    lda       DOUR                ; GET TIME
                    bsr       SUBSP
                    std       DISP+11
                    lda       DMIN
                    jsr       CBCD
                    std       DISP+14
                    lda       #$3A                ; 0.5 Hz FLASHING COLON
                    sta       DISP+13
                    rts

;*******************************************************************************

SUBSP               proc
                    jsr       CBCD
                    cmpa      #'0'                ; LEADING ZERO?
                    bne       Done@@
                    lda       #' '                ; YES, MAKE IT A SPACE
Done@@              rts

;*******************************************************************************
;                 Standby display (alarm not armed)
;*******************************************************************************

STBYD               proc
                    bsr       STIME
                    brset     stat2,$10,ALRMA     ; ALARM ARMED?
                    ldb       DDOW+1              ; NO, GET DAY OF WEEK
                    ldx       #DNAME              ; WAS: DNAME3
                    bsr       T3X                 ; AND CONVERT TO STRING
                    std       DISP
                    lda       2,x
                    sta       DISP+2

                    lda       #' '
                    sta       DISP+3
                    sta       DISP+6
                    sta       DISP+10
                    lda       DOM+1               ; DAY OF MONTH
                    bsr       SUBSP
                    std       DISP+4
                    ldb       MNTH                ; MONTH
                    ldx       #MNAME              ; WAS: MNAME3
                    bsr       T3X                 ; CONVERT TO STRING
                    std       DISP+7
                    lda       2,x
                    sta       DISP+9
                    rts

;*******************************************************************************

T3X                 proc
                    lda       #3
                    mul
                    abx
                    ldd       ,x
                    rts

;*******************************************************************************
;              LW data, confidence & seconds display
;*******************************************************************************

ALTD1               proc
                    lda       #' '
                    sta       DISP+11
                    sta       DISP+13
                    lda       CONF
                    jsr       SPLIT
                    stb       DISP+12
                    lda       QSEC                ; QSEC X 256 IN D
                    clrb
                    ldx       #978                ; SCALE FOR QSEC = 229
                    idiv                          ; TO BE JUST BELOW 60
                    xgdx
                    tba
                    jsr       CBCD
                    std       DISP+14
;                   bra       DSUB1

;*******************************************************************************

DSUB1               proc
                    ldd       #$2D20
                    stb       DISP+6
                    stb       DISP+10
                    brclr     stat2,$02,_1@@      ; SLEEP TIMER RUNNING?
                    ldb       #$2E                ; YES, . IN 2nd CHARACTER
_1@@                std       DISP
                    brclr     stat1,$01,SYNNV@@
                    lda       CODE
                    bne       _2@@                ; BLOCK 0?
                    brset     BLOCK,$80,_2@@      ; YES, TIME?
                    ldb       #'t'                ; YES
                    bra       SKSP@@

_2@@                jsr       SPLIT
SKSP@@              stb       DISP
SYNNV@@             lda       BLOCK
                    jsr       SPLIT
                    std       DISP+2
                    lda       BLOCK+1
                    jsr       SPLIT
                    std       DISP+4
                    lda       BLOCK+2

                    ldb       DISP+11             ; t 7D65 37C2 1:23
                    cmpb      #' '                ; ^
                    bne       MOVEIT              ; SPACE?
                    jsr       SPLIT               ; DIVIDE HEX DATA
                    std       DISP+7              ; INTO TWO BLOCKS
                    lda       BLOCK+3             ; OF FOUR IF THE
                    jsr       SPLIT               ; 12th CHARACTER
                    std       DISP+9              ; IS A SPACE
                    rts

;*******************************************************************************

MOVEIT              proc
                    jsr       SPLIT
                    std       DISP+6
                    lda       BLOCK+3
                    jsr       SPLIT
                    std       DISP+8
                    rts

;*******************************************************************************
;                   LW data year & week display
;*******************************************************************************


ALTD2               proc
                    ldx       #ALT2ST
                    jsr       XFER16
                    lda       TYPE                ; LEAP YEAR (CYCLE) TYPE
                    adda      #$30
                    sta       DISP+2
                    lda       SDAY+1              ; YEAR START DAY
                    adda      #$30
                    sta       DISP+4
                    lda       SDAY+1              ; IF 0 (NO TIME/DATE RECEIVED)
                    beq       ILLSD@@             ; THEN DON’T CALCULATE YEAR

                    ldb       TYPE
                    lda       #7                  ; CALCULATE OFFSET TABLE OFFSET
                    mul
                    addb      SDAY+1
                    decb
                    ldx       #Table@@
                    abx
                    ldb       ,x                  ; GET OFFSET FROM TABLE
                    lda       #19                 ; START AT 1995
                    pshb
                    cmpb      #4
                    bls       _1@@                ; 199x?
                    inca                          ; NO, 20xx
_1@@                jsr       CBCD
                    std       DISP+6              ; 19 OR 20 TO DISPLAY BUFFER
                    pula
                    adda      #95                 ; YEAR TENS AND UNITS
                    bsr       CBCD8               ; CONVERT TO ASCII BCD AND PUT INTO DISP+8 & +9
ILLSD@@             lda       DWEEK               ; WEEK NUMBER
                    jsr       CBCD
                    std       DISP+14
                    rts

Table@@             fcb       1,13,25,9,21,5,17   ; TABLE CONTAINING OFFSET RELATIVE TO
                    fcb       6,18,2,14,26,10,22  ; 1995 ACCORDING TO LEAP YEAR (CYCLE)
                    fcb       23,7,19,3,15,27,11  ; TYPE (03) DOWN TABLE AND YEAR START DAY
                    fcb       12,24,8,20,4,16,0   ; (17) ACROSS TABLE

;*******************************************************************************
;                        Alarm display
;*******************************************************************************


ALRMD               proc
                    ldx       #ALARMS
                    bsr       XFER16
                    brclr     stat2,$10,Done@@    ; ALARM ARMED?
                    lda       #$3A                ; YES
                    sta       DISP+12
                    lda       AOUR                ; GET ALARM HOURS
                    jsr       SUBSP
                    std       DISP+10
                    lda       AMIN
                    jsr       CBCD
                    std       DISP+13
                    brclr     stat2,$20,Done@@    ; SETUP?
                    brclr     QSEC,$02,Done@@
                    lda       #$20
                    tab
                    brset     stat2,$40,_1@@      ; HOURS?
                    std       DISP+13             ; NO, FLASH MINUTES
Done@@              rts

_1@@                std       DISP+10             ; YES, FLASH HOURS
                    rts

;*******************************************************************************

CBCD8               proc                          ; CONVERT TO ASCII BCD AND PUT INTO DISP+8 & +9
                    bsr       CBCD
                    std       DISP+8
                    rts

;*******************************************************************************

XFER16              proc
                    ldb       #16
                    ldy       #DISP
Loop@@              lda       ,x
                    sta       ,y
                    inx
                    iny
                    decb
                    bne       Loop@@
                    ldy       #REGS               ; RESTORE FOR I/O
                    rts

;*******************************************************************************
;                       Sleep display
;*******************************************************************************

SLEEPD              proc
                    ldx       #SLPST
                    bsr       XFER16
                    lda       SLEPT
                    bsr       CBCD
                    std       DISP+8
                    rts

;*******************************************************************************
;                       Voltage display
;*******************************************************************************

ALTD3               proc
                    ldx       #ADST
                    bsr       XFER16

                    lda       ADR1+RBO            ; VDD/4 (PE4)
                    inca
                    ldb       #200                ; SCALE AND RETURN WITH UP TO 99 (9.9v) IN ACCA
                    bsr       CSUB                ; AND 10s OF VOLTS IN TEMP
                    bsr       CBCD                ; RETURN WITH ASCII VOLTS IN ACCA AND 10ths IN ACCB
                    sta       DISP+4              ; VOLTS
                    stb       DISP+6              ; 10ths OF VOLTS
                    lda       TEMP                ; 10s OF VOLTS
                    bne       Ascii@@             ; ZERO?
                    lda       #$F0                ; YES, MAKE IT A SPACE
Ascii@@             adda      #'0'                ; CONVERT TO ASCII
                    sta       DISP+3              ; 10s OF VOLTS

                    lda       ADR2+RBO            ; RSSI (PE5)
                    inca
                    ldb       #250                ; SCALE AND RETURN WITH UP TO 99 (1.98v) IN ACCA
                    bsr       CSUB                ; AND UP TO 2 (4v) IN TEMP (MAX: 249 OR 4.98V)
                    lsl       TEMP                ; DOUBLE TEMP TO VOLTS (MAX 4)
                    asla                          ; DOUBLE ACCA TO 100ths OF VOLTS (MAX 198)
                    bsr       TFMH                ; CHECK FOR CARRY TO TEMP
                    bsr       CBCD                ; RETURN WITH ASCII 10ths IN ACCA AND 100ths IN ACCB
                    std       DISP+14             ; AND PUT BOTH IN DISPLAY BUFFER
                    lda       TEMP
                    adda      #$30                ; CONVERT VOLTS TO ASCII
                    sta       DISP+12             ; AND PUT IN DISPLAY BUFFER
                    rts

;*******************************************************************************

CSUB                proc
                    mul                           ; TIMES 200 (OR 250) AND
                    clr       TEMP                ; DIVIDE BY 256 (BY USING
;                   bra       TFMH

;*******************************************************************************

TFMH                proc
Loop@@              cmpa      #100                ; ONLY ACCA AS RESULT)
                    blo       Done@@              ; OVER 99?
                    inc       TEMP                ; YES, OVERFLOW AND
                    suba      #100                ; GET ACCA BELOW 100 BEFORE CONVERSION TO BCD
                    bra       Loop@@              ; AND AGAIN
Done@@              equ       :AnRTS

;*******************************************************************************
;           ACCA Hex>BCD conversion
;                      &
;         Split nibbles into ACCA (MS) and
;          ACCB (LS) and convert to ASCII.
;*******************************************************************************

CBCD                proc                          ; (ADDED BY TONYP BECAUSE IT WAS MISSING)
                    tab                           ; HEX IN A & B
                    anda      #$0F                ; LSB IN A
                    andb      #$F0                ; MSB (x16) IN B
                    adda      #0                  ; CLEAR H AND C BITS
                    daa                           ; DECIMAL ADJUST ACCA (ADD 6 IF OVER 9)
Loop@@              subb      #$10                ; DECREMENT MSB
                    bcs       SPLIT               ; TOO FAR?
                    adda      #$16                ; NO, ADD 16 (BCD) TO
                    daa                           ; ACCA, AND ADJUST
                    bra       Loop@@              ; TRY AGAIN

;*******************************************************************************
;        Split ACCA nibbles into ACCA (MS) and
;         ACCB (LS) and convert both to ASCII.
;*******************************************************************************

SPLIT               proc
                    tab                           ; MSD INTO A, LSD INTO B
                    sec
                    rora                          ; SHIFT MS NIBBLE DOWN
                    sec
                    rora                          ; SHIFT IN TWO 1s TO ADD $30
                    lsra:2                        ; TO CONVERT DECIMAL NUMBERS TO ASCII
                    cmpa      #'9'
                    bls       Ok@@                ; OVER 9?
                    adda      #7                  ; YES, ADJUST FOR AF
Ok@@                andb      #$0F
                    addb      #'0'                ; CONVERT LS NIBBLE TO ASCII
                    cmpb      #'9'
                    bls       Done@@
                    addb      #7                  ; AND ADJUST FOR AF
Done@@              rts

;*******************************************************************************
;         Send and clock data to LCD module.
;
;         Check to see if LCD module is busy.
;*******************************************************************************

CLOCK3              proc
                    lda       #$30                ; $38 FOR /16 DISPLAYS
;                   bra       CLOCK

;*******************************************************************************

CLOCK               proc
                    sta       LCDB,y
                    bset      LCDC,y,$80
                    bclr      LCDC,y,$80          ; CLOCK IT
                    rts

;*******************************************************************************

WAIT                proc
                    bclr      LCDC,y,$A0          ; READ LCD BUSY FLAG
                    bset      LCDC,y,$40
                    clr       LCDBDD,y            ; INPUT ON LCD BUS
Loop@@              bset      LCDC,y,$80          ; CLOCK HIGH
                    lda       LCDB,y              ; READ MODULE
                    bclr      LCDC,y,$80          ; CLOCK LOW
                    bmi       Loop@@              ; BUSY?
                    com       LCDBDD,y            ; OUTPUT ON LCD BUS
                    bclr      LCDC,y,$40
                    rts

;*******************************************************************************
;                    Strings.
;*******************************************************************************

ADST                fcb       'B: --.- T: -.'
                    fcb       '---'
DNAME               fcb       'MonTueWedThuFriSatSun'
                    fcb       '---'
MNAME               fcb       'DecJanFebMarAprMayJunJulAugSepOctNovDecJan'

ALT2ST              fcb       'Y: / (----) W:'
ALARMS              fcb       ' Alarm - Off '
SLPST               fcb       ' Sleep 0 min. '

;*******************************************************************************
;                          RAM clear
;*******************************************************************************

CLRAM               proc
                    ldx       #stat1              ; INITIALISE RAM
Loop@@              clr       ,x
                    inx                           ; 1mS DELAY FOR LCD
                    cpx       #ADIS+1
                    bne       Loop@@
                    rts

;*******************************************************************************
;         LINK batch files (LWRD.BAT & LWRD.LD)
;                 and PCBUG11 Vectors.
;
; ILD11 LWRD.O -MKUF LWRD.MAP -G LWRD -O LWRD.OUT
; IHEX LWRD.OUT -O LWRD.0
; TYPE LWRD.MAP
; SYMBOL LWRD OFF 0
;
; section .RAM1 BSS origin 0x0000 11K4 811E2
; section .ROM1 origin 0x4000 $4000 $F800
; section .VECT origin 0xBFC1 $BFC1 -----
; section .VECTOR origin 0xFFD6 $FFD6 $FFF0
;*******************************************************************************

;*******************************************************************************
;              MC68HC811E2 Vectors.
;*******************************************************************************

                    org       $FFF0               ; SECTION .VECTOR

                    fdb       TINTB               ; RTI
                    fdb       SDATA               ; IRQ
                    fdb       Start               ; XIRQ
                    fdb       Start               ; SWI
                    fdb       Start               ; ILLEGAL OP CODE
                    fdb       Start               ; COP
                    fdb       Start               ; CLOCK MONITOR
                    fdb       Start               ; RESET

                    end
