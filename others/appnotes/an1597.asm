;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;         MC68HC11K4/E2 Longwave RD Clock
;
;              P. Topping 21st August '95
;                       AN1597
;
; (Typed from AN1597.PDF and converted syntax to ASM11 by Tony Papadimitriou)
;*******************************************************************************

HC11                def       2                   ; 2 FOR E2, 4 FOR K4

REGS                equ       $1000               ; REGISTER BASE
PORTA               equ       REGS+$00            ; PORT A ADDRESS
PORTB               equ       REGS+$04            ; B
PORTC               equ       REGS+$03            ; C
PORTD               equ       REGS+$08            ; D
PORTE               equ       REGS+$0A            ; E

PORTCD              equ       REGS+$07            ; PORT C DATA DIRECTION REG.
PORTDD              equ       REGS+$09            ; D

TCNT                equ       REGS+$0E
TMSK2               equ       REGS+$24
PACTL               equ       REGS+$26
SPCR                equ       REGS+$28
ADCTL               equ       REGS+$30
ADR1                equ       REGS+$31
ADR2                equ       REGS+$32
ADR4                equ       REGS+$34
OPTION              equ       REGS+$39
INIT                equ       REGS+$3D

;*******************************************************************************
                    #RAM                          ; RAM allocation
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

doy                 rmb       2                   ; DAY OF YEAR
mnth                rmb       1                   ; MONTH
dom                 rmb       2                   ; DAY OF MONTH
dow                 rmb       2                   ; DAY OF WEEK
week                rmb       1                   ; WEEK NUMBER
sday                rmb       2                   ; YEAR START DAY
type                rmb       1                   ; YEAR TYPE (LEAP)
ofset               rmb       1                   ; LOCAL OFFET
dat                 rmb       7                   ; SERIAL DATA BUFFER
block               rmb       4                   ; BLOCK DATA
bit                 rmb       1                   ; BIT LEVEL
conf                rmb       1                   ; CRC CONFIDENCE
qsec                rmb       1                   ; QUARTER SECONDS
th8                 rmb       1                   ; EIGHTHS OF SECONDS
m9                  rmb       1                   ; 9 MINUTE COUNTER
code                rmb       1                   ; BLOCK CODE
min                 rmb       1                   ; MINUTES
hour                rmb       1                   ; HOURS
dmin                rmb       1                   ; DISPLAYED MINUTES
dhour               rmb       1                   ; DISPLAYED HOURS
ddow                rmb       2                   ; DISPLAYED DAY OF WEEK
dweek               rmb       1                   ; DISPLAYED WEEK NUMBER
tmin                rmb       1                   ; TEMPORARY MINUTES
thour               rmb       1                   ; TEMPORARY HOURS
tdow                rmb       1                   ; TEMPORARY DAY OF WEEK
tweek               rmb       1                   ; TEMPORARY WEEK NUMBER
sup                 rmb       1                   ; VDD (A/D RESULT)
rssi                rmb       1                   ; SIGNAL LEVEL (A/D RESULT)
rss4                rmb       1                   ; AVERAGED SIGNAL LEVEL
disp                rmb       16                  ; LCD MODULE BUFFER
dspold              rmb       16                  ; LCD MODULE PREVIOUS DATA
w1                  rmb       2                   ; |
w2                  rmb       2                   ; | USED IN INTERRUPT
w3                  rmb       2                   ; |
temp                rmb       3
dist                rmb       1                   ; TRANSIENT DISPLAY TIMEOUT COUNTER
slept               rmb       1                   ; SLEEP TIMER MINUTES COUNTER
amin                rmb       1                   ; ALARM MINUTES
ahour               rmb       1                   ; ALARM HOURS
key                 rmb       1                   ; CODE OF PRESSED KEY
kount               rmb       1                   ; KEYBOARD COUNTER
adis                rmb       1                   ; ALTERNATIVE DISPLAY TYPE

;*******************************************************************************
                    #ROM      $F800               ; .ROM1
;*******************************************************************************

;*******************************************************************************
; Reset & initialisation

Start               proc
                    lds       #$00FF              ; INITIALISE STACK POINTER
                    lda       #$40                ; ENABLE REAL TIME INTERRUPTS
                    sta       TMSK2
                    lda       #$B0                ; IRQ EDGE SENSETIVE, A/D ON
                    sta       OPTION
                    lda       #$0B                ; 133072 us WITH A 2.0 MHz XTAL
                    sta       PACTL
                    lda       #$34                ; ENABLE CONTINUOUS A/D
                    sta       ADCTL
                    ldy       #REGS

                    ldd       #$003C              ; 0,1: SCI (PCBUG11), 24: not used
                    std       [PORTD,y            ; 5: CONTROL OUTPUT
                                                  ; PORTE 4: BATTERY (A/D), 5: TUNING (A/D)
                                                  ; PORTE 7: KEYBOARD INHIBIT,0,1,2,3,6: not used
LCDB                equ       PORTC
LCDBDD              equ       PORTCD

                    lda       #$FF
                    sta       [LCDBDD,y
LCDC                equ       PORTB
                    clr       [LCDC,y             ; LCD CONTROL BITS: 5(RS), 6(R/W),7(E)
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
; Idle loop

IDLE                proc
                    brclr     [TCNT,y,$1F,_1@@    ; 64 Hz
                    bra       IDLE

_1@@                brclr     stat2,$01,_2@@      ; DISPLAY TRANSIENT?
                    lda       dist
                    bne       _2@@                ; YES, TIMED OUT?
                    jsr       CLTR

_2@@                brclr     stat1,$08,Scan@@    ; DISPLAY UPDATE REQUIRED?
                    jsr       MOD                 ; YES, DO IT (4 Hz)
                    bclr      stat1,$08           ; AND CLEAR FLAG

Scan@@              brclr     stat2,$10,CHSLP@@   ; ALARM ARMED?
                    ldd       dmin                ; YES, COMPARE TIME WITH ALARM TIME
                    inca                          ; ADD A MINUTE
                    cmpa      #60                 ; NEXT HOUR?
                    bne       ITOK@@
                    clra                          ; YES, CLEAR MINUTES
                    incb                          ; AND INCREMENT HOURS
                    cmpb      #24                 ; NEXT DAY?
                    bne       ITOK@@
                    clrb                          ; YES, CLEAR HOURS
ITOK@@              cpd       amin                ; ALARM TIME
                    bne       CHSLP@@             ; SAME?

                    lda       qsec                ; WAKEUP TWO SECONDS EARLY
                    cmpa      #218
                    bne       CHSLP@@             ; TO PREVENT SWITCHOFF LOCKOUT
                    bclr      [PORTD,y,$20        ; YES, SWITCH ON
                    jsr       INSLP               ; START SLEEP TIMER
                    inc       slept               ; 61 TO COMPENSATE FOR IMMEDIATE DECREMENT
CHSLP@@             brclr     stat2,$02,FLN@@     ; SLEEP TIMER RUNNING?
                    lda       slept               ; YES
                    bne       FLN@@               ; TIME TO FINISH?
                    bclr      stat2,$02           ; YES, CLEAR FLAG
                    bset      [PORTD,y,$20        ; AND SWITCH OFF
FLN@@               brclr     [ADR4,y,$80,SKBD@@  ; KEYBOARD ENABLED?
                    bsr       KBD                 ; YES, READ KEYBOARD
SKBD@@              bra       IDLE

;*******************************************************************************
; Keyboard routine

KBD                 proc
                    bset      [KEYP,y,R1          ; ROW 1
                    bclr      [KEYP,y,R2
                    lda       [KEYP,y             ; READ KEYBOARD
                    bita      #KINS               ; ANY INPUT LINE HIGH?
                    bne       _1@@
                    bset      [KEYP,y,R2          ; ROW 2
                    bclr      [KEYP,y,R1
                    lda       [KEYP,y             ; READ KEYBOARD
                    bita      #KINS               ; ANY INPUT LINE HIGH?
                    bne       _1@@
                    clr       key                 ; NO KEY PRESSED
                    rts

_1@@                anda      #$1B
                    cmpa      key                 ; SAME AS LAST TIME?
                    beq       _2@@
                    sta       key                 ; NO, SAVE THIS KEY
                    clr       kount
_2@@                inc       kount               ; YES, THE SAME
                    lda       kount
                    cmpa      #3                  ; 3 THE SAME?
                    bne       Done@@              ; IF 3 THEN PERFORM KEY FUNCTION

                    lda       key
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
; On/off key

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
                    brclr     [PORTD,y,$20,On@@   ; ON?
SODM                bclr      [PORTD,y,$20        ; NO, SWITCH ON
                    rts

On@@                bset      [PORTD,y,$20        ; YES, SWITCH OFF
                    rts

;*******************************************************************************

CLTR                proc
                    bclr      stat2,$AD           ; CLEAR DISPLAY FLAGS (TRANSIENT, ALARM, ALT. DISPLAY, SLEEP)
                    clr       adis
                    rts

;*******************************************************************************
; Alarm key

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
TRAN                sta       dist
                    bset      stat2,$01           ; SET DISPLAY TRANSIENT FLAG
                    rts

;*******************************************************************************
; Alternative displays key

DCK                 proc
                    brset     stat2,$20,PINC      ; ALARM SETUP?
;                   bra       NEXTD

;*******************************************************************************

NEXTD               proc
                    bset      stat2,$80           ; NO, SET ALTERNATIVE DISPLAY FLAG AND
                    bclr      stat2,$2D           ; CLEAR OTHER DISPLAY FLAGS
                    inc       adis                ; INCREMENT DISPLAY TYPE
                    lda       adis
                    cmpa      #4                  ; TOO FAR?
                    beq       CLTR                ; IF SO BACK TO ZERO
                    rts

;*******************************************************************************
; Sleep key

SLEEP               proc
                    brset     stat2,$20,PDEC      ; ALARM SETUP?

                    brset     stat2,$04,?DECS     ; NO, ALREADY SLEEP DISPLAY?
                    brset     stat2,$02,?STR2     ; NO, SLEEP TIMER ALREADY RUNNING?
;                   bra       INSLP

;*******************************************************************************

INSLP               proc
Loop@@              lda       #60                 ; NO, INITIALISE SLEEP TIMER
                    sta       slept
                    bset      stat2,$02           ; START SLEEP TIMER
?STR2               bsr       CLTR                ; YES, CLEAR DISPLAY TRANSIENTS
                    bset      stat2,$04           ; SLEEP DISPLAY
                    bra       SLPTOK@@            ; NO DECREMENT IF FIRST TIME

?DECS               lda       slept               ; DECREMENT SLEEP TIMER
                    suba      #5
                    sta       slept
                    bmi       Loop@@              ; IF UNDERFLOW, WRAP ROUND TO 60
SLPTOK@@            bsr       T25
                    bra       SODM

;*******************************************************************************
; Increment alarm time

PINC                proc
                    brset     stat2,$40,_1@@      ; SETUP HOURS?
                    lda       amin
                    inca
                    cmpa      #59
                    ble       MinOK@@
                    clra
MinOK@@             sta       amin
                    bra       T5S                 ; 10 SECOND TIMEOUT

_1@@                lda       ahour
                    inca
                    cmpa      #23
                    ble       _2@@
                    clra
_2@@                sta       ahour
T5S                 jmp       A5SD                ; 10 SECOND TIMEOUT

;*******************************************************************************
; Decrement alarm time

PDEC                proc
                    brset     stat2,$40,_1@@      ; SETUP HOURS?
                    dec       amin
                    bpl       T5S
                    lda       #59
                    sta       amin
                    bra       T5S
_1@@                dec       ahour
                    bpl       T5S
                    lda       #23
                    sta       ahour
                    bra       T5S

;*******************************************************************************
; Timer interrupt routine

TINTB               proc
                    ldy       #REGS
                    bclr      $25,y,$BF           ; CLEAR RTI INTERRUPT FLAG
                    inc       th8                 ; EIGTHS OF SECONDS
                    lda       th8
                    cmpa      #2                  ; QUARTER SECOND?
                    beq       Quart@@
Done@@              rti

Quart@@             clr       th8
                    dec       dist                ; DECREMENT TRANSIENT DISPLAY TIMER
                    bset      stat1,$08           ; UPDATE DISPLAY
          ;-------------------------------------- ; Update clock
                    inc       qsec                ; UPDATE QUARTER SECONDS
                    ldb       qsec
                    lda       m9                  ; 9 MINUTE COUNTER
                    bne       _1@@                ; TIME TO COMPENSATE FOR 2.000 MHz CRYSTAL?
                    cmpb      #228                ; YES, 228 QUARTER SECONDS A MINUTE
                    bra       _2@@

_1@@                cmpb      #229                ; NO, 229 (DIVIDE RATIO=2x228.888)
_2@@                bne       Done@@              ; IE 457.778, 457.778x131.072=60.00185 sec/min)
                    clr       qsec                ; IF 228 OR 229 THEN CLEAR SECONDS
                    inc       min                 ; AND UPDATE MINUTES
                    dec       slept               ; AND SLEEP TIMER
                    lda       m9                  ; AND 9 MINUTE COUNTER
                    inca
                    cmpa      #9
                    bne       _3@@                ; TENTH MINUTE FINISHED?
                    clra                          ; YES, START AGAIN
_3@@                sta       m9

                    lda       min
                    cmpa      #60
                    bne       NOTC                ; PAST 59?
                    clr       min                 ; YES, CLEAR
                    inc       hour                ; UPDATE HOURS
                    lda       hour
                    cmpa      #24
                    bne       NOTC                ; PAST 23?
                    clr       hour                ; YES CLEAR
;                   bra       TEST1

;*******************************************************************************
; Update date

TEST1               proc
                    inc       dow+1               ; NEXT DAY
                    lda       dow+1
                    cmpa      #7                  ; PAST SUNDAY?
                    bls       NOTC
                    ldb       #1                  ; YES, BACK TO MONDAY
                    stb       dow+1
TEST2               inc       week                ; INCREMENT WEEK NUMBER
                    lda       week

                    ldb       sday+1
                    cmpb      #4                  ; 1st JANUARY WAS A THURSDAY?
                    beq       W53                 ; IF SO, 53 WEEKS
                    cmpb      #3                  ; WEDNESDAY?
                    bne       W52                 ; NEITHER WED NOR THU SO 52 WEEKS
                    tst       type                ; WED., BUT IS IT LEAP?
                    bne       W52                 ; IF NOT THEN ONLY 52
W53                 cmpa      #53                 ; (THU.) OR (WED. & LEAP) SO 53 WEEKS
                    bra       TWN

W52                 cmpa      #52                 ; ELSE, 52 WEEKS
TWN                 bls       NOTC                ; TOO BIG?
                    ldb       #1                  ; YES, BACK TO 1

                    stb       week
                    inc       sday+1
                    lda       type                ; IF LEAP THEN START DAY INCREASES BY 2
                    bne       CSD
                    inc       sday+1              ; SO INCREMENT AGAIN
CSD                 ldd       sday
                    cmpb      #7                  ; UPDATED START DAY TO BIG?
                    bls       NOV2
                    subb      #7                  ; YES, CORRECT
                    std       sday
NOV2                lda       type                ; YEAR TYPE
                    inca
                    anda      #$03                ; IF 4, BACK TO 0
                    sta       type

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
                    ldd       TCNT                ; READ TIMER
                    std       w1                  ; SAVE IT
                    subd      w2                  ; SUBTRACT PREVIOUS
                    std       w3                  ; AND SAVE DELTA
                    cmpa      #2*QBP              ; OVER 20 ms?
                    blo       LT20
                    ldd       w1                  ; YES, UPDATE PREVIOUS WITH CURRENT TIME
                    std       w2
                    ldd       w3                  ; RELOAD DELTA
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
; Shift in bit and calculate CRC

MULT                proc
                    sta       temp
                    lda       #8
                    sta       temp+1
                    lda       temp+2
Loop@@              ror       temp
                    bcc       Cont@@
                    eora      ,x
                    eorb      1,x
Cont@@              inx:2
                    dec       temp+1
                    bne       Loop@@
                    sta       temp+2
                    rts

;*******************************************************************************

BITIN1              proc
                    bset      stat1,$80           ; FORCE TO 1
;                   bra       BITIN

;*******************************************************************************

BITIN               proc
                    lda       stat1
                    rola                          ; PUT stat1 $80 BIT INTO C BIT
                    rol       dat+6               ; MOVE ALL (50) BITS UP
                    jsr       SHFT
                    brclr     stat1,$01,TRY2      ; BIT BY BIT CHECK?
                    dec       bit                 ; NO, WAIT FOR BIT 50
                    bne       Done@@              ; THIS TIME?
TRY1                lda       #50                 ; YES, RELOAD BIT COUNTER
                    sta       bit
TRY2                brclr     dat,$02,NOTV        ; PREBIT SHOULD BE A 1
                    lda       dat+6               ; LSB
                    ldb       dat+5               ; MSB (5 BITS)
                    andb      #$1F
                    sta       temp+2
                    lda       dat+5
                    anda      #$E0
                    ldx       #$B510              ; OFFSET FOR MISSING MATRIX ENTRIES (WAS: B510)
                    bsr       MULT
                    lda       dat+4
                    bsr       MULT
                    lda       dat+3
                    bsr       MULT
                    lda       dat+2
                    bsr       MULT
                    lda       dat+1
                    bsr       MULT
                    brclr     dat+0,$01,FIN
                    eora      #$3B
                    eorb      #$17
;                   bra       FIN
Done@@              equ       :AnRTS

;*******************************************************************************
; CRC check and confidence handling

FIN                 proc
                    cpd       #0
                    beq       VALID
NOTV                lda       conf
                    cmpa      #$0F                ; CONFIDENCE 15?
                    beq       DecConf@@
                    bclr      stat1,$01           ; NO, BIT BY BIT CRC CHECK
                    tsta
                    beq       Done@@              ; CONFIDENCE ZERO?
                    dec       bit
                    bne       Done@@              ; USE BIT COUNTER TO SLOW CONFIDENCE
                    lda       #15                 ; DROP DURING BIT BY BIT ATTEMPT TO
                    sta       bit                 ; RESYNCRONISE
DecConf@@           dec       conf
Done@@              rts

;*******************************************************************************

VALID               proc
                    bset      stat1,$01
                    lda       conf
                    cmpa      #14
                    bhi       _1@@
                    inca
                    sta       conf
_1@@                lda       #50
                    sta       bit
                    bsr       SHFT
                    bsr       SHFT
                    bsr       SHFT
                    ldd       dat+1
                    std       block
                    ldd       dat+3
                    std       block+2
                    lda       dat
                    anda      #$0F
                    sta       code
                    bne       Done@@              ; BLOCK 0?
                    brclr     block,$80,TIME      ; BLOCK 0, TIME?
Done@@              rts

;*******************************************************************************

SHFT                proc
                    rol       dat+5
                    rol       dat+4
                    rol       dat+3
                    rol       dat+2
                    rol       dat+1
                    rol       dat
                    rts

;*******************************************************************************
; Process time block

TIME                proc
                    ldd       block+2
                    lsld:2
                    anda      #$3F
                    cmpa      #59
                    bhi       Done@@              ; OVER 59?
                    sta       tmin                ; NO, MINUTES OK
                    ldd       block+1
                    lsrd
                    lsrb:3
                    cmpb      #23
                    bhi       Done@@              ; OVER 23?
                    stb       thour               ; NO, HOURS OK
                    lda       block+1
                    rora
                    anda      #$07
                    beq       Done@@              ; ZERO?
                    sta       tdow                ; NO, DAYOFWEEK OK
                    ldd       block
                    lsrd:2
                    lsrb:2
                    beq       Done@@              ; ZERO?
                    cmpb      #53                 ; NO, OVER 53?
                    bhi       Done@@              ; NO, WEEK NUMBER OK
                    stb       tweek
                    anda      #$07
                    beq       Done@@              ; YEAR START DAY ZERO?
                    sta       sday+1              ; NO, OK
                    ldb       block
                    clra
                    lsld:3
                    sta       type                ; YEAR TYPE (LEAP)
                    lda       block+3
                    anda      #$3F
                    sta       ofset               ; LOCAL TIM OFFSET
                    clr       qsec
                    clr       th8
                    ldd       tdow                ; UPDATE DOW & WEEK
                    std       dow+1
                    ldd       tmin                ; UPDATE MINUTE & HOUR
                    std       min
                    brset     stat1,$40,CDATE     ; DATE ALREADY VALID?
                    jsr       NOTALR              ; NO, FIRST TIME, STANDBY
                    bset      stat1,$40           ; AND SET FLAG
;                   bra       CDATE
Done@@              equ       :AnRTS

;*******************************************************************************
; Calculate offset

CDATE               proc
                    ldd       min                 ; XFER MINUTES AND HOURS
                    std       dmin
                    lda       week                ; XFER WEEK NUMBER
                    std       dweek
                    ldd       dow                 ; XFER DAYOFWEEK
                    std       ddow
;                   bra       LOCAL

;*******************************************************************************
; Local time difference adjustment (neg.)

LOCAL               proc
                    ldb       ofset               ; CHECK FOR OFFSET
                    brclr     ofset,$20,POS       ; POSITIVE?
                    negb                          ; NO, NEGATIVE HOURS IN B
                    lsrb
                    orb       #$F0                ; MS BITS TO 1s
                    bsr       HALF                ; HALF HOUR ADJUSTMENT
                    addb      dhour               ; HOUR OFFSET, MINUS UTC HOURS
                    bcs       ZOM@@               ; OVERFLOW?
                    addb      #24                 ; NO, ADD 24 HOURS
                    lda       ddow+1
                    deca                          ; AND GO BACK A DAY
                    bne       Ok@@                ; WAS MONDAY?
                    dec       dweek               ; YES, LAST WEEK
                    lda       #7                  ; SUNDAY
Ok@@                sta       ddow+1
ZOM@@               andb      #$1F
                    bra       TFIN

;*******************************************************************************

HALF                proc
                    bcc       Done@@              ; 1/2 HOUR?
                    lda       dmin                ; YES
                    adda      #30                 ; ADD 30 MINUTES
                    cmpa      #59
                    bls       Save@@              ; OVERFLOW?
                    suba      #60                 ; YES, SUBTRACT 60 MINUTES
                    inc       dhour               ; AND ADD 1 HOUR
Save@@              sta       dmin
Done@@              rts

;*******************************************************************************
; Local time difference adjustment (pos)

POS                 proc
                    lsrb                          ; HOURS IN B
                    bsr       HALF                ; HALF HOUR ADJUSTMENT
                    addb      dhour               ; HOUR OFFSET, ADD UTC HOURS
                    cmpb      #23
                    bls       TFIN                ; OVERFLOW?
                    subb      #24                 ; YES, SUBTRACT 24 HOURS
                    lda       ddow+1
                    inca                          ; AND INCREMENT DAYOFWEEK
                    cmpa      #7
                    ble       Ok@@                ; WAS SUNDAY?
                    inc       dweek               ; YES, NEXT WEEK
                    lda       #1                  ; MONDAY
Ok@@                sta       ddow+1
;                   bra       TFIN

;*******************************************************************************

TFIN                proc
                    stb       dhour
;                   bra       DATE

;*******************************************************************************
; Calculate date (month & dayofmonth)

DATE                proc
                    lda       dweek               ; WEEK NUMBER ADJUSTED FOR LOCAL OFFSET
                    ldb       sday+1              ; CAN BE 0 OR 54 (53 IN A 52 WEEK YEAR)
                    cmpb      #5                  ; IF 1st JAN/31st DEC RANSITION CAUSED
                    blo       WNOK@@              ; BY OFFSET
                    inca                          ; ADJUST WEEK FOR YEARSTARTDAYOFWEEK
WNOK@@              ldb       #7                  ; AND MULTIPLY BY 7 TO GET DAYOFYEAR
                    mul
                    addd      ddow                ; ADD CURRENT DAYOFWEEK
                    addd      #55                 ; START AT 1st NOV (PREVIOUS YEAR)
                    subd      sday                ; SUBTRACT YEAR START DAYOFWEEK
                    std       doy                 ; SAVE DAYOFYEAR (DEBUG)
                    brclr     stat1,$40,Done@@    ; DATE VALID?
                    clr       mnth                ; MONTH=0: NOVEMBER (PREVIOUS YEAR)
                    ldx       #Table@@
                    tst       type
                    bne       Loop@@              ; LEAP YEAR?
                    ldx       #Table@@+24         ; YES, USE SECOND TABLE
Loop@@              inc       mnth
                    subd      ,x
                    inx
                    inx
                    cpd       ,x
                    bhi       Loop@@
                    std       dom
Done@@              rts

Table@@             fdb       30,31               ; NOVEMBER, DECEMBER
                    fdb       31,28,31,30,31,30,31,31,30,31,30,31  ; JANUARYDECEMBER
                    fdb       31,29,31,30,31,30,31,31,30,31,30,31  ; JANUARYDECEMBER (LEAP)
                    fdb       31                  ; JANUARY

;*******************************************************************************
; Display type selection

MOD                 proc
                    brset     stat2,$04,Sleep@@   ; SLEEP DISPLAY?
                    brset     stat2,$08,Alarm@@   ; NO, ALARM DISPLAY?

                    brclr     stat2,$80,Normal@@  ; NO,ALTERNATIVE DISPLAYS?
                    lda       adis
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

Normal@@            brset     [PORTD,y,$20,StandBy@@ ; STANDBY?
                    bsr       NORMD               ; NORMAL DISPLAY
                    bra       Row@@

StandBy@@           bsr       STBYD               ; STANDBY DISPLAY
                    bra       Row@@

Alarm@@             jsr       ALRMD               ; ALARM DISPLAY

Row@@               ldx       #disp
Loop@@              lda       ,x
                    cmpa      16,x                ; HAS CHARACTER CHANGED?
                    bne       DIFF
                    inx
                    cpx       #disp+16
                    bne       Loop@@
                    rts

;*******************************************************************************

DIFF                proc
                    jsr       WAIT
                    lda       #$80                ; ADDRESS DISPLAY RAM
                    jsr       CLOCK               ; LATCH IT
                    ldx       #disp
Loop@@              jsr       WAIT
                    bset      [LCDC,y,$20         ; WRITE DATA
                    lda       ,x                  ; GET A BYTE
                    sta       16,x
                    jsr       CLOCK               ; SEND IT TO MODULE
                    inx
                    cpx       #disp+16            ; DONE?
                    bne       Loop@@
                    rts                           ; REMOVE FOR /16 DISPLAY
#ifdef
;*******************************************************************************
; Additional code for /16 LCD modules (also change CLOCK3 to lda  #$38)

LCD16               proc
                    jsr       WAIT
                    lda       #$A8                ;ADDRESS 40
                    jsr       CLOCK               ;SEND IT TO MODULE
                    ldx       #disp
Loop@@              jsr       WAIT
         #if HC11 = 2                             ;E2
                    bset      LCDC,y,$20          ;WRITE DATA
         #else                                    ;K4
                    bset      LCDC,y,$04          ;WRITE DATA
         #endif
                    lda       8,x                 ;GET A BYTE
                    jsr       CLOCK               ;SEND IT TO MODULE
                    inx
                    cpx       #disp+8             ;DONE?
                    bne       Loop@@
                    rts
#endif
;*******************************************************************************
; Normal and Standby (alarm armed) displays

NORMD               proc
                    bsr       STIME
                    jmp       DSUB1

;*******************************************************************************

ALRMA               proc
                    ldx       #ALARMS
                    jsr       XFER16
                    lda       ahour               ; GET ALARM HOURS
                    jsr       CBCD
                    std       disp
                    lda       amin
                    jsr       CBCD
                    std       disp+2
;                   bra       STIME

;*******************************************************************************

STIME               proc
                    lda       dhour               ; GET TIME
                    bsr       SUBSP
                    std       disp+11
                    lda       dmin
                    jsr       CBCD
                    std       disp+14
                    lda       #$3A                ; 0.5 Hz FLASHING COLON
                    sta       disp+13
                    rts

;*******************************************************************************

SUBSP               proc
                    jsr       CBCD
                    cmpa      #'0'                ; LEADING ZERO?
                    bne       Done@@
                    lda       #' '                ; YES, MAKE IT A SPACE
Done@@              rts

;*******************************************************************************
; Standby display (alarm not armed)

STBYD               proc
                    bsr       STIME
                    brset     stat2,$10,ALRMA     ; ALARM ARMED?
                    ldb       ddow+1              ; NO, GET DAY OF WEEK
                    ldx       #DNAME              ; WAS: DNAME3
                    bsr       T3X                 ; AND CONVERT TO STRING
                    std       disp
                    lda       2,x
                    sta       disp+2

                    lda       #' '
                    sta       disp+3
                    sta       disp+6
                    sta       disp+10
                    lda       dom+1               ; DAY OF MONTH
                    bsr       SUBSP
                    std       disp+4
                    ldb       mnth                ; MONTH
                    ldx       #MNAME              ; WAS: MNAME3
                    bsr       T3X                 ; CONVERT TO STRING
                    std       disp+7
                    lda       2,x
                    sta       disp+9
                    rts

;*******************************************************************************

T3X                 proc
                    lda       #3
                    mul
                    abx
                    ldd       ,x
                    rts

;*******************************************************************************
; LW data, confidence & seconds display

ALTD1               proc
                    lda       #' '
                    sta       disp+11
                    sta       disp+13
                    lda       conf
                    jsr       SPLIT
                    stb       disp+12
                    lda       qsec                ; QSEC X 256 IN D
                    clrb
                    ldx       #978                ; SCALE FOR QSEC = 229
                    idiv                          ; TO BE JUST BELOW 60
                    xgdx
                    tba
                    jsr       CBCD
                    std       disp+14
;                   bra       DSUB1

;*******************************************************************************

DSUB1               proc
                    ldd       #$2D20
                    stb       disp+6
                    stb       disp+10
                    brclr     stat2,$02,_1@@      ; SLEEP TIMER RUNNING?
                    ldb       #$2E                ; YES, . IN 2nd CHARACTER
_1@@                std       disp
                    brclr     stat1,$01,SYNNV@@
                    lda       code
                    bne       _2@@                ; BLOCK 0?
                    brset     block,$80,_2@@      ; YES, TIME?
                    ldb       #'t'                ; YES
                    bra       SKSP@@

_2@@                jsr       SPLIT
SKSP@@              stb       disp
SYNNV@@             lda       block
                    jsr       SPLIT
                    std       disp+2
                    lda       block+1
                    jsr       SPLIT
                    std       disp+4
                    lda       block+2

                    ldb       disp+11             ; t 7D65 37C2 1:23
                    cmpb      #' '                ; ^
                    bne       MOVEIT              ; SPACE?
                    jsr       SPLIT               ; DIVIDE HEX DATA
                    std       disp+7              ; INTO TWO BLOCKS
                    lda       block+3             ; OF FOUR IF THE
                    jsr       SPLIT               ; 12th CHARACTER
                    std       disp+9              ; IS A SPACE
                    rts

;*******************************************************************************

MOVEIT              proc
                    jsr       SPLIT
                    std       disp+6
                    lda       block+3
                    jsr       SPLIT
                    std       disp+8
                    rts

;*******************************************************************************
; LW data year & week display

ALTD2               proc
                    ldx       #ALT2ST
                    jsr       XFER16
                    lda       type                ; LEAP YEAR (CYCLE) TYPE
                    adda      #'0'
                    sta       disp+2
                    lda       sday+1              ; YEAR START DAY
                    adda      #'0'
                    sta       disp+4
                    lda       sday+1              ; IF 0 (NO TIME/DATE RECEIVED)
                    beq       ILLSD@@             ; THEN DON’T CALCULATE YEAR

                    ldb       type
                    lda       #7                  ; CALCULATE OFFSET TABLE OFFSET
                    mul
                    addb      sday+1
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
                    std       disp+6              ; 19 OR 20 TO DISPLAY BUFFER
                    pula
                    adda      #95                 ; YEAR TENS AND UNITS
                    bsr       CBCD8               ; CONVERT TO ASCII BCD AND PUT INTO DISP+8 & +9
ILLSD@@             lda       dweek               ; WEEK NUMBER
                    jsr       CBCD
                    std       disp+14
                    rts

Table@@             fcb       1,13,25,9,21,5,17   ; TABLE CONTAINING OFFSET RELATIVE TO
                    fcb       6,18,2,14,26,10,22  ; 1995 ACCORDING TO LEAP YEAR (CYCLE)
                    fcb       23,7,19,3,15,27,11  ; TYPE (03) DOWN TABLE AND YEAR START DAY
                    fcb       12,24,8,20,4,16,0   ; (17) ACROSS TABLE

;*******************************************************************************
; Alarm display

ALRMD               proc
                    ldx       #ALARMS
                    bsr       XFER16
                    brclr     stat2,$10,Done@@    ; ALARM ARMED?
                    lda       #$3A                ; YES
                    sta       disp+12
                    lda       ahour               ; GET ALARM HOURS
                    jsr       SUBSP
                    std       disp+10
                    lda       amin
                    jsr       CBCD
                    std       disp+13
                    brclr     stat2,$20,Done@@    ; SETUP?
                    brclr     qsec,$02,Done@@
                    lda       #$20
                    tab
                    brset     stat2,$40,_1@@      ; HOURS?
                    std       disp+13             ; NO, FLASH MINUTES
Done@@              rts

_1@@                std       disp+10             ; YES, FLASH HOURS
                    rts

;*******************************************************************************

CBCD8               proc                          ; CONVERT TO ASCII BCD AND PUT INTO DISP+8 & +9
                    bsr       CBCD
                    std       disp+8
                    rts

;*******************************************************************************

XFER16              proc
                    ldb       #16
                    ldy       #disp
Loop@@              lda       ,x
                    sta       ,y
                    inx
                    iny
                    decb
                    bne       Loop@@
                    ldy       #REGS               ; RESTORE FOR I/O
                    rts

;*******************************************************************************
; Sleep display

SLEEPD              proc
                    ldx       #SLPST
                    bsr       XFER16
                    lda       slept
                    bsr       CBCD
                    std       disp+8
                    rts

;*******************************************************************************
; Voltage display

ALTD3               proc
                    ldx       #ADST
                    bsr       XFER16

                    lda       ADR1                ; VDD/4 (PE4)
                    inca
                    ldb       #200                ; SCALE AND RETURN WITH UP TO 99 (9.9v) IN ACCA
                    bsr       CSUB                ; AND 10s OF VOLTS IN TEMP
                    bsr       CBCD                ; RETURN WITH ASCII VOLTS IN ACCA AND 10ths IN ACCB
                    sta       disp+4              ; VOLTS
                    stb       disp+6              ; 10ths OF VOLTS
                    lda       temp                ; 10s OF VOLTS
                    bne       Ascii@@             ; ZERO?
                    lda       #$F0                ; YES, MAKE IT A SPACE
Ascii@@             adda      #'0'                ; CONVERT TO ASCII
                    sta       disp+3              ; 10s OF VOLTS

                    lda       ADR2                ; RSSI (PE5)
                    inca
                    ldb       #250                ; SCALE AND RETURN WITH UP TO 99 (1.98v) IN ACCA
                    bsr       CSUB                ; AND UP TO 2 (4v) IN TEMP (MAX: 249 OR 4.98V)
                    lsl       temp                ; DOUBLE TEMP TO VOLTS (MAX 4)
                    asla                          ; DOUBLE ACCA TO 100ths OF VOLTS (MAX 198)
                    bsr       TFMH                ; CHECK FOR CARRY TO TEMP
                    bsr       CBCD                ; RETURN WITH ASCII 10ths IN ACCA AND 100ths IN ACCB
                    std       disp+14             ; AND PUT BOTH IN DISPLAY BUFFER
                    lda       temp
                    adda      #'0'                ; CONVERT VOLTS TO ASCII
                    sta       disp+12             ; AND PUT IN DISPLAY BUFFER
                    rts

;*******************************************************************************

CSUB                proc
                    mul                           ; TIMES 200 (OR 250) AND
                    clr       temp                ; DIVIDE BY 256 (BY USING
;                   bra       TFMH

;*******************************************************************************

TFMH                proc
Loop@@              cmpa      #100                ; ONLY ACCA AS RESULT)
                    blo       Done@@              ; OVER 99?
                    inc       temp                ; YES, OVERFLOW AND
                    suba      #100                ; GET ACCA BELOW 100 BEFORE CONVERSION TO BCD
                    bra       Loop@@              ; AND AGAIN
Done@@              equ       :AnRTS

;*******************************************************************************
; ACCA Hex>BCD conversion &
; Split nibbles into ACCA (MS) and ACCB (LS) and convert to ASCII.

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
; Split ACCA nibbles into ACCA (MS) and ACCB (LS) and convert both to ASCII.

SPLIT               proc
                    tab                           ; MSD INTO A, LSD INTO B
                    sec
                    rora                          ; SHIFT MS NIBBLE DOWN
                    sec
                    rora                          ; SHIFT IN TWO 1s TO ADD '0'
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
; Send and clock data to LCD module. Check to see if LCD module is busy.

CLOCK3              proc
                    lda       #$30                ; $38 FOR /16 DISPLAYS
;                   bra       CLOCK

;*******************************************************************************

CLOCK               proc
                    sta       [LCDB,y
                    bset      [LCDC,y,$80
                    bclr      [LCDC,y,$80         ; CLOCK IT
                    rts

;*******************************************************************************

WAIT                proc
                    bclr      [LCDC,y,$A0         ; READ LCD BUSY FLAG
                    bset      [LCDC,y,$40
                    clr       [LCDBDD,y           ; INPUT ON LCD BUS
Loop@@              bset      [LCDC,y,$80         ; CLOCK HIGH
                    lda       [LCDB,y             ; READ MODULE
                    bclr      [LCDC,y,$80         ; CLOCK LOW
                    bmi       Loop@@              ; BUSY?
                    com       [LCDBDD,y           ; OUTPUT ON LCD BUS
                    bclr      [LCDC,y,$40
                    rts

;*******************************************************************************
; Strings
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
; RAM clear

CLRAM               proc
                    ldx       #stat1              ; INITIALISE RAM
Loop@@              clr       ,x
                    inx                           ; 1mS DELAY FOR LCD
                    cpx       #adis+1
                    bne       Loop@@
                    rts

;*******************************************************************************
; LINK batch files (LWRD.BAT & LWRD.LD) and PCBUG11 Vectors.
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
                    #VECTORS                      ; MC68HC811E2 Vectors.
;*******************************************************************************
                    org       $FFF0               ; SECTION .VECTOR

                    dw        TINTB               ; RTI
                    dw        SDATA               ; IRQ
                    dw        Start               ; XIRQ
                    dw        Start               ; SWI
                    dw        Start               ; ILLEGAL OP CODE
                    dw        Start               ; COP
                    dw        Start               ; CLOCK MONITOR
                    dw        Start               ; RESET
