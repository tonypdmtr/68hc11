;*******************************************************************************
; From: spud-boy@juno.com <spud-boy@juno.com>
; To: 68HC11@oakhill-csic.sps.mot.com <68HC11@oakhill-csic.sps.mot.com>
; Subject: Re: Assembly Program
; Date: Tuesday, 16 November 1999 3:27 pm
;
; Message sent by spud-boy@juno.com
; to the 68hc11 Mailing List.
;
; > Does anyone have a StopWatch program, or anything like one for this
; > MP?
; >
; > Thanks,
; >
; > Ron
;
; The following is a stopwatch program I made with a 40x2 HD44780 LCD. I
; have two rows for 6 switches each controlling 1 of 6 independent
; stopwatches displayed on the LCD. It worked great, but I'm not sure if
; the following is the last review of the software.
;
; This program uses:
; PA7  Pulse accumulator for keypad
; TOC2 for keypad debounce count
; TOC1 for time base.
; 3MHz crystal on a 68HC11F1
; This program is only interrupt driven; it goes into a loop where it waits
; for something to happen.
;
; I wrote this to fit into the EEPROM, so it is as bare as they come. I
; used this after writing to the EEPROM by tying RX and TX to each other
; then doing a reset.
;
; Some pieces of the code are from examples out of the PinkBook.
; If you have any questions contact:
; Spud-boy@juno.com
;
; Jamie.
;*******************************************************************************

RAM                 equ       $0020
ROM                 equ       $0D80               ; EEPROM 0D80
REG                 equ       $1000
PORTA               equ       $00
DDRA                equ       $01
DDRC                equ       $07
DDRF                equ       $03
PORTC               equ       $06
PORTE               equ       $0A
SPCR                equ       $28
BAUD                equ       $2B
SCCR1               equ       $2C
SCCR2               equ       $2D
SCSR                equ       $2E
SCDAT               equ       $2F
PORTD               equ       $08
SCSR1               equ       $2E                 ; COMM STATUS REG
SCDR                equ       $2F                 ; COMM I/O BUFFER
TCNT                equ       $0E                 ; 16 BIT COUNTER
OPTION              equ       $39                 ; ANALOG TO DIGITAL POWER UP LOCATION
ADCTL               equ       $30                 ; AD CONTROL
TOF                 equ       $00D0               ; TIMER OVERFLOW VECTOR
PVXIRQ              equ       $00F1
PVTOC1              equ       $00DF
PVTOC2              equ       $00DC
PVPACC              equ       $00CA
TFLG2               equ       $25                 ; TIMER FLAGS 2
TMSK2               equ       $24                 ; TIMER MASK 2
TFLG1               equ       $23                 ; TIMER FLAGS 1
TMSK1               equ       $22                 ; TIMER MASK 1
ADC0                equ       $31
ADC1                equ       $32
ADC2                equ       $33
ADC3                equ       $34
HPRIO               equ       $3C                 ; CONFIGURATION REGISTER
TOC1                equ       $16
TOC2                equ       $18
TOC3                equ       $1A
PORTF               equ       $05
PORTG               equ       $02
DDRG                equ       $03
PACNT               equ       $27
PACTL               equ       $26
OPT2                equ       $38
TIME                equ       $0000
MSEC                equ       $00
SEC                 equ       $01
MIN                 equ       $02

;*******************************************************************************
                    #RAM      RAM
;*******************************************************************************

dbufr               rmb       9
index               rmb       1
dindex              rmb       1
pending             rmb       2
ctrl                rmb       1                   ; ENABLE/DISABLE TIMER
status              rmb       2
sstatus             rmb       2
temp                rmb       1
lcdadr              rmb       2
vindex              rmb       1
vtime               rmb       1
cindex              rmb       1

;*******************************************************************************
                    #ROM      ROM
;*******************************************************************************

Start               proc
                    lds       #$037F              ; STACK IS AT TOP OF RAM

                    ldx       #TIME               ; BEGINNING OF LOOP TO CLEAR VARIABLES
                    clrd                          ; SO THE TIMER SHOWS 0 AT STARTUP
Clear@@             std       ,x                  ; CLEAR 16 BYTES IN TIME BLOCK
                    inx:2                         ; MOVE 2 BYTES OVER
                    cpx       #$000F              ; COMPARE TO 16 - END OF TIME DISPLAY VARS
                    blt       Clear@@             ; DONE CLEARING VARIABLES

                    lda       #$7E                ; STORE JUMP IN VECTOR LOCATIONS
                    sta       PVTOC1              ; TIME BASE VECTOR
                    sta       PVPACC              ; KEYPRESS ACKNOWLEDGE VECTOR
                    sta       PVTOC2              ; DEBOUNCING TIMER VECTOR
                    ldx       #COUNTER            ; ADDRESS FOR COUNTING SEQUENCE
                    stx       PVTOC1+1
                    ldx       #PACC               ; KEYPRESS ACK SEQUENCE
                    stx       PVPACC+1
                    ldx       #DBNCE              ; KEY DEBOUNCE DELAY SEQUENCE
                    stx       PVTOC2+1

                    ldx       #REG
                    clr       PORTC,x             ; LCD SETUP
                    clr       PORTF,x
                    lda       #$FF
                    sta       DDRC,x              ; SET PORTC TO OUTPUT FOR LCD 8 BIT DATA BUS
                    sta       DDRF,x              ; SET PORTF TO OUTPUT FOR LCD CONTROL (E,RS,R/W)

                    lda       #$30                ; LCD INIT BEGIN
                    bsr:3     DLY                 ; MUST HAVE A DELAY - NO BUSY FLAG FROM LCD YET
                    lda       #$3C                ; LCD 2 LINE, FONT
                    bsr       DLY
                    lda       #$0C                ; LCD DISPLAY ON, SPACE MODE CURSOR OFF
                    bsr       DLY
                    bra       SKIP                ; JUMP AROUND DELAY - BUSY FLAG WILL WORK NOW ON LCD

;*******************************************************************************

DLY                 proc
                    sta       PORTC,x             ; PUT COMMAND ON DATA BUS
                    bset      PORTF,x,$01         ; TOGGLE E ON LCD
                    bclr      PORTF,x,$01
;                   bra       DLY1

;*******************************************************************************

DLY1                proc
                    ldy       #1000               ; DELAY FOR LCD CONTROL COMMANDS
Loop@@              dey
                    bne       Loop@@
                    rts                           ; END LCD INIT

;*******************************************************************************

SKIP                proc
                    clr       ctrl                ; STOP ALL TIMERS AT STARTUP
                                                  ; 0 DISABLE TIMER, 1 ENABLE TIMER
                    ldx       #REG
                    clr       DDRG,x              ; SET PORTG FOR INPUT OF KEYS
                    bset      OPT2,x,$80          ; PORTG IN WIRED OR MODE
                    bset      PACTL,x,$40         ; SETUP PULSE ACCUMULATOR FOR KEYPRESS

                    bset      TMSK1,x,$80         ; CLEAR TOC1 FLAG FOR TIMEBASE
                    bset      TMSK2,x,$10         ; CLEAR PULSE ACCUMULATOR FLAG
                    cli                           ; ENABLE INTERUPTS
                    clr       dindex              ; CLEAR DEBOUNCING DELAY COUNTER

WaitInt@@           wai                           ; WAIT FOR INTERRUPT
                    bra       WaitInt@@           ; ENTIRE PROGRAM IS INTERRUPT DRIVEN.

;*******************************************************************************

LCD                 proc
                    ldx       #REG
                    lda       ,y                  ; GET CHARACTER
                    beq       Done@@
                    sta       PORTC,x             ; PUT DATA ON PORTC
                    bset      PORTF,x,$01         ; E HIGH
                    bclr      PORTF,x,$01         ; E LOW
                    clr       DDRC,x              ; PORTC INPUT
                    bset      PORTF,x,$04         ; SET R/W TO 1
                    brclr     PORTC,x,$80,*       ; WAIT FOR BUSY FLAG TO CLEAR

                    ldx       #10
Delay@@             dex                           ; WAIT FOR LCD ADDR TO INCREMENT!
                    bne       Delay@@
                    ldx       #REG
                    bclr      PORTF,x,$04         ; SET R/W TO 0
                    ldb       #$FF
                    stb       DDRC,x              ; PORTC OUTPUT

                    iny                           ; POINT TO NEXT CHARACTER
                    bra       LCD

Done@@              bset      PORTF,x,$02         ; SET R/S TO 1
                    rts                           ; END DISPLAY

;*******************************************************************************

HTOD                proc
                    ldx       #10
                    idiv
                    xgdx
                    addb      #'0'
                    stb       dbufr
                    xgdx
                    addb      #'0'
                    stb       dbufr+1
                    clr       dbufr+2
                    rts                           ; END HEX TO DECIMAL

;*******************************************************************************

COUNTER             proc
                    ldx       #REG
                    bclr      TFLG1,x,$7F         ; CLEAR TIMER OUTPUT COMPARE FLAG
                    ldd       TOC1,x
                    addd      #30017              ; TRIM THE TOC FOR GOOD TIME KEEPING
                    std       TOC1,x
                    ldx       #TIME
                    lda       #$01
                    sta       cindex

MainLoop@@          lda       ctrl
                    bita      cindex
                    beq       Cont@@

                    inc       MSEC,x              ; INCREMENT MILLISECONDS
                    ldb       MSEC,x
                    cmpb      #100                ; OVERFLOW AT 100
                    blt       Cont@@

                    clr       MSEC,x
                    inc       SEC,x               ; INCREMENT SECONDS
                    ldb       SEC,x
                    cmpb      #100                ; OVERFLOW AT 100
                    blt       Cont@@

                    clr       SEC,x
                    inc       MIN,x               ; INCREMENT MINUTES
                    ldb       MIN,x
                    cmpb      #100                ; CLEAR AT 100 !
                    blt       Cont@@
                    clr       MIN,x

Cont@@              lsl       cindex
                    lda       cindex
                    inx:3                         ; SHIFT ADDRESSES FOR NEXT TIMER 1 TO 5
                    cpx       #13
                    blt       MainLoop@@

                    ldx       #REG
                    bclr      PORTF,x,$02         ; SET R/S TO 0 CGRAM
                    ldy       #HOME               ; HOME LCD DISPLAY
                    jsr       LCD
                    jsr       DLY1                ; DELAY AFTER CONTROL WRITES

                    ldx       #TIME               ; LOOP TO WRITE TIMERS TO LCD

Write@@             clra
                    ldb       MIN,x
                    pshx
                    bsr       HTOD
                    ldy       #dbufr
                    jsr       LCD
                    clra
                    pulx
                    ldb       SEC,x
                    pshx
                    bsr       HTOD
                    ldy       #dbufr
                    jsr       LCD
                    ldy       #DOT                ; SEND A . FOR DECIMAL PLACE
                    jsr       LCD

                    clra
                    pulx
                    ldb       MSEC,x
                    pshx
                    jsr       HTOD
                    ldy       #dbufr
                    jsr       LCD
                    ldy       #SPACE              ; PLACE A SPACE AFTER THE TIME
                    jsr       LCD
                    pulx
                    inx:3
                    cpx       #13
                    blt       Write@@             ; DO ALL FIVE TIMERS
                    rti

;*******************************************************************************

PACC                proc
                    ldx       #REG
                    bclr      TFLG2,x,$EF         ; CLEAR FLAG PACC
                    bclr      PACTL,x,$40         ; DISABLE PACC

                    lda       dindex
                    bne       Done@@
                    ldb       PORTG,x
                    lda       PORTA,x
                    eora      #$FF
                    eorb      #$FF
                    sta       status+1
                    stb       status

                    ldd       TCNT,x              ; RESET DEBOUNCING TIMER
                    std       TOC2,x

                    bset      TMSK1,x,$40         ; ENABLE TOC2 FOR DEBOUNCE
                    lda       #1
                    sta       vindex
                    ldx       #TIME

Valid@@             lda       status
                    bita      vindex
                    bne       ClearTimer@@

                    lda       status+1
                    bitb      vindex
                    beq       Skip@@              ; NO CHANGE

                    ldb       status+1
                    eorb      ctrl
                    stb       ctrl
                    clr       status+1
                    bra       Skip@@

ClearTimer@@        lda       ctrl
                    bita      status
                    bne       Cont@@

                    clr       MIN,x
                    clr       SEC,x
                    clr       MSEC,x

Cont@@              clr       status

Skip@@              inx:3
                    lsl       vindex
                    lda       vindex
                    cmpa      #32
                    bne       Valid@@

Done@@              rti

;*******************************************************************************

DBNCE               proc
                    ldx       #REG
                    bclr      TFLG1,x,$BF         ; DEBOUNCE KEYS TO GET GOOD RESPONCE
                    inc       dindex
                    lda       dindex
                    cmpa      #16
                    blt       Done@@

                    bclr      TMSK1,x,$40         ; DISABLE TOC2 FLAG FOR COUNT DOWN
                    bset      PACTL,x,$40         ; ENABLE
                    bclr      TFLG2,x,$EF         ; CLEAR FLAG PACC
                    clr       dindex
Done@@              rti

DOT                 fcs       '.'
SPACE               fcs       ' '
HOME                fcs       $02
