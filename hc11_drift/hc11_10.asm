;*******************************************************************************
; HC11_DRIFT
; Version 1.0
; Copyright Gert Gottschalk (drgert1@yahoo.com)
; Algorithm by Paul Mortfield (paul@BackyardAstronomer.com)
; You may copy this software for non commercial use,
; as long as you mention the original developer's copyright
;*******************************************************************************
; Several optimizations by Tony Papadimitriou <tonyp@acm.org>
;*******************************************************************************

; Instructions:
;   1  running Buffalo
;   2  "load t" (enter)
;   3  send the file
;       HyperTerminal (transfer, send text file, all files)
;   4  wait for completion of file send
;   5  "md 2000" (enter) will display the memory containing the program
;   6  "go 2000" (enter) to run the program

                    #ExtraOn

;*************************************
; Major switches for assembly control
;*************************************

DEBUG               def       0
ST4_90              def       0
GGMODE              def       0

;*************************************
; Fatal Error Codes
;*************************************

;       01      rd_stat checksum error (10x)
;       02      ST4 fails to respond to reset
;       03      wr_stat $06 response failure
;       04      rd_xref checksum error (10x)
;       05      rd_yref checksum error (10x)
;       06      wr_xref $06 response failure
;       07      wr_yref $06 response failure
;       10      rd_stat response timeout error
;       11      wr_stat response timeout error
;       12      rd_xref response timeout error
;       13      rd_yref response timeout error
;       14      wr_xref response timeout error
;       15      wr_yref response timeout error

;*******************************************************************************
; REGISTER (define registers within the CPU)
;*******************************************************************************

REGS                equ       $1000               ; start of CPU register block
PORTA               equ       REGS+$00            ; PORTA
DDRA                equ       REGS+$01            ; PORTA DDR
PORTG               equ       REGS+$02            ; PORTG
DDRG                equ       REGS+$03            ; PORTG DDR
PORTB               equ       REGS+$04            ; PORTB
PORTCL              equ       REGS+$05            ; PROTC LATCHED DATA REG.
DDRC                equ       REGS+$07            ; PORTC DDR
PORTD               equ       REGS+$08            ; PORTD
DDRD                equ       REGS+$09            ; PORTD DDR
PORTE               equ       REGS+$0A            ; PORT E
CFORC               equ       REGS+$0B            ; TIMER COMPARE FORCE REG.
OC1M                equ       REGS+$0C            ; O/P COMPARE 1 MASK REG.
OC1D                equ       REGS+$0D            ; O/P COMPARE 1 DATA REG
TCNT                equ       REGS+$0E            ; H TIMER COUNT
TCNTL               equ       REGS+$0F            ; L
TIC1                equ       REGS+$10            ; 1 H TIMER O/P COMPARE
TIC1L               equ       REGS+$11            ; 1 L
TIC2                equ       REGS+$12            ; 2 H
TIC2L               equ       REGS+$13            ; 2 L
TIC3                equ       REGS+$14            ; 3 H
TIC3L               equ       REGS+$15            ; 3 L
TOC1                equ       REGS+$16            ; 1 H TIMER O/P COMPARE
TOC1L               equ       REGS+$17            ; 1 L
TOC2                equ       REGS+$18            ; 2 H
TOC2L               equ       REGS+$19            ; 2 L
TOC3                equ       REGS+$1A            ; 3 H
TOC3L               equ       REGS+$1B            ; 3 L
TOC4                equ       REGS+$1C            ; 4 H
TOC4L               equ       REGS+$1D            ; 4 L
TOC5                equ       REGS+$1E            ; 5 H
TOC5L               equ       REGS+$1F            ; 5 L
TCTL1               equ       REGS+$20            ; TIMER CONTROL REG 1
TCTL2               equ       REGS+$21            ; TIMER CONTROL REG 2
TMSK1               equ       REGS+$22            ; TIMER MASK 1
TFLG1               equ       REGS+$23            ; TIMER FLAG 1
TMSK2               equ       REGS+$24            ; TIMER MASK 2
TFLG2               equ       REGS+$25            ; TIMER FLAG 2
PACTL               equ       REGS+$26            ; PULSE ACCUMULATOR CTRL REG
PACNT               equ       REGS+$27            ; PULSE ACCUMULATOR COUNT REG
SPCR                equ       REGS+$28
SPSR                equ       REGS+$29
SPDR                equ       REGS+$2A
BAUD                equ       REGS+$2B            ; SCI BAUD REG
SCCR1               equ       REGS+$2C            ; SCI CONTROL 1 REG
SCCR2               equ       REGS+$2D            ; SCI CONTROL 2 REG
SCSR                equ       REGS+$2E            ; SCI STATUS REG
SCDR                equ       REGS+$2F            ; SCI DATA REG
ADCTL               equ       REGS+$30
ADR1                equ       REGS+$31
ADR2                equ       REGS+$32
ADR3                equ       REGS+$33
ADR4                equ       REGS+$34
BPROT               equ       REGS+$35
OPTION              equ       REGS+$39            ; OPTION REG
COPRST              equ       REGS+$3A            ; COP RESET REG
PPROG               equ       REGS+$3B            ; EEPROM PROG REG
HPRIO               equ       REGS+$3C            ; HPRIO REG
INIT                equ       REGS+$3D            ; INIT
CONFIG              equ       REGS+$3F            ; CONFIG REG
CSSTRH              equ       REGS+$5C            ; CS CYCLE STRETCH
CSCTL               equ       REGS+$5D            ; CS CONTROL
CSGADR              equ       REGS+$5E            ; GENERAL PURPOSE CS (RAM)
CSGSIZ              equ       REGS+$5F            ; GENERAL PURPOSE CS SIZE

;*******************************************************************************
; Constants (define references that do not change)
;*******************************************************************************

RAM_SIO             equ       $00C5               ; Buffalo sio IRQ jmp address
RAM_RTI             equ       $00EC               ; Buffalo timer IRQ jmp address
LED                 equ       %10000000           ; led output bit mask
DATA                equ       %00000001           ; LED port data mask
CLK                 equ       %00000010           ; LED port clk mask
LOAD                equ       %00000100           ; LED port load mask
DRFDG0              equ       $01
DRFDG1              equ       $02
DRFDG2              equ       $03
DRFRMP              equ       $04
ANGDG0              equ       $05
ANGDG1              equ       $06
ANGDG2              equ       $07
ANGRMP              equ       $08

ALLKEY              equ       %00111100           ; Mask for keys
ANGKEY              equ       %00001100
ANGPL               equ       %00001000
ANGMI               equ       %00000100
DRFKEY              equ       %00110000
DRFPL               equ       %00100000
DRFMI               equ       %00010000
ENTKEY              equ       %00001000

XCSTHI              equ       $0000
XCSTMD              equ       $0296               ; 180*3600*60*0.01375*255/PI
XCSTLO              equ       $1FBE               ; = 43392959 = $0000 0296 1FBE

YCSTHI              equ       $0000
YCSTMD              equ       $0302               ; 180*3600*60*0.01600*255/PI
YCSTLO              equ       $78B9               ; = 50493625 = $0000 0302 78B9

ST4ST               equ       $002f
ST4XRF              equ       $7E7C
ST4YRF              equ       $7E7D

ST4XMAX             equ       192
ST4YMAX             equ       165

;*******************************************************************************
                    #RAM      $0800               ; variable area
;*******************************************************************************

pdmir               rmb       1                   ; mirror of portd keys
pamir               rmb       1                   ; mirror of porta keys
angl                rmb       2                   ; angle
angmod              rmb       1                   ; angle mod 90
quadr               rmb       1                   ; quadrant
drift               rmb       2                   ; dirft
flen                rmb       2                   ; focal length
xrmplod             rmb       2                   ; led ramp counter load value x
xrmpcnt             rmb       2                   ; led ramp counter x
xrmpld              rmb       1                   ; led for x ramp
yrmplod             rmb       2                   ; led ramp counter load value y
yrmpcnt             rmb       2                   ; led ramp coutner y
yrmpld              rmb       1                   ; led for y ramp
rgled               rmb       1                   ; mirror red/green LED
entflg              rmb       1                   ; enter key pressed
cor_mod             rmb       1                   ; correction active mode flag
timout              rmb       1                   ; time out counter
sec_cnt             rmb       1                   ; seconds counter for timer irq 1/122
sec_flg             rmb       1                   ; 1 second toggle flag (chg each 1/2 sec)
lst_sec             rmb       1                   ; previous value of 1 sec flag
key_cnt             rmb       1                   ; 1/5 sec counter for key query
angdsp              rmb       3                   ; mirror angle digits
drfdsp              rmb       3                   ; mirror drift digits
tmp                 rmb       2                   ; temp storage
sendbuf             rmb       16                  ; send buffer for irq serial io
sendrd              rmb       2                   ; send buffer write ptr
sendwr              rmb       2                   ; send buffer read ptr
recvbuf             rmb       16                  ; receive buffer for irq serial io
recvrd              rmb       2                   ; receive buffer write ptr
recvwr              rmb       2                   ; receive buffer read ptr
scsr_mr             rmb       1                   ; SIO status reg mirror
rtiv_mr             rmb       2                   ; orig rti vector
siov_mr             rmb       2                   ; orig sio vector
st4refx             rmb       1                   ; reference x-coord from ST4
st4refy             rmb       1                   ; reference y-coord from ST4
st4buf              rmb       8                   ; receive buffer from st4
errcnt              rmb       1                   ; error counter for ST4 communication
errcnt1             rmb       1                   ; 2nd levl errcount
countx              rmb       2                   ; 1sec down x-counter for next ST4 ref x-coord modify
county              rmb       2                   ; 1sec down y-counter for next ST4 ref y-coord modify
cntxld              rmb       2                   ; 1sec down x-counter load value
cntyld              rmb       2                   ; 1sec down y-counter load value
xvalid              rmb       1                   ; flag if x corrections are enabled
yvalid              rmb       1                   ; flag if y corrections are enabled
          ;-------------------------------------- ; Stuff for multiply divide
counter             rmb       1                   ; Counter for mul/div
.divisor            rmb       2                   ; Ptr to MS byte of divisor
divisor_size        rmb       1                   ; Number of bytes in the divisor
dividend_size       rmb       1                   ; Number of bytes in the dividend
.quotient_byte      rmb       2                   ; Ptr to current byte of quotient
quotient_remaining  rmb       1                   ; Number of bytes remaining to be found in quotient
quotient_trial_val  rmb       1                   ; Current quotient trial value
          ;-------------------------------------- ; Temp storage for multiply
a24                 rmb       3                   ; Argument for multiple precision mul
b24                 rmb       3                   ; Argument for multiple precision mul
a48                 rmb       6                   ; Product from multiple precision mul
          ;-------------------------------------- ; Temp storage for  divide
d24                 rmb       3                   ; Divisor
q24                 rmb       3                   ; Quotient - Must immediately precede dividend
d48                 rmb       6                   ; Dividend
          ;-------------------------------------- ; Temp storage for divide
work_space          rmb       2                   ; Work space for divide

;*******************************************************************************
                    #ROM      $C000               ; location in FLASH RAM
;*******************************************************************************
          #ifnz DEBUG
                    org       $2000
          #endif
;*******************************************************************************

Start               proc
                    sei                           ; disable irq to set real time irq routine
          #ifz DEBUG
                    lds       #$07FF
          #endif
                    ldx       #REGS               ; register base address
                    lda       [DDRA,x
                    anda      #%11110111          ; make 'ent' key -> input
                    ora       #%10000111          ; make LED interface -> output
                    sta       [DDRA,x
                    lda       [DDRD,x
                    anda      #%11000011          ; make pd2-pd5 'keys' -> input
                    sta       [DDRD,x
                    lda       #%00000001
                    sta       [PACTL,x
                    bset      [TMSK2,x,%01000000  ; allow real time irq to cause int
                    lda       #%00110000          ; first prescaler/13 sec ps/1 => 9600bd@8MHz
                    sta       [BAUD,x
          #ifz DEBUG
                    lda       #%00010000          ; bit8+7 are recv bit8 and send bit8
          #else
                    ldd       RAM_RTI             ; in Debug mode use buffalo jump vectors
                    std       rtiv_mr
                    ldd       #RTI_Handler
                    std       RAM_RTI             ; replace with ROM irq later

                    ldd       RAM_SIO
                    std       siov_mr
                    ldd       #SCI_Handler
                    std       RAM_SIO             ; replace with ROM irq later
                    lda       #%00000000
          #endif
                    sta       [SCCR1,x
                    lda       #%00101100          ; enable transmitter and receiver of SCI
                    sta       [SCCR2,x

                    ldd       #sendbuf            ; init sio queues prior to first possible irq
                    std       sendrd
                    std       sendwr
                    ldd       #recvbuf
                    std       recvrd
                    std       recvwr
          #ifnz GGMODE
                    ldd       #180                ; maps to 1800mm Focal Length in GGs setup
                    std       drift
          #endif
                    clrd
                    std       angl
          #ifz GGMODE
                    std       drift
          #endif
                    std       xrmplod
                    std       xrmpcnt
                    std       yrmplod
                    std       yrmpcnt
                    sta       entflg
                    sta       xrmpld
                    sta       yrmpld
                    sta       rgled
                    sta       cor_mod
                    sta       timout
                    sta       sec_cnt
                    sta       sec_flg
                    sta       lst_sec
                    sta       key_cnt
                    sta       xvalid
                    sta       yvalid
          #ifz DEBUG
                    sta       st4refx
                    sta       st4refy
          #else
                    lda       #80
                    sta       st4refx
                    lda       #70
                    sta       st4refy
          #endif
                    ldx       #REGS               ; Eat up any trash in serial input data reg
_1@@                lda       [SCSR,x
                    anda      #%00100000
                    beq       _2@@
                    lda       [SCDR,x             ; count shows -> one read is enough to clr bit
                    bra       _1@@

_2@@                cli

                    jsr       ang2rmp
                    jsr       drf2rmp
                    jsr       ang2dsp
                    jsr       dft2dsp
                    jsr       dispang
                    jsr       dispdrf
                    jsr       InitLED             ; show 'FL ' on left display
                    jsr       ldgren
          #ifnz DEBUG
                    lda       #'I'
                    jsr       sendv24
                    jsr       crlf

                    jsr       ldorng
                    lda       #$ff
                    sta       timout
_3@@                tst       timout
                    bne       _3@@
          #else
                    jsr       rd_stat             ; retrieve status
                    ora       #$20                ; set bit 5 -> call for interrupt ST4
                    jsr       wr_stat

                    lda       #10
                    sta       errcnt1             ; initialize error counter
_4@@                jsr       rd_stat
                    anda      #$20                ; wait for status to clear
                    beq       _5@@                ; done
                    dec       errcnt1
                    bne       _4@@
                    ldd       #2                  ; if 10 errors encountered -> goto fatal halt
                    jmp       Fatal
          #endif
_5@@                lda       #9                  ; hi decode mode
                    ldb       #%00000111          ; lo left BIN right BCD
                    jsr       Disp
                    lda       #ANGDG2
                    ldb       #%01000111          ; 'F'
                    jsr       Disp
                    lda       #ANGDG1
                    ldb       #%00001110          ; 'L'
                    jsr       Disp
                    lda       #ANGDG0
                    ldb       #%00000000          ; ' '
                    jsr       Disp
                    jsr       ldred

_6@@                jsr       dft2dsp             ; wait for enter focal length
                    jsr       dispfl
                    jsr       ang2rmp
                    jsr       drf2rmp
                    tst       entflg
                    beq       _6@@

                    ldd       drift
                    std       flen                ; store focal length
                    clrd
                    std       drift
                    jsr       InitLED             ; overwrite 'FL' back to numeric

_7@@                tst       entflg
                    bne       _7@@
          #ifnz DEBUG
                    ldd       flen                ; print focal length for debug
                    jsr       out_dec
                    jsr       crlf
          #endif

;*******************************************************************************

MainLoop            proc
                    tst       cor_mod             ; 0= pre correction. 1= correction mode
                    bne       _1@@
                    jmp       _16@@               ; skip if not yet in correction mode

_1@@                jsr       ldgren              ; in corr mode led = green
                    tst       sec_flg             ; count down x y correction 1sec timers
                    bne       _2@@                ; go there if sec flag == 1
                    jmp       _17@@               ; nothing to do

_2@@                tst       lst_sec             ; if secflg == 1 and lastsec == 0 -> we just have pos edge
                    beq       _3@@                ; we have found the edge -> go there
                    jmp       _17@@               ; otherwise goto end of main loop

_3@@                inc       lst_sec             ; we just have a positive edge on secflag -> do the stuff
          #ifnz DEBUG
                    ldd       countx
                    jsr       out_dec
                    lda       #' '
                    jsr       sendv24
                    ldd       county
                    jsr       out_dec
                    jsr       crlf
          #endif
                    tst       xvalid              ; test if we have excluded X
                    beq       _9@@                ; don't consider x

                    ldx       xrmpcnt             ; dec ramp x counter
                    dex
                    stx       xrmpcnt
                    bne       _4@@                ; if not zero -> no ramp step needed

                    ldx       xrmplod             ; reset ramp x counter
                    stx       xrmpcnt
                    dec       xrmpld              ; dec x led ramp value

_4@@                ldx       countx
                    dex
                    stx       countx
                    bne       _9@@                ; countx > 0

                    ldx       cntxld              ; reset x-counter + apply x correction
                    stx       countx
                    ldx       xrmplod
                    stx       xrmpcnt
                    lda       #5
                    sta       xrmpld
                    jsr       ang2rmp
          #ifnz DEBUG
                    lda       #'X'
                    jsr       sendv24
                    jsr       crlf
          #endif
                    lda       quadr
          #ifz ST4_90                             ; This table for straight through guide scope
                    beq       _5@@                ; quadr=0 -> increment xref
                    cmpa      #1
                    beq       _6@@                ; quadr=1 -> decrement xref
                    cmpa      #2
                    beq       _6@@                ; quadr=2 -> decrement xref
                    cmpa      #3
                    beq       _5@@                ; quadr=3 -> increment xref
          #else                                   ; This table for guide scope with 90deg prism
                    beq       _6@@                ; quadr=0 -> decrement xref
                    cmpa      #1
                    beq       _5@@                ; quadr=1 -> increment xref
                    cmpa      #2
                    beq       _5@@                ; quadr=2 -> increment xref
                    cmpa      #3
                    beq       _6@@                ; quadr=3 -> decrement xref
          #endif
                    bra       _9@@
_5@@                lda       st4refx             ; get xref
                    cmpa      #ST4XMAX
                    beq       _9@@                ; see if we would go beyond max x -> yes, do nothing
                    inca                          ; increment xref
                    bra       _7@@

_6@@                lda       st4refx             ; get xref
                    beq       _9@@                ; see if we would go below 0 -> yes, do nothing
                    deca                          ; decrement xref
_7@@                sta       st4refx
_8@@                jsr       wr_xref             ; write to ST4
                    jsr       rd_xref
                    cmpa      st4refx
                    beq       _9@@
                    lda       st4refx
                    bra       _8@@

_9@@                tst       yvalid
                    beq       _15@@               ; don't consider y

                    ldx       yrmpcnt             ; dec ramp x counter
                    dex
                    stx       yrmpcnt
                    bne       _10@@               ; if not zero -> no ramp step needed

                    ldx       yrmplod             ; reset ramp x counter
                    stx       yrmpcnt
                    dec       yrmpld              ; dec x led ramp value

_10@@               ldx       county
                    dex
                    stx       county
                    bne       _15@@               ; county > 0 -> nothing to do

                    ldx       cntyld              ; reset y-counter + apply y correction
                    stx       county
                    ldx       yrmplod
                    stx       yrmpcnt
                    lda       #5
                    sta       yrmpld
                    jsr       drf2rmp
          #ifnz DEBUG
                    lda       #'Y'
                    jsr       sendv24
                    jsr       crlf
          #endif
                    lda       quadr
                    beq       _11@@               ; quadr=0 -> increment yref
                    cmpa      #1
                    beq       _11@@               ; quadr=1 -> increment yref
                    cmpa      #2
                    beq       _12@@               ; quadr=2 -> decrement yref
                    cmpa      #3
                    beq       _12@@               ; quadr=3 -> decrement yref
                    bra       _15@@               ; illegal quadrant value. should be 0...3

_11@@               lda       st4refy             ; get yref
                    cmpa      #ST4YMAX
                    beq       _15@@               ; see if we would go beyond max y -> yes, do nothing
                    inca                          ; increment yref
                    bra       _13@@

_12@@               lda       st4refy             ; get xref
                    beq       _15@@               ; see if we would go below 0 -> yes, do nothing
                    deca                          ; decrement yref
_13@@               sta       st4refy
_14@@               jsr       wr_yref             ; write to ST4
                    jsr       rd_yref
                    cmpa      st4refy
                    beq       _15@@
                    lda       st4refy
                    bra       _14@@

_15@@               bra       _18@@

_16@@               jsr       ldorng
_17@@               tst       sec_flg             ; see if sec flag still ==1
                    bne       _18@@               ; wait as long as secflag ==1
                    clr       lst_sec             ; reset lastflag so that we can find next edge
_18@@               jsr       ang2dsp             ; do all the display stuff
                    jsr       dft2dsp
                    jsr       dispang
                    jsr       dispdrf
                    jsr       ang2rmp
                    jsr       drf2rmp

                    tst       entflg              ; see if we have enter key pressed
                    bne       _20@@               ; if enter key pressed
                                                  ; -> have to do the x y calc and reset counters
_19@@               jmp       MainLoop            ; if no enter key go back to main loop start

_20@@               ldx       drift
                    beq       _19@@               ; No drift => dont enter corrections mode + go to main
                    lda       #1
                    sta       cor_mod

                    jsr       rd_xref             ; retrieve current ref position on ST4
                    sta       st4refx
                    jsr       rd_yref
                    sta       st4refy
          ;-------------------------------------- ; calc x y down counter values
                    ldd       angl                ; get angle to calc modulus 90deg and quadrant
                    clrx                          ; x will become quadrant
_21@@               cmpd      #89                 ; see if we can subtract 90deg (once again)
                    ble       _22@@               ; no. => 0<=D<=90
                    subd      #90                 ; yes. subtract
                    inx                           ; increment quadrant counter
                    bra       _21@@               ; loop around

_22@@               stb       angmod
                    xgdx
                    stb       quadr

                    ldd       angl
                    cmpd      #90                 ; if angle = 90 or 270 -> disable x-calc + x-corr
                    beq       _23@@
                    cmpd      #270
                    beq       _23@@

                    lda       #1
                    sta       xvalid
                    bra       _24@@

_23@@               clr       xvalid              ; disable x-calc + x-corr
                    clr       xrmpld
                    jsr       ang2rmp
                    bra       _25@@               ; jmp to y-calc. no x-calc required.

_24@@               ldd       flen                ; do x calculation
                    std       a24+1
                    clr       a24                 ; a24= Flen / 10
                    ldd       drift
                    std       b24+1               ; b24= Drift * 10
                    clr       b24
                    jsr       Mul24               ; a48= Flen * Drift

                    ldd       a48+3
                    std       d24
                    lda       a48+5
                    sta       d24+2               ; d24= a48= Flen * Drift

                    ldd       #XCSTHI
                    std       d48
                    ldd       #XCSTMD
                    std       d48+2
                    ldd       #XCSTLO
                    std       d48+4               ; d48= X_CONST

                    jsr       Div24               ; q24= X_CONST / ( Flen * Drift )
                    lda       angmod
                    jsr       cos_x
                    sta       d24+2
                    clr       d24+1
                    clr       d24                 ; d24= cos(angle) * 255

                    clr       d48
                    clr       d48+1
                    clr       d48+2
                    ldd       q24
                    std       d48+3
                    lda       q24+2
                    sta       d48+5               ; d48= X_CONST / ( Flen * Drift )

                    jsr       Div24               ; q24= X_CONST / ( Flen * Drift * cos(angle) )

                    ldd       q24+1
                    std       cntxld
                    std       countx              ; x counter value done
                    ldx       #5
                    idiv
                    stx       xrmplod
                    stx       xrmpcnt
                    lda       #5
                    sta       xrmpld

_25@@               ldd       angl
                    beq       _26@@               ; if angle=0 or 180 disable y-calc + y-corr
                    cmpd      #180
                    beq       _26@@
                    lda       #1
                    sta       yvalid
                    bra       _27@@

_26@@               clr       yvalid
                    clr       yrmpld
                    jsr       drf2rmp
                    bra       _28@@               ; skip y-calc

_27@@               ldd       flen                ; do y calculation
                    std       a24+1
                    clr       a24                 ; a24= Flen / 10
                    ldd       drift
                    std       b24+1               ; b24= Drift * 10
                    clr       b24
                    jsr       Mul24               ; a48= Flen * Drift

                    ldd       a48+3
                    std       d24
                    lda       a48+5
                    sta       d24+2               ; d24= a48= Flen * Drift

                    ldd       #YCSTHI
                    std       d48
                    ldd       #YCSTMD
                    std       d48+2
                    ldd       #YCSTLO
                    std       d48+4               ; d48= X_CONST

                    jsr       Div24               ; q24= X_CONST / ( Flen * Drift )
                    lda       angmod
                    jsr       sin_x
                    sta       d24+2
                    clr       d24+1
                    clr       d24                 ; d24= sin(angle) * 255

                    clr       d48
                    clr       d48+1
                    clr       d48+2
                    ldd       q24
                    std       d48+3
                    lda       q24+2
                    sta       d48+5               ; d48= X_CONST / ( Flen * Drift )

                    jsr       Div24               ; q24= X_CONST / ( Flen * Drift * sin(angle) )

                    ldd       q24+1
                    std       cntyld
                    std       county
                    ldx       #5
                    idiv
                    stx       yrmplod
                    stx       yrmpcnt
                    lda       #5
                    sta       yrmpld
_28@@
          #ifnz DEBUG
                    ldd       angl
                    jsr       out_dec
                    lda       #' '
                    jsr       sendv24
                    ldd       drift
                    jsr       out_dec
                    lda       #' '
                    jsr       sendv24
                    ldb       angmod
                    clra
                    jsr       out_dec
                    lda       #' '
                    jsr       sendv24
                    lda       quadr
                    adda      #'0'
                    jsr       sendv24
                    lda       #' '
                    jsr       sendv24
                    ldd       cntxld
                    jsr       out_dec
                    lda       #' '
                    jsr       sendv24
                    ldd       cntyld
                    jsr       out_dec
                    jsr       crlf
          #endif
_29@@               tst       entflg
                    bne       _29@@
                    jmp       MainLoop

;*******************************************************************************

InitLED             proc
                    lda       #15                 ; hi test register
                    ldb       #%00000000          ; lo 0 = normal ops
                    jsr       Disp

                    lda       #12                 ; hi shut down reg
                    ldb       #%00000001          ; lo 1 = normal ops
                    jsr       Disp

                    lda       #11                 ; hi scan limit reg
                    ldb       #%00000111          ; lo %0111 display all digits
                    jsr       Disp

                    lda       #10                 ; hi intensity reg
                    ldb       #%00001000          ; lo 1/2 intensity
                    jsr       Disp

                    lda       #9                  ; hi decode mode
                    ldb       #%01110111          ; lo left BCD right BCD
                    jmp       Disp

;*******************************************************************************

ang2rmp             proc
                    ldb       xrmpld
                    clra
                    addd      #LED_Table
                    xgdx
                    ldb       ,x
                    lda       #ANGRMP
                    jmp       Disp

;*******************************************************************************

dgreen              proc
                    lda       rgled
                    anda      #%11111100
                    ora       #%00000001
                    bra       dled

;*******************************************************************************

dred                proc
                    lda       rgled
                    anda      #%11111100
                    ora       #%00000010
                    bra       dled

;*******************************************************************************

dorng               proc
                    lda       rgled
                    anda      #%11111100
                    ora       #%00000011
;                   bra       dled
dled                sta       rgled
                    rts

;*******************************************************************************

ldgren              proc
                    bsr       dgreen
;                   bra       drf2rmp

;*******************************************************************************

drf2rmp             proc
                    ldb       yrmpld
                    clra
                    addd      #LED_Table
                    xgdx
                    ldb       ,x
                    orb       rgled
                    lda       #DRFRMP
                    jmp       Disp

;*******************************************************************************

ldred               proc
                    bsr       dred
                    bra       drf2rmp

;*******************************************************************************

ldorng              proc
                    bsr       dorng
                    bra       drf2rmp

;*******************************************************************************

ang2dsp             proc
                    ldd       angl
                    ldx       #10
                    idiv
                    stb       angdsp
                    xgdx
                    ldx       #10
                    idiv
                    stb       angdsp+1
                    xgdx
                    stb       angdsp+2
                    rts

;*******************************************************************************

dft2dsp             proc
                    ldd       drift
                    ldx       #10
                    idiv
                    stb       drfdsp
                    xgdx
                    ldx       #10
                    idiv
                    stb       drfdsp+1
                    xgdx
                    stb       drfdsp+2
                    rts

;*******************************************************************************

dispang             proc
                    lda       #ANGDG0
                    ldb       angdsp
                    bsr       Disp
                    lda       #ANGDG1
                    ldb       angdsp+1
                    bsr       Disp
                    lda       #ANGDG2
                    ldb       angdsp+2
                    bra       Disp

;*******************************************************************************

dispdrf             proc
                    lda       #DRFDG0
                    ldb       drfdsp
                    bsr       Disp
                    lda       #DRFDG1
                    ldb       drfdsp+1
                    orb       #%10000000
                    bsr       Disp
                    lda       #DRFDG2
                    ldb       drfdsp+2
                    bra       Disp

;*******************************************************************************

dispfl              proc
                    lda       #DRFDG0
                    ldb       drfdsp
                    bsr       Disp
                    lda       #DRFDG1
                    ldb       drfdsp+1
                    bsr       Disp
                    lda       #DRFDG2
                    ldb       drfdsp+2
                    bra       Disp

;*******************************************************************************

dispdec             proc
                    clra                          ; accu b into angle disp as dec
                    ldx       #10
                    idiv
                    pshb
                    xgdx
                    ldx       #10
                    idiv
                    pshb
                    xgdx
                    lda       #ANGDG2
                    bsr       Disp
                    pulb
                    lda       #ANGDG1
                    bsr       Disp
                    pulb
                    lda       #ANGDG0
;                   bra       disp

;*******************************************************************************

Disp                proc
                    push
                    ldx       #REGS
                    bclr      [PORTA,x,CLK
                    bclr      [PORTA,x,LOAD
                    bclr      [PORTA,x,DATA
                    ldy       #16
Loop@@              lsld
                    bcc       Zero@@
                    bset      [PORTA,x,DATA
                    bra       Clock@@
Zero@@              bclr      [PORTA,x,DATA
Clock@@             bset      [PORTA,x,CLK
                    bclr      [PORTA,x,CLK
                    dey
                    bne       Loop@@
                    bset      [PORTA,x,LOAD
                    pull
                    rts

;*******************************************************************************

Fatal               proc
                    std       angl
                    clrd
                    std       drift
                    jsr       ang2dsp
                    jsr       dft2dsp
                    jsr       dispang
                    jsr       dispdrf
                    jsr       ldred
                    bra       *

;*******************************************************************************
          #ifnz DEBUG
rd_stat             proc
                    clra
wr_stat             rts

rd_xref             proc
                    lda       st4refx
                    rts

;*******************************************************************************

rd_yref             proc
                    lda       st4refy
                    rts

;*******************************************************************************

wr_xref             proc
                    tab
                    lda       #'X'
                    bsr       sendv24
                    clra
                    bsr       out_dec
                    bra       crlf

;*******************************************************************************

wr_yref             proc
                    tab
                    lda       #'Y'
                    bsr       sendv24
                    clra
                    bsr       out_dec
                    bra       crlf
          #else
rd_stat             proc
                    lda       #10
                    sta       errcnt              ; init error counter
rd_st02             lda       #2                  ; read RAM cmd. and reetry point for error recovery
                    jsr       sendv24
                    lda       #1                  ; number of bytes
                    jsr       sendv24
                    lda       #1                  ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4ST             ; LSB of Status address
                    jsr       sendv24
                    lda       #]ST4ST             ; MSB of status address
                    jsr       sendv24
                    lda       #$33                ; checksum=$02+$01+$01+$2f+$00
                    jsr       sendv24
          ;-------------------------------------- ; response :    02 01 ST CS
                    ldx       #st4buf             ; pointer to read buffer
rd_st05             lda       #$ff                ; init value for timeout
                    sta       timout
                    ldy       recvrd
rd_st10             cpy       recvwr              ; check if we have received something
                    bne       rd_st15             ; we have received something
                    tst       timout              ; wait a while to receive data
                    bne       rd_st10             ; lop around to try wait more
                    dec       errcnt              ; time out! count down error
                    bne       rd_st02             ; loop back to send command
                    ldd       #10                 ; had 10 failures -> fatal error
                    bra       Fatal               ; go fatal

rd_st15             lda       ,y
                    sta       ,x                  ; copy v24 receive data to st4 receive buffer
                    inx
                    cpy       #recvbuf+15
                    bne       rd_st20
                    ldy       #recvbuf-1
rd_st20             iny
                    sty       recvrd
                    cpx       #st4buf+4
                    bne       rd_st05             ; go back for next char. incl re-init of timeout

                    lda       st4buf              ; all received. now check
                    cmpa      #2
                    bne       rd_st95
                    lda       st4buf+1
                    cmpa      #1
                    bne       rd_st95

                    clra
                    ldx       #st4buf
rd_st30             adda      ,x                  ; add up checksum
                    inx
                    cpx       #st4buf+3
                    bne       rd_st30
                    cmpa      ,x                  ; compare with received checksum value
                    bne       rd_st95             ; goto error handler if wrong checksum
                    lda       st4buf+2            ; retrieve status byte and return it
                    rts

rd_st95             dec       errcnt              ; count down for error
                    beq       rd_st97
                    jmp       rd_stat             ; if received data are bad goto re issue command

rd_st97             ldd       #1                  ; if 10 errors encountered -> goto fatal halt
                    jmp       Fatal

;*******************************************************************************

wr_stat             proc
                    sta       tmp
                    lda       #10
                    sta       errcnt
Loop@@              lda       #1                  ; write RAM cmd
                    jsr       sendv24
                    lda       #4                  ; number of bytes
                    jsr       sendv24
                    lda       #1                  ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4ST             ; LSB of Status address
                    jsr       sendv24
                    lda       #]ST4ST             ; MSB of status address
                    jsr       sendv24
                    lda       tmp                 ; status byte
                    jsr       sendv24
                    lda       tmp
                    adda      #$35                ; checksum $35=$01+$04+$01+$2f+$00
                    jsr       sendv24
          ;-------------------------------------- ; response :    06
                    ldy       recvrd
                    lda       #$ff
                    sta       timout
_1@@                cpy       recvwr
                    bne       _2@@                ; wait till we have received something
                    tst       timout
                    bne       _1@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #11
                    bra       Fail@@

_2@@                lda       ,y
                    cpy       #recvbuf+15
                    bne       _3@@
                    ldy       #recvbuf-1
_3@@                iny
                    sty       recvrd
                    cmpa      #6
                    beq       Done@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #3
Fail@@              jmp       Fatal
Done@@              equ       :AnRTS

;*******************************************************************************
; x ref addr $7e7c

rd_xref             proc
Go@@                lda       #10
                    sta       errcnt
Loop@@              lda       #2                  ; read RAM cmd
                    jsr       sendv24
                    lda       #1                  ; number of bytes
                    jsr       sendv24
                    clra                          ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4XRF            ; LSB of x ref coordinate address
                    jsr       sendv24
                    lda       #]ST4XRF            ; MSB of x ref coordinate address
                    jsr       sendv24
                    lda       #$FD                ; checksum $FD=$02+$01+$00+$7c+$7e
                    jsr       sendv24
          ;-------------------------------------- ; response $02 $01 xref CS
                    ldx       #st4buf
_1@@                lda       #$ff
                    sta       timout
                    ldy       recvrd
_2@@                cpy       recvwr
                    bne       _3@@                ; wait till we have received something
                    tst       timout
                    bne       _2@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #12
                    jmp       Fatal

_3@@                lda       ,y
                    sta       ,x                  ; copy v24 receive data to st4 receive buffer
                    inx
                    cpy       #recvbuf+15
                    bne       _4@@
                    ldy       #recvbuf-1
_4@@                iny
                    sty       recvrd
                    cpx       #st4buf+4
                    bne       _1@@

                    lda       st4buf              ; all received. now check
                    cmpa      #2
                    bne       _6@@
                    lda       st4buf+1
                    cmpa      #1
                    bne       _6@@

                    clra
                    ldx       #st4buf
_5@@                adda      ,x                  ; add up checksum
                    inx
                    cpx       #st4buf+3
                    bne       _5@@
                    cmpa      ,x                  ; compare with received checksum value
                    bne       _6@@                ; goto error handler if wrong checksum
                    lda       st4buf+2            ; retrieve x ref byte and return it
                    rts

_6@@                dec       errcnt              ; count down for error
                    jne       Go@@

                    ldd       #4                  ; if 10 errors encountered -> goto fatal halt
                    jmp       Fatal

;*******************************************************************************
; y ref addr $7e7d

rd_yref             proc
                    lda       #10
                    sta       errcnt
Loop@@              lda       #2                  ; read RAM cmd
                    jsr       sendv24
                    lda       #1                  ; number of bytes
                    jsr       sendv24
                    clra                          ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4YRF            ; LSB of y ref coordinate address
                    jsr       sendv24
                    lda       #]ST4YRF            ; MSB of y ref coordinate address
                    jsr       sendv24
                    lda       #$FE                ; checksum $FE=$02+$01+$00+$7d+$7e
                    jsr       sendv24
          ;-------------------------------------- ; response $02 $01 yref CS
                    ldx       #st4buf
_1@@                lda       #$ff
                    sta       timout
                    ldy       recvrd
_2@@                cpy       recvwr
                    bne       _3@@                ; wait till we have received something
                    tst       timout
                    bne       _2@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #13
                    jmp       Fatal

_3@@                lda       ,y
                    sta       ,x                  ; copy v24 receive data to st4 receive buffer
                    inx
                    cpy       #recvbuf+15
                    bne       _4@@
                    ldy       #recvbuf-1
_4@@                iny
                    sty       recvrd
                    cpx       #st4buf+4
                    bne       _1@@

                    lda       st4buf              ; all received. now check
                    cmpa      #2
                    bne       _6@@
                    lda       st4buf+1
                    cmpa      #1
                    bne       _6@@

                    clra
                    ldx       #st4buf
_5@@                adda      ,x                  ; add up checksum
                    inx
                    cpx       #st4buf+3
                    bne       _5@@
                    cmpa      ,x                  ; compare with received checksum value
                    bne       _6@@                ; goto error handler if wrong checksum
                    lda       st4buf+2            ; retrieve x ref byte and return it
                    rts

_6@@                dec       errcnt              ; count down for error
                    jne       Loop@@
                    ldd       #5                  ; if 10 errors encountered -> goto fatal halt
                    jmp       Fatal

;*******************************************************************************

wr_xref             proc
                    sta       tmp
                    lda       #10
                    sta       errcnt
Loop@@              lda       #1                  ; write RAM cmd
                    jsr       sendv24
                    lda       #4                  ; number of bytes
                    jsr       sendv24
                    clra                          ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4XRF            ; LSB of x ref address $7e7c
                    jsr       sendv24
                    lda       #]ST4XRF            ; MSB of x ref address
                    jsr       sendv24
                    lda       tmp                 ; x ref byte
                    jsr       sendv24
                    lda       tmp
                    adda      #$ff                ; checksum $ff=$01+$04+$00+$7c+$7e
                    jsr       sendv24
          ;-------------------------------------- ; response :    06
                    ldy       recvrd
                    lda       #$ff
                    sta       timout
_1@@                cpy       recvwr
                    bne       _2@@                ; wait till we have received something
                    tst       timout
                    bne       _1@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #14
                    bra       Fail@@

_2@@                lda       ,y
                    cpy       #recvbuf+15
                    bne       _3@@
                    ldy       #recvbuf-1
_3@@                iny
                    sty       recvrd
                    cmpa      #6
                    beq       Done@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #6
Fail@@              jmp       Fatal
Done@@              rts

;*******************************************************************************

wr_yref             proc
                    sta       tmp
                    lda       #10
                    sta       errcnt
Loop@@              lda       #1                  ; write RAM cmd
                    bsr       sendv24
                    lda       #4                  ; number of bytes
                    bsr       sendv24
                    clra                          ; 0=ext, 1=int ram
                    bsr       sendv24
                    lda       #[ST4YRF            ; LSB of y ref address $7e7d
                    bsr       sendv24
                    lda       #]ST4YRF            ; MSB of y ref address
                    bsr       sendv24
                    lda       tmp                 ; x ref byte
                    bsr       sendv24
                    lda       tmp                 ; checksum $00=$01+$04+$00+$7d+$7e
                    bsr       sendv24
          ;-------------------------------------- ; response :    06
                    ldy       recvrd
                    lda       #$ff
                    sta       timout
_1@@                cpy       recvwr
                    bne       _2@@                ; wait till we have received something
                    tst       timout
                    bne       _1@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #15
                    bra       Fail@@

_2@@                lda       ,y
                    cpy       #recvbuf+15
                    bne       _3@@
                    ldy       #recvbuf-1
_3@@                iny
                    sty       recvrd
                    cmpa      #6
                    beq       Done@@
                    dec       errcnt
                    bne       Loop@@
                    ldd       #7
Fail@@              jmp       Fatal
Done@@              equ       :AnRTS
          #endif

;*******************************************************************************

out_dec             proc
                    ldy       #5
_1@@                ldx       #10
                    idiv
                    pshb
                    xgdx
                    dey
                    bne       _1@@
          ;--------------------------------------
                    ldb       #5
_2@@                pula
                    adda      #'0'                ;convert to ASCII
                    bsr       sendv24
                    decb
                    bne       _2@@
                    rts

;*******************************************************************************

sendv24             proc
                    pshx
                    ldx       sendwr
                    sta       ,x
                    cpx       #sendbuf+15
                    bne       _1@@
                    ldx       #sendbuf-1
_1@@                inx
Loop@@              cpx       sendrd
                    beq       Loop@@
                    stx       sendwr
                    pulx
                    rts

;*******************************************************************************

crlf                proc
                    psha
                    lda       #13                 ;Carriage Return
                    bsr       sendv24
                    lda       #10                 ;Line Feed
                    bsr       sendv24
                    pula
                    rts

;*******************************************************************************
;Calculate the 48 bit unsigned product of two 24 bit unsigned numbers.
;Place the multiplicand in a24, the multiplier in b24, then JSR Mul24.
;Result is returned in a48.  Put smaller value in b24 when given a
;choice.
;          Note: a48 must immediately follow b24 in memory !!!!
;b24 may overlap a48 to save space if desired - some changes req'd.
;The approach used is similar to multiplication with pencil and paper,
;where bytes are used as if they were digits. It may be extended to
;any reasonable number of bytes quite easily.

Mul24               proc
                    pshd                          ; Preserve regs
                    pshx
                    ldx       #b24                ; Address of args
                    clr       3,x                 ; Clear a48
                    clr       4,x
                    clr       5,x
                    clr       6,x
                    clr       7,x
                    clr       8,x
                    lda       #3                  ; # bytes in multiplier
                    sta       counter

Loop@@              lda       2,x
                    beq       Cont@@              ; Skip if 0 multiplier
                    ldb       a24+2               ; Least signifigant byte of multiplicand
                    mul
                    addd      7,x
                    std       7,x
                    bcc       _1@@
                    inc       6,x                 ; Propagate the carry
                    bne       _1@@
                    inc       5,x
_1@@                lda       2,x
                    ldb       a24+1
                    mul
                    addd      6,x
                    std       6,x
                    bcc       _2@@
                    inc       5,x                 ; Propagate Carry
_2@@                lda       2,x
                    ldb       a24                 ; Most signifigant byte
                    mul
                    addd      5,x
                    std       5,x                 ; No carry possible on hi end
Cont@@              dex                           ; Move left 1 byte in multiplier & result
                    dec       counter
                    bne       Loop@@
                    pulx                          ; Restore regs
                    puld
                    rts

;*******************************************************************************
; Calculate the 24 bit unsigned quotient resulting from division of a
; 48 bit dividend by a 24 bit divisor. Place the divisor in d24 and the
; dividend in d48. The result will be returned in q24.
;
; The approach is similar to long division by hand, but here the
; IDIV instruction is used to form the trial quotient. Care is taken
; to ensure that the trial quotient is always <= the required quotient.
;
; The division routine can be modified to handle any reasonable number
; of bytes, but it is more challenging than extending the multiplication
; routine.
;
; Typical operation is two passes through the loop per byte in the dividend.

Div24               proc
                    pshd
                    pshx
                    pshy

                    clr       q24                 ; Clear quotient to 0
                    clr       q24+1
                    clr       q24+2
                    ldb       #6                  ; Size of dividend, max
                    ldy       #d48                ; Count bytes in the dividend
Loop@@              tst       ,y
                    bne       DIV1
                    decb
                    beq       DIVER1              ; Done if dividend = 0, Return 0
                    iny
                    bra       Loop@@

DIVER1              jmp       DIVDUN              ; Extend branch range

; Find size of divisor and dividend & thus the hi byte of quotient.
; Also do gross checks to avoid dividing by 0, etc.

DIV1                stb       dividend_size       ; Save # bytes in dividend
                    lda       #$FF
                    negb
                    addd      #d48+5              ; + Addr of LS Byte-1 in dividend
                    xgdy                          ; Y points to MSB of dividend
                    lda       #3                  ; Count bytes in the divisor
                    ldx       #d24
DIV1CT              tst       ,x
                    bne       DIV2
                    deca
                    beq       DIVER1              ; Quit if divide by 0, Return 0
                    inx
                    bra       DIV1CT

DIV2                stx       .divisor            ; Ptr to MSB of divisor
                    sta       divisor_size        ; Number of bytes in divisor
                    suba      dividend_size
                    bgt       DIVER1              ; Quit if divisor > dividend, Return 0
                    ldb       1,y                 ; Get hi byte of dividend
                    cmpb      ,x                  ; IF hi byte of dividend > hi byte of divisor
                    blo       DIV3                ; THEN one more byte needed in the quotient
                    deca
                    dey                           ; and in Dividend
DIV3                tab
                    nega                          ; Make it positive
                    sta       quotient_remaining  ; # Bytes in quotient
                    cmpa      #4                  ; ??? MAKE THIS PREVENT OVERFLOW ???
                    bgt       DIVER1              ; Quit if quotient too big, return 0
                    lda       #$FF                ; Make it a negative 16 bit value
                    addd      #q24+2              ; Ptr to hi byte of quotient
                    std       .quotient_byte

; Decide whether to use 1 or 2 bytes as trial divisor

DIVLUP              ldx       .divisor            ; Get ptr to MSB of divisor
                    lda       divisor_size        ; Get # bytes in divisor
                    deca
                    jeq       DIV1BYT             ; Only 1 byte in divisor
                    sta       counter             ; Temp, # bytes in divisor -1
                    ldd       1,y                 ; Get hi word of dividend in B
                    cpd       ,x                  ; IF hi word of dividend < hi word of divisor
                    blo       DIVBYB              ; THEN use only MSB of divisor
                    bhi       DIVW2B              ; Divisor < Dividend & 2+ bytes in divisor
                    lda       counter
                    deca
                    beq       DIVWRD              ; BR if divisor = MSB's of dividend
                    ldb       3,y                 ; Get next byte of dividend in B
                    cmpb      2,x                 ; IF next byte of dividend < this byte of divisor
                    blo       DIVBYB              ; THEN use only MSB of divisor, rounded up
                    ldb       #1                  ; ELSE Trial Quotient = 1
                    bra       DIVSTO

DIVBYB              jmp       DIVBYT              ; Extend branch range

;*******************************************************************************
; Trial divisor is 2 bytes. Round up if required.

DIVW2B              lda       counter
                    deca                          ; Make A = 0 if exactly 2 bytes in divisor
DIVWRD              ldx       ,x                  ; Get MSW of divisor
                    tsta
                    beq       DIVWNR              ; BR if exactly 2 bytes in divisor
                    inx                           ; Round divisor up
DIVWNR              ldd       1,y                 ; Get MSW of dividend
                    idiv
                    xgdx                          ; Get trial quotient into B

; Multiply divisor by trial quotient and subtract from dividend

DIVSTO              stb       quotient_trial_val  ; Save trial quotient
                    ldx       .quotient_byte
                    addb      ,x                  ; Add Trial Q to current Q
                    stb       ,x
                    ldb       divisor_size
                    stb       counter
                    ldx       .divisor
                    lda       ,x                  ; Get hi byte of divisor
                    ldb       quotient_trial_val  ; Get trial quotient byte
                    mul
                    std       work_space          ; Seems crude, but it works...
                    ldd       ,y
                    subd      work_space          ; No borrow possible from hi byte
                    std       ,y
                    dec       counter
                    beq       DIVLND              ; BR if divisor is 1 byte
                    ldx       .divisor
                    lda       1,x                 ; Get next byte of divisor
                    ldb       quotient_trial_val
                    mul
                    std       work_space
                    ldd       1,y
                    subd      work_space
                    std       1,y
                    bcc       SkipPropagation
                    dec       ,y                  ; Propagate borrow
SkipPropagation     dec       counter
                    beq       DIVLND              ; BR if divisor is 2 bytes
                    ldx       .divisor
                    lda       2,x
                    ldb       quotient_trial_val
                    mul
                    std       work_space
                    ldd       2,y
                    subd      work_space
                    std       2,y
                    bcc       DIVLND
                    ldx       ,y                  ; Propagate borrow
                    dex
                    stx       ,y
DIVLND              ldd       ,y
                    beq       DIVDN0
                    tsta
                    beq       DIVLN2              ; Almost always branches, 4000:1 or so
                    ldx       .divisor            ; Ptr to MSB of divisor
                    bra       DIVBYR              ; Last divide by byte was too coarse, correct it

DIVDN0              iny                           ; Hi word of dividend=0, move 1 position right
                    dec       quotient_remaining
                    blt       DIVDUN
                    inc       .quotient_byte+1
DIVLN2              jmp       DIVLUP              ; Loop to do next byte

;*******************************************************************************
; Trial divisor is hi byte of divisor.  Always round it up.

DIVBYT              proc
                    dec       quotient_remaining  ; Divisor > dividend, move 1 position right
                    blt       DIVDUN              ; BR if done
                    inc       .quotient_byte+1
                    iny
DIVBYR              ldb       ,x                  ; Get MSB of divisor
                    clra
                    xgdx
                    inx                           ; Round divisor up
                    ldd       ,y                  ; Dividend, current hi 16 bits
                    beq       DIVDN0              ; Shortcut on 0 dividend ?? Does this help??
                    idiv                          ; MSW of dividend / MSB of divisor = Trial Q
                    xgdx                          ; Get quotient in D
                    tsta
                    beq       Done@@
                    ldb       #$F0                ; Overflow on this trial, force max
Done@@              jmp       DIVSTO

;*******************************************************************************
; Handle single byte divisor specially

DIV1BYT             proc
                    ldb       ,x                  ; Get divisor, single byte
                    clra
                    xgdx
Loop@@              cpx       ,y
                    bls       Go@@
                    dec       quotient_remaining  ; Divisor > Dividend, move 1 position right
                    blt       DIVDUN              ; BR if done
                    inc       .quotient_byte+1
                    iny
                    bra       Loop@@

Go@@                ldd       ,y                  ; Dividend, current hi 16 bits
                    idiv                          ; MSW of dividend / divisor = Trial Q
                    xgdx                          ; Get quotient in D
                    jmp       DIVSTO

;*******************************************************************************

DIVDUN              puly
                    pulx                          ; Restore regs
                    puld
                    rts

;*******************************************************************************

cos_x               proc
                    ldb       quadr
                    beq       _1@@
                    cmpb      #1
                    beq       _2@@
                    cmpb      #3
                    beq       _2@@
                    cmpb      #2
                    bne       *                   ;should not happen

_1@@                nega
                    adda      #90

_2@@                tab
                    clra
                    addd      #SineTable
                    xgdx
                    lda       ,x
                    rts

;*******************************************************************************

sin_x               proc
                    ldb       quadr
                    beq       _2@@
                    cmpb      #1
                    beq       _1@@
                    cmpb      #2
                    beq       _2@@
                    cmpb      #3
                    bne       *                   ;should not happen

_1@@                nega
                    adda      #90

_2@@                tab
                    clra
                    addd      #SineTable
                    xgdx
                    lda       ,x
                    rts

;*******************************************************************************

RTI_Handler         proc
                    ldx       #REGS
                    lda       #%01000000
                    sta       [TFLG2,x

                    tst       timout
                    beq       _1@@
                    dec       timout

_1@@                inc       sec_cnt
                    lda       sec_cnt
                    cmpa      #62
                    bne       _2@@
                    clr       sec_cnt
                    lda       sec_flg
                    eora      #%00000001
                    sta       sec_flg

_2@@                inc       key_cnt
                    lda       key_cnt
                    cmpa      #12
                    beq       _3@@
                    rti                           ; skip all irq until key_cnt = 12

_3@@                clr       key_cnt
                    lda       [PORTD,x
                    sta       pdmir
                    anda      #ALLKEY
                    cmpa      #ALLKEY
                    beq       _9@@
                    anda      #ANGKEY
                    cmpa      #ANGKEY
                    beq       _7@@                ; no ANG key pressed
                    cmpa      #ANGPL
                    bne       _5@@

                    ldd       angl
                    incd
                    cmpd      #360
                    bne       _4@@
                    clrd
                    std       angl
_4@@                std       angl
                    bra       _7@@

_5@@                ldd       angl
                    decd
                    bpl       _6@@
                    ldd       #359
_6@@                std       angl

_7@@                lda       pdmir
                    anda      #DRFKEY
                    cmpa      #DRFKEY
                    beq       _9@@
                    cmpa      #DRFPL
                    bne       _8@@

                    ldd       drift
                    cmpd      #999
                    beq       _9@@
                    incd
                    std       drift
                    bra       _9@@

_8@@                ldd       drift
                    beq       _9@@
                    decd
                    std       drift
;                   bra       _9@@

_9@@                lda       [PORTA,x
                    anda      #ENTKEY
                    bne       _10@@
                    lda       #1
                    sta       entflg
                    bra       _11@@

_10@@               clr       entflg
_11@@               bra       sio_snd             ; see if we have to initiate sending

;*******************************************************************************

SCI_Handler         proc
                    ldx       #REGS
                    lda       [SCSR,x
                    sta       scsr_mr
                    anda      #%00100000
                    bne       _1@@
                    lda       scsr_mr
                    anda      #%10000000
                    bne       sio_snd
                    rti                           ; if not rcv or snd. No other irq served.

_1@@                lda       [SCDR,x             ; dont care for overrun error yet
                    ldy       recvwr
                    sta       ,y
                    cpy       #recvbuf+15
                    bne       _2@@
                    ldy       #recvbuf-1
_2@@                iny
                    sty       recvwr
                    lda       scsr_mr
                    anda      #%10000000
                    bne       sio_snd
                    rti

;*******************************************************************************

sio_snd             proc
                    ldy       sendrd
                    cpy       sendwr
                    beq       _4@@                ; nothing to send
                    brclr     [SCSR,x,%10000000,* ; wait till send reg empty

                    pshx
                    ldx       #8
                    clrb
                    lda       ,y
                    psha
Loop@@              lsra
                    bcc       Cont@@
                    incb
Cont@@              dex
                    bne       Loop@@
                    pula
                    pulx
                    andb      #1
                    bne       _1@@
                    bclr      [SCCR1,x,%01000000
                    bra       _2@@

_1@@                bset      [SCCR1,x,%01000000

_2@@                sta       [SCDR,x
                    cpy       #sendbuf+15
                    bne       _3@@
                    ldy       #sendbuf-1
_3@@                iny
                    sty       sendrd
                    cpy       sendwr
                    beq       _4@@                ; nothing more to send
                    bset      [SCCR2,x,%10000000  ; if more bytes to send set irq enable bit
                    bra       Done@@

_4@@                bclr      [SCCR2,x,%10000000  ; if no more bytes to send clr irq enable
Done@@              rti

;*******************************************************************************

LED_Table           fcb       %00000000
                    fcb       %00000100
                    fcb       %00100100
                    fcb       %00110100
                    fcb       %00111100
                    fcb       %01111100
;LED_Table1
                    fcb       %00000000
                    fcb       %00000100
                    fcb       %00100000
                    fcb       %00010000
                    fcb       %00001000
                    fcb       %01000000

SineTable           fcb       000                 ; 0
                    fcb       004
                    fcb       009
                    fcb       013
                    fcb       018
                    fcb       022
                    fcb       027
                    fcb       031
                    fcb       035
                    fcb       040
                    fcb       044                 ; 10
                    fcb       049
                    fcb       053
                    fcb       057
                    fcb       062
                    fcb       066
                    fcb       070
                    fcb       075
                    fcb       079
                    fcb       083
                    fcb       087                 ; 20
                    fcb       091
                    fcb       096
                    fcb       100
                    fcb       104
                    fcb       108
                    fcb       112
                    fcb       116
                    fcb       120
                    fcb       124
                    fcb       127                 ; 30
                    fcb       131
                    fcb       135
                    fcb       139
                    fcb       143
                    fcb       146
                    fcb       150
                    fcb       153
                    fcb       157
                    fcb       160
                    fcb       164                 ; 40
                    fcb       167
                    fcb       171
                    fcb       174
                    fcb       177
                    fcb       180
                    fcb       183
                    fcb       186
                    fcb       190
                    fcb       192
                    fcb       195                 ; 50
                    fcb       198
                    fcb       201
                    fcb       204
                    fcb       206
                    fcb       209
                    fcb       211
                    fcb       214
                    fcb       216
                    fcb       219
                    fcb       221                 ; 60
                    fcb       223
                    fcb       225
                    fcb       227
                    fcb       229
                    fcb       231
                    fcb       233
                    fcb       235
                    fcb       236
                    fcb       238
                    fcb       240                 ; 70
                    fcb       241
                    fcb       243
                    fcb       244
                    fcb       245
                    fcb       246
                    fcb       247
                    fcb       248
                    fcb       249
                    fcb       250
                    fcb       251                 ; 80
                    fcb       252
                    fcb       253
                    fcb       253
                    fcb       254
                    fcb       254
                    fcb       254
                    fcb       255
                    fcb       255
                    fcb       255
                    fcb       255                 ; 90
#ifz DEBUG
;*******************************************************************************
                    #EEPROM   $FF00               ; start of EEPROM
;*******************************************************************************

RESET               proc                          ; INITIALIZE THE CPU
                    lds       #$01FF              ; put stack in CPU RAM
                    ldx       #REGS               ; register base address
                    lda       #%10010001          ; adpu, irqe, dly, cop = 65mS
                    sta       [OPTION,x
                    lda       #%00000000
                    sta       [TMSK2,x
                    lda       #%00000000
                    sta       [BPROT,x            ; make CONFIG & EEPROM writable
                    lds       #$03ff
                    lda       #%00000101
                    sta       [CSCTL,x            ; enable program CS for 32K
                    lda       #%00000000
                    sta       [CSGADR,x           ; RAM starts at address 0000H
                    lda       #%00000001
                    sta       [CSGSIZ,x           ; RAM block size is 32K
                    lda       #%00011111
                    sta       [DDRG,x             ; bank select bits = outputs
                    lda       #%00000000
                    sta       [PORTG,x            ; select 1ST bank
                    jmp       Start

;*******************************************************************************

AnRTI               rti                           ; keine Bearbeitung fuer diese IRQ's

;*******************************************************************************
                    #VECTORS  $FFD6               ; Start der Vectortabelle
;*******************************************************************************

                    dw        SCI_Handler
                    dw:12     AnRTI
                    dw        RTI_Handler
                    dw:6      AnRTI
                    dw        RESET
#endif
