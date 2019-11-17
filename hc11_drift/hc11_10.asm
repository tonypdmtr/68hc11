;*******************************************************************************
; HC11_DRIFT
; Version 1.0
; Copyright Gert Gottschalk (drgert1@yahoo.com)
; Algorithm by Paul Mortfield (paul@BackyardAstronomer.com)
; You may copy this software for non commercial use,
; as long as you mention the original developer's copyright
;*******************************************************************************

; Instructions:
;   1  running Buffalo
;   2  "load t" (enter)
;   3  send the file
;       HyperTerminal (transfer, send text file, all files)
;   4  wait for completion of file send
;   5  "md 2000" (enter) will display the memory containing the program
;   6  "go 2000" (enter) to run the program

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

REGBS               equ       $1000               ; start of CPU register block
PORTA               equ       $00                 ; PORT A
DDRA                equ       $01                 ; PORT A DDR
PORTG               equ       $02                 ; PORT G
DDRG                equ       $03                 ; PORT G DDR
PORTB               equ       $04                 ; PORT B
PORTCL              equ       $05                 ; PROT C LATCHED DATA REG.
DDRC                equ       $07                 ; DATA DIRECTION REGISTER C
PORTD               equ       $08                 ; PORT D
DDRD                equ       $09                 ; DATA DIRECTION REGISTER D
PORTE               equ       $0A                 ; PORT E
CFORC               equ       $0B                 ; TIMER COMPARE FORCE REG.
OC1M                equ       $0C                 ; O/P COMPARE 1 MASK REG.
OC1D                equ       $0D                 ; O/P COMPARE 1 DATA REG
TCNT                equ       $0E                 ; TIMER COUNT H
TCNTL               equ       $0F                 ; L
TIC1                equ       $10                 ; TIMER O/P COMPARE 1 H
TIC1L               equ       $11                 ; 1 L
TIC2                equ       $12                 ; 2 H
TIC2L               equ       $13                 ; 2 L
TIC3                equ       $14                 ; 3 H
TIC3L               equ       $15                 ; 3 L
TOC1                equ       $16                 ; TIMER O/P COMPARE 1 H
TOC1L               equ       $17                 ; 1 L
TOC2                equ       $18                 ; 2 H
TOC2L               equ       $19                 ; 2 L
TOC3                equ       $1A                 ; 3 H
TOC3L               equ       $1B                 ; 3 L
TOC4                equ       $1C                 ; 4 H
TOC4L               equ       $1D                 ; 4 L
TOC5                equ       $1E                 ; 5 H
TOC5L               equ       $1F                 ; 5 L
TCTL1               equ       $20                 ; TIMER CONTROL REG 1
TCTL2               equ       $21                 ; TIMER CONTROL REG 2
TMSK1               equ       $22                 ; TIMER MASK 1
TFLG1               equ       $23                 ; TIMER FLAG 1
TMSK2               equ       $24                 ; TIMER MASK 2
TFLG2               equ       $25                 ; TIMER FLAG 2
PACTL               equ       $26                 ; PULSE ACCUMULATOR CONT. REG.
PACNT               equ       $27                 ; PULSE ACCUMULATOR COUNT REG.
SPCR                equ       $28
SPSR                equ       $29
SPDR                equ       $2A
BAUD                equ       $2B                 ; SCI BAUD REG
SCCR1               equ       $2C                 ; SCI CONTROL 1 REG
SCCR2               equ       $2D                 ; SCI CONTROL 2 REG
SCSR                equ       $2E                 ; SCI STATUS REG
SCDR                equ       $2F                 ; SCI DATA REG
ADCTL               equ       $30
ADR1                equ       $31
ADR2                equ       $32
ADR3                equ       $33
ADR4                equ       $34
BPROT               equ       $35
OPTION              equ       $39                 ; OPTION REG
COPRST              equ       $3A                 ; COP RESET REG
PPROG               equ       REGBS+$3B           ; EEPROM PROG REG
HPRIO               equ       $3C                 ; HPRIO REG
INIT                equ       $3D                 ; INIT
CONFIG              equ       $3F                 ; CONFIG REG
CSSTRH              equ       $5C                 ; CS CYCLE STRETCH
CSCTL               equ       $5D                 ; CS CONTROL
CSGADR              equ       $5E                 ; GENERAL PURPOSE CS (RAM)
CSGSIZ              equ       $5F                 ; GENERAL PURPOSE CS SIZE

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
                    #RAM
;*******************************************************************************
                    org       $0800               ; variable area

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
angdsp0             rmb       1                   ; mirror angle digit 0
angdsp1             rmb       1                   ; mirror angle digit 1
angdsp2             rmb       1                   ; mirror angle digit 2
drfdsp0             rmb       1                   ; mirror drift digit 0
drfdsp1             rmb       1                   ; mirror drift digit 1
drfdsp2             rmb       1                   ; mirror dirft digit 2
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

;****************************
; Stuff for multiply divide
;****************************

counter             rmb       1                   ; Counter for mul/div
.divisor            rmb       2                   ; Ptr to MS byte of divisor
divisor_size        rmb       1                   ; Number of bytes in the divisor
dividend_size       rmb       1                   ; Number of bytes in the dividend
.quotient_byte      rmb       2                   ; Ptr to current byte of quotient
quotient_remaining  rmb       1                   ; Number of bytes remaining to be found in quotient
quotient_trial_val  rmb       1                   ; Current quotient trial value

; Temp storage for multiply

a24                 rmb       3                   ; Argument for multiple precision mul
b24                 rmb       3                   ; Argument for multiple precision mul
a48                 rmb       6                   ; Product from multiple precision mul

; Temp storage for  divide

d24                 rmb       3                   ; Divisor
q24                 rmb       3                   ; Quotient - Must immediately precede dividend
d48                 rmb       6                   ; Dividend

; Temp storage for divide

work_space          rmb       2                   ; Work space for divide

;*******************************************************************************
                    #ROM
;*******************************************************************************
          #ifz DEBUG
                    org       $C000               ; location in FLASH RAM
          #else
                    org       $2000
          #endif
;*******************************************************************************

Start               proc
                    sei                           ; disable irq to set real time irq routine
          #ifz DEBUG
                    lds       #$07FF
          #endif
                    ldx       #REGBS              ; register base address
                    lda       DDRA,x
                    anda      #%11110111          ; make 'ent' key -> input
                    ora       #%10000111          ; make LED interface -> output
                    sta       DDRA,x
                    lda       DDRD,x
                    anda      #%11000011          ; make pd2-pd5 'keys' -> input
                    sta       DDRD,x
                    lda       #%00000001
                    sta       PACTL,x
                    bset      TMSK2,x,%01000000   ; allow real time irq to cause int
                    lda       #%00110000          ; first prescaler/13 sec ps/1 => 9600bd@8MHz
                    sta       BAUD,x
          #ifz DEBUG
                    lda       #%00010000          ; bit8+7 are recv bit8 and send bit8
          #else
                    ldd       RAM_RTI             ; in Debug mode use buffalo jump vectors
                    std       rtiv_mr
                    ldd       #isr_RTI
                    std       RAM_RTI             ; replace with ROM irq later

                    ldd       RAM_SIO
                    std       siov_mr
                    ldd       #isr_SCI
                    std       RAM_SIO             ; replace with ROM irq later
                    lda       #%00000000
          #endif
                    sta       SCCR1,x
                    lda       #%00101100          ; enable transmitter and receiver of SCI
                    sta       SCCR2,x

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
                    ldx       #REGBS              ; Eat up any trash in serial input data reg
start10             lda       SCSR,x
                    anda      #%00100000
                    beq       start20
                    lda       SCDR,x              ; count shows -> one read is enough to clr bit
                    bra       start10

start20             cli

                    jsr       ang2rmp
                    jsr       drf2rmp
                    jsr       ang2dsp
                    jsr       dft2dsp
                    jsr       dispang
                    jsr       dispdrf
                    jsr       initled             ; show 'FL ' on left display
                    jsr       ldgren
          #ifnz DEBUG
                    lda       #'I'
                    jsr       sendv24
                    jsr       crlf

                    jsr       ldorng
                    lda       #$ff
                    sta       timout
startxx             tst       timout
                    bne       startxx
          #else
start21             jsr       rd_stat             ; retrieve status
                    ora       #$20                ; set bit 5 -> call for interrupt ST4
                    jsr       wr_stat

                    lda       #10
                    sta       errcnt1             ; initialize error counter
start22             jsr       rd_stat
                    anda      #$20                ; wait for status to clear
                    beq       start23             ; done
                    dec       errcnt1
                    bne       start22
                    ldd       #2                  ; if 10 errors encountered -> goto fatal halt
                    jmp       Fatal
          #endif
start23             lda       #$09                ; hi decode mode
                    ldb       #%00000111          ; lo left BIN right BCD
                    jsr       disp
                    lda       #ANGDG2
                    ldb       #%01000111          ; 'F'
                    jsr       disp
                    lda       #ANGDG1
                    ldb       #%00001110          ; 'L'
                    jsr       disp
                    lda       #ANGDG0
                    ldb       #%00000000          ; ' '
                    jsr       disp
                    jsr       ldred

start30             jsr       dft2dsp             ; wait for enter focal length
                    jsr       dispfl
                    jsr       ang2rmp
                    jsr       drf2rmp
                    tst       entflg
                    beq       start30

                    ldd       drift
                    std       flen                ; store focal length
                    clrd
                    std       drift
                    jsr       initled             ; overwrite 'FL' back to numeric

start40             tst       entflg
                    bne       start40
          #ifnz DEBUG
                    ldd       flen                ; print focal length for debug
                    jsr       out_dec
                    jsr       crlf
          #endif
main                tst       cor_mod             ; 0= pre correction. 1= correction mode
                    bne       main_a0
                    jmp       main00              ; skip if not yet in correction mode

main_a0             jsr       ldgren              ; in corr mode led = green
                    tst       sec_flg             ; count down x y correction 1sec timers
                    bne       main_a2             ; go there if sec flag == 1
                    jmp       main00a             ; nothing to do

main_a2             tst       lst_sec             ; if secflg == 1 and lastsec == 0 -> we just have pos edge
                    beq       main_a4             ; we have found the edge -> go there
                    jmp       main00a             ; otherwise goto end of main loop

main_a4             inc       lst_sec             ; we just have a positive edge on secflag -> do the stuff
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
                    beq       main0b              ; don't consider x

                    ldx       xrmpcnt             ; dec ramp x counter
                    dex
                    stx       xrmpcnt
                    bne       main_a6             ; if not zero -> no ramp step needed

                    ldx       xrmplod             ; reset ramp x counter
                    stx       xrmpcnt
                    dec       xrmpld              ; dec x led ramp value

main_a6             ldx       countx
                    dex
                    stx       countx
                    bne       main0b              ; countx > 0

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
                    beq       main0c0             ; quadr=0 -> increment xref
                    cmpa      #1
                    beq       main0d0             ; quadr=1 -> decrement xref
                    cmpa      #2
                    beq       main0d0             ; quadr=2 -> decrement xref
                    cmpa      #3
                    beq       main0c0             ; quadr=3 -> increment xref
                    bra       main0b
          #else                                   ; This table for guide scope with 90deg prism
                    beq       main0d0             ; quadr=0 -> decrement xref
                    cmpa      #1
                    beq       main0c0             ; quadr=1 -> increment xref
                    cmpa      #2
                    beq       main0c0             ; quadr=2 -> increment xref
                    cmpa      #3
                    beq       main0d0             ; quadr=3 -> decrement xref
                    bra       main0b
          #endif
main0c0             lda       st4refx             ; get xref
                    cmpa      #ST4XMAX
                    beq       main0b              ; see if we would go beyond max x -> yes, do nothing
                    inca                          ; increment xref
                    bra       main0d2

main0d0             lda       st4refx             ; get xref
                    beq       main0b              ; see if we would go below 0 -> yes, do nothing
                    deca                          ; decrement xref
main0d2             sta       st4refx
main0d3             jsr       wr_xref             ; write to ST4
                    jsr       rd_xref
                    cmpa      st4refx
                    beq       main0b
                    lda       st4refx
                    bra       main0d3

main0b              tst       yvalid
                    beq       main0d              ; don't consider y

                    ldx       yrmpcnt             ; dec ramp x counter
                    dex
                    stx       yrmpcnt
                    bne       main0bx             ; if not zero -> no ramp step needed

                    ldx       yrmplod             ; reset ramp x counter
                    stx       yrmpcnt
                    dec       yrmpld              ; dec x led ramp value

main0bx             ldx       county
                    dex
                    stx       county
                    bne       main0d              ; county > 0 -> nothing to do

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
                    beq       main0f0             ; quadr=0 -> increment yref
                    cmpa      #1
                    beq       main0f0             ; quadr=1 -> increment yref
                    cmpa      #2
                    beq       main0g0             ; quadr=2 -> decrement yref
                    cmpa      #3
                    beq       main0g0             ; quadr=3 -> decrement yref
                    bra       main0d              ; illegal quadrant value. should be 0...3

main0f0             lda       st4refy             ; get yref
                    cmpa      #ST4YMAX
                    beq       main0d              ; see if we would go beyond max y -> yes, do nothing
                    inca                          ; increment yref
                    bra       main0g2

main0g0             lda       st4refy             ; get xref
                    beq       main0d              ; see if we would go below 0 -> yes, do nothing
                    deca                          ; decrement yref
main0g2             sta       st4refy
main0g3             jsr       wr_yref             ; write to ST4
                    jsr       rd_yref
                    cmpa      st4refy
                    beq       main0d
                    lda       st4refy
                    bra       main0g3

main0d              bra       main01

main00              jsr       ldorng
main00a             tst       sec_flg             ; see if sec flag still ==1
                    bne       main01              ; wait as long as secflag ==1
                    clr       lst_sec             ; reset lastflag so that we can find next edge
main01              jsr       ang2dsp             ; do all the display stuff
                    jsr       dft2dsp
                    jsr       dispang
                    jsr       dispdrf
                    jsr       ang2rmp
                    jsr       drf2rmp

main08              tst       entflg              ; see if we have enter key pressed
                    bne       main08a             ; if enter key pressed
                                                  ; -> have to do the x y calc and reset counters
main081             jmp       main                ; if no enter key go back to main loop start

main08a             ldx       drift
                    beq       main081             ; No drift => dont enter corrections mode + go to main
                    lda       #1
                    sta       cor_mod

                    jsr       rd_xref             ; retrieve current ref position on ST4
                    sta       st4refx
                    jsr       rd_yref
                    sta       st4refy

          ; calc x y down counter values

                    ldd       angl                ; get angle to calc modulus 90deg and quadrant
                    ldx       #0                  ; x will become quadrant
main09              cmpd      #89                 ; see if we can subtract 90deg (once again)
                    ble       main10              ; no. => 0<=D<=90
                    subd      #90                 ; yes. subtract
                    inx                           ; increment quadrant counter
                    bra       main09              ; loop around

main10              stb       angmod
                    xgdx
                    stb       quadr

xcalc               ldd       angl
                    cmpd      #90                 ; if angle = 90 or 270 -> disable x-calc + x-corr
                    beq       xcalc10
                    cmpd      #270
                    beq       xcalc10

                    lda       #1
                    sta       xvalid
                    bra       xcalc20

xcalc10             clr       xvalid              ; disable x-calc + x-corr
                    clr       xrmpld
                    jsr       ang2rmp
                    bra       ycalc               ; jmp to y-calc. no x-calc required.

xcalc20             ldd       flen                ; do x calculation
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

ycalc               ldd       angl
                    beq       ycalc10             ; if angle=0 or 180 disable y-calc + y-corr
                    cmpd      #180
                    beq       ycalc10
                    lda       #1
                    sta       yvalid
                    bra       ycalc20

ycalc10             clr       yvalid
                    clr       yrmpld
                    jsr       drf2rmp
                    bra       main25              ; skip y-calc

ycalc20             ldd       flen                ; do y calculation
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
main25
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
main26              tst       entflg
                    bne       main26
                    jmp       main

initled             lda       #$0f                ; hi test register
                    ldb       #%00000000          ; lo 0 = normal ops
                    jsr       disp

                    lda       #$0c                ; hi shut down reg
                    ldb       #%00000001          ; lo 1 = normal ops
                    jsr       disp

                    lda       #$0b                ; hi scan limit reg
                    ldb       #%00000111          ; lo %0111 display all digits
                    jsr       disp

                    lda       #$0a                ; hi intensity reg
                    ldb       #%00001000          ; lo 1/2 intensity
                    jsr       disp

                    lda       #$09                ; hi decode mode
                    ldb       #%01110111          ; lo left BCD right BCD
                    jmp       disp

ang2rmp             ldb       xrmpld
                    clra
                    addd      #ledtab
                    xgdx
                    ldb       ,x
                    lda       #ANGRMP
                    jmp       disp

drf2rmp             ldb       yrmpld
                    clra
                    addd      #ledtab
                    xgdx
                    ldb       ,x
                    orb       rgled
                    lda       #DRFRMP
                    jmp       disp

dgreen              lda       rgled
                    anda      #%11111100
                    ora       #%00000001
                    bra       dled

dred                lda       rgled
                    anda      #%11111100
                    ora       #%00000010
                    bra       dled

dorng               lda       rgled
                    anda      #%11111100
                    ora       #%00000011
                    bra       dled

dblak               lda       rgled
                    anda      #%11111100
;                   bra       dled

dled                sta       rgled
                    rts

ldgren              bsr       dgreen
                    bra       drf2rmp

ldred               bsr       dred
                    bra       drf2rmp

ldorng              bsr       dorng
                    bra       drf2rmp

ldblak              bsr       dblak
                    bra       drf2rmp

ang2dsp             ldd       angl
                    ldx       #10
                    idiv
                    stb       angdsp0
                    xgdx
                    ldx       #10
                    idiv
                    stb       angdsp1
                    xgdx
                    stb       angdsp2
                    rts

dft2dsp             ldd       drift
                    ldx       #10
                    idiv
                    stb       drfdsp0
                    xgdx
                    ldx       #10
                    idiv
                    stb       drfdsp1
                    xgdx
                    stb       drfdsp2
                    rts

dispang             lda       #ANGDG0
                    ldb       angdsp0
                    bsr       disp
                    lda       #ANGDG1
                    ldb       angdsp1
                    bsr       disp
                    lda       #ANGDG2
                    ldb       angdsp2
                    bra       disp

dispdrf             lda       #DRFDG0
                    ldb       drfdsp0
                    bsr       disp
                    lda       #DRFDG1
                    ldb       drfdsp1
                    orb       #%10000000
                    bsr       disp
                    lda       #DRFDG2
                    ldb       drfdsp2
                    bra       disp

dispfl              lda       #DRFDG0
                    ldb       drfdsp0
                    bsr       disp
                    lda       #DRFDG1
                    ldb       drfdsp1
                    bsr       disp
                    lda       #DRFDG2
                    ldb       drfdsp2
                    bra       disp

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
                    bsr       disp
                    pulb
                    lda       #ANGDG1
                    bsr       disp
                    pulb
                    lda       #ANGDG0
;                   bra       disp

;*******************************************************************************

disp                proc
                    psha
                    pshb
                    pshx
                    pshy
                    ldx       #REGBS
                    bclr      PORTA,x,CLK
                    bclr      PORTA,x,LOAD
                    bclr      PORTA,x,DATA
                    ldy       #16
Loop@@              lsld
                    bcc       Zero@@
                    bset      PORTA,x,DATA
                    bra       Clock@@

Zero@@              bclr      PORTA,x,DATA
Clock@@             bset      PORTA,x,CLK
                    bclr      PORTA,x,CLK
                    dey
                    bne       Loop@@
                    bset      PORTA,x,LOAD
                    puly
                    pulx
                    pulb
                    pula
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
rd_stat             clra
wr_stat             rts

rd_xref             lda       st4refx
                    rts

rd_yref             lda       st4refy
                    rts

wr_xref             psha
                    lda       #'X'
                    bsr       sendv24
                    pula
                    tab
                    clra
                    bsr       out_dec
                    bra       crlf

wr_yref             psha
                    lda       #'Y'
                    bsr       sendv24
                    pula
                    tab
                    clra
                    bsr       out_dec
                    bra       crlf
          #else
rd_stat             lda       #10
                    sta       errcnt              ; init error counter
rd_st02             lda       #$02                ; read RAM cmd. and reetry point for error recovery
                    jsr       sendv24
                    lda       #$01                ; number of bytes
                    jsr       sendv24
                    lda       #$01                ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4ST             ; LSB of Status address
                    jsr       sendv24
                    lda       #]ST4ST             ; MSB of status address
                    jsr       sendv24
                    lda       #$33                ; checksum=$02+$01+$01+$2f+$00
                    jsr       sendv24

; response :    02 01 ST CS

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
                    cmpa      #$02                ; $02
                    bne       rd_st95
                    lda       st4buf+1
                    cmpa      #$01                ; $01
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

wr_stat             sta       tmp
                    lda       #10
                    sta       errcnt
wr_st02             lda       #$01                ; write RAM cmd
                    jsr       sendv24
                    lda       #$04                ; number of bytes
                    jsr       sendv24
                    lda       #$01                ; 0=ext, 1=int ram
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

; response :    06

wr_st05             ldy       recvrd
                    lda       #$ff
                    sta       timout
wr_st10             cpy       recvwr
                    bne       wr_st15             ; wait till we have received something
                    tst       timout
                    bne       wr_st10
                    dec       errcnt
                    bne       wr_st02
                    ldd       #11
                    jmp       Fatal

wr_st15             lda       ,y
                    cpy       #recvbuf+15
                    bne       wr_st20
                    ldy       #recvbuf-1
wr_st20             iny
                    sty       recvrd
                    cmpa      #$06
                    beq       wr_st25
                    dec       errcnt
                    bne       wr_st02
                    ldd       #$03
                    jmp       Fatal

wr_st25             equ       :AnRTS

; x ref addr $7e7c

rd_xref             lda       #10
                    sta       errcnt
rd_xr02             lda       #$02                ; read RAM cmd
                    jsr       sendv24
                    lda       #$01                ; number of bytes
                    jsr       sendv24
                    lda       #$00                ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4XRF            ; LSB of x ref coordinate address
                    jsr       sendv24
                    lda       #]ST4XRF            ; MSB of x ref coordinate address
                    jsr       sendv24
                    lda       #$FD                ; checksum $FD=$02+$01+$00+$7c+$7e
                    jsr       sendv24

; response $02 $01 xref CS

                    ldx       #st4buf
rd_xr05             lda       #$ff
                    sta       timout
                    ldy       recvrd
rd_xr10             cpy       recvwr
                    bne       rd_xr15             ; wait till we have received something
                    tst       timout
                    bne       rd_xr10
                    dec       errcnt
                    bne       rd_xr02
                    ldd       #12
                    jmp       Fatal

rd_xr15             lda       ,y
                    sta       ,x                  ; copy v24 receive data to st4 receive buffer
                    inx
                    cpy       #recvbuf+15
                    bne       rd_xr20
                    ldy       #recvbuf-1
rd_xr20             iny
                    sty       recvrd
                    cpx       #st4buf+4
                    bne       rd_xr05

                    lda       st4buf              ; all received. now check
                    cmpa      #$02                ; $02
                    bne       rd_xr95
                    lda       st4buf+1
                    cmpa      #$01                ; $01
                    bne       rd_xr95

                    clra
                    ldx       #st4buf
rd_xr30             adda      ,x                  ; add up checksum
                    inx
                    cpx       #st4buf+3
                    bne       rd_xr30
                    cmpa      ,x                  ; compare with received checksum value
                    bne       rd_xr95             ; goto error handler if wrong checksum
                    lda       st4buf+2            ; retrieve x ref byte and return it
                    rts

rd_xr95             dec       errcnt              ; count down for error
                    beq       rd_xr97
                    jmp       rd_xref

rd_xr97             ldd       #4                  ; if 10 errors encountered -> goto fatal halt
                    jmp       Fatal

; y ref addr $7e7d

rd_yref             lda       #10
                    sta       errcnt
rd_yr02             lda       #$02                ; read RAM cmd
                    jsr       sendv24
                    lda       #$01                ; number of bytes
                    jsr       sendv24
                    lda       #$00                ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4YRF            ; LSB of y ref coordinate address
                    jsr       sendv24
                    lda       #]ST4YRF            ; MSB of y ref coordinate address
                    jsr       sendv24
                    lda       #$FE                ; checksum $FE=$02+$01+$00+$7d+$7e
                    jsr       sendv24

; response $02 $01 yref CS

                    ldx       #st4buf
rd_yr05             lda       #$ff
                    sta       timout
                    ldy       recvrd
rd_yr10             cpy       recvwr
                    bne       rd_yr15             ; wait till we have received something
                    tst       timout
                    bne       rd_yr10
                    dec       errcnt
                    bne       rd_yr02
                    ldd       #13
                    jmp       Fatal

rd_yr15             lda       ,y
                    sta       ,x                  ; copy v24 receive data to st4 receive buffer
                    inx
                    cpy       #recvbuf+15
                    bne       rd_yr20
                    ldy       #recvbuf-1
rd_yr20             iny
                    sty       recvrd
                    cpx       #st4buf+4
                    bne       rd_yr05

                    lda       st4buf              ; all received. now check
                    cmpa      #$02                ; $02
                    bne       rd_yr95
                    lda       st4buf+1
                    cmpa      #$01                ; $01
                    bne       rd_yr95

                    clra
                    ldx       #st4buf
rd_yr30             adda      ,x                  ; add up checksum
                    inx
                    cpx       #st4buf+3
                    bne       rd_yr30
                    cmpa      ,x                  ; compare with received checksum value
                    bne       rd_yr95             ; goto error handler if wrong checksum
                    lda       st4buf+2            ; retrieve x ref byte and return it
                    rts

rd_yr95             dec       errcnt              ; count down for error
                    beq       rd_yr97
                    jmp       rd_yr02

rd_yr97             ldd       #5                  ; if 10 errors encountered -> goto fatal halt
                    jmp       Fatal

wr_xref             sta       tmp
                    lda       #10
                    sta       errcnt
wr_xr02             lda       #$01                ; write RAM cmd
                    jsr       sendv24
                    lda       #$04                ; number of bytes
                    jsr       sendv24
                    lda       #$00                ; 0=ext, 1=int ram
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

; response :    06

wr_xr05             ldy       recvrd
                    lda       #$ff
                    sta       timout
wr_xr10             cpy       recvwr
                    bne       wr_xr15             ; wait till we have received something
                    tst       timout
                    bne       wr_xr10
                    dec       errcnt
                    bne       wr_xr02
                    ldd       #14
                    jmp       Fatal

wr_xr15             lda       ,y
                    cpy       #recvbuf+15
                    bne       wr_xr20
                    ldy       #recvbuf-1
wr_xr20             iny
                    sty       recvrd
                    cmpa      #$06
                    beq       wr_xr25
                    dec       errcnt
                    bne       wr_xr02
                    ldd       #$06
                    jmp       Fatal

wr_xr25             rts

wr_yref             sta       tmp
                    lda       #10
                    sta       errcnt
wr_yr02             lda       #$01                ; write RAM cmd
                    jsr       sendv24
                    lda       #$04                ; number of bytes
                    jsr       sendv24
                    lda       #$00                ; 0=ext, 1=int ram
                    jsr       sendv24
                    lda       #[ST4YRF            ; LSB of y ref address $7e7d
                    jsr       sendv24
                    lda       #]ST4YRF            ; MSB of y ref address
                    bsr       sendv24
                    lda       tmp                 ; x ref byte
                    bsr       sendv24
                    lda       tmp
                    adda      #$00                ; checksum $00=$01+$04+$00+$7d+$7e
                    bsr       sendv24

; response :    06

wr_yr05             ldy       recvrd
                    lda       #$ff
                    sta       timout
wr_yr10             cpy       recvwr
                    bne       wr_yr15             ; wait till we have received something
                    tst       timout
                    bne       wr_yr10
                    dec       errcnt
                    bne       wr_yr02
                    ldd       #15
                    jmp       Fatal

wr_yr15             lda       ,y
                    cpy       #recvbuf+15
                    bne       wr_yr20
                    ldy       #recvbuf-1
wr_yr20             iny
                    sty       recvrd
                    cmpa      #$06
                    beq       wr_yr25
                    dec       errcnt
                    bne       wr_yr02
                    ldd       #$07
                    jmp       Fatal

wr_yr25             equ       :AnRTS
          #endif
out_dec             ldy       #5
out_d10             ldx       #10
                    idiv
                    pshb
                    xgdx
                    dey
                    bne       out_d10

                    ldy       #5
out_d20             pula
                    adda      #'0'
                    bsr       sendv24
                    dey
                    bne       out_d20
                    rts

outbyte             psha                          ; output accu a 1 byte as 2 hex digits
                    anda      #$F0
                    lsra:4
                    bsr       outbyt1
                    pula
                    anda      #$0F

outbyt1             adda      #'0'                ; output 4 bits as 1 hex digit
                    cmpa      #'9'
                    ble       sendv24
                    adda      #7

sendv24             pshy
                    ldy       sendwr
                    sta       ,y
                    cpy       #sendbuf+15
                    bne       sendv50
                    ldy       #sendbuf-1
sendv50             iny
sendv60             cpy       sendrd
                    beq       sendv60
                    sty       sendwr
                    puly
                    rts

;*******************************************************************************

crlf                proc
                    psha
                    lda       #13
                    bsr       sendv24
                    lda       #10
                    bsr       sendv24
                    pula
                    rts

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
                    pshb                          ; Preserve regs
                    psha
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
                    pula
                    pulb
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
                    pshb
                    psha
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

DIVER1              jmp       DIVBY0              ; Extend branch range

;  Find size of divisor and dividend & thus the hi byte of quotient.
;  Also do gross checks to avoid dividing by 0, etc.
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

;  Decide whether to use 1 or 2 bytes as trial divisor
DIVLUP              ldx       .divisor            ; Get ptr to MSB of divisor
                    lda       divisor_size        ; Get # bytes in divisor
                    deca
                    beq       DIV1BY              ; Only 1 byte in divisor
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
DIV1BY              jmp       DIV1BYT

;  Trial divisor is 2 bytes. Round up if required.
DIVW2B              lda       counter
                    deca                          ; Make A = 0 if exactly 2 bytes in divisor
DIVWRD              ldx       ,x                  ; Get MSW of divisor
                    tsta
                    beq       DIVWNR              ; BR if exactly 2 bytes in divisor
                    inx                           ; Round divisor up
DIVWNR              ldd       1,y                 ; Get MSW of dividend
                    idiv
                    xgdx                          ; Get trial quotient into B
;  Multiply divisor by trial quotient and subtract from dividend
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
                    bcc       *+5
                    dec       ,y                  ; Propagate borrow
                    dec       counter
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

;  Trial divisor is hi byte of divisor.  Always round it up.
DIVBYT              dec       quotient_remaining  ; Divisor > dividend, move 1 position right
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
                    beq       DIVBT2
                    ldb       #$F0                ; Overflow on this trial, force max
DIVBT2              jmp       DIVSTO

; Handle single byte divisor specially
DIV1BYT             ldb       ,x                  ; Get divisor, single byte
                    clra
                    xgdx
DIV1SKP             cpx       ,y
                    bls       DIV1BGO
                    dec       quotient_remaining  ; Divisor > Dividend, move 1 position right
                    blt       DIVDUN              ; BR if done
                    inc       .quotient_byte+1
                    iny
                    bra       DIV1SKP

DIV1BGO             ldd       ,y                  ; Dividend, current hi 16 bits
                    idiv                          ; MSW of dividend / divisor = Trial Q
                    xgdx                          ; Get quotient in D
                    jmp       DIVSTO
DIVDUN
DIVBY0              puly
                    pulx                          ; Restore regs
                    pula
                    pulb
                    rts

cos_x               ldb       quadr
                    beq       cos_x10
                    cmpb      #1
                    beq       cos_x20
                    cmpb      #2
                    beq       cos_x10
                    cmpb      #3
                    beq       cos_x20

cos_x10             sta       tmp
                    lda       #90
                    suba      tmp

cos_x20             tab
                    clra
                    addd      #sintab
                    xgdx
                    lda       ,x
                    rts

sin_x               ldb       quadr
                    beq       sin_x10
                    cmpb      #1
                    beq       sin_x20
                    cmpb      #2
                    beq       sin_x10
                    cmpb      #3
                    beq       sin_x20
                    bra       *

sin_x20             sta       tmp
                    lda       #90
                    suba      tmp

sin_x10             tab
                    clra
                    addd      #sintab
                    xgdx
                    lda       ,x
                    rts

;*******************************************************************************

isr_RTI             proc
                    ldx       #REGBS
                    lda       #%01000000
                    sta       TFLG2,x

                    tst       timout
                    beq       key005
                    dec       timout

key005              inc       sec_cnt
                    lda       sec_cnt
                    cmpa      #62
                    bne       key00
                    clr       sec_cnt
                    lda       sec_flg
                    eora      #%00000001
                    sta       sec_flg

key00               inc       key_cnt
                    lda       key_cnt
                    cmpa      #12
                    beq       key02
                    rti                           ; skip all irq until key_cnt = 12

key02               clr       key_cnt
                    lda       PORTD,x
                    sta       pdmir
                    anda      #ALLKEY
                    cmpa      #ALLKEY
                    beq       key10
                    anda      #ANGKEY
                    cmpa      #ANGKEY
                    beq       key20               ; no ANG key pressed
                    cmpa      #ANGPL
                    bne       angminu

angplus             ldd       angl
                    incd
                    cmpd      #360
                    bne       angpl10
                    clrd
                    std       angl
angpl10             std       angl
                    bra       key20

angminu             ldd       angl
                    decd
                    bpl       angmi10
                    ldd       #359
angmi10             std       angl

key20               lda       pdmir
                    anda      #DRFKEY
                    cmpa      #DRFKEY
                    beq       key10
                    cmpa      #DRFPL
                    bne       drfminu

drfplus             ldd       drift
                    cmpd      #999
                    beq       key10
                    incd
                    std       drift
                    bra       key10

drfminu             ldd       drift
                    beq       key10
                    decd
                    std       drift
;                   bra       key10

key10               lda       PORTA,x
                    anda      #ENTKEY
                    bne       key30
                    lda       #$01
                    sta       entflg
                    bra       key99

key30               clr       entflg

key99               bra       sio_snd             ; see if we have to initiate sending

;*******************************************************************************

isr_SCI             proc
                    ldx       #REGBS
                    lda       SCSR,x
                    sta       scsr_mr
                    anda      #%00100000
                    bne       sio_rcv
                    lda       scsr_mr
                    anda      #%10000000
                    bne       sio_snd
                    rti                           ; if not rcv or snd. No other irq served.

sio_rcv             lda       SCDR,x              ; dont care for overrun error yet
                    ldy       recvwr
                    sta       ,y
                    cpy       #recvbuf+15
                    bne       sio_r10
                    ldy       #recvbuf-1
sio_r10             iny
                    sty       recvwr
                    lda       scsr_mr
                    anda      #%10000000
                    bne       sio_snd
                    rti

sio_snd             ldy       sendrd
                    cpy       sendwr
                    beq       sio_s08             ; nothing to send
sio_s03             brclr     SCSR,x,%10000000,sio_s03  ; wait till send reg empty

                    pshx
                    ldx       #8
                    clrb
                    lda       ,y
                    psha
sio_s0a             lsra
                    bcc       sio_s04
                    incb
sio_s04             dex
                    bne       sio_s0a
                    pula
                    pulx
                    andb      #$01
                    bne       sio_s0c
                    bclr      SCCR1,x,%01000000
                    bra       sio_s0d

sio_s0c             bset      SCCR1,x,%01000000

sio_s0d             sta       SCDR,x
                    cpy       #sendbuf+15
                    bne       sio_s05
                    ldy       #sendbuf-1
sio_s05             iny
                    sty       sendrd
                    cpy       sendwr
                    beq       sio_s08             ; nothing more to send
                    bset      SCCR2,x,%10000000   ; if more bytes to send set irq enable bit
                    bra       sio_s10

sio_s08             bclr      SCCR2,x,%10000000   ; if no more bytes to send clr irq enable
sio_s10             rti

ledtab              fcb       %00000000
                    fcb       %00000100
                    fcb       %00100100
                    fcb       %00110100
                    fcb       %00111100
                    fcb       %01111100

ledtab1             fcb       %00000000
                    fcb       %00000100
                    fcb       %00100000
                    fcb       %00010000
                    fcb       %00001000
                    fcb       %01000000

sintab              fcb       000                 ; 0
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
                    #EEPROM
                    org       $FF00               ; start of EEPROM

RESET               proc                          ; INITIALIZE THE CPU
                    lds       #$01FF              ; put stack in CPU RAM
                    ldx       #$1000              ; register base address
                    lda       #%10010001          ; adpu, irqe, dly, cop = 65mS
                    sta       OPTION,x
                    lda       #%00000000
                    sta       TMSK2,x
                    lda       #%00000000
                    sta       BPROT,x             ; make CONFIG & EEPROM writable
                    lds       #$03ff
                    lda       #%00000101
                    sta       CSCTL,x             ; enable program CS for 32K
                    lda       #%00000000
                    sta       CSGADR,x            ; RAM starts at address 0000H
                    lda       #%00000001
                    sta       CSGSIZ,x            ; RAM block size is 32K
                    lda       #%00011111
                    sta       DDRG,x              ; bank select bits = outputs
                    lda       #%00000000
                    sta       PORTG,x             ; select 1ST bank
                    jmp       Start

;******************************************************************************
;                           Interrupt Service Routinen                       *
;******************************************************************************

;       Commented vectors are serviced in the code

;isr_SCI
isr_SPI
isr_PAIE
isr_PAO
isr_TIMO
isr_TIC4
isr_TOC4
isr_TOC3
isr_TOC2
isr_TOC1
isr_TIC3
isr_TIC2
isr_TIC1
;isr_RTI
isr_IRQ
isr_XIRQ
isr_SWI
isr_ILLOP
isr_COPFAIL
isr_CLMONFAIL
                    rti                           ; keine Bearbeitung fuer diese IRQ's

;***********************************
; VECTOR TABLE
;***********************************

                    #VECTORS
                    org       $FFD6               ; Start der Vectortabelle

                    dw        isr_SCI
                    dw        isr_SPI
                    dw        isr_PAIE
                    dw        isr_PAO
                    dw        isr_TIMO
                    dw        isr_TIC4
                    dw        isr_TOC4
                    dw        isr_TOC3
                    dw        isr_TOC2
                    dw        isr_TOC1
                    dw        isr_TIC3
                    dw        isr_TIC2
                    dw        isr_TIC1
                    dw        isr_RTI
                    dw        isr_IRQ
                    dw        isr_XIRQ
                    dw        isr_SWI
                    dw        isr_ILLOP
                    dw        isr_COPFAIL
                    dw        isr_CLMONFAIL
                    dw        RESET
          #endif
