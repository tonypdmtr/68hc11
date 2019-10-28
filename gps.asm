;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; The original version of this program was found here:
; http://incolor.inetnebr.com/mariley/projects/68HC11/68hc11.htm
;
; Optimized and adapted to ASM11 by Tony Papadimitriou <tonyp@acm.org>
;*******************************************************************************
E2
                    #ListOff
                    #Uses     mcu.inc
                    #ListOn

;*******************************************************************************
                    #RAM
;*******************************************************************************

lat                 rmb       15+1
lon                 rmb       16+1
qual                rmb       1+1                 ; QUALITY
          ;-------------------------------------- ; LINE 3 OF LCD
alt                 rmb       6
hdg                 rmb       6
spd                 rmb       5+1
          ;-------------------------------------- ; LINE 4
sat                 rmb       7
time                rmb       8+1

;*******************************************************************************
;                      Symbol Definitions
;*******************************************************************************

; PORTC

IOPAT               equ       $FF                 ; I/O patterns 0=IN 1=OUT 11111111

; SPI masks

TDRE                equ       %10000000
RDRF                equ       %00100000
TXCOM               equ       %01000000           ; transmit complete =1
OVRUN               equ       %00001000           ; overrun error

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    lds       #STACKTOP           ; set up stack (essential if you use subroutines)
          ;-------------------------------------- ; initialize display
                    jsr:2     DELAYL
          ;-------------------------------------- ; initialize the serial control register
                    lda       #%00110001          ; 00110001 = 4800 BAUD
                    sta       BAUD
                    clr       SCCR1
                    lda       #%00001100
                    sta       SCCR2               ; RX & TX enabled
                    clr       SPCR
          ;-------------------------------------- ; setup NMEA sentences
                    ldx       #NMEAON
                    jsr       SENDX

                    jsr:2     DELAYL
          ;-------------------------------------- ; initialize output to LCD
                    ldx       #TEXT1              ; send text1 to serial port
                    jsr       SENDX
Comma@@             ldb       #'.'                ; send ..... while waiting
                    jsr       TXWAIT
                    stb       SCDR
          ;-------------------------------------- ; read from GPS
Loop@@              ldx       #NMEA               ; start looking for "$GP"
                    ldb       #3
_1@@                jsr       RVWAIT              ; wait for data from GPS
                    lda       SCDR                ; load byte from serial port
                    cmpa      ,x                  ; compare to position format
                    bne       Loop@@              ; if SCDR is not "$GP" start over
                    inx
                    decb
                    bne       _1@@

                    ldx       #GPGGA
                    ldb       #4
String@@            jsr       RVWAIT              ; wait for data from GPS
                    lda       SCDR                ; load byte from serial port
                    cmpa      ,x                  ; compare to position format
                    bne       Loop@@              ; if SCDR is not "GGA," go to next string
                    inx
                    decb
                    bne       String@@
          ;--------------------------------------
          ; DECODE MESSAGE
          ; $GPGGA,hhmmss.ss,ddmm.mmmm,n,dddmm.mmmm,e,q,ss,y.y,a.a,z,g.g,z,t.t,iiii*CC<CR><LF>
          ;-------------------------------------- ; hour
                    lda       #2                  ; get HH
                    ldx       #time
Time@@              jsr       GETCHAR
                    cmpb      #','                ; if a comma is detected the data is invalid
                    beq       Comma@@
                    stb       ,x
                    inx
                    deca
                    bne       Time@@

                    ldb       #':'                ; insert ":"
                    stb       ,x
                    inx

                    tstb
          ;-------------------------------------- ; minute
                    lda       #2                  ; get mm
                    jsr       GETLINE

                    ldb       #':'                ; insert ":"
                    stb       ,x
                    inx
          ;-------------------------------------- ; second
                    lda       #2                  ; get ss
                    jsr       GETLINE

                    clr       ,x                  ; insert NULL
          ;-------------------------------------- ; skip fractional seconds
                    lda       #4                  ; skip .ss,
SkipFs@@            jsr       GETCHAR
                    deca
                    bne       SkipFs@@
          ;-------------------------------------- ; latitude
                    lda       #2                  ; get ddmm.mmmm,n
                    ldx       #lat
                    jsr       GETLINE

                    ldb       #$DF                ; insert degree symbol
                    stb       ,x
                    inx

                    lda       #7                  ; get ddmm.mmmm,n
                    jsr       GETLINE
          ;-------------------------------------- ; insert a space instead of the comma
                    jsr       GETCHAR
                    ldb       #' '
                    stb       ,x
                    inx

                    jsr       GETCHAR
                    stb       ,x
                    inx

                    lda       #3
                    jsr       SPACE

                    clr       ,x                  ; insert NULL
                    jsr       GETCHAR             ; skip comma
          ;-------------------------------------- ; longitude
                    lda       #3                  ; get ddmm.mmmm,n
                    ldx       #lon
                    jsr       GETLINE

                    ldb       #$DF                ; insert degree symbol
                    stb       ,x
                    inx

                    lda       #7                  ; get ddmm.mmmm,n
                    jsr       GETLINE
          ;-------------------------------------- ; insert a space instead of the comma
                    jsr       GETCHAR
                    ldb       #' '
                    stb       ,x
                    inx

                    jsr       GETCHAR
                    stb       ,x
                    inx

                    lda       #3                  ; insert 3 spaces
                    jsr       SPACE

                    clr       ,x                  ; insert NULL
                    jsr       GETCHAR             ; skip comma
          ;-------------------------------------- ; GPS quality indication
                    ldx       #qual
                    jsr       GETCHAR
                    stb       ,x
                    jsr       GETCHAR             ; skip comma
          ;-------------------------------------- ; sats being used
                    ldx       #sat
                    lda       #2                  ; get ss
                    jsr       GETLINE

                    lda       #5
                    jsr       SPACE
          ;-------------------------------------- ; skip ,y.y,
                    lda       #5
_2@@                jsr       GETCHAR
                    deca
                    bne       _2@@
          ;-------------------------------------- ; altitude
                    ldx       #alt
                    lda       #3                  ; get alt
                    jsr       GETLINE

                    ldb       #'m'
                    stb       ,x
                    inx

                    ldb       #' '
                    stb       ,x
                    stb       1,x
                    inx:2
          ;-------------------------------------- ; heading and speed
Loop2@@             ldx       #NMEA               ; start looking for "$GP"
                    ldb       #3
_3@@                jsr       RVWAIT              ; wait for data from GPS
                    lda       SCDR                ; load byte from serial port
                    cmpa      ,x                  ; compare to position format
                    bne       Loop2@@             ; if SCDR is not "$GP" start over
                    inx
                    decb
                    bne       _3@@

                    ldx       #GPVTG
                    ldb       #4
String2@@           jsr       RVWAIT              ; wait for data from GPS
                    lda       SCDR                ; load byte from serial port
                    cmpa      ,x                  ; compare to position format
                    bne       Loop2@@             ; if SCDR is not "GGA," go to next string
                    inx
                    decb
                    bne       String2@@
          ;--------------------------------------
          ; DECODE MESSAGE
          ; $GPVTG,,T,,M,,N,,K*4E
          ;--------------------------------------
                    ldx       #hdg
                    lda       #3                  ; get heading
HDG1@@              jsr       GETCHAR

                    cmpb      #','
                    beq       Degree@@

                    cmpb      #'.'
                    beq       Degree@@
                    stb       ,x
                    inx
                    deca
                    bne       HDG1@@

Degree@@            ldb       #$DF                ; degree symbol
                    stb       ,x
                    inx

                    ldb       #'T'                ; insert a "T"
                    stb       ,x
                    inx

                    lda       #5
                    jsr       SPACE

                    clr       ,x                  ; insert NULL
          ;-------------------------------------- ; display data to LCD - latitude
                    ldx       #LINE1
                    bsr       SENDX

                    ldx       #lat
                    bsr       SENDX
          ;-------------------------------------- ;longitude
                    ldx       #LINE2
                    bsr       SENDX

                    ldx       #lon
                    bsr       SENDX
          ;-------------------------------------- ;line 3
                    ldb       qual
                    cmpb      #'0'
                    bne       Avail@@

                    ldx       #GPSOFF
                    bsr       SENDX
                    bra       Out@@

Avail@@             cmpb      #'1'
                    bne       Diff@@
                    ldx       #LINE3
                    bsr       SENDX

                    ldx       #alt
                    bsr       SENDX
                    bra       Out@@

Diff@@              cmpb      #$32
                    ldx       #DIFFIX
                    bsr       SENDX
          ;-------------------------------------- ;line 4
Out@@               ldx       #LINE4
                    bsr       SENDX

                    ldx       #sat
                    bsr       SENDX
                    jmp       Loop@@

;*******************************************************************************
; SUBROUTINES
;*******************************************************************************

;*******************************************************************************
; Purpose: SEND TO SERIAL PORT ROUTINE
; Input  : None
; Output : None

SENDX               proc
                    dex
                    pshb
Loop@@              bsr       TXWAIT
                    inx
                    ldb       ,x
                    beq       Done@@
                    stb       SCDR
                    bra       Loop@@
Done@@              pulb
                    rts

;*******************************************************************************
; Purpose: WAIT FOR RS-232 TO BE READY TO RECEIVE DATA
; Input  : None
; Output : None

TXWAIT              proc
Loop@@              tst       SCSR
                    bpl       Loop@@
                    rts

;*******************************************************************************
; Purpose: X points to buffer, A holds number of bytes (B is destroyed)
; Input  : X -> Buffer
;        : A = Number of bytes
; Output : None
; Note(s): B destroyed

GETLINE             proc
Loop@@              bsr       GETCHAR
                    stb       ,x
                    inx
                    deca
                    bne       Loop@@
                    rts

;*******************************************************************************

GETCHAR             proc
                    bsr       RVWAIT
                    ldb       SCDR
                    rts

;*******************************************************************************
; Purpose: WAIT FOR DATA FROM RS-232
; Input  : None
; Output : None

RVWAIT              proc
                    psha
Loop@@              lda       SCSR
                    bita      #RDRF
                    beq       Loop@@
                    pula
                    rts

;*******************************************************************************
; Purpose: Long Delay
; Input  : None
; Output : None

DELAYL              proc
                    pshx
                    ldx       #$EFFF
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

;*******************************************************************************
; Purpose: Short Delay
; Input  : None
; Output : None

DELAYS              proc
                    psha
                    lda       #64
Loop@@              deca
                    bne       Loop@@
                    pula
                    rts

;*******************************************************************************
; Purpose: INSERT SPACES (ldA  WITH THE NUMBER OF BLANK SPACE THEN CALL)
; Input  : None
; Output : None

SPACE               proc
                    pshb
Loop@@              ldb       #' '
                    stb       ,x
                    inx
                    deca
                    bne       Loop@@
                    pulb
                    rts

;*******************************************************************************
; TEXT AND POSITION DATA
;*******************************************************************************
          ;-------------------------------------- ; LCD initial text
TEXT1               fcb       $FE,$01,$FE,$0C
                    fcc       'ACQUIRING SATELLITES'
                    fcs       '    PLEASE WAIT...'
          ;-------------------------------------- ; GPS quality messages
DIFFIX              fcs       $FE,$A0,'GPS DIFF. FIX MODE  '
GPSOFF              fcs       $FE,$A0,'  GPS UNAVAILABLE!  '
          ;--------------------------------------
LINE1               fcs       $FE,$80,'LAT  '
LINE2               fcs       $FE,$C0,'LON '
LINE3               fcs       $FE,$A0,'Alt='
LINE4               fcs       $FE,$E0,'SATs='

;************** DATA STORAGE ********************
; Look for the following NMEA-0183 format string(s)

NMEA                fcc       '$GP'
GPGGA               fcc       'GGA,'
GPVTG               fcc       'VTG,'
NMEAON              fcb       $FE,$08
                    fcc       '$PMOTG,VTG,0001',CR,LF
                    fcc       '$PMOTG,GGA,0001',CR,LF
                    fcc       '$PMOTG,GLL,0000',CR,LF
                    fcc       '$PMOTG,GSA,0000',CR,LF
                    fcc       '$PMOTG,GSV,0000',CR,LF
                    fcc       '$PMOTG,RMC,0000',CR,LF
                    fcc       '$PMOTG,ZDA,0000',CR,LF
                    fcs       $FE,$0C,$FE,$01

                    @vector   Vreset,Start

                    end       :s19crc
