;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; The original version of this program was found here:                CRC: $0943
; http://incolor.inetnebr.com/mariley/projects/68HC11/68hc11.htm
;
; Optimized and adapted to ASM11 by Tony Papadimitriou <tonyp@acm.org>
;*******************************************************************************
E2
                    #ListOff
                    #Uses     mcu.inc
                    #ListOn

                    #RAM

LAT                 rmb       15+1
LON                 rmb       16+1
QUAL                rmb       1+1                 ; QUALITY

; LINE 3 OF LCD

ALT                 rmb       6
HDG                 rmb       6
SPD                 rmb       5+1

; LINE 4

SAT                 rmb       7
TIME                rmb       8+1

;*******************************************************************************
;                      SYMBOL DEFINITIONS
;*******************************************************************************

; PORT C

IOPAT               equ       $FF                 ; I/O PATTERNS 0=IN 1=OUT 11111111

; SPI MASKS

TDRE                equ       %10000000
RDRF                equ       %00100000
TXCOM               equ       %01000000           ; TRANSMIT COMPLETE =1
OVRUN               equ       %00001000           ; OVER RUN ERROR

;****** start of program ****************************

                    #ROM

Start               proc
                    lds       #STACKTOP           ; set up stack (essential if you use subroutines)

;********* INITIALIZE DISPLAY *****************

                    jsr:2     DELAYL

          ; INITIALIZE THE SERIAL CONTROL REGISTER

                    lda       #%00110001          ; 00110001 = 4800 BAUD
                    sta       BAUD
                    clr       SCCR1
                    lda       #%00001100
                    sta       SCCR2               ; RECEIVER & TRANS. ENABLED
                    clr       SPCR

;******* SETUP NMEA SENTENCES *******************

                    ldx       #NMEAON
                    jsr       SENDX

                    jsr:2     DELAYL

          ; INITIALIZE OUTPUT TO LCD

                    ldx       #TEXT1              ; SEND TEXT1 TO SERIAL PORT
                    jsr       SENDX

COMMA               ldb       #'.'                ; SEND ..... WHILE WAITING
                    jsr       TXWAIT
                    stb       SCDR

;*********** READ FROM GPS ****************

LOOP                ldx       #NMEA               ; START LOOKING FOR "$GP"
                    ldb       #3
?GP1                jsr       RVWAIT              ; WAIT FOR DATA FROM GPS
                    lda       SCDR                ; LOAD BYTE FROM SERIAL PORT
                    cmpa      ,X                  ; COMPARE TO POSITION FORMAT
                    bne       LOOP                ; IF SCDR IS NOT "$GP" START OVER
                    inx
                    decb
                    bne       ?GP1

                    ldx       #GPGGA
                    ldb       #4
?STRING             jsr       RVWAIT              ; WAIT FOR DATA FROM GPS
                    lda       SCDR                ; LOAD BYTE FROM SERIAL PORT
                    cmpa      ,X                  ; COMPARE TO POSITION FORMAT
                    bne       LOOP                ; IF SCDR IS NOT "GGA," GO TO NEXT STRING
                    inx
                    decb
                    bne       ?STRING

; DECODE MESSAGE
; $GPGGA,hhmmss.ss,ddmm.mmmm,n,dddmm.mmmm,e,q,ss,y.y,a.a,z,g.g,z,t.t,iiii*CC<CR><LF>

;*************** Time *************************************
; HOUR
                    lda       #$02                ; GET HH
                    ldx       #TIME
?TIMEL1             jsr       GETCHAR
                    cmpb      #','                ; IF A COMMA IS DETECTED THE DATA IS INVALID
                    beq       COMMA
                    stb       ,X
                    inx
                    deca
                    bne       ?TIMEL1

                    ldb       #':'                ; INSERT ":"
                    stb       ,X
                    inx

                    tstb
          ; MINUTE
                    lda       #2                  ; GET MM
                    jsr       GETLINE

                    ldb       #':'                ; INSERT ":"
                    stb       ,X
                    inx
          ; SECOND
                    lda       #2                  ; GET SS
                    jsr       GETLINE

                    clr       ,X                  ; INSERT NULL

;************** SKIP FRACTIONAL SECONDS *******************

                    lda       #4                  ; SKIP .SS,
SKIPFS              jsr       GETCHAR
                    deca
                    bne       SKIPFS

;************** LATITUDE ********************************

                    lda       #2                  ; GET ddmm.mmmm,n
                    ldx       #LAT
                    jsr       GETLINE

                    ldb       #$DF                ; INSERT DEGREE SYMBOL
                    stb       ,X
                    inx

                    lda       #$07                ; GET ddmm.mmmm,n
                    jsr       GETLINE

          ; INSERT A SPACE INSTEAD OF THE COMMA

                    jsr       GETCHAR
                    ldb       #' '
                    stb       ,X
                    inx

                    jsr       GETCHAR
                    stb       ,X
                    inx

                    lda       #3
                    jsr       SPACE

                    clr       ,X                  ; INSERT NULL
          ; SKIP COMMA
                    jsr       GETCHAR

;************* LONGITUDE *********************************

                    lda       #3                  ; GET ddmm.mmmm,n
                    ldx       #LON
                    jsr       GETLINE

                    ldb       #$DF                ; INSERT DEGREE SYMBOL
                    stb       ,X
                    inx

                    lda       #7                  ; GET ddmm.mmmm,n
                    jsr       GETLINE

          ; INSERT A SPACE INSTEAD OF THE COMMA

                    jsr       GETCHAR
                    ldb       #' '
                    stb       ,X
                    inx

                    jsr       GETCHAR
                    stb       ,X
                    inx

                    lda       #3                  ; INSERT 3 SPACES
                    jsr       SPACE

                    clr       ,X                  ; INSERT NULL
          ; SKIP COMMA
                    jsr       GETCHAR

;************** GPS QUALITY INDICATION **************
                    ldx       #QUAL
                    jsr       GETCHAR
                    stb       ,X
          ; SKIP COMMA
                    jsr       GETCHAR

;************* SATS BEING USED ***************************

                    ldx       #SAT
                    lda       #2                  ; GET ss
                    jsr       GETLINE

                    lda       #5
                    jsr       SPACE

;************** SKIP ,y.y, *******************

                    lda       #5
SKIPYS              jsr       GETCHAR
                    deca
                    bne       SKIPYS

;*************** ALTITUDE ********************
                    ldx       #ALT
                    lda       #3                  ; GET ALT
                    jsr       GETLINE

                    ldb       #'m'
                    stb       ,X
                    inx

                    ldb       #' '
                    stb       ,X
                    stb       1,X
                    inx:2

;********* HEADING AND SPEED ********************

LOOP2               ldx       #NMEA               ; START LOOKING FOR "$GP"
                    ldb       #$03

GP2                 jsr       RVWAIT              ; WAIT FOR DATA FROM GPS
                    lda       SCDR                ; LOAD BYTE FROM SERIAL PORT
                    cmpa      ,X                  ; COMPARE TO POSITION FORMAT
                    bne       LOOP2               ; IF SCDR IS NOT "$GP" START OVER
                    inx
                    decb
                    bne       GP2

                    ldx       #GPVTG
                    ldb       #$04

STRING2             jsr       RVWAIT              ; WAIT FOR DATA FROM GPS
                    lda       SCDR                ; LOAD BYTE FROM SERIAL PORT
                    cmpa      ,X                  ; COMPARE TO POSITION FORMAT
                    bne       LOOP2               ; IF SCDR IS NOT "GGA," GO TO NEXT STRING
                    inx
                    decb
                    bne       STRING2

; DECODE MESSAGE
; $GPVTG,,T,,M,,N,,K*4E

                    ldx       #HDG
                    lda       #3                  ; GET HEADING
HDG1                jsr       GETCHAR

                    cmpb      #','
                    beq       DEGREE

                    cmpb      #'.'
                    beq       DEGREE
                    stb       ,X
                    inx
                    deca
                    bne       HDG1

DEGREE              ldb       #$DF                ; DEGREE SYMBOL
                    stb       ,X
                    inx

                    ldb       #'T'                ; INSERT A "T"
                    stb       ,X
                    inx

                    lda       #5
                    jsr       SPACE

                    clr       ,X                  ; INSERT NULL

;*******************************************************************************
;************************** DISPLAY DATA TO LCD ********************************
;*******************************************************************************

;************* LATTITUDE ***********************************

                    ldx       #LINE1
                    bsr       SENDX

                    ldx       #LAT
                    bsr       SENDX

;************* LONGITUDE ***********************************

                    ldx       #LINE2
                    bsr       SENDX

                    ldx       #LON
                    bsr       SENDX

;************* LINE 3 **************************************

                    ldb       QUAL
                    cmpb      #'0'
                    bne       AVAIL

                    ldx       #GPSOFF
                    bsr       SENDX
                    bra       OUT

AVAIL               cmpb      #'1'
                    bne       DIF
                    ldx       #LINE3
                    bsr       SENDX

                    ldx       #ALT
                    bsr       SENDX

                    bra       OUT

DIF                 cmpb      #$32
                    ldx       #DIFFIX
                    bsr       SENDX

;************* LINE 4 **************************************

OUT                 ldx       #LINE4
                    bsr       SENDX

                    ldx       #SAT
                    bsr       SENDX
; END OF LOOP
                    jmp       LOOP

;*******************************************************************************
;******************************* SUBROUTINES ***********************************
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
                    ldb       ,X
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
                    tst       SCSR                ; load Status Register RS232
                    bpl       TXWAIT
                    rts

;*******************************************************************************
; Purpose: X points to buffer, A holds number of bytes (B is destroyed)
; Input  : X -> Buffer
;        : A = Number of bytes
; Output : None
; Note(s): B destroyed

GETLINE             proc
                    bsr       GETCHAR
                    stb       ,X
                    inx
                    deca
                    bne       GETLINE
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
Loop@@              lda       SCSR                ; load Status Register RS232
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
                    lda       #$40
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
                    stb       ,X
                    inx
                    deca
                    bne       Loop@@
                    pulb
                    rts

;*******************************************************************************
; TEXT AND POSITION DATA
;*******************************************************************************

; LCD INITIAL TEXT

TEXT1               fcb       $FE,$01,$FE,$0C
                    fcc       'ACQUIRING SATELLITES'
                    fcs       '    PLEASE WAIT...'

; GPS QUALITY MESSAGES

DIFFIX              fcs       $FE,$A0,'GPS DIFF. FIX MODE  '
GPSOFF              fcs       $FE,$A0,'  GPS UNAVAILABLE!  '

;*******************************************************************************

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
