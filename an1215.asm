;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; DOPID assembly function                                             CRC: $9AA3
; These routines calculate the new PWM duty cycle using the MC68HC11N4 math
; coprocessor.  The code can be run on an M68HC11EVS with an M68HC11K4 emulator
; and MC68HC11N4 processor.  The EVS monitor should be 1.1 or later. The EVS
; and vectors were set to SPECIAL TEST MODE to aid debug.  This code is called
; by a C routine but could be converted to an all assembly environment by
; defining the variables in assembly instead of as externals.
;*******************************************************************************

WaitALUF            macro
                    brclr     ALUF,#1,*           ; WAIT FOR ACF
                    endm

; MODULE  DOPID
; PUBLIC  DOPID

; P68H11
; RSEG    CODE

PWDTY1              set       $006C
ADCTL               set       $0030
ADR1                set       $0031
CREGH               set       $0040
CREGMH              set       $0041
CREGML              set       $0042
CREGL               set       $0043
ALUC                set       $0044
AREGH               set       $0045
AREGL               set       $0046
BREGH               set       $0047
BREGL               set       $0048
ALUF                set       $0049

                    #RAM

ADRCX               rmb       2                   ;EXTERNAL VARIABLES
ADRCXM1             rmb       2                   ;SIGNED INTS
CMNDVX              rmb       2
ERRX                rmb       2
ERRM1X              rmb       2
ERRM2X              rmb       2
ERRM3X              rmb       2
KPNUM               rmb       2
KPDEN               rmb       2
KINUM               rmb       2
KIDEN               rmb       2
KDNUM               rmb       2
KDDEN               rmb       2
PERDTNUM            rmb       2
PERDTDEN            rmb       2
INT56               rmb       2
FC56                rmb       2
TEMP1               rmb       2
TEMP2               rmb       2
TEMP3               rmb       2
TEMP4               rmb       2
KPTRM               rmb       4                   ;LONGS
KDTRM               rmb       4
KITRM               rmb       4
LTEMP1              rmb       4
LTEMP2              rmb       4
LTEMP3              rmb       4
LTEMP4              rmb       4
LTEMP5              rmb       4
LTEMP6              rmb       4
LTEMP7              rmb       4
LTEMP8              rmb       4
LTEMP9              rmb       4
LTEMPA              rmb       4
FCINT56             rmb       4
INTFC56             rmb       4
OLDDTY              rmb       4
NEWDTY              rmb       4

                    #ROM      *

;******** OUTPUT LAST PERIOD RESULT AND DO KP TERM ********

DOPID               lda       NEWDTY+1            ; OUTPUT PREVIOUS CALC.
                    sta       PWDTY1
                    clra
                    ldb       ADR1                ; GET CHANNEL 1 A/D RESULT.
                    std       ADRCX               ; DO KP TERM
                    ldb       #$10                ; FIRST START NEW A/D
                    stb       ADCTL               ; CONVERSION
                    ldd       CMNDVX
                    subd      ADRCX               ; FORM ERROR TERM
                    std       ERRX
                    bmi       NFLAG1              ; SET UP SIGN FLAG IN TEMP3
                    clr       TEMP3               ; POS
                    bra       NFLGG2

NFLAG1              lda       #$FF                ; NEG
                    sta       TEMP3
NFLGG2              lda       #$80                ; SET ALU FOR SMUL
                    sta       ALUC
                    ldd       ERRX
                    std       AREGH
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       KPNUM
                    std       BREGH               ; TRIGGER SMUL
                    @WaitALUF
                    lda       #$D0                ; SET ALU FOR SIDIV
                    std       ALUC
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       KPDEN
                    std       AREGH               ; TRIGGER SIDIV
                    @WaitALUF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    @WaitALUF
                    ldd       CREGH               ; GET INT PART OF RESULT
                    std       KPTRM

                    std       LTEMP1
                    ldd       CREGML              ; GET FRACTION
                    std       KPTRM+2
                    std       LTEMP1+2
                    jsr       ADLNG               ; NOW ADD TO OLDDTY
                    bsr       DOKIT               ; DO I TERM, ADD TO OLDDTY
                    jsr       DOKDT               ; DO D TERM, ADD TO OLDDTY
                    ldd       NEWDTY              ; CHECK LIMITS
                    bmi       JAMZP
                    cpd       #$FF
                    bmi       ?AnRTS
                    ldd       #$FF                ; JAM FF
                    std       NEWDTY              ; SATURATED HIGH
                    std       OLDDTY
?AnRTS              rts

JAMZP               clrd                          ; JAM 00
                    std       NEWDTY              ; SATURATED LOW
                    std       OLDDTY
                    rts

; ROUTINE TO DO INTEGRAL TERM

DOKIT               ldd       ADRCX               ; GET CURRENT CONVERSION
                    addd      ADRCXM1             ; FORM (ADRCX + ADRCXM1)/2
                    lsrd
                    std       LTEMP2
                    bcs       JMHAFI
                    clrd
                    std       LTEMP2+2            ; FRACTIONAL PART OF FINAL ERROR
                    bra       INTKIE              ; TERM WILL ALWAYS BE 0 or 0.5

JMHAFI              ldd       #$8000
                    std       LTEMP2+2
INTKIE              ldd       CMNDVX
                    subd      LTEMP2
                    brclr     LTEMP2+2,#$80,NOFCN
                    std       LTEMP2
                    ble       NGFLG3
                    decd
NOFCN               std       LTEMP2              ; CMNDVX - ((ADRCX + ADRCXM1)/2)
                    bmi       NGFLG3              ; SET UP SIGN FLAG IN TEMP3
                    clra
                    bra       NGFLG2

NGFLG3              lda       #$FF
NGFLG2              sta       TEMP3
                    lda       #$D0                ; SET ALU FOR SIDIV TO FORM
                    std       ALUC                ; KINUM/KIDEN
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    clrd                          ; SET UP KI NUMERATOR
                    std       CREGH
                    ldd       KINUM
                    std       CREGML
                    ldd       KIDEN
                    std       AREGH               ; TRIGGER SIDIV
                    @WaitALUF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    @WaitALUF
                    ldd       CREGH
                    std       LTEMP3
                    ldd       CREGML
                    std       LTEMP3+2
                    lda       #$D0                ; SET ALU FOR SIDIV TO FORM
                    std       ALUC                ; PERDTNUM/PERDTDEN
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    clrd                          ; SET UP PERDTNUM
                    std       CREGH
                    ldd       PERDTNUM
                    std       CREGML
                    ldd       PERDTDEN
                    std       AREGH               ; TRIGGER SIDIV
                    @WaitALUF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    @WaitALUF
                    ldd       CREGH
                    std       LTEMP4
                    ldd       CREGML
                    std       LTEMP4+2
                    ldd       LTEMP3              ; NOW FORM LTEMP2*LTEMP3*LTEMP4
                    std       LTEMP5
                    ldd       LTEMP3+2
                    std       LTEMP5+2
                    ldd       LTEMP4
                    std       LTEMP6
                    ldd       LTEMP4+2
                    std       LTEMP6+2
                    lda       TEMP3               ; SAVE SIGN FLAG
                    sta       TEMP4               ; AND USE TEMP3 AS A FLAG
                    clr       TEMP3               ; FOR LTEMP6 BEING POSITIVE
                    jsr       MULLNG              ; DO LTEMP3*LTEMP4(PERDT*KI)
                    ldd       LTEMP7              ; NOW PUT RESULT IN LTEMP5
                    std       LTEMP5
                    ldd       LTEMP7+2
                    std       LTEMP5+2
                    lda       TEMP4               ; RETRIEVE SIGN FLAG
                    sta       TEMP3
                    ldd       LTEMP2
                    std       LTEMP6              ; ERROR FOR KI TERM
                    ldd       LTEMP2+2
                    std       LTEMP6+2
                    jsr       MULLNG              ; DO RESULT*LTEMP2
                    ldd       LTEMP7
                    std       LTEMP1
                    std       KITRM
                    ldd       LTEMP7+2
                    std       LTEMP1+2
                    std       KITRM+2             ; ADD KI TERM INTO NEWDTY
                    jsr       ADLNG               ; KITERM DONE
                    rts                           ; RETURN

;*********    ROUTINE TO DO KD TERM     **********

DOKDT               ldd       ERRX                ; FORM (ERRX - ERRM3X)
                    subd      ERRM3X              ; + 3*(ERRM1X - ERRM2X)
                    std       TEMP1
                    ldd       ERRM1X
                    subd      ERRM2X
                    std       AREGH               ; FORM 3*(ERRM1X - ERRM2X)
                    lda       #$80
                    sta       ALUC
                    lda       #1
                    sta       ALUF
                    ldd       #3
                    std       BREGH
                    @WaitALUF
                    ldd       CREGML
                    addd      TEMP1
                    std       LTEMPA
                    bpl       POSGN

                    lda       #$FF
                    sta       TEMP3
                    bra       KDFLGD

POSGN               clr       TEMP3
KDFLGD              clrd
                    std       LTEMPA+2            ; DONE
                    clr       ALUC                ; FORM 6*PERDTNUM
                    lda       #1
                    sta       ALUF
                    ldd       #6
                    std       AREGH
                    ldd       PERDTNUM
                    std       BREGH
                    @WaitALUF
                    ldd       CREGML
                    std       TEMP2
NOFCND              lda       #$D0                ; SET ALU FOR SIDIV TO FORM
                    std       ALUC                ; KDNUM/KDDEN
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    clrd                          ; SET UP KD NUMERATOR
                    std       CREGH
                    ldd       KDNUM
                    std       CREGML
                    ldd       KDDEN
                    std       AREGH               ; TRIGGER SIDIV
                    @WaitALUF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    @WaitALUF
                    ldd       CREGH
                    std       LTEMP8
                    ldd       CREGML
                    std       LTEMP8+2
                    lda       #$D0                ; SET ALU FOR SIDIV TO FORM
                    std       ALUC                ; PERDTDEN/(PERDTNUM*6)
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    clrd                          ; SET UP PERDTNUM
                    std       CREGH
                    ldd       PERDTDEN
                    std       CREGML
                    ldd       TEMP2
                    std       AREGH               ; TRIGGER SIDIV
                    @WaitALUF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    @WaitALUF
                    ldd       CREGH
                    std       LTEMP9
                    ldd       CREGML
                    std       LTEMP9+2
                    ldd       LTEMP8              ; NOW FORM LTEMPA*LTEMP8*LTEMP9
                    std       LTEMP5
                    ldd       LTEMP8+2
                    std       LTEMP5+2
                    ldd       LTEMP9
                    std       LTEMP6
                    ldd       LTEMP9+2
                    std       LTEMP6+2

                    lda       TEMP3               ; SAVE SIGN FLAG
                    sta       TEMP4               ; AND USE TEMP3 AS A FLAG
                    clr       TEMP3               ; FOR LTEMP6 BEING POSITIVE
                    bsr       MULLNG              ; DO LTEMP8*LTEMP9
                    ldd       LTEMP7              ; NOW PUT RESULT IN LTEMP5
                    std       LTEMP5
                    ldd       LTEMP7+2
                    std       LTEMP5+2
                    lda       TEMP4               ; RETRIEVE SIGNED ERROR
                    sta       TEMP3
                    ldd       LTEMPA
                    std       LTEMP6              ; ERROR FOR KD TERM
                    ldd       LTEMPA+2
                    std       LTEMP6+2
                    bsr       MULLNG              ; DO RESULT*LTEMPA
                    ldd       LTEMP7
                    std       LTEMP1
                    std       KDTRM
                    ldd       LTEMP7+2
                    std       LTEMP1+2
                    std       KDTRM+2
                    jsr       ADLNG               ; ADD KD TERM INTO NEWDTY
                    rts                           ; KDTERM DONE

;  SUBROUTINE TO MULTIPLY LONGS(INTEGER & FRACTION)
;  LTEMP5*LTEMP6=LTEMP7 ONLY LTEMP6 CAN HAVE
;  A NEGATIVE TERM TO HANDLE.

MULLNG              lda       #$80                ; SET ALU FOR SMUL
                    sta       ALUC                ; AND MULTIPLY INTS
                    ldd       LTEMP5
                    std       AREGH
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       LTEMP6
                    std       BREGH               ; TRIGGER SMUL
                    @WaitALUF
                    ldd       CREGML
                    std       INT56
                    lda       #1                  ; CLEAR ACF AND DO NEXT MULT
                    sta       ALUF
                    lda       #$80                ; TEST TEMP3 SIGN
                    bita      TEMP3               ; SEE IF ERR IS NEG
                    bmi       NEGFRAC             ; TERM IS NEGATIVE
                    ldd       LTEMP6+2            ; GET FRAC NOT NEG
                    std       BREGH               ; TRIGGER SMUL
                    @WaitALUF
                    ldd       CREGH               ; SCALE AND STORE
                    std       INTFC56
                    ldd       CREGML
                    std       INTFC56+2
                    bra       NXFRAC

NEGFRAC             clrd                          ; NEGATE FRAC
                    subd      LTEMP6+2
                    std       BREGH               ; TRIGGER SMUL
                    @WaitALUF
                    clrd                          ; NEGATE RESULT
                    subd      CREGH               ; SCALE AND STORE
                    bpl       INTFIX2

                    incd
INTFIX2             std       INTFC56
                    clrd
                    subd      CREGML
                    std       INTFC56+2
NXFRAC              ldd       LTEMP5+2            ; GET FRAC AND MULTIPLY

                    std       AREGH               ; WITH POSSIBLE NEG INT
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       LTEMP6
                    std       BREGH               ; TRIGGER SMUL
                    @WaitALUF
                    lda       #$80
                    bita      TEMP3
                    bpl       DFXINT
                    lda       #$80                ; SEE IF SIGN OVERFLOW
                    bita      AREGH               ; ON FRACTION
                    bpl       DFXINT

                    clrd
                    subd      CREGH
                    std       CREGH
DFXINT              ldd       CREGH               ; THIS SCALES FRAC
                    bpl       PFFIX

NFFIX               incd
PFFIX               std       FCINT56
                    ldd       CREGML
                    std       FCINT56+2
                    lda       #1                  ; NOW DO FRAC*FRAC
                    sta       ALUF                ; CLEAR ACF
                    clr       ALUC                ; SET UNSIGNED MULT FOR FRACS
                    lda       #$80                ; TEST ERR SIGN
                    bita      TEMP3               ; SEE IF ERR IS NEG
                    bmi       NFCFRAC             ; TERM IS NEGATIVE
                    ldd       LTEMP6+2            ; GET FRAC NOT NEG
                    std       BREGH               ; TRIGGER SMUL
                    @WaitALUF
                    ldd       CREGH               ; SCALE AND STORE
                    std       FC56
                    bra       SUMMUL

NFCFRAC             clrd                          ; NEGATE FRAC
                    subd      LTEMP6+2
                    std       BREGH               ; TRIGGER SMUL
                    @WaitALUF
                    clrd                          ; NEGATE RESULT
                    subd      CREGH               ; SCALE AND STORE
                    std       FC56
SUMMUL              ldd       INT56               ; NOW SUMM ALL PRODUCTS
                    addd      FCINT56             ; INTS ARE ALL SIGNED
                    addd      INTFC56             ; CAN JUST ADD UP
                    std       LTEMP7
                    lda       #$80                ; TEST ERRX SIGN
                    bita      TEMP3               ; SEE IF FRACS ARE NEG
                    bmi       SUMNFC              ; FRACS ARE NEGATIVE
                    ldd       FCINT56+2           ; POSITIVE
                    addd      INTFC56+2
                    bcc       SUMFC1

                    xgdx                          ; SAVE SUM
                    ldd       #1                  ; ADD CARRY INTO INT
                    addd      LTEMP7
                    std       LTEMP7
                    xgdx                          ; RETRIEVE SUM
SUMFC1              addd      FC56
                    std       LTEMP7+2
                    bcs       FCCAR2
                    rts

FCCAR2              ldd       #1                  ; ADD CARRY INTO INT
                    addd      LTEMP7
                    std       LTEMP7
                    rts

SUMNFC              clrd                          ; COMPLEMENT NEG FRACS
                    subd      FCINT56+2
                    std       FCINT56+2
                    clrd
                    subd      INTFC56+2
                    std       INTFC56+2
                    clrd
                    subd      FC56
                    std       FC56
                    ldd       FCINT56+2           ; NEGATIVE
                    addd      INTFC56+2
                    bcc       SUMFC2              ; REMEMBER SIGN BIT!!

                    xgdx                          ; SAVE SUM
                    ldd       #$FFFF              ; ADD BORROW INTO INT
                    addd      LTEMP7
                    std       LTEMP7
                    xgdx                          ; RETRIEVE SUM
SUMFC2              addd      FC56
                    std       LTEMP7+2
                    bcc       SMFCDN

                    ldd       #$FFFF              ; ADD BORROW INTO INT
                    addd      LTEMP7
                    std       LTEMP7
SMFCDN              clrd                          ; CONVERT BACK TO NEG
                    subd      LTEMP7+2
                    std       LTEMP7+2
                    rts


; SUBROUTINE TO ADD INTEGER AND FRACTION IN LTEMP1 TO OLDDTY

ADLNG               lda       #$80                ; TEST ERRX SIGN
                    bita      TEMP3
                    bmi       KXNEG               ; TERM IS NEGATIVE
                    ldd       LTEMP1              ; GET INT PART
                    addd      OLDDTY              ; ADD AND STORE INT
                    std       NEWDTY
                    ldd       LTEMP1+2            ; GET FRAC PART
                    addd      OLDDTY+2            ; ADD AND STORE FRAC
                    std       NEWDTY+2
                    bcc       ADDONE

                    ldd       #1                  ; ADD CARRY FROM FRAC
                    addd      NEWDTY
                    std       NEWDTY
                    bra       ADDONE

KXNEG               ldd       LTEMP1              ; GET INT PART
                    addd      OLDDTY              ; ADD AND STORE INT
                    std       NEWDTY
                    ldd       LTEMP1+2            ; GET FRAC PART
                    addd      OLDDTY+2            ; ADD AND STORE FRAC
                    std       NEWDTY+2            ; ACTUALLY A SUBTRACTION
                    bcs       ADDONE
DECINT              ldd       #$FFFF              ; SUBTRACT BORROW FROM FRAC
                    addd      NEWDTY
                    std       NEWDTY
ADDONE              ldd       NEWDTY              ; UPDATE OLDDTY FOR NEXT TERM
                    std       OLDDTY              ; OR FINISH
                    ldd       NEWDTY+2
                    std       OLDDTY+2
                    rts                           ; RETURN TO CALLING ROUTINE

                    end       :s19crc
