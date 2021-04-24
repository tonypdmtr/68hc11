;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;                   DOPID assembly function                           CRC: $6742
; These routines calculate the new PWM duty cycle using the
; MC68HC11N4 math coprocessor. The code can be run on an
; M68HC11EVS with an M68HC11K4 emulator and MC68HC11N4
; processor.  The EVS monitor should be 1.1 or later. The EVS
; and vectors were set to SPECIAL TEST MODE to aid debug.
; This code is called by a C routine but could be converted to
; an all assembly environment by defining the variables in
; assembly instead of as externals.
;*******************************************************************************
; 1999.02.02: Adapted to ASM11 v9.65 by Tony Papadimitriou <tonyp@acm.org>
;             (Extracted from Mot's Application Note AN1215)
;             Converted to a stand-alone routine.
; 2000.11.23: Adapted to ASM11 v9.65b+ by Tony Papadimitriou <tonyp@acm.org>
;             Optimized slightly
;*******************************************************************************

                    #ExtraOn
                    #CaseOn
                    #SpacesOff
                    #OptRelOn
                    #OptRtsOn

#IfMain ;-----------------------------------------------------------------------
                    #RAM      0
                    #ROM      $F800
                    #Memory   $F800 $FFBF
                    #Memory   $FFD6 $FFFF
#endif ;------------------------------------------------------------------------

;*******************************************************************************
                    #RAM
;*******************************************************************************

ADRCX               rmb       2                   ; $0084 EXTERNAL VARIABLES
ADRCXM1             rmb       2                   ; $0086 SIGNED INTS
CMNDVX              rmb       2                   ; $0082
ERRX                rmb       2                   ; $0088
ERRM1X              rmb       2                   ; $008A
ERRM2X              rmb       2                   ; $008C
ERRM3X              rmb       2                   ; $008E
KPNUM               rmb       2                   ; $0090
KPDEN               rmb       2                   ; $0092
KINUM               rmb       2                   ; $0094
KIDEN               rmb       2                   ; $0096
KDNUM               rmb       2                   ; $0098
KDDEN               rmb       2                   ; $009A
PERDTNUM            rmb       2                   ; $009C
PERDTDEN            rmb       2                   ; $009E
INT56               rmb       2                   ; $00A0
FC56                rmb       2                   ; $00A2
TEMP1               rmb       2                   ; $00A4
TEMP2               rmb       2                   ; $00A6
TEMP3               rmb       2                   ; $00A8
TEMP4               rmb       2                   ; $00AA
KPTRM               rmb       4                   ; $00B4 LONGS
KITRM               rmb       4                   ; $00BC
KDTRM               rmb       4                   ; $00B8
LTEMP1              rmb       4                   ; $00C0
LTEMP2              rmb       4                   ; $00C4
LTEMP3              rmb       4                   ; $00C8
LTEMP4              rmb       4                   ; $00CC
LTEMP5              rmb       4                   ; $00D0
LTEMP6              rmb       4                   ; $00D4
LTEMP7              rmb       4                   ; $00D8
LTEMP8              rmb       4                   ; $00DC
LTEMP9              rmb       4                   ; $00E0
LTEMPA              rmb       4                   ; $00E4
FCINT56             rmb       4                   ; $00E8
INTFC56             rmb       4                   ; $00EC
OLDDTY              rmb       4                   ; $00B0
NEWDTY              rmb       4                   ; $00AC

;*******************************************************************************
                    #ROM
;*******************************************************************************

PWDTY1              equ       $006C
ADCTL               equ       $0030
ADR1                equ       $0031
CREGH               equ       $0040
CREGMH              equ       $0041
CREGML              equ       $0042
CREGL               equ       $0043
ALUC                equ       $0044
AREGH               equ       $0045
AREGL               equ       $0046
BREGH               equ       $0047
BREGL               equ       $0048
ALUF                equ       $0049

#ifdef
                    extern    ADRCX:ZPAGE         ; $0084 EXTERNAL VARIABLES
                    extern    ADRCXM1:ZPAGE       ; $0086 SIGNED INTS
                    extern    CMNDVX:ZPAGE        ; $0082
                    extern    ERRX:ZPAGE          ; $0088
                    extern    ERRM1X:ZPAGE        ; $008A
                    extern    ERRM2X:ZPAGE        ; $008C
                    extern    ERRM3X:ZPAGE        ; $008E
                    extern    KPNUM:ZPAGE         ; $0090
                    extern    KPDEN:ZPAGE         ; $0092
                    extern    KINUM:ZPAGE         ; $0094
                    extern    KIDEN:ZPAGE         ; $0096
                    extern    KDNUM:ZPAGE         ; $0098
                    extern    KDDEN:ZPAGE         ; $009A
                    extern    PERDTNUM:ZPAGE      ; $009C
                    extern    PERDTDEN:ZPAGE      ; $009E
                    extern    INT56:ZPAGE         ; $00A0
                    extern    FC56:ZPAGE          ; $00A2
                    extern    TEMP1:ZPAGE         ; $00A4
                    extern    TEMP2:ZPAGE         ; $00A6
                    extern    TEMP3:ZPAGE         ; $00A8
                    extern    TEMP4:ZPAGE         ; $00AA
                    extern    KPTRM:ZPAGE         ; $00B4 LONGS
                    extern    KITRM:ZPAGE         ; $00BC
                    extern    KDTRM:ZPAGE         ; $00B8
                    extern    LTEMP1:ZPAGE        ; $00C0
                    extern    LTEMP2:ZPAGE        ; $00C4
                    extern    LTEMP3:ZPAGE        ; $00C8
                    extern    LTEMP4:ZPAGE        ; $00CC
                    extern    LTEMP5:ZPAGE        ; $00D0
                    extern    LTEMP6:ZPAGE        ; $00D4
                    extern    LTEMP7:ZPAGE        ; $00D8
                    extern    LTEMP8:ZPAGE        ; $00DC
                    extern    LTEMP9:ZPAGE        ; $00E0
                    extern    LTEMPA:ZPAGE        ; $00E4
                    extern    FCINT56:ZPAGE       ; $00E8
                    extern    INTFC56:ZPAGE       ; $00EC
                    extern    OLDDTY:ZPAGE        ; $00B0
                    extern    NEWDTY:ZPAGE        ; $00AC
#endif
;*******************************************************************************
; OUTPUT LAST PERIOD RESULT AND DO KP TERM

DOPID               proc
                    lda       NEWDTY+1            ; OUTPUT PREVIOUS CALC.
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

NFLAG1              lda       #-1                 ; NEG
                    sta       TEMP3
NFLGG2              lda       #$80                ; SET ALU FOR SMUL
                    sta       ALUC
                    ldd       ERRX
                    std       AREGH
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       KPNUM
                    std       BREGH               ; TRIGGER SMUL
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    lda       #$D0                ; SET ALU FOR SIDIV
                    std       ALUC
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       KPDEN
                    std       AREGH               ; TRIGGER SIDIV
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    ldd       CREGH               ; GET INT PART OF RESULT
                    std       KPTRM
                    std       LTEMP1
                    ldd       CREGML              ; GET FRACTION
                    std       KPTRM+2
                    std       LTEMP1+2
                    jsr       ADLNG               ; NOW ADD TO OLDDTY
                    bsr       DOKIT               ; DO I TERM, ADD TO OLDDTY (JSR)
                    jsr       DOKDT               ; DO D TERM, ADD TO OLDDTY
                    ldd       NEWDTY              ; CHECK LIMITS
                    bmi       JAMZP
                    cpd       #$00FF
                    bmi       KXDONE
                    ldd       #$00FF              ; JAM FF
                    std       NEWDTY              ; SATURATED HIGH
                    std       OLDDTY
KXDONE              rts

JAMZP               clrd                          ; JAM 00
                    std       NEWDTY              ; SATURATED LOW
                    std       OLDDTY
                    rts

;*******************************************************************************
; ROUTINE TO DO INTEGRAL TERM *

DOKIT               proc
                    ldd       ADRCX               ; GET CURRENT CONVERSION
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

NGFLG3              lda       #-1
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
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
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
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
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
                    jmp       ADLNG               ; KITERM DONE

;*******************************************************************************
; ROUTINE TO DO KD TERM

DOKDT               proc
                    ldd       ERRX                ; FORM (ERRX - ERRM3X)
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
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    ldd       CREGML
                    addd      TEMP1
                    std       LTEMPA
                    bpl       POSGN               ; SET UP SIGN FLAG IN TEMP3
                    lda       #-1
                    sta       TEMP3
                    bra       KDFLGD

POSGN               clr       TEMP3
KDFLGD              clrd
                    std       LTEMPA+2            ; DONE
                    sta       ALUC                ; FORM 6*PERDTNUM
                    inca
                    sta       ALUF
                    ldd       #6
                    std       AREGH
                    ldd       PERDTNUM
                    std       BREGH
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
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
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    lda       #$01                ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
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
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    lda       #$E8                ; TRIGGER SFDIV
                    std       ALUC
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
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
                    bsr       MULLNG              ; DO LTEMP8*LTEMP9 (JSR)
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
                    bsr       MULLNG              ; DO RESULT*LTEMPA (JSR)
                    ldd       LTEMP7
                    std       LTEMP1
                    std       KDTRM
                    ldd       LTEMP7+2
                    std       LTEMP1+2
                    std       KDTRM+2
                    jmp       ADLNG               ; ADD KD TERM INTO NEWDTY

;*******************************************************************************
; SUBROUTINE TO MULTIPLY LONGS(INTEGER & FRACTION)
; LTEMP5*LTEMP6=LTEMP7 ONLY LTEMP6 CAN HAVE
; A NEGATIVE TERM TO HANDLE.

MULLNG              proc
                    lda       #$80                ; SET ALU FOR SMUL
                    sta       ALUC                ; AND MULTIPLY INTS
                    ldd       LTEMP5
                    std       AREGH
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       LTEMP6
                    std       BREGH               ; TRIGGER SMUL
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    ldd       CREGML
                    std       INT56
                    lda       #$01                ; CLEAR ACF AND DO NEXT MULT
                    sta       ALUF
                    lda       #$80                ; TEST TEMP3 SIGN
                    bita      TEMP3               ; SEE IF ERR IS NEG
                    bmi       NEGFRAC             ; TERM IS NEGATIVE
                    ldd       LTEMP6+2            ; GET FRAC NOT NEG
                    std       BREGH               ; TRIGGER SMUL
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    ldd       CREGH               ; SCALE AND STORE
                    std       INTFC56
                    ldd       CREGML
                    std       INTFC56+2
                    bra       NXFRAC

NEGFRAC             proc
                    clrd                          ; NEGATE FRAC
                    subd      LTEMP6+2
                    std       BREGH               ; TRIGGER SMUL
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    clrd                          ; NEGATE RESULT
                    subd      CREGH               ; SCALE AND STORE
                    bpl       INTFIX2
                    incd
INTFIX2             std       INTFC56
                    clrd
                    subd      CREGML
                    std       INTFC56+2
                    clra
NXFRAC              ldd       LTEMP5+2            ; GET FRAC AND MULTIPLY
                    std       AREGH               ; WITH POSSIBLE NEG INT
                    lda       #1                  ; CLEAR ACF
                    sta       ALUF
                    ldd       LTEMP6
                    std       BREGH               ; TRIGGER SMUL
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
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
                    incd
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
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
                    ldd       CREGH               ; SCALE AND STORE
                    std       FC56
                    bra       SUMMUL

NFCFRAC             clrd                          ; NEGATE FRAC
                    subd      LTEMP6+2
                    std       BREGH               ; TRIGGER SMUL
                    brclr     ALUF,#$01,*         ; WAIT FOR ACF
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
                    bcc       SMFCDP
                    ldd       #1                  ; ADD CARRY INTO INT
                    addd      LTEMP7
                    std       LTEMP7
                    rts

;*******************************************************************************

SUMNFC              proc
                    clrd                          ; COMPLEMENT NEG FRACS
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
                    ldd       #-1                 ; ADD BORROW INTO INT
                    addd      LTEMP7
                    std       LTEMP7
                    xgdx                          ; RETRIEVE SUM
SUMFC2              addd      FC56
                    std       LTEMP7+2
                    bcc       SMFCDN
                    ldd       #-1                 ; ADD BORROW INTO INT
                    addd      LTEMP7
                    std       LTEMP7
SMFCDN              clrd                          ; CONVERT BACK TO NEG
                    subd      LTEMP7+2
                    std       LTEMP7+2
SMFCDP              rts

;*******************************************************************************
; SUBROUTINE TO ADD INTEGER AND FRACTION IN LTEMP1 TO OLDDTY *

ADLNG               proc
                    lda       #$80                ; TEST ERRX SIGN
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
                    ldd       #$FFFF              ; SUBTRACT BORROW FROM FRAC
                    addd      NEWDTY
                    std       NEWDTY
ADDONE              ldd       NEWDTY              ; UPDATE OLDDTY FOR NEXT TERM
                    std       OLDDTY              ; OR FINISH
                    ldd       NEWDTY+2
                    std       OLDDTY+2
                    rts                           ; RETURN TO CALLING ROUTINE
