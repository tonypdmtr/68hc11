;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;                                  HC11FP
;
;                             Copyright 1986,1991
;                                     by
;                               Gordon Doughman
;
; The source code for this floating point package for the MC68HC11 may be
; freely distributed under the rules of public domain. However it is a
; copyrighted work and as such the original source code or any derivitive
; including parts of the original source code may not be sold as a product
; or included as part of a product for sale without permission of the author.
; Any object code produced by the source code however, may be included as part
; of a product for sale.
;
; The author reserves the right to make changes to this file. Although this
; software has been carefully reviewed and is believed to be reliable,
; Motorola nor the author assumes any liability arising from its use. This
; software may be freely used and/or modified at no cost or obligation to the
; user.
;
; Version 1.0 Released in 1986
; Version 1.1 Released 12/20/91 - Fixed a rounding problem in the FLTDIV routine.
;                               - Added code to clear the upper 8-bits of FPACC1MN
;                                 before performing the UINT2FLT & SINT2FLT.
;                               - Added code to clear the sign byte of FPACC1 at
;                                 the beginning of the SINT2FLT routine.
;                               - Added capability to recognize 'E' or 'e' as
;                                 exponent designator in ASC2FLT routine.
;                               - Fixed a problem in FLTADD detecting a 0 mantissa
;                                 after the subtraction of two unlike signed
;                                 numbers.
;*******************************************************************************

;*******************************************************************************
                    #RAM      $0000
;*******************************************************************************

FPACC1EX            rmb       1                   ; FLOATING POINT ACCUMULATOR #1..
FPACC1MN            rmb       3
MANTSGN1            rmb       1                   ; MANTISSA SIGN FOR FPACC1 (0=+, FF=-).
FPACC2EX            rmb       1                   ; FLOATING POINT ACCUMULATOR #2.
FPACC2MN            rmb       3
MANTSGN2            rmb       1                   ; MANTISSA SIGN FOR FPACC2 (0=+, FF=-).

FLTFMTER            equ       1                   ; floating point format error in ASCFLT
OVFERR              equ       2                   ; floating point overflow error
UNFERR              equ       3                   ; floating point underflow error
DIV0ERR             equ       4                   ; division by 0 error
TOLGSMER            equ       5                   ; number too large or small to convert to int.
NSQRTERR            equ       6                   ; tried to take the square root of negative #
TAN90ERR            equ       7                   ; TANgent of 90 degrees attempted

; ttl ASCFLT
;*******************************************************************************
; ASCII TO FLOATING POINT ROUTINE
;
; This routine will accept most any ASCII floating point format
; and return a 32-bit floating point number. The following are
; some examples of legal ASCII floating point numbers.
;
; 20.095
; 0.125
; 7.2984E10
; 167.824E5
; 5.9357E-7
; 500
;
; The floating point number returned is in "FPACC1".
;
; The exponent is biased by 128 to facilitate floating point
; comparisons. A pointer to the ASCII string is passed to the
; routine in the D-register.
;*******************************************************************************
;                   #RAM      $0000
;*******************************************************************************
; FPACC1EX          rmb       1                   ; FLOATING POINT ACCUMULATOR #1..
; FPACC1MN          rmb       3
; MANTSGN1          rmb       1                   ; MANTISSA SIGN FOR FPACC1 (0=+, FF=-).
; FPACC2EX          rmb       1                   ; FLOATING POINT ACCUMULATOR #2.
; FPACC2MN          rmb       3
; MANTSGN2          rmb       1                   ; MANTISSA SIGN FOR FPACC2 (0=+, FF=-).
; FLTFMTER          equ       1
; LOCAL VARIABLES (ON STACK POINTED TO BY Y)

EXPSIGN             equ       0                   ; EXPONENT SIGN (0=+, FF=-).
PWR10EXP            equ       1                   ; POWER 10 EXPONENT.

;*******************************************************************************
                    #ROM      $C000               ; (TEST FOR EVB)
;*******************************************************************************

ASCFLT              proc
                    pshx                          ; SAVE POINTER TO ASCII STRING.
                    jsr       PSHFPAC2            ; SAVE FPACC2.
                    ldx       #0                  ; PUSH ZEROS ON STACK TO INITIALIZE LOCALS.
                    pshx                          ; ALLOCATE ; 2 BYTES FOR LOCALS.
                    stx       FPACC1EX            ; CLEAR FPACC1.
                    stx       FPACC1EX+2
                    clr       MANTSGN1            ; MAKE THE MANTISSA SIGN POSITIVE INITIALLY.
                    tsy                           ; POINT TO LOCALS.
                    ldx       6,y                 ; GET POINTER TO ASCII STRING.
                    lda       ,x                  ; GET 1ST CHARACTER IN STRING.
                    jsr       NUMERIC             ; IS IT A NUMBER.
                    bcs       _3@@                ; YES. GO PROCESS IT.
          ;-------------------------------------- ; LEADING MINUS SIGN ENCOUNTERED?
                    cmpa      #'-'                ; NO. IS IT A MINUS SIGN?
                    bne       _1@@                ; NO. GO CHECK FOR DECIMAL POINT.
                    com       MANTSGN1            ; YES. SET MANTISSA SIGN. LEADING MINUS BEFORE?
                    inx                           ; POINT TO NEXT CHARACTER.
                    lda       ,x                  ; GET IT.
                    jsr       NUMERIC             ; IS IT A NUMBER?
                    bcs       _3@@                ; YES. GO PROCESS IT.
          ;-------------------------------------- ; LEADING DECIMAL POINT?
_1@@                cmpa      #'.'                ; IS IT A DECIMAL POINT?
                    bne       _2@@                ; NO. FORMAT ERROR.
                    inx                           ; YES. POINT TO NEXT CHARACTER.
                    lda       ,x                  ; GET IT.
                    jsr       NUMERIC             ; MUST HAVE AT LEAST ONE DIGIT AFTER D.P.
                    bcc       _2@@                ; GO REPORT ERROR.
                    jmp       _14@@               ; GO BUILD FRACTION.
          ;-------------------------------------- ; FLOATING POINT FORMAT ERROR
_2@@                ins:2                         ; DE-ALLOCATE LOCALS.
                    jsr       PULFPAC2            ; RESTORE FPACC2.
                    pulx                          ; GET POINTER TO TERMINATING CHARACTER IN STRING.
                    lda       #FLTFMTER           ; FORMAT ERROR.
                    sec                           ; SET ERROR FLAG.
                    rts
          ;-------------------------------------- ; PRE DECIMAL POINT MANTISSA BUILD
_3@@                lda       ,x
                    jsr       NUMERIC
                    bcc       _13@@
                    jsr       ADDNXTD
                    inx
                    bcc       _3@@
          ;-------------------------------------- ; PRE DECIMAL POINT MANTISSA OVERFLOW
_4@@                inc       FPACC1EX            ; INC FOR EACH DIGIT ENCOUNTERED PRIOR TO D.P.
                    lda       ,x                  ; GET NEXT CHARACTER.
                    inx                           ; POINT TO NEXT.
                    jsr       NUMERIC             ; IS IT S DIGIT?
                    bcs       _4@@                ; YES. KEEP BUILDING POWER 10 MANTISSA.
                    cmpa      #'.'                ; NO. IS IT A DECIMAL POINT?
                    bne       _6@@                ; NO. GO CHECK FOR THE EXPONENT.
          ;-------------------------------------- ; ANY FRACTIONAL DIGITS ARE NOT SIGNIFIGANT
_5@@                lda       ,x                  ; GET THE NEXT CHARACTER.
                    jsr       NUMERIC             ; IS IT A DIGIT?
                    bcc       _6@@                ; NO. GO CHECK FOR AN EXPONENT.
                    inx                           ; POINT TO THE NEXT CHARACTER.
                    bra       _5@@                ; FLUSH REMAINING DIGITS.

_6@@                cmpa      #'E'                ; NO. IS IT THE EXPONENT? (upper case)
                    beq       _7@@                ; YES. GO PROCESS IT.
                    cmpa      #'e'                ; IS IT THE EXPONENT? (lower case)
                    beq       _7@@                ; YES. GO PROCESS IT.
                    jmp       FINISH              ; NO. GO FINISH THE CONVERSION.
          ;-------------------------------------- ; PROCESS THE EXPONENT
_7@@                inx                           ; POINT TO NEXT CHARACTER.
                    lda       ,x                  ; GET THE NEXT CHARACTER.
                    jsr       NUMERIC             ; SEE IF IT'S A DIGIT.
                    bcs       _10@@               ; YES. GET THE EXPONENT.
                    cmpa      #'-'                ; NO. IS IT A MINUS SIGN?
                    beq       _8@@                ; YES. GO FLAG A NEGATIVE EXPONENT.
                    cmpa      #'+'                ; NO. IS IT A PLUS SIGN?
                    beq       _9@@                ; YES. JUST IGNORE IT.
                    bra       _2@@                ; NO. FORMAT ERROR.

_8@@                com       EXPSIGN,y           ; FLAG A NEGATIVE EXPONENT. IS IT 1ST?
_9@@                inx                           ; POINT TO NEXT CHARACTER.
                    lda       ,x                  ; GET NEXT CHARACTER.
                    jsr       NUMERIC             ; IS IT A NUMBER?
                    bcc       _2@@                ; NO. FORMAT ERROR.
_10@@               suba      #'0'                ; MAKE IT BINARY.
                    sta       PWR10EXP,y          ; BUILD THE POWER 10 EXPONENT.
                    inx                           ; POINT TO NEXT CHARACTER.
                    lda       ,x                  ; GET IT.
                    jsr       NUMERIC             ; IS IT NUMERIC?
                    bcc       _11@@               ; NO. GO FINISH UP THE CONVERSION.
                    ldb       PWR10EXP,y          ; YES. GET PREVIOUS DIGIT.
                    lslb:2                        ; MULT. 4.
                    addb      PWR10EXP,y          ; BY 5.
                    lslb                          ; BY 10.
                    suba      #'0'                ; MAKE SECOND DIGIT BINARY.
                    aba                           ; ADD IT TO FIRST DIGIT.
                    sta       PWR10EXP,y
                    cmpa      #38                 ; IS THE EXPONENT OUT OF RANGE?
                    bhi       _2@@                ; YES. REPORT ERROR.
_11@@               lda       PWR10EXP,y          ; GET POWER 10 EXPONENT.
                    tst       EXPSIGN,y           ; WAS IT NEGATIVE?
                    bpl       _12@@               ; NO. GO ADD IT TO BUILT 10 PWR EXPONENT.
                    nega
_12@@               adda      FPACC1EX            ; FINAL TOTAL PWR 10 EXPONENT.
                    sta       FPACC1EX            ; SAVE RESULT.
                    bra       FINISH              ; GO FINISH UP CONVERSION.
          ;-------------------------------------- ; PRE-DECIMAL POINT NON-DIGIT FOUND, IS IT A DECIMAL POINT?
_13@@               cmpa      #'.'                ; IS IT A DECIMAL POINT?
                    bne       _6@@                ; NO. GO CHECK FOR THE EXPONENT.
                    inx                           ; YES. ; POINT TO NEXT CHARACTER.
          ;-------------------------------------- ; POST DECIMAL POINT PROCESSING
_14@@               lda       ,x                  ; GET NEXT CHARACTER.
                    jsr       NUMERIC             ; IS IT NUMERIC?
                    bcc       _6@@                ; NO. GO CHECK FOR EXPONENT.
                    bsr       ADDNXTD             ; YES. ADD IN THE DIGIT.
                    inx                           ; POINT TO THE NEXT CHARACTER.
                    bcs       _5@@                ; IF OVER FLOW, FLUSH REMAINING DIGITS.
                    dec       FPACC1EX            ; ADJUST THE 10 POWER EXPONENT.
                    bra       _14@@               ; PROCESS ALL FRACTIONAL DIGITS.

;*******************************************************************************

ADDNXTD             proc
                    lda       FPACC1MN            ; GET UPPER 8 BITS.
                    sta       FPACC2MN            ; COPY INTO FPAC2.
                    ldd       FPACC1MN+1          ; GET LOWER 16 BITS OF MANTISSA.
                    std       FPACC2MN+1          ; COPY INTO FPACC2.
                    lsld                          ; MULT. ; BY 2.
                    rol       FPACC1MN            ; OVERFLOW?
                    bcs       Fail@@              ; YES. DON'T ADD THE DIGIT IN.
                    lsld                          ; MULT ; BY 4.
                    rol       FPACC1MN            ; OVERFLOW?
                    bcs       Fail@@              ; YES. DON'T ADD THE DIGIT IN.
                    addd      FPACC2MN+1          ; BY 5.
                    psha                          ; SAVE ; A.
                    lda       FPACC1MN            ; GET UPPER 8 BITS.
                    adca      #0                  ; ADDIN POSSABLE CARRY FROM LOWER 16 BITS.
                    adda      FPACC2MN            ; ADD IN UPPER 8 BITS.
                    sta       FPACC1MN            ; SAVE IT.
                    pula                          ; RESTORE ; A.
                    bcs       Fail@@              ; OVERFLOW? IF SO DON'T ADD IT IN.
                    lsld                          ; BY ; 10.
                    rol       FPACC1MN
                    std       FPACC1MN+1          ; SAVE THE LOWER 16 BITS.
                    bcs       Fail@@              ; OVERFLOW? IF SO DON'T ADD IT IN.
                    ldb       ,x                  ; GET CURRENT DIGIT.
                    subb      #'0'                ; MAKE IT BINARY.
                    clra                          ; 16-BIT.
                    addd      FPACC1MN+1          ; ADD IT IN TO TOTAL.
                    std       FPACC1MN+1          ; SAVE THE RESULT.
                    lda       FPACC1MN            ; GET UPPER 8 BITS.
                    adca      #0                  ; ADD IN POSSIBLE CARRY. OVERFLOW?
                    bcs       Fail@@              ; YES. COPY OLD MANTISSA FROM FPACC2.
                    sta       FPACC1MN            ; NO. EVERYHING OK.
                    rts

Fail@@              ldd       FPACC2MN+1          ; RESTORE THE ORIGINAL MANTISSA BECAUSE
                    std       FPACC1MN+1          ; OF OVERFLOW.
                    lda       FPACC2MN
                    sta       FPACC1MN
                    rts

;*******************************************************************************
; NOW FINISH UP CONVERSION BY MULTIPLYING THE RESULTANT MANTISSA
; BY 10 FOR EACH POSITIVE POWER OF 10 EXPONENT RECIEVED OR BY .1
; (DIVIDE BY 10) FOR EACH NEGATIVE POWER OF 10 EXPONENT RECIEVED.

FINISH              proc
                    stx       6,y                 ; SAVE POINTER TO TERMINATING CHARACTER IN STRING.
                    ldx       #FPACC1EX           ; POINT TO FPACC1.
                    bsr       CHCK0               ; SEE IF THE NUMBER IS ZERO.
                    beq       _3@@                ; QUIT IF IT IS.
                    lda       FPACC1EX            ; GET THE POWER 10 EXPONENT.
                    sta       PWR10EXP,y          ; SAVE IT.
                    lda       #$80+24             ; SET UP INITIAL EXPONENT (# OF BITS + BIAS).
                    sta       FPACC1EX
                    bsr       FPNORM              ; GO NORMALIZE THE MANTISSA.
                    tst       PWR10EXP,y          ; IS THE POWER 10 EXPONENT POSITIVE OR ZERO?
                    beq       _3@@                ; IT'S ZERO, WE'RE DONE.
                    bpl       _1@@                ; IT'S POSITIVE MULTIPLY BY 10.
                    ldx       #CONSTP1            ; NO. GET CONSTANT .1 (DIVIDE BY 10).
                    jsr       GETFPAC2            ; GET CONSTANT INTO FPACC2.
                    neg       PWR10EXP,y          ; MAKE THE POWER 10 EXPONENT POSITIVE.
                    bra       _2@@                ; GO DO THE MULTIPLIES.

_1@@                ldx       #CONST10            ; GET CONSTANT '10' TO MULTIPLY BY.
                    jsr       GETFPAC2            ; GET CONSTANT INTO FPACC2.
_2@@                bsr       FLTMUL              ; GO MULTIPLY FPACC1 BY FPACC2, RESULT IN FPACC1.
                    dec       PWR10EXP,y          ; DECREMENT THE POWER 10 EXPONENT.
                    bne       _2@@                ; GO CHECK TO SEE IF WE'RE DONE.
_3@@                ins:2                         ; DE-ALLOCATE ; LOCALS.
                    jsr       PULFPAC2            ; RESTORE FPACC2.
                    pulx                          ; GET ; POINTER TO TERMINATING CHARACTER IN STRING.
                    rts                           ; RETURN WITH NUMBER IN FPACC1.

;*******************************************************************************

NUMERIC             proc
                    cmpa      #'0'                ; IS IT LESS THAN AN ASCII 0?
                    blo       Fail@@              ; YES. NOT NUMERIC.
                    cmpa      #'9'                ; IS IT GREATER THAN AN ASCII 9?
                    bhi       Fail@@              ; YES. NOT NUMERIC.
                    sec                           ; IT ; WAS NUMERIC. SET THE CARRY.
                    rts

Fail@@              clc                           ; NON-NUMERIC ; CHARACTER. CLEAR THE CARRY.
                    rts

;*******************************************************************************

FPNORM              proc
                    ldx       #FPACC1EX           ; POINT TO FPACC1.
                    bsr       CHCK0               ; CHECK TO SEE IF IT'S 0.
                    beq       Done@@              ; YES. JUST RETURN.
                    tst       FPACC1MN            ; IS THE NUMBER ALREADY NORMALIZED?
                    bmi       Done@@              ; YES. JUST RETURN..
                    ldd       FPACC1MN+1          ; GET THE LOWER 16 BITS OF THE MANTISSA.
Loop@@              dec       FPACC1EX            ; DECREMENT THE EXPONENT FOR EACH SHIFT.
                    beq       Fail@@              ; EXPONENT WENT TO 0. UNDERFLOW.
                    lsld                          ; SHIFT ; THE LOWER 16 BITS.
                    rol       FPACC1MN            ; ROTATE THE UPPER 8 BITS. NUMBER NORMALIZED?
                    bpl       Loop@@              ; NO. KEEP SHIFTING TO THE LEFT.
                    std       FPACC1MN+1          ; PUT THE LOWER 16 BITS BACK INTO FPACC1.
Done@@              clc                           ; SHOW ; NO ERRORS.
                    rts                           ; YES. ; RETURN.

Fail@@              sec                           ; FLAG ; ERROR.
                    rts

;*******************************************************************************

CHCK0               proc                          ; CHECKS FOR ZERO IN FPACC POINTED TO BY X.
                    pshd                          ; SAVE ; D.
                    ldd       ,x                  ; GET FPACC EXPONENT & HIGH 8 BITS.
                    bne       Done@@              ; NOT ZERO. RETURN.
                    ldd       2,x                 ; CHECK LOWER 16 BITS.
Done@@              puld                          ; RESTORE ; D.
                    rts                           ; RETURN WITH CC SET.

CONSTP1             long      $7D4CCCCD           ; 0.1 DECIMAL
CONST10             long      $84200000           ; 10.0 DECIMAL

; ttl FLTMUL
;*******************************************************************************
; FPMULT: FLOATING POINT MULTIPLY
;
; THIS FLOATING POINT MULTIPLY ROUTINE MULTIPLIES "FPACC1" BY
; "FPACC2" AND PLACES THE RESULT IN TO FPACC1. FPACC2 REMAINS
; UNCHANGED.
; WORSE CASE = 2077 CYCLES = 1039 uS @ 2MHz
; BEST CASE = 1475 CYCLES = 738 uS @ 2MHz
; AVERAGE = 1776 CYCLES = 888 uS @ 2MHz
;*******************************************************************************

FLTMUL              proc
                    jsr       PSHFPAC2            ; SAVE FPACC2.
                    ldx       #FPACC1EX           ; POINT TO FPACC1
                    bsr       CHCK0               ; CHECK TO SEE IF FPACC1 IS ZERO.
                    beq       _5@@                ; IT IS. ANSWER IS 0.
                    ldx       #FPACC2EX           ; POINT TO FPACC2.
                    bsr       CHCK0               ; IS IT 0?
                    bne       _1@@                ; NO. CONTINUE.
                    clrd
                    std       FPACC1EX            ; MAKE FPACC1 0.
                    std       FPACC1MN+1
                    bra       _5@@

_1@@                lda       MANTSGN1            ; GET FPACC1 EXPONENT.
                    eora      MANTSGN2            ; SET THE SIGN OF THE RESULT.
                    sta       MANTSGN1            ; SAVE THE SIGN OF THE RESULT.
                    lda       FPACC1EX            ; GET FPACC1 EXPONENT.
                    adda      FPACC2EX            ; ADD IT TO FPACC2 EXPONENT.
                    bpl       _3@@                ; IF RESULT IS MINUS AND
                    bcc       _4@@                ; THE CARRY IS SET THEN:
_2@@                lda       #OVFERR             ; OVERFLOW ERROR.
                    sec                           ; SET ; ERROR FLAG.
                    bra       _6@@

_3@@                bcs       _4@@                ; IF RESULT IS PLUS & THE CARRY IS SET THEN ALL OK.
                    lda       #UNFERR             ; ELSE UNDERFLOW ERROR OCCURED.
                    sec                           ; FLAG ; ERROR.
                    bra       _6@@

_4@@                adda      #$80                ; ADD 128 BIAS BACK IN THAT WE LOST.
                    sta       FPACC1EX            ; SAVE THE NEW EXPONENT.
                    bsr       UMULT               ; GO MULTIPLY THE "INTEGER" MANTISSAS.
                    tst       FPACC1EX            ; WAS THERE AN OVERFLOW ERROR FROM ROUNDING?
                    beq       _2@@                ; YES. RETURN ERROR.
_5@@                clc                           ; SHOW ; NO ERRORS. Moved the _5@@ label to this instruction from the TST instruction above. G.S.D. 12/20/91
_6@@                jsr       PULFPAC2            ; RESTORE FPACC2.
                    rts

;*******************************************************************************

UMULT               proc
                    clrx
                    pshx:2                        ; CREATE PARTIAL PRODUCT REGISTER AND COUNTER.
                    tsx                           ; POINT TO THE VARIABLES.
                    lda       #24                 ; SET COUNT TO THE NUMBER OF BITS.
                    sta       ,x
Loop@@              lda       FPACC2MN+2          ; GET THE L.S. BYTE OF THE MULTIPLIER.
                    lsra                          ; PUT L.S. BIT IN CARRY.
                    bcc       _1@@                ; IF CARRY CLEAR, DON'T ADD MULTIPLICAND TO P.P.
                    ldd       FPACC1MN+1          ; GET MULTIPLICAND L.S. 16 BITS.
                    addd      2,x                 ; ADD TO PARTIAL PRODUCT.
                    std       2,x                 ; SAVE IN P.P.
                    lda       FPACC1MN            ; GET UPPER 8 BITS OF MULTIPLICAND.
                    adca      1,x                 ; ADD IT W/ CARRY TO P.P.
                    sta       1,x                 ; SAVE TO PARTIAL PRODUCT.
_1@@                ror       1,x                 ; ROTATE PARTIAL PRODUCT TO THE RIGHT.
                    ror       2,x
                    ror       3,x
                    ror       FPACC2MN            ; SHIFT THE MULTIPLIER TO THE RIGHT 1 BIT.
                    ror       FPACC2MN+1
                    ror       FPACC2MN+2
                    dec       ,x                  ; DONE YET?
                    bne       Loop@@              ; NO. KEEP GOING.
                    tst       1,x                 ; DOES PARTIAL PRODUCT NEED TO BE NORMALIZED?
                    bmi       _2@@                ; NO. GET ANSWER & RETURN.
                    lsl       FPACC2MN            ; GET BIT THAT WAS SHIFTED OUT OF P.P REGISTER.
                    rol       3,x                 ; PUT IT BACK INTO THE PARTIAL PRODUCT.
                    rol       2,x
                    rol       1,x
                    dec       FPACC1EX            ; FIX EXPONENT.
_2@@                tst       FPACC2MN            ; DO WE NEED TO ROUND THE PARTIAL PRODUCT?
                    bpl       _3@@                ; NO. JUST RETURN.
                    ldd       2,x                 ; YES. GET THE LEAST SIGNIFIGANT 16 BITS.
                    addd      #1                  ; ADD 1.
                    std       2,x                 ; SAVE RESULT.
                    lda       1,x                 ; PROPIGATE THROUGH.
                    adca      #0
                    sta       1,x
                    bcc       _3@@                ; IF CARRY CLEAR ALL IS OK.
                    ror       1,x                 ; IF NOT OVERFLOW. ROTATE CARRY INTO P.P.
                    ror       2,x
                    ror       3,x
                    inc       FPACC1EX            ; UP THE EXPONENT.
_3@@                ins                           ; TAKE ; COUNTER OFF STACK.
                    pulx                          ; GET ; M.S. 16 BITS OF PARTIAL PRODUCT.
                    stx       FPACC1MN            ; PUT IT IN FPACC1.
                    pula                          ; GET ; L.S. 8 BITS OF PARTIAL PRODUCT.
                    sta       FPACC1MN+2          ; PUT IT IN FPACC1.
                    rts

; ttl FLTADD
;*******************************************************************************
; FLOATING POINT ADDITION
;
; This subroutine performs floating point addition of the two numbers
; in FPACC1 and FPACC2. The result of the addition is placed in
; FPACC1 while FPACC2 remains unchanged. This subroutine performs
; full signed addition so either number may be of the same or opposite
; sign.
; WORSE CASE = 782 CYCLES = 391 uS @ 2MHz
; BEST CASE = 123 CYCLES = 62 uS @ 2MHz
; AVERAGE = 409 CYCLES = 205 uS @ 2MHz
;*******************************************************************************

FLTADD              proc
                    jsr       PSHFPAC2            ; SAVE FPACC2.
                    ldx       #FPACC2EX           ; POINT TO FPACC2
                    jsr       CHCK0               ; IS IT ZERO?
                    bne       FLTADD1             ; NO. GO CHECK FOR 0 IN FPACC1.
FLTADD6             clc                           ; NO ; ERRORS.
FLTADD10            jsr       PULFPAC2            ; RESTORE FPACC2.
                    rts                           ; ANSWER ; IN FPACC1. RETURN.

FLTADD1             ldx       #FPACC1EX           ; POINT TO FPACC1.
                    jsr       CHCK0               ; IS IT ZERO?
                    bne       FLTADD2             ; NO. GO ADD THE NUMBER.
FLTADD4             ldd       FPACC2EX            ; ANSWER IS IN FPACC2. MOVE IT INTO FPACC1.
                    std       FPACC1EX
                    ldd       FPACC2MN+1          ; MOVE LOWER 16 BITS OF MANTISSA.
                    std       FPACC1MN+1
                    lda       MANTSGN2            ; MOVE FPACC2 MANTISSA SIGN INTO FPACC1.
                    sta       MANTSGN1
                    bra       FLTADD6

FLTADD2             lda       FPACC1EX            ; GET FPACC1 EXPONENT.
                    cmpa      FPACC2EX            ; ARE THE EXPONENTS THE SAME?
                    beq       FLTADD7             ; YES. GO ADD THE MANTISSA'S.
                    suba      FPACC2EX            ; NO. FPACC1EX-FPACC2EX. IS FPACC1 > FPACC2?
                    bpl       FLTADD3             ; YES. GO CHECK RANGE.
                    nega                          ; NO. ; FPACC1 < FPACC2. MAKE DIFFERENCE POSITIVE.
                    cmpa      #23                 ; ARE THE NUMBERS WITHIN RANGE?
                    bhi       FLTADD4             ; NO. FPACC2 IS LARGER. GO MOVE IT INTO FPACC1.
                    tab                           ; PUT ; DIFFERENCE IN B.
                    addb      FPACC1EX            ; CORRECT FPACC1 EXPONENT.
                    stb       FPACC1EX            ; SAVE THE RESULT.
                    ldx       #FPACC1MN           ; POINT TO FPACC1 MANTISSA.
                    bra       FLTADD5             ; GO DENORMALIZE FPACC1 FOR THE ADD.

FLTADD3             cmpa      #23                 ; FPACC1 > FPACC2. ARE THE NUMBERS WITHIN RANGE?
                    bhi       FLTADD6             ; NO. ANSWER ALREADY IN FPACC1. JUST RETURN.
                    ldx       #FPACC2MN           ; POINT TO THE MANTISSA TO DENORMALIZE.
FLTADD5             lsr       ,x                  ; SHIFT THE FIRST BYTE OF THE MANTISSA.
                    ror       1,x                 ; THE SECOND.
                    ror       2,x                 ; AND THE THIRD.
                    deca                          ; DONE ; YET?
                    bne       FLTADD5             ; NO. KEEP SHIFTING.
FLTADD7             lda       MANTSGN1            ; GET FPACC1 MANTISSA SIGN.
                    cmpa      MANTSGN2            ; ARE THE SIGNS THE SAME?
                    beq       FLTADD11            ; YES. JUST GO ADD THE TWO MANTISSAS.
                    tst       MANTSGN1            ; NO. IS FPACC1 THE NEGATIVE NUMBER?
                    bpl       FLTADD8             ; NO. GO DO FPACC1-FPACC2.
                    ldx       FPACC2MN            ; YES. EXCHANGE FPACC1 & FPACC2 BEFORE THE SUB.
                    pshx                          ; SAVE ; IT.
                    ldx       FPACC1MN            ; GET PART OF FPACC1.
                    stx       FPACC2MN            ; PUT IT IN FPACC2.
                    pulx                          ; GET ; SAVED PORTION OF FPACC2
                    stx       FPACC1MN            ; PUT IT IN FPACC1.
                    ldx       FPACC2MN+2          ; GET LOWER 8 BITS & SIGN OF FPACC2.
                    pshx                          ; SAVE ; IT.
                    ldx       FPACC1MN+2          ; GET LOWER 8 BITS & SIGN OF FPACC1.
                    stx       FPACC2MN+2          ; PUT IT IN FPACC2.
                    pulx                          ; GET ; SAVED PART OF FPACC2.
                    stx       FPACC1MN+2          ; PUT IT IN FPACC1.
FLTADD8             ldd       FPACC1MN+1          ; GET LOWER 16 BITS OF FPACC1.
                    subd      FPACC2MN+1          ; SUBTRACT LOWER 16 BITS OF FPACC2.
                    std       FPACC1MN+1          ; SAVE RESULT.
                    lda       FPACC1MN            ; GET HIGH 8 BITS OF FPACC1 MANTISSA.
                    sbca      FPACC2MN            ; SUBTRACT HIGH 8 BITS OF FPACC2.
                    sta       FPACC1MN            ; SAVE THE RESULT. IS THE RESULT NEGATIVE?
                    bcc       FLTADD9             ; NO. GO NORMALIZE THE RESULT.
                    lda       FPACC1MN            ; YES. NEGATE THE MANTISSA.
                    coma
                    psha                          ; SAVE THE RESULT.
                    ldd       FPACC1MN+1          ; GET LOWER 16 BITS.
                    negd                          ; FORM THE TWO'S COMPLEMENT.
                    std       FPACC1MN+1          ; SAVE THE RESULT.
                    pula                          ; GET UPPER 8 BITS BACK.
                    adca      #0                  ; ADD IN POSSIBLE CARRY.
                    sta       FPACC1MN            ; SAVE RESULT.
                    lda       #$FF                ; SHOW THAT FPACC1 IS NEGATIVE.
                    sta       MANTSGN1

; The following 7 lines were added 12/18/91 to check for a 0 mantissa after the subtraction
; performed above. If the mantissa is 0, the FPACC1 exponent & sign byte are cleared.

FLTADD9             ldd       FPACC1MN            ; Did the FPACC1 mantissa go to 0 after the subtract?
                    bne       FLTADD13            ; No. Go normalize the result.
                    tst       FPACC1MN+2          ; The upper 16-bits were 0, how about the lower 8-bits?
                    bne       FLTADD13            ; No. Go normalize the result.
                    clr       FPACC1EX            ; The mantissa is 0. Set the exponent & sign byte to 0.
                    clr       MANTSGN1
                    bra       FLTADD12            ; Return with no errors.

FLTADD13            jsr       FPNORM              ; GO NORMALIZE THE RESULT.
                    bcc       FLTADD12            ; EVERYTHING'S OK SO RETURN.
                    lda       #UNFERR             ; UNDERFLOW OCCURED DURING NORMALIZATION.
                    sec                           ; FLAG ERROR.
                    jmp       FLTADD10

FLTADD12            jmp       FLTADD6             ; CAN'T BRANCH THAT FAR FROM HERE.

FLTADD11            ldd       FPACC1MN+1          ; GET LOWER 16 BITS OF FPACC1.
                    addd      FPACC2MN+1          ; ADD IT TO THE LOWER 16 BITS OF FPACC2.
                    std       FPACC1MN+1          ; SAVE RESULT IN FPACC1.
                    lda       FPACC1MN            ; GET UPPER 8 BITS OF FPACC1.
                    adca      FPACC2MN            ; ADD IT (WITH CARRY) TO UPPER 8 BITS OF FPACC2.
                    sta       FPACC1MN            ; SAVE THE RESULT.
                    bcc       FLTADD12            ; NO OVERFLOW SO JUST RETURN.
                    ror       FPACC1MN            ; PUT THE CARRY INTO THE MANTISSA.
                    ror       FPACC1MN+1          ; PROPAGATE THROUGH MANTISSA.
                    ror       FPACC1MN+2
                    inc       FPACC1EX            ; UP THE MANTISSA BY 1.
                    bne       FLTADD12            ; EVERYTHING'S OK JUST RETURN.
                    lda       #OVFERR             ; RESULT WAS TOO LARGE. OVERFLOW.
                    sec                           ; FLAG ERROR.
                    jmp       FLTADD10

; ttl FLTSUB
;*******************************************************************************
; FLOATING POINT SUBTRACT SUBROUTINE
;
; This subroutine performs floating point subtraction ( FPACC1-FPACC2)
; by inverting the sign of FPACC2 and then calling FLTADD since
; FLTADD performs complete signed addition. Upon returning from
; FLTADD the sign of FPACC2 is again inverted to leave it unchanged
; from its original value.
;
; WORSE CASE = 797 CYCLES = 399 uS @ 2MHz
; BEST CASE = 148 CYCLES = 74 uS @ 2MHz
; AVERAGE = 434 CYCLES = 217 uS @ 2MHz
;*******************************************************************************

FLTSUB              proc
                    bsr       _@@                 ; INVERT SIGN.
                    jsr       FLTADD              ; GO DO FLOATING POINT ADD.
_@@                 lda       MANTSGN2            ; GET FPACC2 MANTISSA SIGN.
                    eora      #$FF                ; INVERT THE SIGN.
                    sta       MANTSGN2            ; PUT BACK.
                    rts

; ttl FLTDIV
;*******************************************************************************
; FLOATING POINT DIVIDE
;
; This subroutine performs signed floating point divide. The
; operation performed is FPACC1/FPACC2. The divisor (FPACC2) is left
; unaltered and the answer is placed in FPACC1. There are several
; error conditions that can be returned by this routine. They are:
; a) division by zero. b) overflow. c) underflow. As with all
; other routines, an error is indicated by the carry being set and
; the error code being in the A-reg.
;*******************************************************************************

FLTDIV              proc
                    ldx       #FPACC2EX           ; POINT TO FPACC2.
                    jsr       CHCK0               ; IS THE DIVISOR 0?
                    bne       FLTDIV1             ; NO. GO SEE IF THE DIVIDEND IS ZERO.
                    lda       #DIV0ERR            ; YES. RETURN A DIVIDE BY ZERO ERROR.
                    sec                           ; FLAG ERROR.
                    rts

FLTDIV1             ldx       #FPACC1EX           ; POINT TO FPACC1.
                    jsr       CHCK0               ; IS THE DIVIDEND 0?
                    bne       FLTDIV2             ; NO. GO PERFORM THE DIVIDE.
                    clc                           ; YES. ANSWER IS ZERO. NO ERRORS.
                    rts

FLTDIV2             jsr       PSHFPAC2            ; SAVE FPACC2.
                    lda       MANTSGN2            ; GET FPACC2 MANTISSA SIGN.
                    eora      MANTSGN1            ; SET THE SIGN OF THE RESULT.
                    sta       MANTSGN1            ; SAVE THE RESULT.
                    ldx       #0                  ; SET UP WORK SPACE ON THE STACK.
                    pshx:3
                    lda       #24                 ; PUT LOOP COUNT ON STACK.
                    psha
                    tsx                           ; SET UP POINTER TO WORK SPACE.
                    ldd       FPACC1MN            ; COMPARE FPACC1 & FPACC2 MANTISSAS.
                    cpd       FPACC2MN            ; ARE THE UPPER 16 BITS THE SAME?
                    bne       FLTDIV3             ; NO.
                    lda       FPACC1MN+2          ; YES. COMPARE THE LOWER 8 BITS.
                    cmpa      FPACC2MN+2
FLTDIV3             bhs       FLTDIV4             ; IS FPACC2 MANTISSA > FPACC1 MANTISSA? NO.
                    inc       FPACC2EX            ; ADD 1 TO THE EXPONENT TO KEEP NUMBER THE SAME.
                                                  ; DID OVERFLOW OCCUR?
                    bne       FLTDIV14            ; NO. GO SHIFT THE MANTISSA RIGHT 1 BIT.
FLTDIV8             lda       #OVFERR             ; YES. GET ERROR CODE.
                    sec                           ; FLAG ; ERROR.
FLTDIV6             pulx:3                        ; REMOVE WORKSPACE FROM STACK.
                    ins
                    jsr       PULFPAC2            ; RESTORE FPACC2.
                    rts

FLTDIV4             ldd       FPACC1MN+1          ; DO AN INITIAL SUBTRACT IF DIVIDEND MANTISSA IS
                    subd      FPACC2MN+1          ; GREATER THAN DIVISOR MANTISSA.
                    std       FPACC1MN+1
                    lda       FPACC1MN
                    sbca      FPACC2MN
                    sta       FPACC1MN
                    dec       ,x                  ; SUBTRACT 1 FROM THE LOOP COUNT.
FLTDIV14            lsr       FPACC2MN            ; SHIFT THE DIVISOR TO THE RIGHT 1 BIT.
                    ror       FPACC2MN+1
                    ror       FPACC2MN+2
                    lda       FPACC1EX            ; GET FPACC1 EXPONENT.
                    ldb       FPACC2EX            ; GET FPACC2 EXPONENT.
                    negb                          ; ADD THE TWO'S COMPLEMENT TO SET FLAGS PROPERLY.
                    aba
                    bmi       FLTDIV5             ; IF RESULT MINUS CHECK CARRY FOR POSS. OVERFLOW.
                    bcs       FLTDIV7             ; IF PLUS & CARRY SET ALL IS OK.
                    lda       #UNFERR             ; IF NOT, UNDERFLOW ERROR.
                    bra       FLTDIV6             ; RETURN WITH ERROR.

FLTDIV5             bcs       FLTDIV8             ; IF MINUS & CARRY SET OVERFLOW ERROR.
FLTDIV7             adda      #$81                ; ADD BACK BIAS+1 (THE '1' COMPENSATES FOR ALGOR.)
                    sta       FPACC1EX            ; SAVE RESULT.
FLTDIV9             ldd       FPACC1MN            ; SAVE DIVIDEND IN CASE SUBTRACTION DOESN'T GO.
                    std       4,x
                    lda       FPACC1MN+2
                    sta       6,x
                    ldd       FPACC1MN+1          ; GET LOWER 16 BITS FOR SUBTRACTION.
                    subd      FPACC2MN+1
                    std       FPACC1MN+1          ; SAVE RESULT.
                    lda       FPACC1MN            ; GET HIGH 8 BITS.
                    sbca      FPACC2MN
                    sta       FPACC1MN
                    bpl       FLTDIV10            ; SUBTRACTION WENT OK. GO DO SHIFTS.
                    ldd       4,x                 ; RESTORE OLD DIVIDEND.
                    std       FPACC1MN
                    lda       6,x
                    sta       FPACC1MN+2
FLTDIV10            rol       3,x                 ; ROTATE CARRY INTO QUOTIENT.
                    rol       2,x
                    rol       1,x
                    lsl       FPACC1MN+2          ; SHIFT DIVIDEND TO LEFT FOR NEXT SUBTRACT.
                    rol       FPACC1MN+1
                    rol       FPACC1MN
                    dec       ,x                  ; DONE YET?
                    bne       FLTDIV9             ; NO. KEEP GOING.
                    com       1,x                 ; RESULT MUST BE COMPLEMENTED.
                    com       2,x
                    com       3,x
                    ldd       FPACC1MN+1          ; DO 1 MORE SUBTRACT FOR ROUNDING.
                    subd      FPACC2MN+1          ; ( DON'T NEED TO SAVE THE RESULT. )
                    lda       FPACC1MN
                    sbca      FPACC2MN            ; ( NO NEED TO SAVE THE RESULT. )
                    ldd       2,x                 ; GET LOW 16 BITS.
                    bcc       FLTDIV11            ; IF IT DIDNT GO RESULT OK AS IS.
                    clc                           ; CLEAR ; THE CARRY.
                    bra       FLTDIV13            ; GO SAVE THE NUMBER.

FLTDIV11            addd      #1                  ; ROUND UP BY 1.
FLTDIV13            std       FPACC1MN+1          ; PUT IT IN FPACC1.
                    lda       1,x                 ; GET HIGH 8 BITS.
                    adca      #0
                    sta       FPACC1MN            ; SAVE RESULT.
                    bcc       FLTDIV12            ; IF CARRY CLEAR ANSWER OK.
                    ror       FPACC1MN            ; IF NOT OVERFLOW. ROTATE CARRY IN.
                    ror       FPACC1MN+1
                    ror       FPACC1MN+2
                    inc       FPACC1EX            ; Compensate the exponent for rotate right. Added 12/17/91 G.S.D.
                    bne       FLTDIV12            ; if the exponent didn't go to zero, the answer's OK.
                    jmp       FLTDIV8             ; if not an overflow occurred.

FLTDIV12            clc                           ; NO ; ERRORS.
                    jmp       FLTDIV6

; ttl FLTASC
;*******************************************************************************
; FLOATING POINT TO ASCII CONVERSION SUBROUTINE
;
; This subroutine performs floating point to ASCII conversion of
; the number in FPACC1. The ascii string is placed in a buffer
; pointed to by the X index register. The buffer must be at least
; 14 bytes long to contain the ASCII conversion. The resulting
; ASCII string is terminated by a zero (0) byte. Upon exit the
; X Index register will be pointing to the first character of the
; string. FPACC1 and FPACC2 will remain unchanged.
;*******************************************************************************

FLTASC              proc
                    pshx                          ; SAVE THE POINTER TO THE STRING BUFFER.
                    ldx       #FPACC1EX           ; POINT TO FPACC1.
                    jsr       CHCK0               ; IS FPACC1 0?
                    bne       _1@@                ; NO. GO CONVERT THE NUMBER.
                    pulx                          ; RESTORE POINTER.
                    ldd       #'0'<8              ; GET ASCII CHARACTER + TERMINATING BYTE.
                    std       ,x                  ; PUT IT IN THE BUFFER.
                    rts

_1@@                ldx       FPACC1EX            ; SAVE FPACC1.
                    pshx
                    ldx       FPACC1MN+1
                    pshx
                    lda       MANTSGN1
                    psha
                    jsr       PSHFPAC2            ; SAVE FPACC2.
                    clrx
                    pshx:2                        ; ALLOCATE LOCALS.
                    pshx                          ; SAVE SPACE FOR STRING BUFFER POINTER.
                    tsy                           ; POINT TO LOCALS.
                    ldx       15,y                ; GET POINTER FROM STACK.
                    lda       #$20                ; PUT A SPACE IN THE BUFFER IF NUMBER NOT NEGATIVE.
                    tst       MANTSGN1            ; IS IT NEGATIVE?
                    beq       _2@@                ; NO. GO PUT SPACE.
                    clr       MANTSGN1            ; MAKE NUMBER POSITIVE FOR REST OF CONVERSION.
                    lda       #'-'                ; YES. PUT MINUS SIGN IN BUFFER.
_2@@                sta       ,x
                    inx                           ; POINT TO NEXT LOCATION.
                    stx       ,y                  ; SAVE POINTER.
Loop@@              ldx       #N9999999           ; POINT TO CONSTANT 9999999.
                    jsr       GETFPAC2            ; GET INTO FPACC2.
                    jsr       FLTCMP              ; COMPARE THE NUMBERS. IS FPACC1 > 9999999?
                    bhi       _4@@                ; YES. GO DIVIDE FPACC1 BY 10.
                    ldx       #P9999999           ; POINT TO CONSTANT 999999.9
                    jsr       GETFPAC2            ; MOVE IT INTO FPACC2.
                    jsr       FLTCMP              ; COMPARE NUMBERS. IS FPACC1 > 999999.9?
                    bhi       _5@@                ; YES. GO CONTINUE THE CONVERSION.
                    dec       2,y                 ; DECREMENT THE MULT./DIV. COUNT.
                    ldx       #CONST10            ; NO. MULTIPLY BY 10. POINT TO CONSTANT.
_3@@                jsr       GETFPAC2            ; MOVE IT INTO FPACC2.
                    jsr       FLTMUL
                    bra       Loop@@              ; GO DO COMPARE AGAIN.

_4@@                inc       2,y                 ; INCREMENT THE MULT./DIV. COUNT.
                    ldx       #CONSTP1            ; POINT TO CONSTANT ".1".
                    bra       _3@@                ; GO DIVIDE FPACC1 BY 10.

_5@@                ldx       #CONSTP5            ; POINT TO CONSTANT OF ".5".
                    jsr       GETFPAC2            ; MOVE IT INTO FPACC2.
                    jsr       FLTADD              ; ADD .5 TO NUMBER IN FPACC1 TO ROUND IT.
                    ldb       FPACC1EX            ; GET FPACC1 EXPONENT.
                    subb      #$81                ; TAKE OUT BIAS +1.
                    negb                          ; MAKE IT NEGATIVE.
                    addb      #23                 ; ADD IN THE NUMBER OF MANTISSA BITS -1.
                    bra       _7@@                ; GO CHECK TO SEE IF WE NEED TO SHIFT AT ALL.

_6@@                lsr       FPACC1MN            ; SHIFT MANTISSA TO THE RIGHT BY THE RESULT (MAKE
                    ror       FPACC1MN+1          ; THE NUMBER AN INTEGER).
                    ror       FPACC1MN+2
                    decb                          ; DONE SHIFTING?
_7@@                bne       _6@@                ; NO. KEEP GOING.
                    lda       #1                  ; GET INITIAL VALUE OF "DIGITS AFTER D.P." COUNT.
                    sta       3,y                 ; INITIALIZE IT.
                    lda       2,y                 ; GET DECIMAL EXPONENT.
                    adda      #8                  ; ADD THE NUMBER OF DECIMAL +1 TO THE EXPONENT.
                                                  ; WAS THE ORIGINAL NUMBER > 9999999?
                    bmi       _8@@                ; YES. MUST BE REPRESENTED IN SCIENTIFIC NOTATION.
                    cmpa      #8                  ; WAS THE ORIGINAL NUMBER < 1?
                    bhs       _8@@                ; YES. MUST BE REPRESENTED IN SCIENTIFIC NOTATION.
                    deca                          ; NO. NUMBER CAN BE REPRESENTED IN 7 DIGITS.
                    sta       3,y                 ; MAKE THE DECIMAL EXPONENT THE DIGIT COUNT BEFORE
                                                  ; THE DECIMAL POINT.
                    lda       #2                  ; SETUP TO ZERO THE DECIMAL EXPONENT.
_8@@                suba      #2                  ; SUBTRACT 2 FROM THE DECIMAL EXPONENT.
                    sta       2,y                 ; SAVE THE DECIMAL EXPONENT.
                    tst       3,y                 ; DOES THE NUMBER HAVE AN INTEGER PART? (EXP. >0)
                    bgt       _10@@               ; YES. GO PUT IT OUT.9
                    lda       #'.'                ; NO. GET DECIMAL POINT.
                    ldx       ,y                  ; GET POINTER TO BUFFER.
                    sta       ,x                  ; PUT THE DECIMAL POINT IN THE BUFFER.
                    inx                           ; POINT TO NEXT BUFFER LOCATION.
                    tst       3,y                 ; IS THE DIGIT COUNT TILL EXPONENT =0?
                    beq       _9@@                ; NO. NUMBER IS <.1
                    lda       #'0'                ; YES. FORMAT NUMBER AS .0XXXXXXX
                    sta       ,x                  ; PUT THE 0 IN THE BUFFER.
                    inx                           ; POINT TO THE NEXT LOCATION.
_9@@                stx       ,y                  ; SAVE NEW POINTER VALUE.
_10@@               ldx       #DECDIG             ; POINT TO THE TABLE OF DECIMAL DIGITS.
                    lda       #7                  ; INITIALIZE THE THE NUMBER OF DIGITS COUNT.
                    sta       5,y
_11@@               clr       4,y                 ; CLEAR THE DECIMAL DIGIT ACCUMULATOR.
_12@@               ldd       FPACC1MN+1          ; GET LOWER 16 BITS OF MANTISSA.
                    subd      1,x                 ; SUBTRACT LOWER 16 BITS OF CONSTANT.
                    std       FPACC1MN+1          ; SAVE RESULT.
                    lda       FPACC1MN            ; GET UPPER 8 BITS.
                    sbca      ,x                  ; SUBTRACT UPPER 8 BITS.
                    sta       FPACC1MN            ; SAVE RESULT. UNDERFLOW?
                    bcs       _13@@               ; YES. GO ADD DECIMAL NUMBER BACK IN.
                    inc       4,y                 ; ADD 1 TO DECIMAL NUMBER.
                    bra       _12@@               ; TRY ANOTHER SUBTRACTION.

_13@@               ldd       FPACC1MN+1          ; GET FPACC1 MANTISSA LOW 16 BITS.
                    addd      1,x                 ; ADD LOW 16 BITS BACK IN.
                    std       FPACC1MN+1          ; SAVE THE RESULT.
                    lda       FPACC1MN            ; GET HIGH 8 BITS.
                    adca      ,x                  ; ADD IN HIGH 8 BITS OF CONSTANT.
                    sta       FPACC1MN            ; SAVE RESULT.
                    lda       4,y                 ; GET DIGIT.
                    adda      #'0'                ; MAKE IT ASCII.
                    pshx                          ; SAVE POINTER TO CONSTANTS.
                    ldx       ,y                  ; GET POINTER TO BUFFER.
                    sta       ,x                  ; PUT DIGIT IN BUFFER.
                    inx                           ; POINT TO NEXT BUFFER LOCATION.
                    dec       3,y                 ; SHOULD WE PUT A DECIMAL POINT IN THE BUFFER YET?
                    bne       _14@@               ; NO. CONTINUE THE CONVERSION.
                    lda       #'.'                ; YES. GET DECIMAL POINT.
                    sta       ,x                  ; PUT IT IN THE BUFFER.
                    inx                           ; POINT TO THE NEXT BUFFER LOCATION.
_14@@               stx       ,y                  ; SAVE UPDATED POINTER.
                    pulx                          ; RESTORE POINTER TO CONSTANTS.
                    inx:3                         ; POINT TO NEXT CONSTANT.
                    dec       5,y                 ; DONE YET?
                    bne       _11@@               ; NO. CONTINUE CONVERSION OF "MANTISSA".
                    ldx       ,y                  ; YES. POINT TO BUFFER STRING BUFFER.
_15@@               dex                           ; POINT TO LAST CHARACTER PUT IN THE BUFFER.
                    lda       ,x                  ; GET IT.
                    cmpa      #'0'                ; WAS IT AN ASCII 0?
                    beq       _15@@               ; YES. REMOVE TRAILING ZEROS.
                    inx                           ; POINT TO NEXT AVAILABLE LOCATION IN BUFFER.
                    ldb       2,y                 ; DO WE NEED TO PUT OUT AN EXPONENT?
                    beq       _17@@               ; NO. WE'RE DONE.
                    lda       #'E'                ; YES. PUT AN 'E' IN THE BUFFER.
                    sta       ,x
                    inx                           ; POINT TO NEXT BUFFER LOCATION.
                    lda       #'+'                ; ASSUME EXPONENT IS POSITIVE.
                    sta       ,x                  ; PUT PLUS SIGN IN THE BUFFER.
                    tstb                          ; IS IT REALLY MINUS?
                    bpl       _16@@               ; NO. IS'S OK AS IS.
                    negb                          ; YES. MAKE IT POSITIVE.
                    lda       #'-'                ; PUT THE MINUS SIGN IN THE BUFFER.
                    sta       ,x
_16@@               inx                           ; POINT TO NEXT BUFFER LOCATION.
                    stx       ,y                  ; SAVE POINTER TO STRING BUFFER.
                    clra                          ; SET UP FOR DIVIDE.
                    ldx       #10                 ; DIVIDE DECIMAL EXPONENT BY 10.
                    idiv
                    pshb                          ; SAVE REMAINDER.
                    xgdx                          ; PUT QUOTIENT IN D.
                    addb      #'0'                ; MAKE IT ASCII.
                    ldx       ,y                  ; GET POINTER.
                    stb       ,x                  ; PUT NUMBER IN BUFFER.
                    inx                           ; POINT TO NEXT LOCATION.
                    pulb                          ; GET SECOND DIGIT.
                    addb      #'0'                ; MAKE IT ASCII.
                    stb       ,x                  ; PUT IT IN THE BUFFER.
                    inx                           ; POINT TO NEXT LOCATION.
_17@@               clr       ,x                  ; TERMINATE STRING WITH A ZERO BYTE.
                    pulx:3                        ; CLEAR LOCALS FROM STACK.
                    jsr       PULFPAC2            ; RESTORE FPACC2.
                    pula
                    sta       MANTSGN1
                    pulx                          ; RESTORE FPACC1.
                    stx       FPACC1MN+1
                    pulx
                    stx       FPACC1EX
                    pulx                          ; POINT TO THE START OF THE ASCII STRING.
                    rts

?                   macro
                    fcb       ~1~>16,~1~>8&$FF,~1~&$FF
                    endm

DECDIG              @?        1000000
                    @?        100000
                    @?        10000
                    @?        1000
                    @?        100
                    @?        10
                    @?        1

P9999999            long      $947423FE           ; CONSTANT 999999.9
N9999999            long      $9818967F           ; CONSTANT 9999999.
CONSTP5             long      $80000000           ; CONSTANT .5

FLTCMP              proc
                    tst       MANTSGN1            ; IS FPACC1 NEGATIVE?
                    bpl       _1@@                ; NO. CONTINUE WITH COMPARE.
                    tst       MANTSGN2            ; IS FPACC2 NEGATIVE?
                    bpl       _1@@                ; NO. CONTINUE WITH COMPARE.
                    ldd       FPACC2EX            ; YES. BOTH ARE NEGATIVE SO COMPARE MUST BE DONE
                    cpd       FPACC1EX            ; BACKWARDS. ARE THEY EQUAL SO FAR?
                    bne       Done@@              ; NO. RETURN WITH CONDITION CODES SET.
                    ldd       FPACC2MN+1          ; YES. COMPARE LOWER 16 BITS OF MANTISSAS.
                    cpd       FPACC1MN+1
                    rts                           ; RETURN WITH CONDITION CODES SET.

_1@@                lda       MANTSGN1            ; GET FPACC1 MANTISSA SIGN.
                    cmpa      MANTSGN2            ; BOTH POSITIVE?
                    bne       Done@@              ; NO. RETURN WITH CONDITION CODES SET.
                    ldd       FPACC1EX            ; GET FPACC1 EXPONENT & UPPER 8 BITS OF MANTISSA.
                    cpd       FPACC2EX            ; SAME AS FPACC2?
                    bne       Done@@              ; NO. RETURN WITH CONDITION CODES SET.
                    ldd       FPACC1MN+1          ; GET FPACC1 LOWER 16 BITS OF MANTISSA.
                    cpd       FPACC2MN+1          ; COMPARE WITH FPACC2 LOWER 16 BITS OF MANTISSA.
Done@@              rts                           ; RETURN WITH CONDITION CODES SET.

; ttl INT2FLT
;*******************************************************************************
; UNSIGNED INTEGER TO FLOATING POINT
;
; This subroutine performs "unsigned" integer to floating point
; conversion of a 16 bit word. The 16 bit integer must be in the
; lower 16 bits of FPACC1 mantissa. The resulting floating point
; number is returned in FPACC1.
;*******************************************************************************

UINT2FLT            proc
                    ldx       #FPACC1EX           ; POINT TO FPACC1.
                    clr       ,x                  ; clear the upper 8-bits of the mantissa Added 12/17/91 G.S.D.
                    jsr       CHCK0               ; IS IT ALREADY 0?
                    beq       Done@@              ; YES. JUST RETURN.
                                                  ; CONVERT
                    lda       #$98                ; GET BIAS + NUMBER OF BITS IN MANTISSA.
                    sta       FPACC1EX            ; INITIALIZE THE EXPONENT.
                    jsr       FPNORM              ; GO MAKE IT A NORMALIZED FLOATING POINT VALUE.
                    clc                           ; NO ERRORS.
Done@@              rts

;*******************************************************************************
; SIGNED INTEGER TO FLOATING POINT
;
; This routine works just like the unsigned integer to floating
; point routine except the the 16 bit integer in the FPACC1
; mantissa is considered to be in two's complement format.  This
; will return a floating point number in the range -32768 to +32767.
;*******************************************************************************

SINT2FLT            proc
                    clr       MANTSGN1            ; initialize the FPACC1 mantissa sign to zero (positive) Added 12/17/91 G.S.D.
                    clr       FPACC1MN            ; Clear the upper 8-bits of the FPACC1 mantissa. Added 12/17/91 G.S.D.
                    ldd       FPACC1MN+1          ; GET THE LOWER 16 BITS OF FPACC1 MANTISSA.
                    psha                          ; SAVE SIGN OF NUMBER.
                    bpl       _1@@                ; IF POSITIVE JUST GO CONVERT.
                    negd                          ; MAKE IT POSITIVE.
                    std       FPACC1MN+1          ; PUT IT BACK IN FPACC1 MANTISSA.
_1@@                bsr       UINT2FLT            ; GO CONVERT.
                    pula                          ; GET SIGN OF ORIGINAL INTEGER.
                    ldb       #$FF                ; GET "MINUS SIGN".
                    tsta                          ; WAS THE NUMBER NEGATIVE?
                    bpl       _2@@                ; NO. RETURN.
                    stb       MANTSGN1            ; YES. SET FPACC1 SIGN BYTE.
_2@@                clc                           ; NO ERRORS.
                    rts

; ttl FLT2INT
;*******************************************************************************
; FLOATING POINT TO INTEGER CONVERSION
;
; This subroutine will perform "unsigned" floating point to integer
; conversion. The floating point number if positive, will be
; converted to an unsigned 16 bit integer ( 0 <= X <= 65535 ). If
; the number is negative it will be converted to a twos complement
; 16 bit integer. This type of conversion will allow 16 bit
; addresses to be represented as positive numbers when in floating
; point format. Any fractional number part is disguarded
;*******************************************************************************

FLT2INT             proc
                    ldx       #FPACC1EX           ; POINT TO FPACC1.
                    jsr       CHCK0               ; IS IT 0?
                    beq       Done@@              ; YES. JUST RETURN.
                    ldb       FPACC1EX            ; GET FPACC1 EXPONENT.
                    cmpb      #$81                ; IS THERE AN INTEGER PART?
                    blo       _3@@                ; NO. GO PUT A 0 IN FPACC1.
                    tst       MANTSGN1            ; IS THE NUMBER NEGATIVE?
                    bmi       _1@@                ; YES. GO CONVERT NEGATIVE NUMBER.
                    cmpb      #$90                ; IS THE NUMBER TOO LARGE TO BE MADE AN INTEGER?
                    bhi       _2@@                ; YES. RETURN WITH AN ERROR.
                    subb      #$98                ; SUBTRACT THE BIAS PLUS THE NUMBER OF BITS.
Loop@@              lsr       FPACC1MN            ; MAKE THE NUMBER AN INTEGER.
                    ror       FPACC1MN+1
                    ror       FPACC1MN+2
                    incb                          ; DONE SHIFTING?
                    bne       Loop@@              ; NO. KEEP GOING.
                    clr       FPACC1EX            ; ZERO THE EXPONENT (ALSO CLEARS THE CARRY).
                    rts

_1@@                cmpb      #$8F                ; IS THE NUMBER TOO SMALL TO BE MADE AN INTEGER?
                    bhi       _2@@                ; YES. RETURN ERROR.
                    subb      #$98                ; SUBTRACT BIAS PLUS NUMBER OF BITS.
                    bsr       Loop@@              ; GO DO SHIFT.
                    ldd       FPACC1MN+1          ; GET RESULTING INTEGER.
                    negd                          ; MAKE IT NEGATIVE.
                    std       FPACC1MN+1          ; SAVE RESULT.
                    clr       MANTSGN1            ; CLEAR MANTISSA SIGN. (ALSO CLEARS THE CARRY)
                    rts

_2@@                lda       #TOLGSMER           ; NUMBER TOO LARGE OR TOO SMALL TO CONVERT TO INT.
                    sec                           ; FLAG ; ERROR.
                    rts

_3@@                clrd
                    std       FPACC1EX            ; ZERO FPACC1.
                    std       FPACC1MN+1          ; (ALSO CLEARS THE CARRY)
Done@@              rts

; ttl FLTSQR
;*******************************************************************************
; SQUARE ROOT SUBROUTINE
;
; This routine is used to calculate the square root of the floating
; point number in FPACC1. If the number in FPACC1 is negative an
; error is returned.
;*******************************************************************************

FLTSQR              proc
                    ldx       #FPACC1EX           ; POINT TO FPACC1.
                    jsr       CHCK0               ; IS IT ZERO?
                    beq       Done@@              ; YES. RETURN.

                    tst       MANTSGN1            ; IS THE NUMBER NEGATIVE?
                    bpl       _1@@                ; NO. GO TAKE ITS SQUARE ROOT.
                    lda       #NSQRTERR           ; YES. ERROR.
                    sec                           ; FLAG ; ERROR.
Done@@              rts

_1@@                jsr       PSHFPAC2            ; SAVE FPACC2.
                    lda       #4                  ; GET ITERATION LOOP COUNT.
                    psha                          ; SAVE IT ON THE STACK.
                    ldx       FPACC1MN+1          ; SAVE INITIAL NUMBER.
                    pshx
                    ldx       FPACC1EX
                    pshx
                    tsy                           ; POINT TO IT.
                    bsr       TFR1TO2             ; TRANSFER FPACC1 TO FPACC2.
                    lda       FPACC2EX            ; GET FPACC1 EXPONENT.
                    suba      #$80                ; REMOVE BIAS FROM EXPONENT.
                    inca                          ; COMPENSATE ; FOR ODD EXPONENTS (GIVES CLOSER GUESS)
                    bpl       _2@@                ; IF NUMBER >1 DIVIDE EXPONENT BY 2 & ADD BIAS.
                    lsra                          ; IF ; <1 JUST DIVIDE IT BY 2.
                    bra       _3@@                ; GO CALCULATE THE SQUARE ROOT.

_2@@                lsra                          ; DIVIDE ; EXPONENT BY 2.
                    adda      #$80                ; ADD BIAS BACK IN.
_3@@                sta       FPACC2EX            ; SAVE EXPONENT/2.
Loop@@              jsr       FLTDIV              ; DIVIDE THE ORIGINAL NUMBER BY THE GUESS.
                    jsr       FLTADD              ; ADD THE "GUESS" TO THE QUOTIENT.
                    dec       FPACC1EX            ; DIVIDE THE RESULT BY 2 TO PRODUCE A NEW GUESS.
                    bsr       TFR1TO2             ; PUT THE NEW GUESS INTO FPACC2.
                    ldd       ,y                  ; GET THE ORIGINAL NUMBER.
                    std       FPACC1EX            ; PUT IT BACK IN FPACC1.
                    ldd       2,y                 ; GET MANTISSA LOWER 16 BITS.
                    std       FPACC1MN+1
                    dec       4,y                 ; BEEN THROUGH THE LOOP 4 TIMES?
                    bne       Loop@@              ; NO. KEEP GOING.
                    ldd       FPACC2EX            ; THE FINAL GUESS IS THE ANSWER.
                    std       FPACC1EX            ; PUT IT IN FPACC1.
                    ldd       FPACC2MN+1
                    std       FPACC1MN+1
                    pulx:2                        ; GET RID OF ORIGINAL NUMBER.
                    ins                           ; GET RID OF LOOP COUNT VARIABLE.
                    jsr       PULFPAC2            ; RESTORE FPACC2.
                    clc                           ; NO ERRORS.
                    rts

;*******************************************************************************

TFR1TO2             proc
                    ldd       FPACC1EX            ; GET FPACC1 EXPONENT & HIGH 8 BIT OF MANTISSA.
                    std       FPACC2EX            ; PUT IT IN FPACC2.
                    ldd       FPACC1MN+1          ; GET FPACC1 LOW 16 BITS OF MANTISSA.
                    std       FPACC2MN+1          ; PUT IT IN FPACC2.
                    lda       MANTSGN1            ; TRANSFER THE SIGN.
                    sta       MANTSGN2
                    rts

; ttl FLTSIN
;*******************************************************************************
; FLOATING POINT SINE
;*******************************************************************************

FLTSIN              proc
                    jsr       PSHFPAC2            ; SAVE FPACC2 ON THE STACK.
                    jsr       ANGRED              ; GO REDUCE THE ANGLE TO BETWEEN +/-PI.
                    pshd                          ; SAVE THE QUAD COUNT.
                                                  ; SAVE THE SINE/COSINE FLAG.
                    jsr       DEG2RAD             ; CONVERT DEGREES TO RADIANS.
                    pula                          ; RESTORE THE SINE/COSINE FLAG.
                    bsr       SINCOS              ; GO GET THE SINE OF THE ANGLE.
                    pula                          ; RESTORE THE QUAD COUNT.
                    cmpa      #2                  ; WAS THE ANGLE IN QUADS 1 OR 2?
                    bls       FLTSIN2             ; YES. SIGN OF THE ANSWER IS OK.
                    com       MANTSGN1            ; NO. SINE IN QUADS 3 & 4 IS NEGATIVE.
FLTSIN2             clc                           ; SHOW NO ERRORS.
                    jsr       PULFPAC2            ; RESTORE FPACC2
                    rts

; ttl FLTCOS
;*******************************************************************************
; FLOATING POINT COSINE
;*******************************************************************************

FLTCOS              proc
                    jsr       PSHFPAC2            ; SAVE FPACC2 ON THE STACK.
                    jsr       ANGRED              ; GO REDUCE THE ANGLE TO BETWEEN +/-PI.
                    pshd                          ; SAVE THE QUAD COUNT.
                                                  ; SAVE THE SINE/COSINE FLAG.
                    jsr       DEG2RAD             ; CONVERT TO RADIANS.
                    pula                          ; RESTORE THE SINE/COSINE FLAG.
                    eora      #$01                ; COMPLEMENT 90'S COMPLEMENT FLAG FOR COSINE.
                    bsr       SINCOS              ; GO GET THE COSINE OF THE ANGLE.
                    pula                          ; RESTORE THE QUAD COUNT.
                    cmpa      #1                  ; WAS THE ORIGINAL ANGLE IN QUAD 1?
                    beq       _@@                 ; YES. SIGN IS OK.
                    cmpa      #4                  ; WAS IT IN QUAD 4?
                    beq       _@@                 ; YES. SIGN IS OK.
                    com       MANTSGN1            ; NO. COSINE IS NEGATIVE IN QUADS 2 & 3.
_@@                 bra       FLTSIN2             ; FLAG NO ERRORS, RESTORE FPACC2, & RETURN.

; ttl SINCOS
;*******************************************************************************
; FLOATING POINT SINE AND COSINE SUBROUTINE
;*******************************************************************************

SINCOS              proc
                    psha                          ; SAVE SINE/COSINE FLAG ON STACK.
                    ldx       FPACC1MN+1          ; SAVE THE VALUE OF THE ANGLE.
                    pshx
                    ldx       FPACC1EX
                    pshx
                    lda       MANTSGN1
                    psha
                    ldx       #SINFACT            ; POINT TO THE FACTORIAL TABLE.
                    pshx                          ; SAVE ; POINTER TO THE SINE FACTORIAL TABLE.
                    pshx                          ; JUST ; ALLOCATE ANOTHER LOCAL (VALUE NOT IMPORTANT)
                    lda       #$4                 ; GET INITIAL LOOP COUNT.
                    psha                          ; SAVE ; AS LOCAL ON STACK
                    tsy                           ; POINT TO LOCALS.
                    bsr       TFR1TO2             ; TRANSFER FPACC1 TO FPACC2.
                    jsr       FLTMUL              ; GET X^2 IN FPACC1.
                    tst       10,y                ; ARE WE DOING THE SINE?
                    beq       SINCOS7             ; YES. GO DO IT.
                    ldx       #COSFACT            ; NO. GET POINTER TO COSINE FACTORIAL TABLE.
                    stx       1,y                 ; SAVE IT.
                    bsr       TFR1TO2             ; COPY X^2 INTO FPACC2.
                    bra       SINCOS4             ; GENERATE EVEN POWERS OF "X" FOR COSINE.

SINCOS7             jsr       EXG1AND2            ; PUT X^2 IN FPACC2 & X IN FPACC1.
SINCOS1             jsr       FLTMUL              ; CREATE X^3,5,7,9 OR X^2,4,6,8.
SINCOS4             ldx       FPACC1MN+1          ; SAVE EACH ONE ON THE STACK.
                    pshx
                    ldx       FPACC1EX
                    pshx
                    lda       MANTSGN1
                    psha                          ; SAVE THE MANTISSA SIGN.
                    dec       ,y                  ; HAVE WE GENERATED ALL THE POWERS YET?
                    bne       SINCOS1             ; NO. GO DO SOME MORE.
                    lda       #4                  ; SET UP LOOP COUNT.
                    sta       ,y
                    tsx                           ; POINT TO POWERS ON THE STACK.
SINCOS2             stx       3,y                 ; SAVE THE POINTER.
                    ldx       1,y                 ; GET THE POINTER TO THE FACTORIAL CONSTANTS.
                    jsr       GETFPAC2            ; PUT THE NUMBER IN FPACC2.
                    inx:4                         ; POINT TO THE NEXT CONSTANT.
                    stx       1,y                 ; SAVE THE POINTER.
                    ldx       3,y                 ; GET POINTER TO POWERS.
                    lda       ,x                  ; GET NUMBER SIGN.
                    sta       MANTSGN1            ; PUT IN FPACC1 MANTISSA SIGN.
                    ldd       1,x                 ; GET LOWER 16-BITS OF THE MANTISSA.
                    std       FPACC1EX            ; PUT IN FPACC1 MANTISSA.
                    ldd       3,x                 ; GET HIGH 8 BITS OF THE MANTISSA & EXPONENT.
                    std       FPACC1MN+1          ; PUT IT IN FPACC1 EXPONENT & MANTISSA.
                    jsr       FLTMUL              ; MULTIPLY THE TWO.
                    ldx       3,y                 ; GET POINTER TO POWERS BACK.
                    ldd       FPACC1MN+1          ; SAVE RESULT WHERE THE POWER OF X WAS.
                    std       3,x
                    ldd       FPACC1EX
                    std       1,x
                    lda       MANTSGN1            ; SAVE SIGN.
                    sta       ,x
                    inx:5                         ; POINT TO THE NEXT POWER.
                    dec       ,y                  ; DONE?
                    bne       SINCOS2             ; NO. GO DO ANOTHER MULTIPLICATION.
                    lda       #3                  ; GET LOOP COUNT.
                    sta       ,y                  ; SAVE IT.
SINCOS3             ldx       3,y                 ; POINT TO RESULTS ON THE STACK.
                    dex:5                         ; POINT TO PREVIOUS RESULT.
                    stx       3,y                 ; SAVE THE NEW POINTER.
                    lda       ,x                  ; GET NUMBERS SIGN.
                    sta       MANTSGN2            ; PUT IT IN FPACC2.
                    ldd       1,x                 ; GET LOW 16 BITS OF THE MANTISSA.
                    std       FPACC2EX            ; PUT IN FPACC2.
                    ldd       3,x                 ; GET HIGH 8 BIT & EXPONENT.
                    std       FPACC2MN+1          ; PUT IN FPACC2.
                    jsr       FLTADD              ; GO ADD THE TWO NUMBERS.
                    dec       ,y                  ; DONE?
                    bne       SINCOS3             ; NO. GO ADD THE NEXT TERM IN.
                    tst       10,y                ; ARE WE DOING THE SINE?
                    beq       SINCOS5             ; YES. GO PUT THE ORIGINAL ANGLE INTO FPACC2.
                    ldx       #ONE                ; NO. FOR COSINE PUT THE CONSTANT 1 INTO FPACC2.
                    jsr       GETFPAC2
                    bra       SINCOS6             ; GO ADD IT TO THE SUM OF THE TERMS.

SINCOS5             lda       5,y                 ; GET THE VALUE OF THE ORIGINAL ANGLE.
                    sta       MANTSGN2            ; PUT IT IN FPACC2.
                    ldd       6,y
                    std       FPACC2EX
                    ldd       8,y
                    std       FPACC2MN+1
SINCOS6             jsr       FLTADD              ; GO ADD IT TO THE SUM OF THE TERMS.
                    givex     #31                 ; CLEAR ALL THE TERMS & TEMPS OFF THE STACK.
                    rts

;*******************************************************************************

ANGRED              proc
                    clra                          ; INITIALIZE THE 45'S COMPLEMENT FLAG.
                    psha                          ; PUT IT ON THE STACK.
                    inca                          ; INITIALIZE THE QUAD COUNT TO 1.
                    psha                          ; PUT IT ON THE STACK.
                    tsy                           ; POINT TO IT.
                    ldx       #THREE60            ; POINT TO THE CONSTANT 360.
                    jsr       GETFPAC2            ; GET IT INTO FPACC.
                    tst       MANTSGN1            ; IS THE INPUT ANGLE NEGATIVE:
                    bpl       ANGRED1             ; NO. SKIP THE ADD.
                    jsr       FLTADD              ; YES. MAKE THE ANGLE POSITIVE BY ADDING 360 DEG.
ANGRED1             dec       FPACC2EX            ; MAKE THE CONSTANT IN FPACC2 90 DEGREES.
                    dec       FPACC2EX
ANGRED2             jsr       FLTCMP              ; IS THE ANGLE LESS THAN 90 DEGREES ALREADY?
                    bls       ANGRED3             ; YES. RETURN WITH QUAD COUNT.
                    jsr       FLTSUB              ; NO. REDUCE ANGLE BY 90 DEGREES.
                    inc       ,y                  ; INCREMENT THE QUAD COUNT.
                    bra       ANGRED2             ; GO SEE IF IT'S LESS THAN 90 NOW.

ANGRED3             lda       ,y                  ; GET THE QUAD COUNT.
                    cmpa      #1                  ; WAS THE ORIGINAL ANGLE IN QUAD 1?
                    beq       ANGRED4             ; YES. COMPUTE TRIG FUNCTION AS IS.
                    cmpa      #3                  ; NO. WAS THE ORIGINAL ANGLE IN QUAD 3?
                    beq       ANGRED4             ; YES. COMPUTE THE TRIG FUNCTION AS IF IN QUAD 1.
                    lda       #$FF                ; NO. MUST COMPUTE THE TRIG FUNCTION OF THE 90'S
                    sta       MANTSGN1            ; COMPLEMENT ANGLE.
                    jsr       FLTADD              ; ADD 90 DEGREES TO THE NEGATED ANGLE.
ANGRED4             dec       FPACC2EX            ; MAKE THE ANGLE IN FPACC2 45 DEGREES.
                    jsr       FLTCMP              ; IS THE ANGLE < 45 DEGREES?
                    bls       ANGRED5             ; YES. IT'S OK AS IT IS.
                    inc       FPACC2EX            ; NO. MUST GET THE 90'S COMPLEMENT.
                    lda       #$FF                ; MAKE FPACC1 NEGATIVE.
                    sta       MANTSGN1
                    jsr       FLTADD              ; GET THE 90'S COMPLEMENT.
                    inc       1,y                 ; SET THE FLAG.
ANGRED5             puld                          ; GET THE QUAD COUNT.
                                                  ; GET THE COMPLEMENT FLAG.
                    rts                           ; RETURN WITH THE QUAD COUNT & COMPLEMENT FLAG.

EXG1AND2            ldd       FPACC1EX
                    ldx       FPACC2EX
                    std       FPACC2EX
                    stx       FPACC1EX
                    ldd       FPACC1MN+1
                    ldx       FPACC2MN+1
                    std       FPACC2MN+1
                    stx       FPACC1MN+1
                    lda       MANTSGN1
                    ldb       MANTSGN2
                    sta       MANTSGN2
                    stb       MANTSGN1
                    rts

SINFACT             long      $6E38EF1D           ; +(1/9!)
                    long      $74D00D01           ; -(1/7!)
                    long      $7A088889           ; +(1/5!)
                    long      $7EAAAAAB           ; -(1/3!)

COSFACT             long      $71500D01           ; +(1/8!)
                    long      $77B60B61           ; -(1/6!)
                    long      $7C2AAAAB           ; +(1/4!)
                    long      $80800000           ; -(1/2!)

ONE                 long      $81000000           ; 1.0
PI                  long      $82490FDB           ; 3.1415927
THREE60             long      $89340000           ; 360.0

; ttl FLTTAN
;*******************************************************************************
; FLOATING POINT TANGENT
;*******************************************************************************

FLTTAN              proc
                    bsr       PSHFPAC2            ; SAVE FPACC2 ON THE STACK.
                    jsr       TFR1TO2             ; PUT A COPY OF THE ANGLE IN FPACC2.
                    jsr       FLTCOS              ; GET COSINE OF THE ANGLE.
                    bsr       EXG1AND2            ; PUT RESULT IN FPACC2 & PUT ANGLE IN FPACC1.
                    jsr       FLTSIN              ; GET SIN OF THE ANGLE.
                    jsr       FLTDIV              ; GET TANGENT OF ANGLE BY DOING SIN/COS.
                    bcc       _@@                 ; IF CARRY CLEAR, ANSWER OK.
                    ldx       #MAXNUM             ; TANGENT OF 90 WAS ATTEMPTED. PUT LARGEST
                    bsr       GETFPAC1            ; NUMBER IN FPACC1.
                    lda       #TAN90ERR           ; GET ERROR CODE IN A.
_@@                 bsr       PULFPAC2            ; RESTORE FPACC2.
                    rts

MAXNUM              long      $FE7FFFFF           ; LARGEST POSITIVE NUMBER WE CAN HAVE.

; ttl TRIGUTIL
;*******************************************************************************
; TRIG UTILITIES
;
; The routines "DEG2RAD" and "RAD2DEG" are used to convert angles
; from degrees-to-radians and radians-to-degrees respectively. The
; routine "GETPI" will place the value of PI into FPACC1. This
; routine should be used if the value of PI is needed in calculations
; since it is accurate to the full 24-bits of the mantissa.
;*******************************************************************************

DEG2RAD             proc
                    bsr       PSHFPAC2            ; SAVE FPACC2.
                    ldx       #PIOV180            ; POINT TO CONVERSION CONSTANT PI/180.
DEG2RAD1            bsr       GETFPAC2            ; PUT IT INTO FPACC2.
                    jsr       FLTMUL              ; CONVERT DEGREES TO RADIANS.
                    bsr       PULFPAC2            ; RESTORE FPACC2.
                    rts                           ; (NOTE! DON'T REPLACE THE "JSR/RTS" WITH
                                                  ; A "JMP"; IT WILL NOT WORK.)
;*******************************************************************************

RAD2DEG             proc
                    bsr       PSHFPAC2            ; SAVE FPACC2.
                    ldx       #C180OVPI           ; POINT TO CONVERSION CONSTANT 180/PI.
                    bra       DEG2RAD1            ; GO DO CONVERSION & RETURN.

;*******************************************************************************

GETPI               proc
                    ldx       #PI                 ; POINT TO CONSTANT "PI".
                    bra       GETFPAC1            ; PUT IT IN FPACC1 AND RETURN.

PIOV180             long      $7B0EFA35
C180OVPI            long      $86652EE1

; ttl PSHPULFPAC2
;*******************************************************************************
; The following two subroutines, PSHFPAC2 & PULPFAC2, push FPACC2
; onto and pull FPACC2 off of the hardware stack respectively.
; The number is stored in the "memory format".
;*******************************************************************************

PSHFPAC2            proc
                    pulx                          ; GET THE RETURN ADDRESS OFF OF THE STACK.
                    pshx:2                        ; ALLOCATE FOUR BYTES OF STACK SPACE.
                    xgdx                          ; PUT THE RETURN ADDRESS IN D.
                    tsx                           ; POINT TO THE STORAGE AREA.
                    pshd                          ; PUT THE RETURN ADDRESS BACK ON THE STACK.
                    bra       PUTFPAC2            ; GO PUT FPACC2 ON THE STACK & RETURN.

;*******************************************************************************

PULFPAC2            proc
                    tsx                           ; POINT TO THE RETURN ADDRESS.
                    inx:2                         ; POINT TO THE SAVED NUMBER.
                    bsr       GETFPAC2            ; RESTORE FPACC2.
                    pulx                          ; GET THE RETURN ADDRESS OFF THE STACK.
                    ins:4                         ; REMOVE THE NUMBER FROM THE STACK.
                    jmp       ,x

; ttl GETFPAC
;*******************************************************************************
; GETFPACx SUBROUTINE
;
; The GETFPAC1 and GETFPAC2 subroutines get a floating point number
; stored in memory and put it into either FPACC1 or FPACC2 in a format
; that is expected by all the floating point math routines. These
; routines may easily be replaced to convert any binary floating point
; format (i.e. IEEE format) to the format required by the math
; routines. The "memory" format converted by these routines is shown
; below:
;
; 31_______24 23 22_____________________0
; exponent s mantissa
;
; The exponent is biased by 128 to facilitate floating point
; comparisons. The sign bit is 0 for positive numbers and 1
; for negative numbers. The mantissa is stored in hidden bit
; normalized format so that 24 bits of precision can be obtained.
; Since a normalized floating point number always has its most
; significant bit set, we can use the 24th bit to hold the mantissa
; sign. This allows us to get 24 bits of precision in the mantissa
; and store the entire number in just 4 bytes. The format required by
; the math routines uses a seperate byte for the sign, therfore each
; floating point accumulator requires five bytes.
;*******************************************************************************

GETFPAC1            proc
                    ldd       ,x                  ; GET THE EXPONENT & HIGH BYTE OF THE MANTISSA,
                    beq       _2@@                ; IF NUMBER IS ZERO, SKIP SETTING THE MS BIT.
                    clr       MANTSGN1            ; SET UP FOR POSITIVE NUMBER.
                    tstb                          ; IS NUMBER NEGATIVE?
                    bpl       _1@@                ; NO. LEAVE SIGN ALONE.
                    com       MANTSGN1            ; YES. SET SIGN TO NEGATIVE.
_1@@                orb       #$80                ; RESTORE MOST SIGNIFICANT BIT IN MANTISSA.
_2@@                std       FPACC1EX            ; PUT IN FPACC1.
                    ldd       2,x                 ; GET LOW 16-BITS OF THE MANTISSA.
                    std       FPACC1MN+1          ; PUT IN FPACC1.
                    rts

;*******************************************************************************

GETFPAC2            proc
                    ldd       ,x                  ; GET THE EXPONENT & HIGH BYTE OF THE MANTISSA,
                    beq       _2@@                ; IF NUMBER IS 0, SKIP SETTING THE MS BIT.
                    clr       MANTSGN2            ; SET UP FOR POSITIVE NUMBER.
                    tstb                          ; IS NUMBER NEGATIVE?
                    bpl       _1@@                ; NO. LEAVE SIGN ALONE.
                    com       MANTSGN2            ; YES. SET SIGN TO NEGATIVE.
_1@@                orb       #$80                ; RESTORE MOST SIGNIFICANT BIT IN MANTISSA.
_2@@                std       FPACC2EX            ; PUT IN FPACC1.
                    ldd       2,x                 ; GET LOW 16-BITS OF THE MANTISSA.
                    std       FPACC2MN+1          ; PUT IN FPACC1.
                    rts

; ttl PUTFPAC
;*******************************************************************************
; PUTFPACx SUBROUTINE

; These two subroutines perform to opposite function of GETFPAC1 and
; GETFPAC2. Again, these routines are used to convert from the
; internal format used by the floating point package to a "memory"
; format. See the GETFPAC1 and GETFPAC2, documentation for a
; description of the "memory" format.
;*******************************************************************************

PUTFPAC1            proc
                    ldd       FPACC1EX            ; GET FPACC1 EXPONENT & UPPER 8 BITS OF MANT.
                    tst       MANTSGN1            ; IS THE NUMBER NEGATIVE?
                    bmi       _@@                 ; YES. LEAVE THE M.S. BIT SET.
                    andb      #$7F                ; NO. CLEAR THE M.S. BIT.
_@@                 std       ,x                  ; SAVE IT IN MEMORY.
                    ldd       FPACC1MN+1          ; GET L.S. 16 BITS OF THE MANTISSA.
                    std       2,x
                    rts

;*******************************************************************************

PUTFPAC2            proc
                    ldd       FPACC2EX            ; GET FPACC1 EXPONENT & UPPER 8 BITS OF MANT.
                    tst       MANTSGN2            ; IS THE NUMBER NEGATIVE?
                    bmi       _@@                 ; YES. LEAVE THE M.S. BIT SET.
                    andb      #$7F                ; NO. CLEAR THE M.S. BIT.
_@@                 std       ,x                  ; SAVE IT IN MEMORY.
                    ldd       FPACC2MN+1          ; GET L.S. 16 BITS OF THE MANTISSA.
                    std       2,x
                    rts
