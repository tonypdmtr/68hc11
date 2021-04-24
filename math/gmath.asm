;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************

;*******************************************************************************
;*                        GMATH.ASM
;*
;*                      Copyright 1988
;*                            by
;*                      James C. Shultz
;*
;* The source code for this General Math package for the
;* MC68HC11 may be freely distributed under the rules of
;* public domain.
;*
;* If there are any questions or comments about this General
;* Math package please feel free to contact me.
;*
;*                      James C. Shultz
;*                      J & J Electronic Industries, Inc.
;*                      526 North Main Street
;*                      Glen Ellyn, IL  60137
;*                      (312) 858-2999
;*
;*
;*               REVISION HISTORY:
;*       1.0     12-30-88        Release to public domain of a
;*                               general math package including
;*                               the following:
;*                               HEXBIN - ASCII hex to binary
;*                                        convertion
;*                               UPCASE - Convert Lower case
;*                                        to Upper case
;*                               BTOA - Binary to ASCII
;*                               BTOD - Binary to Decimal
;*                               DTOB - Decimal to Binary
;*                               WMUL - 24 bit by 16 bit multiply
;*                                      with a 40 bit result
;*                               XMUL - 16 bit by 16 bit multiply
;*                                      with a 32 bit result
;*                               ZMUL - 24 bit by 8 bit multiply
;*                                      with a 32 bit result
;*       1.1     MM-DD-YY

;*******************************************************************************
                    #RAM      *
;*******************************************************************************

SHFTRGH             rmb       1                   ; INPUT SHIFT REGISTER HIGH
SHFTRG              rmb       2                   ; INPUT SHIFT REGISTER
TMP1                rmb       1                   ; USED BY HEXBIN
TEMP                rmb       1                   ; USED BY BTOD
TEMP0               rmb       1                   ; USED BY DTOB
TEMP1               rmb       1                   ; USED BY DTOB
TEMP2               rmb       1                   ; USED BY DTOB
TEMP3               rmb       1                   ; USED BY DTOB
VALUE               rmb       8                   ; SPI DISPLAY VALUE
PROD0               rmb       1                   ; WMUL PRODUCT
PROD1               rmb       1                   ; XMUL PRODUCT
PROD2               rmb       1                   ; XMUL PRODUCT
PROD3               rmb       1                   ; XMUL PRODUCT
PROD4               rmb       1                   ; XMUL PRODUCT
FACTA               rmb       2                   ; XMUL FACTOR
FACTB               rmb       2                   ; XMUL FACTOR

;*******************************************************************************
                    #ROM
;*******************************************************************************
#ifmain
;***************************************************************
;*                     INIT & MAIN PROGRAM
;***************************************************************
START
;* .     .       .               ;......
;*       .       .               ;......
;*       .       .               ;......
;*etc. etc

MAIN
;* .     .       .               ;......
;*       .       .               ;......
;*       .       .               ;......
;*etc. etc
                    bra       MAIN                ; AND ANOTHER LOOP
#endif

;*******************************************************************************
;*                     SUBROUTINES
;*******************************************************************************

;*******************************************************************************
;* Start  MATH related subroutines
;*********
;*********
;* HEXBIN(A) - CONVERT THE ASCII CHAR IN (ACCA) TO BINARY
;*             Place result in ACCA & SHFTRG+1 (lo-byte)
;*             Increments TMP1 if ACCA does not have a valid
;*             hex character
;*********

HEXBIN              proc
                    pshd
                    pshx

                    bsr       UPCASE              ; CONVERT TO UPPER CASE

                    cmpa      #'0'
                    blo       Error@@             ; JUMP IF A < $30

                    cmpa      #'9'
                    bls       Number@@            ; JUMP IF 0-9

                    cmpa      #'A'
                    blo       Error@@             ; JUMP ID $39 > A < $41

                    cmpa      #'F'
                    bhi       Error@@             ; JUMP IF A > $46

                    adda      #10-'A'+'0'         ; CONVERT $A-$F

Number@@            suba      #'0'                ; CONVERT TO BINARY

                    ldx       #SHFTRGH
                    ldb       #4
Mult@@              asl       2,x                 ; 3 BYTE SHIFT THROUGH
                    rol       1,x                 ; CARRY BIT
                    rol       ,x                  ; CARRY BIT
                    decb
                    bgt       Mult@@              ; SHIFT 4 TIMES
                    ora       2,x                 ; GET HIGH 4 BITS
                    sta       2,x                 ; WRITE NEW LOW BYTE
                    bra       Done@@

Error@@             inc       TMP1                ; INDICATE NOT HEX

Done@@              pulx
                    puld
                    rts

;*******************************************************************************
;* UPCASE() - RETURNS ACCA CONVERTED TO UPPERCASE
;*********

UPCASE              proc
                    cmpa      #'a'
                    blo       Done@@              ; JUMP IF < a
                    cmpa      #'z'
                    bhi       Done@@              ; JUMP IF > z
                    adda      #'A'-'a'            ; CONVERT
Done@@              rts

;*******************************************************************************
;* BTOA(A) - CONVERT THE BINARY CHAR IN ACCA TO ASCII
;*           Place result at Index X (upper 4 bits) and
;*           X+1 (lower 4 bits)
;*********

BTOA                proc
                    psha                          ; SAVE ACCA

                    psha
                    bsr       BTOAL               ; CONVERT UPPER 4 BITS
                    sta       ,x                  ; STORE IT AT X
                    pula                          ; RESTORE ACCA
                    bsr       BTOAR               ; CONVERT LOWER 4 BITS
                    sta       1,x                 ; STORE IT AT X+1

                    pula                          ; RESTORE ACCA
                    rts

;*******************************************************************************
;* BTOAL(A) - CONVERT THE LEFT 4 BITS IN A TO ASCII HEX
;*********

BTOAL               proc
                    lsra:4                        ; SHIFT RIGHT 4 TIMES

;*******************************************************************************
;* BTOAR(A) - CONVERT THE RIGHT 4 BITS IN A TO ASCII HEX
;*********

BTOAR               proc
                    anda      #$0F                ; MASK FOR LOWER 4 BITS
                    adda      #$90
                    daa
                    adca      #$40
                    daa
                    rts

;*******************************************************************************
;* BTOD() - BINARY TO DECIMAL CONVERTER
;*          Input value is in SHFTRGH (hi-byte),
;*          SHFTRG (middle-byte), and SHFTRG+1 (lo-byte)
;*          The result is in VALUE thru VALUE+8
;*********

BTOD                proc
                    push

                    ldx       #PWRTBL             ; INIT POWER TABLE PTR
                    ldy       #VALUE              ; RESULT AREA
                    clrd                          ; GET ZERO
                    std       ,y                  ; CLEAR DIGIT
                    std       2,Y                 ; CLEAR DIGIT
                    std       4,Y                 ; CLEAR DIGIT
                    std       6,Y                 ; CLEAR DIGIT
Loop@@              lda       SHFTRG+1            ; GET LO BYTE
                    suba      2,x                 ; SUB LO BYTE OF 10
                    sta       TEMP                ; SAVE NEW LO BYTE
                    ldd       SHFTRGH             ; LOAD HI BYTE
                    bcc       Skip@@              ; CK IF BORROW
                    decd                          ; GET FROM HI BYTE
                    bcs       Cont@@              ; NO BORROW GOTO NEXT
Skip@@              subd      ,x                  ; SUB HI BYTE OF 10
                    bcs       Cont@@              ; IF FAIL GOTO NEXT
                    inc       ,y                  ; ELSE ADD ONE TO DISP
                    std       SHFTRGH             ; SAVE UPDATED HIBYTE
                    lda       TEMP                ; GET UPDATED LO BYTE
                    sta       SHFTRG+1            ; SAVE UPDATED LO BYTE
                    bra       Loop@@              ; DO IT AGAIN

Cont@@              inx:3                         ; NEXT LOWER PWR OF 10
                    iny                           ; NEXT LOWER DIGIT
                    cpx       #PWREND             ; CHECK FOR END OF TABLE
                    blo       Loop@@              ; BR IF NOT DONE
                    lda       SHFTRG+1            ; GET REMAINDER
                    sta       ,y                  ; PUT IN LAST DISPLAY DIGIT

                    pull                          ; RESTORE ACCD
                    rts

;*******************************************************************************
;* DTOB() - DECIMAL TO BINARY CONVERTER
;*          Index X points to Decimal string
;*          Results will be in SHFTRGH (hi-byte),
;*          SHFTRG (middle-byte), and SHFTRG+1 (lo-byte)
;*********

DTOB                proc
                    pshd                          ; SAVE ACCD
                    clrd                          ; CLEAR ACCUMULATOR
                    sta       TEMP                ; CLEAR TEMP
                    sta       TEMP0               ; CLEAR TEMP0
                    sta       TEMP2               ; CLEAR TEMP2
                    sta       SHFTRGH             ; CLEAR BINARY HIGH BYTE
                    std       SHFTRG              ; CLEAR BINARY LOW WORD
Loop@@              lda       ,x                  ; LOAD COMPARE VALUE
                    suba      #'0'                ; SUB ASCII OFFSET
                    cmpa      #9                  ; CHECK IF 0 - 9
                    bhi       Done@@              ; END IF NOT 0 - 9
                    sta       TEMP1               ; SAVE IT
                    ldd       SHFTRG              ; GET LOW WORD
                    lsld                          ; MULTIPLY BYTE BY 2
                    rol       TEMP2               ; MULTIPLY HI BYTE BY 2
                    lsld                          ; MULTIPLY BYTE BY 2
                    rol       TEMP2               ; MULTIPLY HI BYTE BY 2
                    addd      SHFTRG              ; ADD
                    bcc       _1@@                ; CHECK FOR CARRY
                    inc       TEMP2               ; DO CARRY
_1@@                sta       TEMP3               ; SAVE ACCA
                    lda       TEMP2               ; GET HIGH
                    adda      SHFTRGH             ; ADD
                    sta       TEMP2               ; SAVE HIGH
                    lda       TEMP3               ; RESTORE ACCA
                    lsld                          ; MULTIPLY BYTE BY 2
                    rol       TEMP2               ; MULTIPLY HI BYTE BY 2
                    addd      TEMP0               ; ADD TEMP
                    bcc       _2@@                ; CHECK FOR CARRY
                    inc       TEMP2               ; DO CARRY
_2@@                std       SHFTRG              ; SAVE
                    lda       TEMP2               ; GET HIGH
                    sta       SHFTRGH             ; SAVE
                    inx                           ; NEXT
                    bra       Loop@@              ; DO NEXT

Done@@              puld                          ; RESTORE ACCD
                    rts                           ; RETURN

;*******************************************************************************
;* WMUL() - 24 BIT BY 16 BIT MULTIPLY, PRODUCT = 40 BITS
;*          Input - the 16 bits are in FACTA (hi-byte) &
;*          FACTA+1 (lo-byte),with the 24 bits being pointed
;*          to by Index X with the low byte being X+2
;*          The result is in PROD0 thru PROD4 with the low
;*          byte being in PROD4
;*********

WMUL                proc
                    pshd                          ; SAVE ACCD
                    clrd                          ; GET ZERO
                    sta       PROD0               ; CLEAR
                    std       PROD1               ; CLEAR PRODUCT
                    lda       FACTA+1             ; GET LOW BYTE
                    ldb       2,x                 ; GET LOW BYTE
                    mul                           ; MULTIPLY
                    std       PROD3               ; SAVE
                    lda       FACTA+1             ; GET LOW BYTE
                    ldb       1,x                 ; GET HIGH BYTE
                    mul                           ; MULTIPLY
                    addd      PROD2               ; SUM INTO PRODUCT
                    std       PROD2               ; SAVE
                    bcc       _1@@                ; CHECK FOR CARRY
                    inc       PROD1               ; DO CARRY
_1@@                lda       FACTA               ; GET HIGH BYTE
                    ldb       2,x                 ; GET LOW BYTE
                    mul                           ; MULTIPLY
                    addd      PROD2               ; SUM INTO PRODUCT
                    std       PROD2               ; SAVE
                    bcc       _2@@                ; CHECK FOR CARRY
                    inc       PROD1               ; DO CARRY
_2@@                lda       FACTA               ; GET HIGH BYTE
                    ldb       1,x                 ; GET HIGH BYTE
                    mul                           ; MULTIPLY
                    addd      PROD1               ; SUM INTO PRODUCT
                    std       PROD1               ; SAVE
                    lda       FACTA+1             ; GET HIGH BYTE
                    ldb       ,x                  ; GET LOW BYTE
                    mul                           ; MULTIPLY
                    addd      PROD1               ; SUM INTO PRODUCT
                    std       PROD1               ; SAVE
                    bcc       _3@@                ; CHECK FOR CARRY
                    inc       PROD0               ; DO CARRY
_3@@                lda       FACTA               ; GET HIGH BYTE
                    ldb       ,x                  ; GET HIGH BYTE
                    mul                           ; MULTIPLY
                    addd      PROD0               ; SUM INTO PRODUCT
                    std       PROD0               ; SAVE
                    puld                          ; RESTORE ACCD
                    rts

;*******************************************************************************
;* XMUL() - 16 BIT BY 16 BIT MULTIPLY, PRODUCT = 32 BITS
;*          Input -  16 bits are in FACTA (hi-byte) &
;*          FACTA+1 (lo-byte),with the other 16 bits in
;*          FACTB (hi-byte) & FACTB+1 (lo-byte)
;*          The result is in PROD1 thru PROD4 with the low
;*          byte being in PROD4
;*********

XMUL                proc
                    pshd                          ; SAVE ACCD
                    clrd                          ; GET ZERO
                    std       PROD1               ; CLEAR PRODUCT
                    lda       FACTA+1             ; GET LOW BYTE
                    ldb       FACTB+1             ; GET LOW BYTE
                    mul                           ; MULTIPLY
                    std       PROD3               ; SAVE
                    lda       FACTA+1             ; GET LOW BYTE
                    ldb       FACTB               ; GET HIGH BYTE
                    mul                           ; MULTIPLY
                    addd      PROD2               ; SUM INTO PRODUCT
                    std       PROD2               ; SAVE
                    bcc       Skip@@              ; CHECK FOR CARRY
                    inc       PROD1               ; DO CARRY
Skip@@              lda       FACTA               ; GET HIGH BYTE
                    ldb       FACTB+1             ; GET LOW BYTE
                    mul                           ; MULTIPLY
                    addd      PROD2               ; SUM INTO PRODUCT
                    std       PROD2               ; SAVE
                    bcc       Cont@@              ; CHECK FOR CARRY
                    inc       PROD1               ; DO CARRY
Cont@@              lda       FACTA               ; GET HIGH BYTE
                    ldb       FACTB               ; GET HIGH BYTE
                    mul                           ; MULTIPLY
                    addd      PROD1               ; SUM INTO PRODUCT
                    std       PROD1               ; SAVE
                    puld                          ; RESTORE ACCD
                    rts

;*******************************************************************************
;* ZMUL() - 24 BIT BY 8 BIT MULTIPLY, PRODUCT = 32 BITS
;*          Input - the 8 bits are in FACTA and the 24
;*          bits are being pointed to by Index X with the
;*          low byte being X+2
;*          The result is in PROD1 thru PROD4 with the low
;*          byte being in PROD4
;*********

ZMUL                proc
                    pshd                          ; SAVE ACCD
                    clrd                          ; GET ZERO
                    std       PROD1               ; CLEAR PRODUCT
                    lda       FACTA               ; GET LOW BYTE
                    ldb       2,x                 ; GET LOW BYTE
                    mul                           ; MULTIPLY
                    std       PROD3               ; SAVE
                    lda       FACTA               ; GET LOW BYTE
                    ldb       1,x                 ; GET MIDDLE BYTE
                    mul                           ; MULTIPLY
                    addd      PROD2               ; SUM INTO PRODUCT
                    std       PROD2               ; SAVE
                    bcc       Skip@@              ; CHECK FOR CARRY
                    inc       PROD1               ; DO CARRY
Skip@@              lda       FACTA               ; GET HIGH BYTE
                    ldb       ,x                  ; GET HI BYTE
                    mul                           ; MULTIPLY
                    addd      PROD1               ; SUM INTO PRODUCT
                    std       PROD1               ; SAVE
                    puld                          ; RESTORE ACCD
                    rts

;*******************************************************************************
;* End MATH related subroutines
;*********

?                   macro
                    fcb       ~1~>16
                    dw        ~1~&$FFFF
                    endm

PWRTBL              @?        10000000
                    @?        1000000
                    @?        100000
                    @?        10000
                    @?        1000
                    @?        100
                    @?        10
                    @?        1
PWREND
                    end       :s19crc
