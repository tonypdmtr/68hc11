;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************

;*******************************************************************************
; CRC_V41 -- Encodes and decodes data blocks using CCITT V.41 CRC.
;===============================================================================
;
; David Evans, September 1999.                 Version No:`LOST_COUNT'
;
; This software subroutine encodes and decodes data frames using a
; CRC that conforms to the CCITT V.41 recommendation. The software has
; been written in a manner that allows it to be easily and quickly
; tailored to your 68HC11 system.  A comprehensive guide to using this
; software can be found at the web-site this software was downloaded
; from.
;
; Prior to installing this subroutine, ensure that the <ORG $E000>
; command on line 59 is removed.
;
; This software has been rigorously tested.  However, although all
; efforts have been made to ensure that this software contains no bugs
; or errors, the author DOES NOT assume liability arising out of the
; application or use of any of the information presented herein.
;
; The author appreciates feedback -- Please feel free to forward any
; complaints, suggestions, corrections or comments to:
;
; arkaydia@hotmail.com
;*******************************************************************************

CRC_DIR             equ       $07
CRC_RET             equ       $08
TEMPOR1             equ       $09
XOR4RX1             equ       $20
XOR4RX2             equ       $21
XOR4RX3             equ       $22
XOR4TX1             equ       $23
XOR4TX2             equ       $24
XOR4TX3             equ       $25
TX_DIVIDEND1        equ       $33
TX_DIVIDEND2        equ       $34
TX_DIVIDEND3        equ       $35
RX_DIVIDEND1        equ       $36
RX_DIVIDEND2        equ       $37
RX_DIVIDEND3        equ       $38
IN_FOR_COUNT        equ       $29
OUT_FOR_COUNT       equ       $31
GEN_POLY1           equ       $88
GEN_POLY2           equ       $10
GEN_POLY3           equ       $80
FINAL_CHECK         equ       $30
TX_BUFF             equ       $50
RX_BUFF             equ       $B0
OUTER_COUNT         equ       31
CHKSUM1             equ       32
CHKSUM2             equ       33

                    #ROM      $E000

crc_V41             proc
                    push

                    lda       CRC_DIR             ; Fetch direction flag.
                    cmpa      #$01                ; If request is decode,
                    beq       CRC_DEC             ; jump to decoding algorithm.
                    lda       CRC_DIR             ; Fetch direction flag again.
                    jeq       CRC_ENC             ; If request is encode, jump to encoding algorithm.
                    lda       #$FF
                    sta       CRC_RET             ; Otherwise return with failure.

                    pull
                    rts

;*******************************************************************************
;*******************************************************************************
;
; Received data frames are DECODED using in this subroutine using CCITT
; V.41 Cyclic Redundancy Code recommendation.
;
;*******************************************************************************
;*******************************************************************************

CRC_DEC             proc
                    ldx       #RX_BUFF            ; Load RX buffer pointer.
                    lda       ,x                  ; Place 1st three bytes in DIVIDEND...
                    sta       RX_DIVIDEND1
                    sta       XOR4RX1
                    lda       $01,X
                    sta       RX_DIVIDEND2
                    sta       XOR4RX2
                    lda       $02,X
                    sta       RX_DIVIDEND3
                    anda      #$80                ; Remove all but the MSB.
                    sta       XOR4RX3
                    lda       #OUTER_COUNT        ; Initialise counter for the 'outer'
                    sta       OUT_FOR_COUNT       ; for loop.
                    lda       #$08                ; Initialise counter for the 'inner'
                    sta       IN_FOR_COUNT        ; for loop.
                    inx:2

OUT_4               clra
                    sta       FINAL_CHECK
RX_DIV              lda       #GEN_POLY1          ; Load accA with highest byte of G(x).
                    cmpa      XOR4RX1             ; Compare with highest XRes byte,
                    blo       A000                ; & if G(x)<Dn, then XOR with G(x).
                    lda       #GEN_POLY1
                    cmpa      XOR4RX1             ; Checking if G(x) > D(x).
                    bhi       A005
                    lda       #GEN_POLY2
                    cmpa      XOR4RX2
                    blo       A000
                    lda       #GEN_POLY2
                    cmpa      XOR4RX2
                    bhi       A005                ; Branch if G(x) < D(x).
                    lda       #GEN_POLY3
                    cmpa      XOR4RX3
                    bhi       A005
                    bra       A000                ; Branch if G(x) == D(x).
A005                jmp       XOR_0               ; Else, G(x)>Dn, so XOR with zero.
A000                bra       XOR_GX

NTRY2               lsl       RX_DIVIDEND1        ; NOW SHIFT THE DIVIDEND 1 BIT LEFT....
                    lda       RX_DIVIDEND2
                    anda      #$80
                    lsra:7                        ; msb -> lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       RX_DIVIDEND1
                    orb       TEMPOR1
                    stb       RX_DIVIDEND1
                    lsl       RX_DIVIDEND2
                    lda       RX_DIVIDEND3
                    anda      #$80
                    lsra:7                        ; msb -> lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       RX_DIVIDEND2
                    orb       TEMPOR1
                    stb       RX_DIVIDEND2
                    lsl       RX_DIVIDEND3

; Now shift the XOR result left 1 bit...
; *---------------------------------------
                    lsl       XOR4RX1             ; Shift the first byte to 1 bit left.
                    lda       XOR4RX1             ; Put shifted byte in accA,
                    ldb       XOR4RX2             ; & put the next byte in accB.
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1             ; Generic buffer.
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4RX1             ; Put new byte in 1st Xres space.
                    lsl       XOR4RX2             ; Shift 2nd XRes byte left 1 bit,
                    lda       XOR4RX2             ; & put it in accA.
                    ldb       XOR4RX3             ; Put last XRes byte in accB,
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4RX2             ; Put the new byte in its place.
                    lsl       XOR4RX3

; Now drag the next bit down for the next division...
; *----------------------------------------------------

                    ldb       RX_DIVIDEND3        ; Load accA with 3rd quotient byte
                    andb      #$80                ; & keep only the MSB...
                    stb       XOR4RX3             ; Then store it away.
                    dec       IN_FOR_COUNT        ; Executed inner loop 8 times?
                    bne       JMP2RX              ; Keep looping if not.
                    dec       OUT_FOR_COUNT
                    bne       JMP_OUT             ; Keep looping.
                    bra       A004                ; Jump to the next step if finished.
JMP2RX              jmp       RX_DIV

JMP_OUT             inx
                    lda       #$08                ; Reinitialise counter of the inner
                    sta       IN_FOR_COUNT        ; for loop.
                    lda       ,x                  ; Fetch next byte from data frame.
                    sta       RX_DIVIDEND3
                    anda      #$80
                    sta       XOR4RX3
                    jmp       OUT_4

; G1(x) was < D1 so XOR data with G(x)...
; *========================================

XOR_GX              lda       #GEN_POLY1
                    eora      XOR4RX1             ; XOR 1st XRes byte with 1st G(X) byte.
                    sta       XOR4RX1             ; Store it in XRes buffer.
                    lda       #GEN_POLY2          ; Load accA with next G(x) byte,
                    eora      XOR4RX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4RX2             ; Store result in XRes buffer.
                    lda       #GEN_POLY3          ; Load accA with last bit of G(x),
                    ldb       XOR4RX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd data byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4RX3             ; & store it away.
                    jmp       NTRY2               ; Begin the next long div. step...

; G1(x) was > D1 so XOR data with zero...
; *========================================

XOR_0               clra
                    eora      XOR4RX1             ; XOR 1st XRes byte with zero.
                    sta       XOR4RX1             ; Store it in XRes buffer.
                    clra                          ; Clear accA,
                    eora      XOR4RX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4RX2             ; Store result in XRes buffer.
                    clra                          ; Clear accA.
                    ldb       XOR4RX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd XRes byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4RX3             ; & store it away.
                    jmp       NTRY2

A004                inx
                    lda       ,x
                    sta       RX_DIVIDEND3
                    anda      #$80
                    sta       XOR4RX3
                    lda       #$07                ; Do this seven times...
                    sta       IN_FOR_COUNT

FINI                lda       #GEN_POLY1          ; Load accA with highest byte of G(x).
                    cmpa      XOR4RX1             ; Compare with highest XRes byte,
                    blo       A007                ; & if G(x)<Dn, then XOR with G(x).
                    lda       #GEN_POLY1
                    cmpa      XOR4RX1             ; Checking if G(x) > D(x).
                    bhi       A006
                    lda       #GEN_POLY2
                    cmpa      XOR4RX2
                    blo       A007
                    lda       #GEN_POLY2
                    cmpa      XOR4RX2
                    bhi       A006                ; Branch if G(x) < D(x).
                    lda       #GEN_POLY3
                    cmpa      XOR4RX3
                    bhi       A006
                    bra       A007                ; Branch if G(x) == D(x).
A006                jmp       XOR_01              ; Else, G(x)>Dn, so XOR with zero.
A007                jmp       XOR_GX1

NTRY3               lsl       RX_DIVIDEND1        ; NOW SHIFT DIVIDEND 1 BIT LEFT....
                    lda       RX_DIVIDEND2
                    anda      #$80
                    lsra:7                        ; msb -> lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       RX_DIVIDEND1
                    orb       TEMPOR1
                    stb       RX_DIVIDEND1
                    lsl       RX_DIVIDEND2
                    lda       RX_DIVIDEND3
                    anda      #$80
                    lsra:7                        ; msb -> lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       RX_DIVIDEND2
                    orb       TEMPOR1
                    stb       RX_DIVIDEND2
                    lsl       RX_DIVIDEND3

; Now shift the XOR result left 1 bit...
; *---------------------------------------

                    lsl       XOR4RX1             ; Shift the first byte to 1 bit left.
                    lda       XOR4RX1             ; Put shifted byte in accA,
                    ldb       XOR4RX2             ; & put the next byte in accB.
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1             ; Generic buffer.
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4RX1             ; Put new byte in 1st Xres space.
                    lsl       XOR4RX2             ; Shift 2nd XRes byte left 1 bit,
                    lda       XOR4RX2             ; & put it in accA.
                    ldb       XOR4RX3             ; Put last XRes byte in accB,
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4RX2             ; Put the new byte in its place.
                    lsl       XOR4RX3

                    ldb       RX_DIVIDEND3        ; Load accA with 3rd quotient byte
                    andb      #$80                ; & keep only the MSB...
                    stb       XOR4RX3             ; Then store it away.
                    dec       IN_FOR_COUNT        ; Executed loop 7 times?
                    bne       JMP_FIN             ; Keep looping if not.
                    bra       FINI_OR
JMP_FIN             jmp       FINI

; One final exclusive OR before finishing...
; *-------------------------------------------

FINI_OR             lda       #$55
                    sta       FINAL_CHECK
                    lda       #GEN_POLY1          ; Load accA with highest byte of G(x).
                    cmpa      XOR4RX1             ; Compare with highest XRes byte,
                    blo       A008                ; & if G(x)<Dn, then XOR with G(x).
                    lda       #GEN_POLY1
                    cmpa      XOR4RX1             ; Checking if G(x) > D(x).
                    bhi       A009
                    lda       #GEN_POLY2
                    cmpa      XOR4RX2
                    blo       A008
                    lda       #GEN_POLY2
                    cmpa      XOR4RX2
                    bhi       A009                ; Branch if G(x) < D(x).
                    lda       #GEN_POLY3
                    cmpa      XOR4RX3
                    bhi       A009
                    bra       A008                ; Branch if G(x) == D(x).
A009                bra       XOR_01              ; Else, G(x)>Dn, so XOR with zero.
A008                bra       XOR_GX1

; Shift the final XOR result left 1 bit...
; *---------------------------------------

NTRY4               lsl       XOR4RX1             ; Shift the first byte to 1 bit left.
                    lda       XOR4RX1             ; Put shifted byte in accA,
                    ldb       XOR4RX2             ; & put the next byte in accB.
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1             ; Generic buffer.
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4RX1             ; Put new byte in 1st Xres space.
                    lsl       XOR4RX2             ; Shift 2nd XRes byte left 1 bit,
                    lda       XOR4RX2             ; & put it in accA.
                    ldb       XOR4RX3             ; Put last XRes byte in accB,
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4RX2             ; Put the new byte in its place.
                    lsl       XOR4RX3

                    lda       XOR4RX1             ; Check the remainder...
                    bne       FAIL                ; Return with failure if it's not 0.
                    lda       XOR4RX2
                    bne       FAIL
                    clra
                    sta       CRC_DIR

                    puly
                    pulx
                    pulb
                    pula
                    rts                           ; Otherwise return with success!

FAIL                lda       #$FF
                    sta       CRC_DIR

                    puly
                    pulx
                    pulb
                    pula
                    rts

; G1(x) was < D1 so XOR data with G(x)...
; *========================================

XOR_GX1             lda       #GEN_POLY1
                    eora      XOR4RX1             ; XOR 1st XRes byte with 1st G(X) byte.
                    sta       XOR4RX1             ; Store it in XRes buffer.
                    lda       #GEN_POLY2          ; Load accA with next G(x) byte,
                    eora      XOR4RX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4RX2             ; Store result in XRes buffer.
                    lda       #GEN_POLY3          ; Load accA with last bit of G(x),
                    ldb       XOR4RX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd data byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4RX3             ; & store it away.
                    lda       FINAL_CHECK
                    cmpa      #$55
                    beq       A002
                    jmp       NTRY3               ; Begin the next long div. step...
A002                bra       NTRY4

; G1(x) was > D1 so XOR data with zero...
; *========================================

XOR_01              clra                          ; Clear accA.
                    eora      XOR4RX1             ; XOR 1st XRes byte with zero.
                    sta       XOR4RX1             ; Store it in XRes buffer.
                    clra                          ; Clear accA,
                    eora      XOR4RX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4RX2             ; Store result in XRes buffer.
                    clra                          ; Clear accA.
                    ldb       XOR4RX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd XRes byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4RX3             ; & store it away.
                    lda       FINAL_CHECK
                    cmpa      #$55
                    jeq       NTRY4
                    jmp       NTRY3

; *^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;***********************************************************************
;***********************************************************************
;
; This routine ENCODES a block of data to be transmitted using the
; CCITT V41 Cyclic Redundancy Code.
;
;***********************************************************************
;***********************************************************************
; *^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

CRC_ENC             ldx       #TX_BUFF            ; Load Tx buffer pointer.
                    clra
                    sta       CHKSUM1,X           ; Append 16 0's to the checksum area.
                    sta       CHKSUM2,X
                    lda       ,x                  ; Place 1st three bytes in DIVIDEND...
                    sta       TX_DIVIDEND1
                    sta       XOR4TX1
                    lda       $01,X
                    sta       TX_DIVIDEND2
                    sta       XOR4TX2
                    lda       $02,X
                    sta       TX_DIVIDEND3
                    anda      #$80                ; Remove all but the MSB.
                    sta       XOR4TX3
                    lda       #OUTER_COUNT        ; Initialise counter for the 'outer'
                    sta       OUT_FOR_COUNT       ; for loop.
                    lda       #$08                ; Initialise counter for the 'inner'
                    sta       IN_FOR_COUNT        ; for loop.
                    inx:2

OUT_FOR             clra
                    sta       FINAL_CHECK
TX_DIV              lda       #GEN_POLY1          ; Load accA with highest byte of G(x).
                    cmpa      XOR4TX1             ; Compare with highest XRes byte,
                    blo       B000                ; & if G(x)<Dn, then XOR with G(x).
                    lda       #GEN_POLY1
                    cmpa      XOR4TX1             ; Checking if G(x) > D(x).
                    bhi       B005
                    lda       #GEN_POLY2
                    cmpa      XOR4TX2
                    blo       B000
                    lda       #GEN_POLY2
                    cmpa      XOR4TX2
                    bhi       B005                ; Branch if G(x) < D(x).
                    lda       #GEN_POLY3
                    cmpa      XOR4TX3
                    bhi       B005
                    bra       B000                ; Branch if G(x) == D(x).
B005                jmp       XOR0                ; Else, G(x)>Dn, so XOR with zero.
B000                bra       XORGX

ENTRY2              lsl       TX_DIVIDEND1        ; NOW SHIFT DIVIDEND 1 BIT LEFT....
                    lda       TX_DIVIDEND2
                    anda      #$80
                    lsra:7                        ; msb -> lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       TX_DIVIDEND1
                    orb       TEMPOR1
                    stb       TX_DIVIDEND1
                    lsl       TX_DIVIDEND2
                    lda       TX_DIVIDEND3
                    anda      #$80
                    lsra:7                        ; msb -> lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       TX_DIVIDEND2
                    orb       TEMPOR1
                    stb       TX_DIVIDEND2
                    lsl       TX_DIVIDEND3

; Now shift the XOR result left 1 bit...
; *---------------------------------------

                    lsl       XOR4TX1             ; Shift the first byte to 1 bit left.
                    lda       XOR4TX1             ; Put shifted byte in accA,
                    ldb       XOR4TX2             ; & put the next byte in accB.
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1             ; Generic buffer.
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4TX1             ; Put new byte in 1st Xres space.
                    lsl       XOR4TX2             ; Shift 2nd XRes byte left 1 bit,
                    lda       XOR4TX2             ; & put it in accA.
                    ldb       XOR4TX3             ; Put last XRes byte in accB,
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4TX2             ; Put the new byte in its place.
                    lsl       XOR4TX3

; Now drag the next bit down for the next division...
; *----------------------------------------------------

                    ldb       TX_DIVIDEND3        ; Load accA with 3rd quotient byte
                    andb      #$80                ; & keep only the MSB...
                    stb       XOR4TX3             ; Then store it away.
                    dec       IN_FOR_COUNT        ; Executed inner loop 8 times?
                    bne       JMP2TX              ; Keep looping if not.
                    dec       OUT_FOR_COUNT
                    bne       JMP2OUT             ; Keep looping otherwise.
                    bra       B004                ; Jump to next step if finished.
JMP2TX              jmp       TX_DIV

JMP2OUT             inx
                    lda       #$08                ; Reinitialise counter of the inner
                    sta       IN_FOR_COUNT        ; for loop.
                    lda       ,x                  ; Fetch next byte from data frame.
                    sta       TX_DIVIDEND3
                    anda      #$80
                    sta       XOR4TX3
                    jmp       OUT_FOR

; G1(x) was < D1 so XOR data with G(x)...
; *========================================

XORGX               lda       #GEN_POLY1
                    eora      XOR4TX1             ; XOR 1st XRes byte with 1st G(X) byte.
                    sta       XOR4TX1             ; Store it in XRes buffer.
                    lda       #GEN_POLY2          ; Load accA with next G(x) byte,
                    eora      XOR4TX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4TX2             ; Store result in XRes buffer.
                    lda       #GEN_POLY3          ; Load accA with last bit of G(x),
                    ldb       XOR4TX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd data byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4TX3             ; & store it away.
                    jmp       ENTRY2              ; Begin the next long div. step...

; G1(x) was > D1 so XOR data with zero...
; *========================================

XOR0                clra
                    eora      XOR4TX1             ; XOR 1st XRes byte with zero.
                    sta       XOR4TX1             ; Store it in XRes buffer.
                    clra                          ; Clear accA,
                    eora      XOR4TX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4TX2             ; Store result in XRes buffer.
                    clra                          ; Clear accA.
                    ldb       XOR4TX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd XRes byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4TX3             ; & store it away.
                    jmp       ENTRY2

B004                inx
                    lda       ,x
                    sta       TX_DIVIDEND3
                    anda      #$80
                    sta       XOR4TX3
                    lda       #$07                ; Do this seven times...
                    sta       IN_FOR_COUNT

FIN                 lda       #GEN_POLY1          ; Load accA with highest byte of G(x).
                    cmpa      XOR4TX1             ; Compare with highest XRes byte,
                    blo       B007                ; & if G(x)<Dn, then XOR with G(x).
                    lda       #GEN_POLY1
                    cmpa      XOR4TX1             ; Checking if G(x) > D(x).
                    bhi       B006
                    lda       #GEN_POLY2
                    cmpa      XOR4TX2
                    blo       B007
                    lda       #GEN_POLY2
                    cmpa      XOR4TX2
                    bhi       B006                ; Branch if G(x) < D(x).
                    lda       #GEN_POLY3
                    cmpa      XOR4TX3
                    bhi       B006
                    bra       B007                ; Branch if G(x) == D(x).
B006                jmp       XOR01               ; Else, G(x)>Dn, so XOR with zero.
B007                jmp       XORGX1

ENTRY3              lsl       TX_DIVIDEND1        ; SHIFT THE DIVIDEND 1 BIT LEFT....
                    lda       TX_DIVIDEND2
                    anda      #$80
                    lsra:7                        ; msb -> lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       TX_DIVIDEND1
                    orb       TEMPOR1
                    stb       TX_DIVIDEND1
                    lsl       TX_DIVIDEND2
                    lda       TX_DIVIDEND3
                    anda      #$80
                    lsra:7                        ; msb is now shifter to lsb
                    sta       TEMPOR1             ; Store in memory so it can be OR'd.
                    ldb       TX_DIVIDEND2
                    orb       TEMPOR1
                    stb       TX_DIVIDEND2
                    lsl       TX_DIVIDEND3

; Now shift the XOR result left 1 bit...
; *---------------------------------------

                    lsl       XOR4TX1             ; Shift the first byte to 1 bit left.
                    lda       XOR4TX1             ; Put shifted byte in accA,
                    ldb       XOR4TX2             ; & put the next byte in accB.
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1             ; Generic buffer.
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4TX1             ; Put new byte in 1st Xres space.
                    lsl       XOR4TX2             ; Shift 2nd XRes byte left 1 bit,
                    lda       XOR4TX2             ; & put it in accA.
                    ldb       XOR4TX3             ; Put last XRes byte in accB,
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4TX2             ; Put the new byte in its place.
                    lsl       XOR4TX3

                    ldb       TX_DIVIDEND3        ; Load accA with 3rd quotient byte
                    andb      #$80                ; & keep only the MSB...
                    stb       XOR4TX3             ; Then store it away.
                    dec       IN_FOR_COUNT        ; Executed loop 7 times?
                    bne       JMP2FIN             ; Keep looping if not.
                    bra       FIN_XOR

JMP2FIN             jmp       FIN

; One final exclusive OR before finishing...
; *-------------------------------------------

FIN_XOR             lda       #$55
                    sta       FINAL_CHECK
                    lda       #GEN_POLY1          ; Load accA with highest byte of G(x).
                    cmpa      XOR4TX1             ; Compare with highest XRes byte,
                    blo       XORGX1              ; & if G(x)<Dn, then XOR with G(x).
                    lda       #GEN_POLY1
                    cmpa      XOR4TX1             ; Checking if G(x) > D(x).
                    bhi       XOR01
                    lda       #GEN_POLY2
                    cmpa      XOR4TX2
                    blo       XORGX1
                    lda       #GEN_POLY2
                    cmpa      XOR4TX2
                    bhi       XOR01               ; Branch if G(x) < D(x).
                    lda       #GEN_POLY3
                    cmpa      XOR4TX3
                    bhi       XOR01
                    bra       XORGX1

; Shift the final XOR result left 1 bit...
; *---------------------------------------

ENTRY4              lsl       XOR4TX1             ; Shift the first byte to 1 bit left.
                    lda       XOR4TX1             ; Put shifted byte in accA,
                    ldb       XOR4TX2             ; & put the next byte in accB.
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1             ; Generic buffer.
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4TX1             ; Put new byte in 1st Xres space.
                    lsl       XOR4TX2             ; Shift 2nd XRes byte left 1 bit,
                    lda       XOR4TX2             ; & put it in accA.
                    ldb       XOR4TX3             ; Put last XRes byte in accB,
                    lsrb:7                        ; msb -> lsb
                    stb       TEMPOR1
                    ora       TEMPOR1             ; Concatenate LSB to accA.
                    sta       XOR4TX2             ; Put the new byte in its place.
                    lsl       XOR4TX3

                    ldx       #TX_BUFF            ; Appending calculated checksum to
                    lda       XOR4TX1             ; the data frame to be transmitted.
                    sta       CHKSUM1,X
                    lda       XOR4TX2
                    sta       CHKSUM2,X
                    clra
                    sta       CRC_DIR

                    puly
                    pulx
                    pulb
                    pula
                    rts                           ; Return from subroutine with success.

; G1(x) was < D1 so XOR data with G(x)...
; *========================================

XORGX1              lda       #GEN_POLY1
                    eora      XOR4TX1             ; XOR 1st XRes byte with 1st G(X) byte.
                    sta       XOR4TX1             ; Store it in XRes buffer.
                    lda       #GEN_POLY2          ; Load accA with next G(x) byte,
                    eora      XOR4TX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4TX2             ; Store result in XRes buffer.
                    lda       #GEN_POLY3          ; Load accA with last bit of G(x),
                    ldb       XOR4TX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd data byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4TX3             ; & store it away.
                    lda       FINAL_CHECK
                    cmpa      #$55
                    beq       B002
                    jmp       ENTRY3              ; Begin the next long div. step...

B002                bra       ENTRY4

; G1(x) was > D1 so XOR data with zero...
; *========================================

XOR01               clra                          ; Clear accA.
                    eora      XOR4TX1             ; XOR 1st XRes byte with zero.
                    sta       XOR4TX1             ; Store it in XRes buffer.
                    clra                          ; Clear accA,
                    eora      XOR4TX2             ; & XOR it with the 2nd XRes byte.
                    sta       XOR4TX2             ; Store result in XRes buffer.
                    clra                          ; Clear accA.
                    ldb       XOR4TX3             ; Load accB with 3rd XRes byte,
                    stb       TEMPOR1             ; & store in a generic buffer.
                    eora      TEMPOR1             ; XOR 3rd XRes byte & last bit of G(x).
                    anda      #$80                ; Get rid of all but the MSB,
                    sta       XOR4TX3             ; & store it away.
                    lda       FINAL_CHECK
                    cmpa      #$55
                    beq       ENTRY4
                    jmp       ENTRY3

; Version No.`LOST_COUNT', late Saturday afternoon, September 18, 1999.
;
; Please forward any comments, suggestions, or corrections to:
; arkaydia@hotmail.com
