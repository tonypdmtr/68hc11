#ifmain ;-----------------------------------------------------------------------
                    org       *
#endif ;------------------------------------------------------------------------
;*************************************************************************
; Name:         LCD Library
; Version:      1.1
; Date:         3/12/2002
; Author:       Dan Kohn
;
; History:      This library was created using the code from AXIOM
;               Manufacturing's AXIDE Disk (most notably the
;               KEYLCD-E.ASM sample code).
;
; Notes:        There is a minimum time that must elapse before
;               sending another character to the LCD. If characters
;               are missing from the display, add a JSR LC_DELAY1 to
;               your code after the JSR LCDOUT.

LCDBAS              equ       $B5F0               ; LCD port address
LCDBUF              rmb       10

;*************************************************************************
; Name:         OUTPUT TO LCD
; Call By:      LCDOUT
; Version:      1.0
; Date:         3/12/2002
; Author:       Axiom Manufacturing
; Modified By:  Dan Kohn
;
; Function:
;
;
; Process:
;
;
;
; On Call:      Reg A contains ASCII code to write to LCD
;
; On Return:    No Registers Modified
;

; This version adjusts for 4x20 keypad row addressing if you're expecting
; continuous rows 1,2,3,4

LCDOUT              proc
                    pshb
                    ldb       LCDBAS              ; get current address

                    cmpb      #$13                ; at end of first row
                    beq       Row1@@              ; jif yes

                    cmpb      #$53                ; at end of 2nd row
                    beq       Row2@@              ; jif yes

                    cmpb      #$27                ; at end of 3rd row
                    beq       Row3@@              ; jif yes

                    sta       LCDBAS+1            ; write data to lcd output port
                    bra       Done@@

Row1@@              ldb       #$28+$80            ; set correct address
                    bra       Cont@@              ; continue fix

Row2@@              ldb       #$14+$80            ; set correct address
                    bra       Cont@@              ; continue fix

Row3@@              ldb       #$54+$80            ; set correct address
Cont@@              sta       LCDBAS+1            ; write data to lcd output port
                    bsr       LC_DELAY
                    stb       LCDBAS              ; make address fix

Done@@              pulb
                    rts

;*************************************************************************
; Name:         LCD Setup
; Call By:      LCDSET
; Version:      1.0
; Date:         3/12/2002
; Author:       AXIOM Manufacturing
; Modified By:  Dan Kohn
;
; Function:     To clear and set up the LCD display
;
; Process:      (Yet to be determined)
;
; On Call:      No Registers need to be set up
;
; On Return:    No Registers Modified
;

LCDSET              proc
                    psha
                    lda       #$3C                ; set 2x40 Display
                    sta       LCDBAS
                    bsr       LC_DELAY
                    lda       #$01                ; Clear & Home
                    sta       LCDBAS
                    bsr       LC_DELAY
                    lda       #$0F                ; Display on
                    sta       LCDBAS
                    bsr       LC_DELAY
                    lda       #$06                ; Cursor shift on
                    sta       LCDBAS
                    lda       #$14                ; Shift right
                    sta       LCDBAS
                    lda       #$02                ; Cursor to Home
                    sta       LCDBAS
                    bsr       LC_DELAY
                    pula
                    rts

;*************************************************************************
; Name:         Display Decimal Value
; Call By:      LCDDEC
; Version:      1.0
; Date:         3/12/2002
; Author:       Dan Kohn
;
; Recognition:  Joel Lee (Typing in code from Text that was modified)
;               Frederick F.Driscoll, Robert F. Coughlin,
;               Robert S. Villanucci (For writing original code)
;
; Function:     Take a hex value in REG D and display it on the LCD
;               screen in ASCII code
;
; Process:      Convert value in REG D to ASCII then load REG X with
;               LCDBUF location then call LCDMSG
;
; Protocol:     On Entry, REG D contains value to display on LCD. No
;               value returned


LCDDEC              psha
                    pshb
                    pshx
                    pshy
                    ldy       #LCDBUF+4           ; *Point to LSB of LCDBUF
Loop@@              ldx       #$0A                ; *divide by 10dec to convert to BCD
                    idiv
                    addb      #48                 ; *add 48 to put value in ASCII
                    stb       ,y                  ; *stores ASCII value to buffer for display
                    xgdx
                    dey                           ; *loop 5 times (for full conversion of
                    cpy       #LCDBUF             ; *a 16 bit number)
                    bhs       Loop@@
                    lda       #'$'                ; *add a '$' character to end of LCDBUF
                    sta       LCDBUF+5
                    ldx       #LCDBUF             ; *Point to LCDBUF and call MSG Display Routine
                    bsr       LCDMSG
                    puly
                    pulx
                    pulb
                    pula
                    rts                           ; *returns from the subroutine

;*************************************************************************
; Name:         Display Message to LCD
; Call By:      LCDMSG
; Version:      1.0
; Date:         3/12/2002
; Author:       Dan Kohn
;
; Function:     Display message to LCD
;
; Process:      Diplay message pointed to by index REG X by getting
;               one character at a time and calling LCDOUT (and DELAY1)
;               and exiting when "$" is found
;
; On Call:      REG X contains message pointer
;
; On Return:    No Registers Modified
;

LCDMSG              proc
                    pshx
                    psha

Loop@@              lda       ,x
                    cmpa      #'$'
                    beq       Done@@
                    jsr       LCDOUT
                    bsr       LC_DELAY1
                    inx
                    bra       Loop@@

Done@@              pula
                    pulx
                    rts

;*************************************************************************
; Name:         DELAY
; Call By:      LC_DELAY
; Version:      1.0
; Date:         3/12/2002
; Author:       Axiom Manufacturing
; Modified By:  Dan Kohn
;
; Function:     To act as a time delay
;
;
; Process:      Delay using two nested loops for a total of
;               57600 loops (or 0.14 Seconds)
;
; On Call:      No Registers need to be set up
;
; On Return:    No Registers Modified
;

LC_DELAY            proc
                    pshb                          ; save registers
                    psha
                    ldb       #$F0                ; set loop count 1
MainLoop@@          lda       #$F0                ; set loop count 2
Loop@@              deca                          ; decriment counter 2
                    bne       Loop@@              ; branch if not 0
                    decb                          ; decriment counter 1
                    bne       MainLoop@@          ; branch if not 0
                    pula                          ; restore registers
                    pulb
                    rts

;*************************************************************************
; Name:         DELAY1
; Call By:      LC_DELAY1
; Version:      1.0
; Date:         3/12/2002
; Author:       Axiom Manufacturing
; Modified By:  Dan Kohn
;
; Function:     To act as a time delay inbetween characters
;
;
; Process:      Delay using two nested loops for a total of
;               20,000 loops (or 0.12mSEC)
;
; On Call:      No Registers need to be set up
;
; On Return:    No Registers Modified

LC_DELAY1           proc
                    pshx                          ; save x
                    ldx       #20000              ; load count
Loop@@              dex                           ; decriment x
                    bne       Loop@@              ; loop if not 0
                    pulx                          ; restore x
                    rts
