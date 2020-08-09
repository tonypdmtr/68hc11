; Display A/D data on LCD
; 18-Oct-1997 fweld
; 26-Oct-1997 fweld

PORTC               equ       $1003               ; data lines
PORTB               equ       $1004               ; control lines
DDRC                equ       $1007               ; direction for port C

ADCTL               equ       $1030               ; AD control
ADR1                equ       $1031               ; AD data
OPTION              equ       $1039               ; on switch for ADC

LCD_E               equ       $01                 ; enable
LCD_RS              equ       $02                 ; control/data select

TEMP                equ       $0000               ; store for A/D value
STACK               equ       $00ff               ; good to know where it is

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       $B600

Start               proc
                    lds       #STACK
                    lda       OPTION
                    ora       #%10000000          ; A/D converter on
                    sta       OPTION

                    lda       #%11111111          ; all outputs
                    sta       DDRC                ; on port C

                    bsr       InitLCD             ; initialize 2x16 LCD

Loop@@              jsr       Home                ; cursor home left edge
                    clr       ADCTL               ; channel
                    bsr       Display
                    jsr       NewLine
                    bsr       Display
                    bra       Loop@@

;*******************************************************************************

Display             proc
Loop@@              bsr       Convert             ; get converted value to A
                    bsr       OutBCD              ; display it
                    lda       #' '
                    bsr       WriteLCD
                    inc       ADCTL
                    lda       ADCTL
                    anda      #%00000011          ; mask off
                    bne       Loop@@              ; do next
                    rts

;*******************************************************************************

Convert             proc
                    lda       ADCTL               ; channel 0
                    sta       ADCTL
Loop@@              tst       ADCTL
                    bpl       Loop@@
                    lda       ADR1
                    rts

;*******************************************************************************

OutBCD              proc
                    ldb       #100                ; set up 100
                    bsr       Go@@                ; translate
                    ldb       #10                 ; 10
                    bsr       Go@@
                    ldb       #1
;                   bra       Go@@
          ;--------------------------------------
Go@@                clr       TEMP                ; loop counter
Loop@@              cba                           ; compare them
                    blo       Done@@              ; already lower
                    sba                           ; A-B=A
                    inc       TEMP                ; count how often
                    bra       Loop@@
Done@@              psha                          ; save it
                    lda       TEMP                ; get counter
                    adda      #'0'                ; make him ascii
                    bsr       WriteLCD            ; display (no need to save B)
                    pula                          ; restore A
                    rts

;*******************************************************************************

Print               proc
Loop@@              lda       ,x
                    beq       Done@@
                    bsr       WriteLCD
                    inx
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************

WriteLCD            proc
                    sta       PORTC               ; put data on bus
                    bsr       Delay               ; wait
                    ldb       PORTB
                    orb       #LCD_E              ; set E
                    stb       PORTB
                    andb      #%11111110          ; clr E
                    stb       PORTB
                    rts

;*******************************************************************************

InitLCD             proc
                    clr       PORTB               ; control mode

                    bsr       Pause               ; wait until LCD ready
                    lda       #%00111000          ; 8-bit mode, 2-lines, 5x8 size
                    bsr       WriteLCD
                    bsr       Pause               ; then wait

                    lda       #%00001100          ; display on, cursor off, blink off
                    bsr       WriteLCD
                    lda       #%00000110          ; increment, display freeze
                    bsr       WriteLCD
          ;-------------------------------------- ; Clear
                    clr       PORTB               ; control mode
                    lda       #%00000001          ; display clear
                    bsr       WriteLCD
                    bra       Done@@
          ;--------------------------------------
Home                clr       PORTB
                    lda       #%00000010          ; cursor home
                    bsr       WriteLCD

                    lda       #%10000000          ; old line
                    bsr       WriteLCD
                    bra       Done@@
          ;--------------------------------------
NewLine             clr       PORTB
                    lda       #%11000000          ; line #2
                    bsr       WriteLCD
          ;--------------------------------------
Done@@              ldb       #LCD_RS             ; enable normal mode
                    stb       PORTB
;                   bra       Pause

;*******************************************************************************

Pause               proc
                    ldy       #$8000
Loop@@              dey
                    bne       Loop@@
                    rts

;*******************************************************************************

Delay               proc
                    clrb
Loop@@              decb
                    bmi       Loop@@
                    rts
