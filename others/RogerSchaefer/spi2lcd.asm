;*******************************************************************************
;                       SPI to LCD
;
; Program to control a character module LCD in the 4 bit mode
; with a MC68HC11 SPI connected to a 74HC595 8 bit shift register
; Qa to Qd of 74HC595 connected to DB4 to DB7 of LCD
; Qe to RS     Qf to E of LCD    R/W pulled to ground
;
; by Roger Schaefer
;    May 2000
;*******************************************************************************

#ifmain ;-----------------------------------------------------------------------
                    #ListOff
                    #Uses     mcu.inc
                    #ListOn
#endif ;------------------------------------------------------------------------

;*******************************************************************************
OC4                 equ       4
OC4F                equ       1<OC4
;*******************************************************************************
;
;*******************************************************************************
; LCD COMMANDS
;*******************************************************************************

Clear_Display       equ       %00000001
Entry_Mode          equ       %00000110
Function_Set        equ       %00101000           ; set this for type of LCD

LCDlatch            equ       OC4                 ; of Port A
PR1PR0              equ       %01                 ; Timer Prescaler Select
PRESCALE            equ       4                   ; if PR1:PR0 = 01 set to 4
MSec                equ       125                 ; @ 2MHz clock use 125
                                                  ; use: #ms*MSec*PRESCALE
;*******************************************************************************
                    #RAM
;*******************************************************************************
                   ;org       $7A

lcd_temp            rmb       1
linechar            rmb       1
lineno              rmb       1
st_lo               rmb       1
lcd_data            rmb       1
lcd_RS              equ       %00010000
lcd_E               equ       %00100000
lcd_E_lo            equ       %11011111

;*******************************************************************************
                    #ROM
;*******************************************************************************
                   ;org       $7000

TEST                bra       LCD_test
LCD_initialize      bra       init_lcd
LCD_enter           bra       tx_lcd              ; enter here with character in B

;*******************************************************************************

LCD_test            proc
                    bsr       init_lcd
                    ldx       #HELLO
                    bra       lcd_strg
          ;-------------------------------------- ;test message
HELLO               fcc       'HELLO WORLD',CR
                    fcc       'HAVE A NICE DAY',CR
                    fcc       'LINE THREE',CR
                    fcs       'LINE FOUR'

;*******************************************************************************
; LCD INTERFACE
;*******************************************************************************

lcd_strg            proc                          ; send string to LCD
Loop@@              ldb       ,x
                    cmpb      #EOT
                    beq       Done@@
                    pshx
                    bsr       tx_lcd
                    pulx
                    inx
                    bra       Loop@@
Done@@              rts                           ;equ       :AnRTS

;*******************************************************************************
; Write character in Reg B to LCD

tx_lcd              proc
                    cmpb      #LF                 ; trap line feed
                    beq       Done@@
                    cmpb      #$08                ; chk for backspace
                    bne       _1@@
                    jsr       back_sp             ; perform lcd backspace
                    bra       Done@@              ; force branch to end of routine
_1@@                cmpb      #CR                 ; new line?
                    bne       _2@@
                    jsr       new_line
                    bra       Done@@
_2@@                bra       wr_lcd              ; write printable character to lcd
Done@@              equ       :AnRTS

;*******************************************************************************
; initialize lcd to correct settings

init_lcd            proc
                    ldx       #REGS
                    jsr       SPIINT
                    jsr:2     wait10ms            ; wait for lcd to settle
                    bsr:3     Set_8_bit_mode      ; Send Function Set three times

                    ldb       #%00000010          ; set 4-bit
                    bsr       Set_4_bit_mode

                    ldb       #Function_Set
                    bsr       con_lcd             ; send

                    ldb       #%00001000          ; display off
                    bsr       con_lcd             ; send

                    ldb       #%00001110          ; display on
                    bsr       con_lcd             ; send lsb

                    bra       cls_lcd

;*******************************************************************************

Set_8_bit_mode      proc
                    ldb       #%11
;                   bra       Set_4_bit_mode

;*******************************************************************************

Set_4_bit_mode      proc
                    stb       lcd_data            ; send byte to lcd
                    bclr      lcd_data,lcd_RS     ; set rs low
                    bset      lcd_data,lcd_E      ; set enable high
                    ldb       lcd_data
                    bsr       SPIsend
                    jmp       wait10ms

;*******************************************************************************

SPIsend             proc
                    pshx
                    ldx       #REGS
                    bsr       SPIsend1
                    andb      #lcd_E_lo           ; set LCD E low
                    bsr       SPIsend1
                    orb       #lcd_E              ; set LCD back high
                    bsr       SPIsend1
                    pulx
                    rts

;*******************************************************************************

SPIsend1            proc
                    stb       [SPDR,x             ; enter with data in B
                    brclr     [SPSR,x,SPIF.,*     ; wait
                    bset      [PORTA,x,LCDlatch   ; shift to output latches
                    jsr       COPRESET            ; kill some time
                    bclr      [PORTA,x,LCDlatch
                    rts

;*******************************************************************************

con_lcd             proc
                    clra
                    bsr       wr_lcd1
                    jmp       wait10ms

;*******************************************************************************
; write character to liquid crystal display
; enter with character in Reg B

wr_lcd              proc
                    lda       #lcd_RS
                    bsr       wr_lcd1
                    !...      could put code for horz scroll here
                    rts

;*******************************************************************************

wr_lcd1             proc
                    stb       lcd_temp
                    lsrb:4
                    psha
                    aba                           ; set rs high or low
                    sta       lcd_data            ; msb byte to lcd
                    pula
                    bset      lcd_data,lcd_E      ; set enable high
                    ldb       lcd_data
                    bsr       SPIsend
                    bsr       timer
                    ldb       lcd_temp
                    andb      #$f                 ; keep 4 lsb
                    aba                           ; set rs high or low
                    sta       lcd_data            ; lsb byte to lcd
                    bset      lcd_data,lcd_E      ; set enable high
                    ldb       lcd_data
                    bsr       SPIsend
                    bsr       timer               ; wait for lcd
                    rts

;*******************************************************************************
; clear lcd and reset cursor

cls_lcd             proc
                    clr       linechar
                    clr       lineno
                    pshb
                    ldb       #Clear_Display      ; clear & home
                    bsr       con_lcd             ; send
                    ldb       #Entry_Mode
                    bsr       con_lcd             ; send
                    pulb
                    rts

;*******************************************************************************
; send new line command to lcd

new_line            proc
                    psha
                    pshb
                    pshx
                    clr       linechar
                    ldb       lineno
                    ldx       #Start_of_line
                    abx
                    ldb       ,x
                    pulx
                    bsr       con_lcd             ; send
                    ldb       #Entry_Mode
                    bsr       con_lcd             ; send
                    lda       lineno
                    inca
                    anda      #%11
                    sta       lineno
                    pulb
                    pula
                    rts

;*******************************************************************************
; back space cursor & overwrite char

back_sp             proc
                    lda       #$10                ; move cursor 1 place left
                    bsr       con_lcd
                    ldb       #' '                ; overwrite char
                    bsr       wr_lcd
                    lda       #$10                ; move cursor 1 place left
                    bsr       con_lcd

                    ldb       #' '                ; overwrite char
                    dec       linechar
                    bpl       Done@@
                    clr       linechar
Done@@              rts
;-------------------------------------------------------------------------------
Start_of_line       fcb       $c0
                    fcb       $94
                    fcb       $d4
                    fcb       $80

;*******************************************************************************
; short software timer loop, approx 120 us
; all registers saved

timer               proc
                    psha
                    pshx
                    ldx       #78                 ; execute the following 78 times
Loop@@              dex                           ; this loop takes 3 cycles
                    bne       Loop@@              ; or about 1.5 us
                    pulx
                    pula
                    rts

;*******************************************************************************
;       DELAY LOOPS
;
; routine to implement general purpose timer, with
; 1 ms increments call with desired time period in
; acc. Routine implements a 1 ms loop.
; Uses Output Compare 4

time1ms             proc
                    sta       st_lo
                    ldx       #REGS
Loop@@              ldd       [TCNT,x
                    addd      #1*MSec*PRESCALE
                    std       [TOC4,x
                    lda       #OC4F
                    sta       [TFLG1,x
                    brclr     [TFLG1,x,OC4F,*
                    lda       #OC4F
                    sta       [TFLG1,x
                    dec       st_lo               ; dec count on each underflow
                    bne       Loop@@              ; exit when time_count = 0
                    rts

;*******************************************************************************

wait1sec            proc
                    bsr       wait400ms
                    bsr       wait200ms
wait400ms           bsr       wait200ms
wait200ms           bsr       wait100ms
;                   bra       wait100ms

;*******************************************************************************
; wait for 100 milliseconds

wait100ms           proc
                    psha
                    lda       #100
                    bsr       time1ms
                    pula
                    rts

;*******************************************************************************
; wait for 10 milliseconds

wait10ms            proc
                    psha
                    lda       #10
                    bsr       time1ms
                    pula
                    rts

;*******************************************************************************
; wait for 1 millisecond

wait1ms             proc
                    psha
                    lda       #1
                    bsr       time1ms
                    pula
                    rts

;*******************************************************************************

SPIINT              proc
                    lda       #$2F
                    sta       PORTD
                    lda       #%00111000
                    sta       DDRD
                    lda       #%01011000          ; enable master SPI
          ;-------------------------------------- ; CPOL=1  CPHA=0
                    sta       SPCR
                    nop
                    lda       SPDR                ; dummy data
                    rts

;*******************************************************************************

COPRESET            proc
                    psha
                    lda       #$55                ; reset cop
                    sta       COPRST
                    coma
                    sta       COPRST
                    pula
                    rts

;*******************************************************************************
