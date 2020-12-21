********************************************************************
*                       SPI to LCD                                 *
*                                                                  *
* Program to control a character module LCD in the 4 bit mode      *
* with a MC68HC11 SPI connected to a 74HC595 8 bit shift register  *
* Qa to Qd of 74HC595 connected to DB4 to DB7 of LCD               *
* Qe to RS     Qf to E of LCD    R/W pulled to ground              *
*                                                                  *
* by Roger Schaefer                                                *
*    May 2000                                                      *
********************************************************************
*
F1      EQU     0       ;define MPU
REGBS   DEF     $1000
        #ListOff
        #Include 811e2.inc
        #ListOn
*
******************
*  LCD COMMANDS  *
******************
*
Clear_Display   EQU     %00000001
Entry_Mode      EQU     %00000110
Function_Set    EQU     %00101000       ;set this for type of LCD
*
LCDlatch        EQU     OC4            ;of Port A
PR1PR0          EQU     %01             ;Timer Prescaler Select
PRESCALE        EQU     4               ;if PR1:PR0 = 01 set to 4
MSec            EQU     125             ; @ 2MHz clock use 125
                                        ; use: #ms*MSec*PRESCALE
                #RAM ORG     $7A
lcd_temp        RMB     1
linechar        RMB     1
lineno          RMB     1
st_lo           RMB     1
lcd_data        RMB     1
lcd_RS          EQU     %00010000
lcd_E           EQU     %00100000
lcd_E_lo        EQU     %11011111
******************************
        #ROM ORG     $7000
TEST:
        BRA     LCD_test
LCD_initialize:
        BRA     init_lcd
LCD_enter:                      ;enter here with character in B
        BRA     tx_lcd
****
LCD_test:
        JSR     init_lcd
        LDX     #HELLO
        JSR     lcd_strg
        rts
****                            ;test message
HELLO   fcc     'HELLO WORLD'
        fcb     CR
        fcc     'HAVE A NICE DAY'
        fcb     CR
        fcc     'LINE THREE'
        fcb     CR
        fcc     'LINE FOUR'
        fcb     EOS
*
************************
*                      *
*     LCD INTERFACE    *
*                      *
************************
*
lcd_strg:                       ;send string to LCD
        ldab    0,x
        cmpb    #EOT
        beq     lcd_strg9
        pshx
        bsr     tx_lcd
        pulx
        inx
        bra     lcd_strg
lcd_strg9:
        rts
*
* Write character in Reg B to LCD
tx_lcd:
tx_lcd6 cmpb    #LF             ;trap line feed
        beq     tx_lcdx
tx_lcd1 cmpb    #$08            ;chk for backspace
        bne     tx_lcd4
        jsr     back_sp         ;perform lcd backspace
        bra     tx_lcdx         ;force branch to end
                                ;of routine
tx_lcd4 cmpb    #CR             ;new line?
        bne     tx_lcd5
        jsr     new_line
        bra     tx_lcdx
tx_lcd5:
        jsr     wr_lcd          ;write printable character
                                ;to lcd
tx_lcdx rts
*
*
* initialize lcd to correct settings
init_lcd:
        ldx     #REGBS
        jsr     SPIINT
        jsr     wait10ms
        jsr     wait10ms        ;wait for lcd to settle
* Send Function Set three times
        bsr     Set_8_bit_mode
        bsr     Set_8_bit_mode
        bsr     Set_8_bit_mode
*
        ldab    #%00000010      ;set 4-bit
        bsr     Set_4_bit_mode
*
        ldab    #Function_Set
        jsr     con_lcd         ;send
*
        ldab    #%00001000      ;display off
        jsr     con_lcd         ;send
*
        ldab    #%00001110      ;display on
        jsr     con_lcd         ;send lsb
*
        jsr     cls_lcd
        rts
*
*
Set_8_bit_mode:
        ldab    #%11
Set_4_bit_mode:
        stab    lcd_data                ;send byte to lcd
        bclr    lcd_data,lcd_RS         ;set rs low
        bset    lcd_data,lcd_E          ;set enable high
        ldab    lcd_data
        bsr     SPIsend
        jsr     wait10ms
        rts
*
*
SPIsend:
        pshx
        ldx     #REGBS
        bsr     SPIsend1
        andb    #lcd_E_lo               ;set LCD E low
        bsr     SPIsend1
        orb     #lcd_E                  ;set LCD back high
        bsr     SPIsend1
        pulx
        rts
SPIsend1:
        stab    SPDR,X                  ;enter with data in B
        brclr   SPSR,X,SPIF,*           ;wait
        bset    PORTA,X,LCDlatch        ;shift to output latches
        jsr     COPRESET                ;kill some time
        bclr    PORTA,X,LCDlatch
        rts
*
con_lcd:
        clra
        bsr     wr_lcd1
        jsr     wait10ms
        rts
*
*       write character to liquid crystal display
*       enter with character in Reg B
*
wr_lcd:
        ldaa    #lcd_RS
        bsr     wr_lcd1
*                                       ;could put code for horz scroll here
wrlcdex rts
*
wr_lcd1:
        stab    lcd_temp
        lsrb
        lsrb
        lsrb
        lsrb
        psha
        aba                             ;set rs high or low
        staa    lcd_data                ;msb byte to lcd
        pula
        bset    lcd_data,lcd_E          ;set enable high
        ldab    lcd_data
        bsr     SPIsend
        jsr     timer
        ldab    lcd_temp
        andb    #$f                     ;keep 4 lsb
        aba                             ;set rs high or low
        staa    lcd_data                ;lsb byte to lcd
        bset    lcd_data,lcd_E          ;set enable high
        ldab    lcd_data
        bsr     SPIsend
        jsr     timer                   ;wait for lcd
        rts
*
*       clear lcd and reset cursor
cls_lcd:
        clr     linechar
        clr     lineno
cls_lcd1:
        pshb
        ldab    #Clear_Display          ;clear & home
        jsr     con_lcd                 ;send
        ldab    #Entry_Mode
        jsr     con_lcd                 ;send
        pulb
        rts
*
* send new line command to lcd
new_line:
        psha
        pshb
        pshx
        clr     linechar
        ldab    lineno
        ldx     #Start_of_line
        abx
        ldab    0,X
        pulx
        jsr     con_lcd         ;send
        ldab    #Entry_Mode
        jsr     con_lcd         ;send
        ldaa    lineno
        inca
        anda    #%11
        staa    lineno
        pulb
        pula
        rts
*
* back space cursor & overwrite char
back_sp ldaa    #$10            ;move cursor 1 place left
        jsr     con_lcd
        ldab    #SPACE          ;overwrite char
        jsr     wr_lcd
        ldaa    #$10            ;move cursor 1 place left
        jsr     con_lcd
*
        ldab    #SPACE          ;overwrite char
        dec     linechar
        bmi     back_sp8
back_sp9:
        rts
back_sp8:
        clr     linechar
        rts
;
;----------------------------
Start_of_line:
        fcb     $c0
        fcb     $94
        fcb     $d4
        fcb     $80
*
* short software timer loop, approx 120 us
* all registers saved
*
timer   psha
        pshx
        ldx     #78             ;execute the following
                                ;78 times
timer_2:
        dex                     ;this loop takes 3 cycles
        bne     timer_2         ;or about 1.5 us
        pulx
        pula
        rts
*
****************************************************
*       DELAY LOOPS                                *
*                                                  *
* routine to implement general purpose timer, with *
* 1 ms increments call with desired time period in *
* acc. Routine implements a 1 ms loop.             *
* Uses Output Compare 4                            *
****************************************************
*
time1ms staa    st_lo
        ldx     #REGBS
timer2  ldd     TCNT,X
        addd    #1*MSec*PRESCALE
        std     TOC4,X
        ldaa    #OC4F
        staa    TFLG1,X
*
timeloop:
        brclr   TFLG1,X,OC4F,timeloop
        ldaa    #OC4F
        staa    TFLG1,X
        dec     st_lo           ;dec count on each underflow
        bne     timer2          ;exit when time_count = 0
        rts
*
wait1sec:
        bsr     wait400ms
        bsr     wait400ms
        bra     wait200ms
wait400ms:
        bsr     wait200ms
wait200ms:
        bsr     wait100ms
*
*       wait for 100 milliseconds
wait100ms:
        psha
        ldaa    #100
        bsr     time1ms
        pula
        rts
*
*       wait for 10 milliseconds
wait10ms:
        psha
        ldaa    #10
        bsr     time1ms
        pula
        rts
*       wait for 1 millisecond
wait1ms:
        psha
        ldaa    #1
        bsr     time1ms
        pula
        rts
*
*
**********************************
*
SPIINT  LDAA    #$2F
        STAA    PORTD+REGBS
        LDAA    #%00111000
        STAA    DDRD+REGBS
        LDAA    #%01011000              ;enable master SPI
                                        ;CPOL=1  CPHA=0
        STAA    SPCR+REGBS
        NOP
        LDAA    SPDR+REGBS              ;dummy data
        RTS
*
*
COPRESET PSHA
         LDAA   #$55                    ;reset cop
         STAA   COPRST+REGBS
         LDAA   #$AA
         STAA   COPRST+REGBS
         PULA
         RTS
*
	end
