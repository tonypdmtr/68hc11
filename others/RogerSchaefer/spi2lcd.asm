;*******************************************************************************
;                       SPI to LCD
;
; Program to control a character module LCD in the 4 bit mode
; with a MC68HC11 SPI connected to a 74HC595 8 bit shift register
; Qa to Qd of 74HC595 connected to DB4 to DB7 of LCD
; Qe to RS     Qf to E of LCD    R/W pulled to ground
;
; by Roger Schaefer May 2000
; Refactored and optimized [-33 bytes] by Tony Papadimitriou
;*******************************************************************************

#ifmain ;-----------------------------------------------------------------------
                    #ListOff
                    #Uses     mcu.inc
                    #ListOn
#endif ;------------------------------------------------------------------------

;*******************************************************************************
; LCD COMMANDS
;*******************************************************************************

CLEAR_DISPLAY       equ       %00000001
ENTRY_MODE          equ       %00000110
FUNCTION_SET        equ       %00101000           ; set this for type of LCD
          ;--------------------------------------
OC4                 equ       4
OC4F                equ       1<OC4

LCD_LATCH           equ       OC4                 ; of Port A
PRESCALER           equ       4                   ; if PR1:PR0 = 01 set to 4
MSEC                equ       BUS_KHZ/PRESCALER/2 ; @ 2MHz clock use 125

LCD_RS              equ       %00010000
LCD_E               equ       %00100000
LCD_E_LO            equ       %11011111

;*******************************************************************************
                    #RAM
;*******************************************************************************

linechar            rmb       1
lineno              rmb       1
lcd_data            rmb       1

;*******************************************************************************
                    #ROM
;*******************************************************************************
          #ifdef
TEST                bra       LCD_Test
LCD_initialize      bra       LCD_Init
LCD_enter           bra       LCD_PutChar         ; enter here with character in B
          #endif
;*******************************************************************************

LCD_Test            proc
                    bsr       LCD_Init
                    ldx       #Msg@@
                    bra       LCD_WriteZ
;-------------------------------------------------------------------------------
; Test message
;-------------------------------------------------------------------------------
Msg@@               fcc       'HELLO WORLD',CR
                    fcc       'HAVE A NICE DAY',CR
                    fcc       'LINE THREE',CR
                    fcs       'LINE FOUR'

;*******************************************************************************
; LCD INTERFACE
;*******************************************************************************

;*******************************************************************************
; Initialize LCD to correct settings

LCD_Init            proc
                    jsr       SPI_Init
                    jsr:2     Wait10ms            ; wait for LCD to settle
                    bsr:3     ?8_bit_mode         ; send Function Set three times
                    ldb       #%00000010          ; set 4-bit
                    bsr       ?4_bit_mode
                    ldb       #FUNCTION_SET
                    bsr       ?LCD_Command
                    ldb       #%00001000          ; display off
                    bsr       ?LCD_Command
                    ldb       #%00001110          ; display on
                    bsr       ?LCD_Command
;                   bra       LCD_Clear

;*******************************************************************************
; Clear LCD and reset cursor

LCD_Clear           proc
                    clr       linechar
                    clr       lineno
                    pshb
                    ldb       #CLEAR_DISPLAY      ; clear & home
                    bsr       ?LCD_Command
                    ldb       #ENTRY_MODE
                    bsr       ?LCD_Command
                    pulb
                    rts

;*******************************************************************************

LCD_WriteZ          proc                          ; send string to LCD
Loop@@              ldb       ,x
                    beq       Done@@
                    pshx
                    bsr       LCD_PutChar
                    pulx
                    inx
                    bra       Loop@@
Done@@              equ       :AnRTS

;*******************************************************************************
; Write character in Reg B to LCD

LCD_PutChar         proc
                    cmpb      #LF                 ; trap line feed
                    beq       Done@@
                    cmpb      #BS                 ; chk for backspace
                    beq       Backspace@@
                    cmpb      #CR                 ; new line?
                    bne       ?LCD_Data           ; write printable character to LCD
                    bra       DoNewLine
Backspace@@         jmp       DoBackspace         ; perform LCD backspace
Done@@              equ       :AnRTS

;*******************************************************************************

?8_bit_mode         proc
                    ldb       #%11
;                   bra       ?4_bit_mode

;*******************************************************************************

?4_bit_mode         proc
                    stb       lcd_data            ; send byte to LCD
                    bclr      lcd_data,LCD_RS     ; set rs low
                    bsr       SPI_Send
                    jmp       Wait10ms

;*******************************************************************************

SPI_Send            proc
                    bset      lcd_data,LCD_E      ; set enable high
                    pshb
                    ldb       lcd_data
                    bsr       SPI_PutChar
                    andb      #LCD_E_LO           ; set LCD E low
                    bsr       SPI_PutChar
                    orb       #LCD_E              ; set LCD back high
                    bsr       SPI_PutChar
                    pulb
                    rts

;*******************************************************************************

SPI_PutChar         proc
                    pshx
                    ldx       #REGS
                    stb       [SPDR,x             ; enter with data in B
                    brclr     [SPSR,x,SPIF.,*     ; wait
                    bset      [PORTA,x,LCD_LATCH  ; shift to output latches
                    jsr       COPRESET            ; kill some time
                    bclr      [PORTA,x,LCD_LATCH
                    pulx
                    rts

;*******************************************************************************

?LCD_Command        proc
                    psha
                    clra
                    bsr       ?LCD_WriteByte
                    pula
                    bra       Wait10ms

;*******************************************************************************
; Write character to LCD
; enter with character in Reg B

?LCD_Data           proc
                    lda       #LCD_RS
                    bsr       ?LCD_WriteByte
                    !...      could put code for horizontal scroll here
                    rts

;*******************************************************************************

?LCD_WriteByte      proc
                    pshd
                    pshb
                    lsrb:4
                    aba                           ; set rs high or low
                    pulb
                    sta       lcd_data            ; msb byte to LCD
                    bsr       SPI_Send
                    bsr       Delay
                    andb      #$0f                ; keep 4 lsb
                    aba                           ; set rs high or low
                    sta       lcd_data            ; lsb byte to LCD
                    puld
                    bsr       SPI_Send
;                   bra       Delay               ; wait for LCD

;*******************************************************************************
; Short software delay, approx 120 us
                              #Cycles
Delay               proc
                    pshx
                    ldx       #DELAY@@
                              #Cycles
Loop@@              dex
                    bne       Loop@@
                              #temp :cycles
                    pulx
                    rts

DELAY@@             equ       120*BUS_KHZ/1000-:cycles-:ocycles/:temp

;*******************************************************************************
; Send new line command to LCD

DoNewLine           proc
                    pshb
                    clr       linechar
                    pshx
                    ldx       #StartOfLine@@
                    ldb       lineno
                    abx
                    ldb       ,x
                    pulx
                    bsr       ?LCD_Command
                    ldb       #ENTRY_MODE
                    bsr       ?LCD_Command
                    ldb       lineno
                    incb
                    andb      #%11
                    stb       lineno
                    pulb
                    rts

StartOfLine@@       fcb       $c0,$94,$d4,$80

;*******************************************************************************
; Backspace cursor & overwrite char

DoBackspace         proc
                    lda       #$10                ; move cursor 1 place left
                    bsr       ?LCD_Command
                    ldb       #' '                ; overwrite char
                    bsr       ?LCD_Data
                    lda       #$10                ; move cursor 1 place left
                    bsr       ?LCD_Command
          ;--------------------------------------
                    ldb       #' '                ; overwrite char
                    dec       linechar
                    bpl       Done@@
                    clr       linechar
Done@@              rts

;*******************************************************************************
; Delay loops
;*******************************************************************************

Wait1sec            proc
                    bsr       Wait400ms
                    bsr       Wait200ms
Wait400ms           bsr       Wait200ms
Wait200ms           bsr       Wait100ms
;                   bra       Wait100ms

;*******************************************************************************
; Wait for 100 milliseconds

Wait100ms           proc
                    psha
                    lda       #100
                    bsr       Delay1ms
                    pula
                    rts

;*******************************************************************************
; Wait for 10 milliseconds

Wait10ms            proc
                    psha
                    lda       #10
                    bsr       Delay1ms
                    pula
                    rts

;*******************************************************************************
; Wait for 1 millisecond

Wait1ms             proc
                    psha
                    lda       #1
                    bsr       Delay1ms
                    pula
                    rts

;*******************************************************************************
; General purpose delay with 1 ms increments
; Call with desired time period in RegA
; Uses Output Compare 4

Delay1ms            proc
                    pshd
          ;--------------------------------------
          #ifz REGS
Loop@@              psha
                    ldd       TCNT
                    addd      #MSEC
                    std       TOC4
                    lda       #OC4F
                    sta       TFLG1
                    brclr     TFLG1,OC4F,*
                    sta       TFLG1
                    pula
                    deca                          ; dec count on each underflow
                    bne       Loop@@              ; exit when time_count = 0
          #else
                    pshx
                    ldx       #REGS
Loop@@              psha
                    ldd       [TCNT,x
                    addd      #MSEC
                    std       [TOC4,x
                    lda       #OC4F
                    sta       [TFLG1,x
                    brclr     [TFLG1,x,OC4F,*
                    sta       [TFLG1,x
                    pula
                    deca                          ; dec count on each underflow
                    bne       Loop@@              ; exit when time_count = 0
                    pulx
          #endif
          ;--------------------------------------
                    puld
                    rts

;*******************************************************************************

SPI_Init            proc
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
