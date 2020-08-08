;*******************************************************************************
;* File      : EVBU.INC
;* Purpose   : INCLUDE file for EVBU boards (M68HC11E9 with BUFFALO)
;* Author    : Tony Papadimitriou <tonyp@acm.org>
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************

                    #Uses     macros.inc
                    #Message  ************************
                    #Message  * Target: EVBU/Buffalo *
                    #Message  ************************

__EVBU__            def       *                   ;Simply tells us this INCLUDE has been used (use with #ifdef __EVBU__)

REGS                equ       $1000               ;Register Base Address

RAM                 equ       0                   ;beginning of available RAM
RAM_END             equ       $47                 ;end of available RAM

STACKTOP            equ       RAM_END             ;Top of Stack

ROM                 equ       $100                ;RAM used for program storage
ROM_END             equ       $1FF                ;end of program storage RAM

EEPROM              equ       $B600               ;beginning of user EEPROM
EEPROM_END          equ       $B7FF               ;end of user EEPROM

VECTORS             equ       $C4                 ;beginning of user JMP vectors
VECTORS_END         equ       $FF                 ;end of user JMP vectors

; The following MEMORY directives help you contain your code within the
; available RAM space when using EVBU.

                    #MEMORY   ROM ROM_END
                    #MEMORY   EEPROM EEPROM_END
                    #MEMORY   VECTORS VECTORS|$00FF

                    #Uses     common.inc

; Register definitions

PORTA               equ       REGS+$00,1          ;Port A Data
PIOC                equ       REGS+$02,1          ;Parallel IO Control
PORTC               equ       REGS+$03,1          ;Port C Data
PORTB               equ       REGS+$04,1          ;Port B Data
PORTCL              equ       REGS+$05,1          ;Port C Latched Data
DDRC                equ       REGS+$07,1          ;Data Direction Register C
PORTD               equ       REGS+$08,1          ;Port D Data
DDRD                equ       REGS+$09,1          ;Data Direction Register D
PORTE               equ       REGS+$0A,1          ;Port E Data
CFORC               equ       REGS+$0B,1          ;Timer Compare Force
OC1M                equ       REGS+$0C,1          ;Output Compare 1 Mask
OC1D                equ       REGS+$0D,1          ;Output Compare 1 Data
TCNT                equ       REGS+$0E,2          ;Timer Count
TIC1                equ       REGS+$10,2          ;Timer Input Capture 1
TIC2                equ       REGS+$12,2          ;Timer Input Capture 2
TIC3                equ       REGS+$14,2          ;Timer Input Capture 3
TOC1                equ       REGS+$16,2          ;Timer Output Compare 1
TOC2                equ       REGS+$18,2          ;Timer Output Compare 2
TOC3                equ       REGS+$1A,2          ;Timer Output Compare 3
TOC4                equ       REGS+$1C,2          ;Timer Output Compare 4
TIC4                equ       REGS+$1E,2          ;Timer Input Capture 4
TOC5                equ       REGS+$1E,2          ;Timer Output Compare 5
TCTL1               equ       REGS+$20,1          ;Timer Control 1
TCTL2               equ       REGS+$21,1          ;Timer Control 2
TMSK1               equ       REGS+$22,1          ;Timer Interrupt Mask 1
TFLG1               equ       REGS+$23,1          ;Timer Interrupt Flag 1
TMSK2               equ       REGS+$24,1          ;Timer Interrupt Mask 2
TFLG2               equ       REGS+$25,1          ;Timer Interrupt Flag 2
PACTL               equ       REGS+$26,1          ;Pulse Accumulator Control
PACNT               equ       REGS+$27,1          ;Pulse Accumulator Counter
SPCR                equ       REGS+$28,1          ;SPI Control Register
SPSR                equ       REGS+$29,1          ;SPI Status Register
SPDR                equ       REGS+$2A,1          ;SPI Data Register
BAUD                equ       REGS+$2B,1          ;Baud Rate Control Register
SCCR1               equ       REGS+$2C,1          ;SCI Control 1
SCCR2               equ       REGS+$2D,1          ;SCI Control 2
SCSR                equ       REGS+$2E,1          ;SCI Status Register
SCDR                equ       REGS+$2F,1          ;SCI Data Register
ADCTL               equ       REGS+$30,1          ;AD Control Status Register
ADR1                equ       REGS+$31,1          ;AD Result 1
ADR2                equ       REGS+$32,1          ;AD Result 2
ADR3                equ       REGS+$33,1          ;AD Result 3
ADR4                equ       REGS+$34,1          ;AD Result 4
BPROT               equ       REGS+$35,1          ;Block Protect
EPROG               equ       REGS+$36,1          ;EPROG (E20 only, not E9)
OPTION              equ       REGS+$39,1          ;System Configuration Options
COPRST              equ       REGS+$3A,1          ;COP Reset
PPROG               equ       REGS+$3B,1          ;EEPROM Programming Control
HPRIO               equ       REGS+$3C,1          ;Highest Interrupt Priority
INIT                equ       REGS+$3D,1          ;INIT
XINIT               equ       $103D,1             ;Out-of-reset INIT
CONFIG              equ       REGS+$3F,1          ;Configuration Register

          ; BUFFALO USER CALLABLE SUBROUTINES (eg. JSR .BPCLR)

                    #temp     $FF7C

.WARMST             next      :temp,3             ;warm start
.BPCLR              next      :temp,3             ;clear breakpoint table
.RPRINT             next      :temp,3             ;display user registers
.HEXBIN             next      :temp,3             ;convert ascii hex char to binary
.BUFFAR             next      :temp,3             ;build hex argument from buffer
.TERMAR             next      :temp,3             ;read hex argument from terminal
.CHGBYT             next      :temp,3             ;modify memory at address in x
.READBU             next      :temp,3             ;read character from buffer
.INCBUF             next      :temp,3             ;increment buffer pointer
.DECBUF             next      :temp,3             ;decrement buffer pointer
.WSKIP              next      :temp,3             ;find non-whitespace char in buffer
.CHKABR             next      :temp,3             ;check for abort from terminal
.UPCASE             next      :temp,3             ;convert to upper case
.WCHEK              next      :temp,3             ;check for white space
.DCHEK              next      :temp,3             ;check for delimeter
.INIT               next      :temp,3             ;initialize i/o device
.INPUT              next      :temp,3             ;low level input routine
.OUTPUT             next      :temp,3             ;low level output routine
.OUTLHL             next      :temp,3             ;display top 4 bits as hex digit
.OUTRHL             next      :temp,3             ;display bottom 4 bits as hex digit
.OUTA               next      :temp,3             ;output ascii character in A
.OUT1BY             next      :temp,3             ;display the hex value of byte at X
.OUT1BS             next      :temp,3             ;out1byt followed by space
.OUT2BS             next      :temp,3             ;display 2 hex bytes at x and a space
.OUTCRL             next      :temp,3             ;carriage return, line feed to terminal
.OUTSTRG            next      :temp,3             ;display string at X (term with $04)
.OUTST0             next      :temp,3             ;outstrg with no initial carr ret
.INCHAR             next      :temp,3             ;wait for and input a char from term
.VECINT             next      :temp,3             ;initialize RAM vector table

                    #VECTORS
                    org       VECTORS

; The following section puts the JMP opcodes in the pseudo-vector area
; so the user need only use DW LABEL instructions to fill the rest
; DWs must be spaced one byte apart from one another to skip over the JMPs
; You could delete this and use whole JMP LABEL instructions starting at
; #VECTORS with the above org in place.

?                   macro
                    fcb       JMP_OPCODE
                    org       *+2
                    mtop      20
                    endm

                    @?

                    org       Vsci+1              ;ready to accept first DW statement

                    #DATA
                    org       EEPROM              ;points to actual EEPROM

                    #RAM
                    org       RAM                 ;points to beginning of user RAM

                    #ROM
                    org       ROM                 ;points to RAM used for program storage
