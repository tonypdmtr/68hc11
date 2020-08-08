;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;                            CLIGNOTE.ASM
;
;                   Flash a LED on PB0 at a frequency of 1 Hz
;*******************************************************************************

REGS                def       0
                    #ListOff
                    #Uses     811e2.inc
                    #ListOn

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       EEPROM

Start               proc
                    lda       #$10
                    sta       INIT                ; RAM at $1000, Registers at $0000
                    sei                           ; inhibits interrupts
                    lds       #STACKTOP           ; initialize stack pointer

Loop@@              bset      PORTB,%00000001     ; allumage de la led

                    lda       #5
                    bsr       Delay               ; temporisation de 0,5 seconde

                    bclr      PORTB,%00000001     ; extinction de la led

                    lda       #5
                    bsr       Delay               ; temporisation de 0,5 seconde

                    bra       Loop@@              ; recommence

;*******************************************************************************
; Purpose: Delay of duration RegA * 0.1s
; Input  : A = delay factor
; Output : None
; Note(s):
                    #Cycles

Delay               proc
                    pshx
                              #Cycles
MainLoop@@          ldx       #DELAY@@
                              #Cycles
Loop@@              dex
                    bne       Loop@@
                              #temp :cycles
                    deca
                    bne       MainLoop@@
                    pulx
                    rts

DELAY@@             equ       100*BUS_KHZ-:cycles-:ocycles/:temp

;*******************************************************************************

                    #VECTORS
                    org       VECTORS

SCI_VECT            dw        Start
SPI_VECT            dw        Start
PAI_VECT            dw        Start
PAO_VECT            dw        Start
TOF_VECT            dw        Start

TOC5_VECT           dw        Start
TOC4_VECT           dw        Start
TOC3_VECT           dw        Start
TOC2_VECT           dw        Start
TOC1_VECT           dw        Start
TIC3_VECT           dw        Start
TIC2_VECT           dw        Start
TIC1_VECT           dw        Start

RTI_VECT            dw        Start
IRQ_VECT            dw        Start
XIRQ_VECT           dw        Start
SWI_VECT            dw        Start
TRAP_VECT           dw        Start
COP_FAIL_VECT       dw        Start
COP_CMF_VECT        dw        Start
RESET_VECT          dw        Start

                    end       :s19crc
