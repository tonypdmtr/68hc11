;*******************************************************************************
; Define Register Locations for ASM Portion of Code
;*******************************************************************************

REGS                def       $1000

PORTA               equ       REGS+$0000
PIOC                equ       REGS+$0002
PORTC               equ       REGS+$0003
PORTB               equ       REGS+$0004
PORTCL              equ       REGS+$0005
DDRC                equ       REGS+$0007
PORTD               equ       REGS+$0008
DDRD                equ       REGS+$0009
PORTE               equ       REGS+$000A
CFORC               equ       REGS+$000B
OC1M                equ       REGS+$000C
OC1D                equ       REGS+$000D
TCNT                equ       REGS+$000E
TIC1                equ       REGS+$0010
TIC2                equ       REGS+$0012
TIC3                equ       REGS+$0014
TOC1                equ       REGS+$0016
TOC2                equ       REGS+$0018
TOC3                equ       REGS+$001A
TOC4                equ       REGS+$001C
TOC5                equ       REGS+$001E
TCTL1               equ       REGS+$0020
TCTL2               equ       REGS+$0021
TMSK1               equ       REGS+$0022
TFLG1               equ       REGS+$0023
TMSK2               equ       REGS+$0024
TFLG2               equ       REGS+$0025
PACTL               equ       REGS+$0026
PACNT               equ       REGS+$0027
SPCR                equ       REGS+$0028
SPSR                equ       REGS+$0029
SPDR                equ       REGS+$002A
BAUD                equ       REGS+$002B
SCCR1               equ       REGS+$002C
SCCR2               equ       REGS+$002D
SCSR                equ       REGS+$002E
SCDR                equ       REGS+$002F
ADCTL               equ       REGS+$0030
ADR1                equ       REGS+$0031
ADR2                equ       REGS+$0032
ADR3                equ       REGS+$0033
ADR4                equ       REGS+$0034
OPTION              equ       REGS+$0039
COPRST              equ       REGS+$003A
PPROG               equ       REGS+$003B
HPRIO               equ       REGS+$003C
INIT                equ       REGS+$003D
TEST1               equ       REGS+$003E
CONFIG              equ       REGS+$003F
