;*******************************************************************************
; Define Register Locations for ASM Portion of Code
;*******************************************************************************

REG_BASE            equ       $1000

PORTA               equ       REG_BASE+$0000
PIOC                equ       REG_BASE+$0002
PORTC               equ       REG_BASE+$0003
PORTB               equ       REG_BASE+$0004
PORTCL              equ       REG_BASE+$0005
DDRC                equ       REG_BASE+$0007
PORTD               equ       REG_BASE+$0008
DDRD                equ       REG_BASE+$0009
PORTE               equ       REG_BASE+$000A
CFORC               equ       REG_BASE+$000B
OC1M                equ       REG_BASE+$000C
OC1D                equ       REG_BASE+$000D
TCNT                equ       REG_BASE+$000E
TIC1                equ       REG_BASE+$0010
TIC2                equ       REG_BASE+$0012
TIC3                equ       REG_BASE+$0014
TOC1                equ       REG_BASE+$0016
TOC2                equ       REG_BASE+$0018
TOC3                equ       REG_BASE+$001A
TOC4                equ       REG_BASE+$001C
TOC5                equ       REG_BASE+$001E
TCTL1               equ       REG_BASE+$0020
TCTL2               equ       REG_BASE+$0021
TMSK1               equ       REG_BASE+$0022
TFLG1               equ       REG_BASE+$0023
TMSK2               equ       REG_BASE+$0024
TFLG2               equ       REG_BASE+$0025
PACTL               equ       REG_BASE+$0026
PACNT               equ       REG_BASE+$0027
SPCR                equ       REG_BASE+$0028
SPSR                equ       REG_BASE+$0029
SPDR                equ       REG_BASE+$002A
BAUD                equ       REG_BASE+$002B
SCCR1               equ       REG_BASE+$002C
SCCR2               equ       REG_BASE+$002D
SCSR                equ       REG_BASE+$002E
SCDR                equ       REG_BASE+$002F
ADCTL               equ       REG_BASE+$0030
ADR1                equ       REG_BASE+$0031
ADR2                equ       REG_BASE+$0032
ADR3                equ       REG_BASE+$0033
ADR4                equ       REG_BASE+$0034
OPTION              equ       REG_BASE+$0039
COPRST              equ       REG_BASE+$003A
PPROG               equ       REG_BASE+$003B
HPRIO               equ       REG_BASE+$003C
INIT                equ       REG_BASE+$003D
TEST1               equ       REG_BASE+$003E
CONFIG              equ       REG_BASE+$003F
