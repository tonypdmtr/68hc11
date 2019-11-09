;-------------------------------------------------------------------------------
;
;               Motorola 68HC11E series I/O register definitions
;
;-------------------------------------------------------------------------------
;
; The symbols defined in this file conform to Motorola's naming conventions.
;
; Symbol suffix denotes usage:
;
; None    Extended:
;         LDAA      PORTA
;
; "_"     Indirect using index register:
;         LDAA      PORTA_,x
;
; "."     Bit mask for individual bits within a register:
;         BCLR      PORTA_,x,PA6.
;
;-------------------------------------------------------------------------------
IOBase              equ       $1000               ; I/O register base address

; Port A

PORTA_              equ       $00                 ; Port A (8-bit offset address)
PORTA               equ       PORTA_+IOBase       ; Port A (16-bit absolute address)
PA0.                equ       $01                 ; Port A register bits
PA1.                equ       $02
PA2.                equ       $04
PA3.                equ       $08
PA4.                equ       $10
PA5.                equ       $20
PA6.                equ       $40
PA7.                equ       $80

; Port B

PORTB_              equ       $04                 ; Port B
PORTB               equ       PORTB_+IOBase
PB0.                equ       $01
PB1.                equ       $02
PB2.                equ       $04
PB3.                equ       $08
PB4.                equ       $10
PB5.                equ       $20
PB6.                equ       $40
PB7.                equ       $80

; Port C

PORTC_              equ       $03                 ; Port C
PORTC               equ       PORTC_+IOBase
PC0.                equ       $01
PC1.                equ       $02
PC2.                equ       $04
PC3.                equ       $08
PC4.                equ       $10
PC5.                equ       $20
PC6.                equ       $40
PC7.                equ       $80

DDRC_               equ       $07                 ; Port C data direction
DDRC                equ       DDRC_+IOBase
DDC0.               equ       $01
DDC1.               equ       $02
DDC2.               equ       $04
DDC3.               equ       $08
DDC4.               equ       $10
DDC5.               equ       $20
DDC6.               equ       $40
DDC7.               equ       $80

PIOC_               equ       $02                 ; Port C I/O control
PIOC                equ       PIOC_+IOBase
INVB.               equ       $01
EGA.                equ       $02
PLS.                equ       $04
OIN.                equ       $08
HNDS.               equ       $10
CWOM.               equ       $20
STAI.               equ       $40
STAF.               equ       $80

PORTCL_             equ       $05                 ; Port C latch
PORTCL              equ       PORTCL_+IOBase
PCL0.               equ       $01
PCL1.               equ       $02
PCL2.               equ       $04
PCL3.               equ       $08
PCL4.               equ       $10
PCL5.               equ       $20
PCL6.               equ       $40
PCL7.               equ       $80

; Port D

PORTD_              equ       $08                 ; Port D
PORTD               equ       PORTD_+IOBase
PD0.                equ       $01
PD1.                equ       $02
PD2.                equ       $04
PD3.                equ       $08
PD4.                equ       $10
PD5.                equ       $20

DDRD_               equ       $09                 ; Port D data direction
DDRD                equ       DDRD_+IOBase
DDD0.               equ       $01
DDD1.               equ       $02
DDD2.               equ       $04
DDD3.               equ       $08
DDD4.               equ       $10
DDD5.               equ       $20

; Port E

PORTE_              equ       $0A                 ; Port E
PORTE               equ       PORTE_+IOBase
PE0.                equ       $01
PE1.                equ       $02
PE2.                equ       $04
PE3.                equ       $08
PE4.                equ       $10
PE5.                equ       $20
PE6.                equ       $40
PE7.                equ       $80

; Timer: Output compare control

CFORC_              equ       $0B                 ; Timer output compare force
CFORC               equ       CFORC_+IOBase
FOC1.               equ       $80
FOC2.               equ       $40
FOC3.               equ       $20
FOC4.               equ       $10
FOC5.               equ       $08

OC1M_               equ       $0C                 ; Output compare #1 mask
OC1M                equ       OC1M_+IOBase
OC1M3.              equ       $08
OC1M4.              equ       $10
OC1M5.              equ       $20
OC1M6.              equ       $40
OC1M7.              equ       $80

OC1D_               equ       $0D                 ; Output compare #1 data
OC1D                equ       OC1D_+IOBase
OC1D3.              equ       $08
OC1D4.              equ       $10
OC1D5.              equ       $20
OC1D6.              equ       $40
OC1D7.              equ       $80

; Timer: counter

TCNT_               equ       $0E                 ; Timer counter
TCNT                equ       TCNT_+IOBase

; Timer: Input captures

TIC1_               equ       $10                 ; Timer input capture #1 [PA2]
TIC1                equ       TIC1_+IOBase

TIC2_               equ       $12                 ; Timer input capture #2 [PA1]
TIC2                equ       TIC2_+IOBase

TIC3_               equ       $14                 ; Timer input capture #3 [PA0]
TIC3                equ       TIC3_+IOBase

TIC4_               equ       $1E                 ; Timer input capture #4 [PA3]
TIC4                equ       TIC4_+IOBase        ; Note: same location as TOC5

; Timer: output compares

TOC1_               equ       $16                 ; Timer output compare #1 [PA4-PA7]
TOC1                equ       TOC1_+IOBase

TOC2_               equ       $18                 ; Timer output compare #2 [PA6]
TOC2                equ       TOC2_+IOBase

TOC3_               equ       $1A                 ; Timer output compare #3 [PA5]
TOC3                equ       TOC2_+IOBase

TOC4_               equ       $1C                 ; Timer output compare #4 [PA4]
TOC4                equ       TOC2_+IOBase

TOC5_               equ       $1E                 ; Timer output compare #5 [PA3]
TOC5                equ       TOC2_+IOBase        ; Note: same location as TIC4

; Timer: control & interrupts

TCTL1_              equ       $20                 ; Timer control register 1
TCTL1               equ       TCTL1_+IOBase
OL5.                equ       $01
OM5.                equ       $02
OL4.                equ       $04
OM4.                equ       $08
OL3.                equ       $10
OM3.                equ       $20
OL2.                equ       $40
OM2.                equ       $80

TCTL2_              equ       $21                 ; Timer control register 2
TCTL2               equ       TCTL2_+IOBase
EDG3A.              equ       $01
EDG3B.              equ       $02
EDG2A.              equ       $04
EDG2B.              equ       $08
EDG1A.              equ       $10
EDG1B.              equ       $20
EDG4A.              equ       $40
EDG4B.              equ       $80

TMSK1_              equ       $22                 ; Timer interrupt mask register 1
TMSK1               equ       TMSK1_+IOBase
IC3I.               equ       $01
IC2I.               equ       $02
IC1I.               equ       $04
IC4I.               equ       $08                 ; (alias:OC5I)
OC5I.               equ       $08                 ; (alias:IC4I)
OC4I.               equ       $10
OC3I.               equ       $20
OC2I.               equ       $40
OC1I.               equ       $80

TFLG1_              equ       $23                 ; Timer interrupt flag register 1
TFLG1               equ       TFLG1_+IOBase
IC3F.               equ       $01
IC2F.               equ       $02
IC1F.               equ       $04
IC4F.               equ       $08                 ; (alias:OC5F)
OC5F.               equ       $08                 ; (alias:IC4F)
OC4F.               equ       $10
OC3F.               equ       $20
OC2F.               equ       $40
OC1F.               equ       $80

TMSK2_              equ       $24                 ; Timer interrupt mask register 2
TMSK2.              equ       TMSK2_+IOBase
PR0.                equ       $01
PR1.                equ       $02
PAII.               equ       $10
PAOVI.              equ       $20
RTII.               equ       $40
TOI.                equ       $80

TFLG2_              equ       $25                 ; Timer interrupt flag register 2
TFLG2               equ       TFLG2_+IOBase
PAIF.               equ       $10
PAOVF.              equ       $20
RTIF.               equ       $40
TOF.                equ       $80

; Pulse accumulator

PACTL_              equ       $26                 ; Pulse accumulator, port A control
PACTL               equ       PACTL_+IOBase
RTR0.               equ       $01
RTR1.               equ       $02
I4O5.               equ       $04
DDRA3.              equ       $08
PEDGE.              equ       $10
PAMOD.              equ       $20
PAEN.               equ       $40
DDRA7.              equ       $80

PACNT_              equ       $27                 ; Pulse accumulator counter
PACNT               equ       PACNT_+IOBase

; Serial Peripheral Interface (SPI)

SPCR_               equ       $28                 ; SPI configuration register
SPCR                equ       IOBase+SPCR_
SPR0.               equ       $01
SPR1.               equ       $02
CPHA.               equ       $04
CPOL.               equ       $08
MSTR.               equ       $10
DWOM.               equ       $20
SPE.                equ       $40
SPIE.               equ       $80

SPSR_               equ       $29                 ; SPI status register
SPSR                equ       IOBase+SPSR_
MODF.               equ       $10
WCOL.               equ       $40
SPIF.               equ       $80

SPDR_               equ       $2A                 ; SPI data register
SPDR                equ       IOBase+SPDR_

; Serial Communications Interface (SCI)

BAUD_               equ       $2B                 ; SCI baud rate control
BAUD                equ       IOBase+BAUD_
SCR0.               equ       $01
SCR1.               equ       $02
SCR2.               equ       $04
RCKB.               equ       $08
SCP0.               equ       $10
SCP1.               equ       $20
SCP2.               equ       $40                 ; (E20 ONLY)
TCLR.               equ       $80

SCCR1_              equ       $2C                 ; SCI configuration register 1
SCCR1               equ       IOBase+SCCR1_
WAKE.               equ       $08
M.                  equ       $10
T8.                 equ       $40
R8.                 equ       $80

SCCR2_              equ       $2D                 ; SCI configuration ragister 2
SCCR2               equ       IOBase+SCCR2_
SBK.                equ       $01
RWU.                equ       $02
RE.                 equ       $04
TE.                 equ       $08
ILIE.               equ       $10
RIE.                equ       $20
TCIE.               equ       $40
TIE.                equ       $80

SCSR_               equ       $2E                 ; SCI status register
SCSR                equ       IOBase+SCSR_
FE.                 equ       $02
NF.                 equ       $04
OR.                 equ       $08
IDLE.               equ       $10
RDRF.               equ       $20
TC.                 equ       $40
TDRE.               equ       $80

SCDR_               equ       $2F                 ; SCI data register
SCDR                equ       IOBase+SCDR_

; Analog-to-digital converter

ADCTL_              equ       $30                 ; A/D converter control
ADCTL               equ       IOBase+ADCTL_
CA.                 equ       $01
CB.                 equ       $02
CC.                 equ       $04
CD.                 equ       $08
MULT.               equ       $10
SCAN.               equ       $20
CCF.                equ       $80

ADR1_               equ       $31                 ; A/D converter result #1
ADR1                equ       IOBase+ADR1_

ADR2_               equ       $32                 ; A/D converter result #2
ADR2                equ       IOBase+ADR2_

ADR3_               equ       $33                 ; A/D converter result #3
ADR3                equ       IOBase+ADR3_

ADR4_               equ       $34                 ; A/D converter result #4
ADR4                equ       IOBase+ADR4_

; Nonvolatile memory programming control

BPROT_              equ       $35                 ; EEPROM protection control
BPROT               equ       IOBase+BPROT_
BPRT0.              equ       $01
BPRT1.              equ       $02
BPRT2.              equ       $04
BPRT3.              equ       $08
PTCON.              equ       $10

EPROG_              equ       $36                 ; EPROM programming control - E20 ONLY
EPROG               equ       IOBase+EPROG_
PGM.                equ       $01
T0.                 equ       $02
T1.                 equ       $04
EXROW.              equ       $08
EXCOL.              equ       $10
ELATE.              equ       $20                 ; Note - non standard name
MBE.                equ       $80

PPROG_              equ       $3B                 ; EEPROM programming control
PPROG               equ       IOBase+PPROG_
EPGM.               equ       $01
EELAT.              equ       $02
ERASE.              equ       $04
ROW.                equ       $08
BYTE.               equ       $10
ELAT.               equ       $20
EVEN.               equ       $40
ODD.                equ       $80

; CPU configuration & misc.

OPTION_             equ       $39                 ; Misc. options register
OPTION              equ       IOBase+OPTION_
CR0.                equ       $01
CR1.                equ       $02
CME.                equ       $08
DLY.                equ       $10
IRQE.               equ       $20
CSEL.               equ       $40
ADPU.               equ       $80

COPRST_             equ       $3A                 ; COP watchdog timer reset
COPRST              equ       IOBase+COPRST_

HPRIO_              equ       $3C                 ; Interrupt priority/MCU options
HPRIO               equ       IOBase+HPRIO_
PSEL0.              equ       $01
PSEL1.              equ       $02
PSEL2.              equ       $04
PSEL3.              equ       $08
IRVNE.              equ       $10
MDA.                equ       $20
SMOD.               equ       $40
RBOOT.              equ       $80

INIT_               equ       $3D                 ; RAM and I/O register mapping control
INIT                equ       IOBase+INIT_
REG0.               equ       $01
REG1.               equ       $02
REG2.               equ       $04
REG3.               equ       $08
RAM0.               equ       $10
RAM1.               equ       $20
RAM2.               equ       $40
RAM3.               equ       $80

TEST1_              equ       $3E                 ; Factory test register
TEST1               equ       IOBase+TEST1_
TCON.               equ       $01
FCOP.               equ       $02
FCM.                equ       $04
DISR.               equ       $08
CBYP.               equ       $10
OCCR.               equ       $20
TILOP.              equ       $80

CONFIG_             equ       $3F                 ; MCU configuration register (in EEPROM)
CONFIG              equ       IOBase+CONFIG_
EEON.               equ       $01
ROMON.              equ       $02
NOCOP.              equ       $04
NOSEC.              equ       $08
EE0.                equ       $10                 ; (E2 ONLY)
EE1.                equ       $20                 ; (E2 ONLY)
EE2.                equ       $40                 ; (E2 ONLY)
EE3.                equ       $80                 ; (E2 ONLY)

; CPU CCR flags

C.                  equ       $01                 ; Carry
V.                  equ       $02                 ; Overflow
Z.                  equ       $04                 ; Zero
N.                  equ       $08                 ; Negative
I.                  equ       $10                 ; I-interrupt mask
H.                  equ       $20                 ; Half-carry
X.                  equ       $40                 ; X-interrupt mask
S.                  equ       $80                 ; STOP disable
