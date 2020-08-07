;*******************************************************************************
;* RACE.ASM (Originally, CELEBSS.ASM)
;* Program for controlling and displaying dash information on two graphic LCDs
;*
;* Author wrote this originally for the 1987-1989 Chevy Celebrity.
;* Original board: 32KB RAM, 8KB ROM, 512B EEPROM, 8 channel 8-bit A/D.
;* LCD: Hitachi LMG7400PFLC 240x128 B/W, 123x68mm viewing area, 5V, -22V LCD,
;* HD61830 controller built-in.
;*
;* Author's page: http://www.fascinationsoftware.com/FS/html/RaceGuage.html
;*
;* Copyright (c) 2020, 2003 Richard Goedeken.
;* Modified for the ASPiSYS F1 Board by Tony G. Papadimitriou <tonyp@acm.org>
;* Assemble with ASM11 v9.65+ by Tony G. Papadimitriou <tonyp@acm.org>
;*
;* RACE.ASM is free software; you can redistribute it and/or modify
;* it under the terms of the GNU General Public License as published by
;* the Free Software Foundation; either version 2 of the License, or
;* (at your option) any later version.
;*
;* RACE.ASM is distributed in the hope that it will be useful,
;* but WITHOUT ANY WARRANTY; without even the implied warranty of
;* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;* GNU General Public License for more details.
;*
;* You should have received a copy of the GNU General Public License
;* along with this program; if not, write to the Free Software
;* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307
;*******************************************************************************

                    #ListOff
                    #Uses     exp-f1.inc
                    #ListOn

;*******************************************************************************
; EQUATES
;*******************************************************************************

REGS                def       $1000               ; start of registers
RAM                 def       $2000               ; start of data
STACKTOP            def       $3FFF               ; top of stack
ROM                 def       $E000               ; start of program
EEPROM              def       $B600               ; start of EEPROM

#Message  ROM: {ROM(h)}, RAM: {RAM(h)}, EEPROM: {EEPROM(h)}, STACK: {STACKTOP(h)}

                    #MEMORY   ROM $FFFF

PORTD               def       REGS+$08            ; Port D I/O Register
DDRD                def       REGS+$09            ; Data direction for port D
PORTE               def       REGS+$0A            ; port e
CFORC               def       REGS+$0B            ; force output compare
TCNT                def       REGS+$0E            ; timer count
TOC5                def       REGS+$1E            ; oc5 reg
TCTL1               def       REGS+$20            ; timer control 1
TMSK1               def       REGS+$22            ; timer mask 1
TFLG1               def       REGS+$23            ; timer flag 1
TMSK2               def       REGS+$24            ; timer mask 2
TFLG2               def       REGS+$25            ; timer flag 2
PACTL               def       REGS+$26            ; Pulse Accumulator Control
PACNT               def       REGS+$27            ; Pulse Accumulator Counter
SPCR                def       REGS+$28            ; SPI Control
BAUD                def       REGS+$2B            ; sci baud reg
SCCR1               def       REGS+$2C            ; sci control1 reg
SCCR2               def       REGS+$2D            ; sci control2 reg
SCSR                def       REGS+$2E            ; sci status reg
SCDAT               def       REGS+$2F            ; sci data reg

ADCTL               def       REGS+$30            ; A/D Control Register
ADR1                def       REGS+$31            ; A/D conversion result registers
ADR2                def       REGS+$32
ADR3                def       REGS+$33
ADR4                def       REGS+$34

BPROT               def       REGS+$35            ; block protect reg
OPTION              def       REGS+$39            ; option reg
COPRST              def       REGS+$3A            ; cop reset reg
PPROG               def       REGS+$3B            ; ee prog reg
HPRIO               def       REGS+$3C            ; hprio reg
CONFIG              def       REGS+$3F            ; config register

; Interrupt Vector Location

Vrti                def       $FFF0
Virq                def       $FFF2
Vreset              def       $FFFE

LCD_DATA            equ       $B5F0
LCD_CTRL            equ       LCD_DATA+1

IMGSIZE             equ       3840                ; image size

;*******************************************************************************

                    #XRAM

MyVars_Begin

TimerFired          rmb       1

Odometer            rmb       3                   ; BCD
MileCount           rmb       2                   ; overflow at 4000 pulses (1 mile)
TripMeter           rmb       2                   ; BCD
TripCount           rmb       2                   ; overflow at 400 pulses (1/10th mile)

BCDSpeed            rmb       2                   ; BCD
BCDRPM              rmb       2                   ; BCD
BCDVoltage          rmb       2                   ; BCD

BCD060Time          rmb       2                   ; BCD
BCDQMTime           rmb       2                   ; BCD
BCDQMSpeed          rmb       2                   ; BCD

sSpeed              rmb       1                   ; sampled binary values of sensors
sVoltage            rmb       1
sTemp               rmb       1
sFuel               rmb       1
sRPM                rmb       1

s060Time            rmb       1
sQMTime             rmb       1
sQMSpeed            rmb       1

cSpeed              rmb       1                   ; changed flags
cVoltage            rmb       1
cTemp               rmb       1
cFuel               rmb       1
cRPM                rmb       1
cTripMeter          rmb       1
cOdometer           rmb       1

c060                rmb       1
cQM                 rmb       1

; All BCD variables are right-justified

; General

LCDAddress          rmb       2
LCDBkColor          rmb       1

; Decompress

DecompState         rmb       1
DecompCount         rmb       1

tRuns               rmb       2
ImgByte             rmb       1
ImgBit              rmb       1
Color               rmb       1

; ReadHuffCode

CPointer            rmb       2
HuffBit             rmb       1
HCode               rmb       2
HCodeBits           rmb       1

; DrawLine

LineX1              rmb       1
LineY1              rmb       1
LineX2              rmb       1
LineY2              rmb       1
LineColor           rmb       1

LHeight             rmb       1
LWidth              rmb       1
LCounter            rmb       1
LineLCDBit          rmb       1
LineLCDAddr         rmb       2
LineYAdd            rmb       2

; DrawLCDDigit

SLDDigit            rmb       1
SLDOffset           rmb       2
SLDDigitBuf         rmb       96

; DisplaySpeed

oldSpeed            rmb       1
newSpeed            rmb       1

; DrawFuel and DrawTemp

BarGraph            rmb       6
BGByte              rmb       1
DPMLCDInvert        rmb       1                   ; * for DisplayPMRace

; Int30MS

PulseCount          rmb       2
QMCount             rmb       2                   ; stop at 1000 pulses (1/4 mile!)

SpeedBuf            rmb       31                  ; for instantaneous speed calculations
SBufIdx             rmb       1
tSpeed0             rmb       1                   ; temp variables
tSpeed1             rmb       1

PMState             rmb       1                   ; Performance mode state

; 0 = off, 1 = testing speed, 2 = race!

PMOldSpeed          rmb       1
PMRaceTime          rmb       2
PMTrip060           rmb       1

SFBuf               rmb       16                  ; for filtering calculated speed
SFBufIdx            rmb       1
SFSample            rmb       1
SFAvgTimer          rmb       1

SlowCount           rmb       1                   ; for updating fuel & temp at 30.5/4 Hz

FuelBuf             rmb       32                  ; for filtering measured fuel level
FBufIdx             rmb       1
FuelSample          rmb       1                   ; filtered (average) fuel level

TempBuf             rmb       32                  ; for filtering measured temperature
TBufIdx             rmb       1
TempSample          rmb       1                   ; filtered (average) temperature

VoltageBuf          rmb       16                  ; for filtering measured voltage value
VBufIdx             rmb       1
VoltSample          rmb       1
VoltAvgTimer        rmb       1

TripTimer           rmb       1

ImgBuf1             rmb       IMGSIZE
ImgBuf2             rmb       IMGSIZE

MyVars_End

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    lds       #STACKTOP           ; set stack pointer, DO NOT set when running under monitor

                    sei
                    jsr       InitSystem
                    jsr       InitVariables
                    jsr       InitDisplay
                    cli

                    clr       TimerFired

Loop@@              lda       TimerFired          ; wait until 30 MS timer fires
                    beq       Loop@@

                    jsr       UpdateDisplay

                    clr       TimerFired

                    bra       Loop@@

;*******************************************************************************

InitLCD             proc
                    clr       LCD_CTRL             ; reset LCD module(s)
                    bsr       ILDelay             ; wait a while
                    lda       #$80
                    sta       LCD_CTRL             ; bring it out of reset
                    bsr       ILDelay             ; wait a while

                    ldd       #$32                ;graphics mode, display active (A=$00, B=$32)
                    bsr       OutLCD1
                    ldd       #$32                ;graphics mode, display active (A=$00, B=$32)
                    bsr       OutLCD2

                    bsr       ILDelay             ; wait a while
                    lda       #$01                ; 8 pixels per byte
                    ldb       #7
                    bsr       OutLCD1
                    lda       #$01                ; 8 pixels per byte
                    ldb       #7
                    bsr       OutLCD2

                    bsr       ILDelay             ; wait a while
                    lda       #$02                ; # of columns = 30 bytes * 8 = 240
                    ldb       #29
                    bsr       OutLCD1
                    lda       #$02                ; # of columns = 30 bytes * 8 = 240
                    ldb       #29
                    bsr       OutLCD2

                    bsr       ILDelay             ; wait a while
                    lda       #$03                ; duty cycle = 1/128
                    ldb       #127
                    bsr       OutLCD1
                    lda       #$03                ; duty cycle = 1/128
                    ldb       #127
                    bsr       OutLCD2

                    bsr       ILDelay
                    lda       #$08                ; Display Start Address = 0
                    clrb
                    bsr       OutLCD1
                    lda       #$08                ; Display Start Address = 0
                    clrb
                    bsr       OutLCD2
                    bsr       ILDelay
                    lda       #$09
                    clrb
                    bsr       OutLCD1
                    lda       #$09
                    clrb
                    bsr       OutLCD2
                    rts

;*******************************************************************************

ILDelay             proc
                    pshx
          #ifdef DEBUG
                    #Message  DEBUG only
                    ldx       #1
          #else
                    ldx       #$E000
          #endif
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

;*******************************************************************************
; A = byte to send to instruction register
; B = byte to send to data register

OutLCD1             proc
                    psha
                    sta       LCD_DATA

                    lda       #$a0
                    sta       LCD_CTRL

                    lda       #$b0
                    sta       LCD_CTRL

                    lda       #$a0
                    sta       LCD_CTRL

                    stb       LCD_DATA

                    lda       #$80
                    sta       LCD_CTRL

                    lda       #$90
                    sta       LCD_CTRL

                    lda       #$80
                    sta       LCD_CTRL

                    pula
                    rts

;*******************************************************************************

OutLCD2             proc
                    psha
                    sta       LCD_DATA
                    lda       #$e0
                    sta       LCD_CTRL
                    lda       #$f0
                    sta       LCD_CTRL
                    lda       #$e0
                    sta       LCD_CTRL

                    stb       LCD_DATA
                    lda       #$c0
                    sta       LCD_CTRL
                    lda       #$d0
                    sta       LCD_CTRL
                    lda       #$c0
                    sta       LCD_CTRL
                    pula
                    rts

;*******************************************************************************

UpdateDisplay       proc
                    lda       PORTD
                    bita      #$08                ; test for hi beam
                    bne       UDNoHiBeam
UDHiBeam            jsr       SetHiBeam
                    bra       UDHiBeamEnd

UDNoHiBeam          jsr       ClearHiBeam

UDHiBeamEnd         tst       cSpeed              ; see if speedo needs to be updated
                    beq       UDSkipSpeedo
                    clr       cSpeed
                    ldb       sSpeed
                    clra
                    ldx       #10
                    idiv                          ;get ones digit
                    stb       BCDSpeed+1
                    xgdx
                    ldx       #10
                    idiv                          ;get tens digit
                    lslb:4                        ;shift left 4 times for BCD
                    orb       BCDSpeed+1
                    stb       BCDSpeed+1
                    xgdx                          ;get hundreds digit into D
                    stb       BCDSpeed
                    jsr       DisplaySpeed

UDSkipSpeedo        tst       cVoltage            ; see if voltage needs to be updated
                    beq       UDSkipVoltage
                    clr       cVoltage
                    ldb       sVoltage
                    clra
                    ldx       #10
                    idiv                          ;get tenths digit
                    stb       BCDVoltage+1
                    xgdx
                    ldx       #10
                    idiv                          ;get ones digit
                    lslb:4                        ;shift left 4 times for BCD
                    orb       BCDVoltage+1
                    stb       BCDVoltage+1
                    xgdx                          ;get tens digit into D
                    stb       BCDVoltage
                    jsr       DisplayVoltage

UDSkipVoltage       tst       cFuel               ; see if fuel level needs to be updated
                    beq       UDSkipFuel
                    clr       cFuel
                    lda       sFuel
                    cmpa      #48
                    blo       UDFuelN1
                    lda       #48
UDFuelN1            jsr       DrawFuel

UDSkipFuel          tst       cTemp               ; see if temperature needs to be updated
                    beq       UDSkipTemp
                    clr       cTemp
                    lda       sTemp
                    jsr       DrawTemp

UDSkipTemp          tst       cTripMeter          ; see if trip meter needs to be displayed
                    beq       UDSkipTrip
                    clr       cTripMeter
                    jsr       DisplayTripMeter

UDSkipTrip          tst       cOdometer           ; see if trip meter needs to be displayed
                    beq       UDSkipOdo
                    clr       cOdometer
                    jsr       DisplayOdometer

UDSkipOdo           tst       c060                ; see if 0-60 perf. value needs to be displayed
                    beq       UDSkip060
                    clr       c060
                    ldb       s060Time
                    clra
                    ldx       #10
                    idiv                          ;get tenths digit
                    stb       BCD060Time+1
                    xgdx
                    ldx       #10
                    idiv                          ;get ones digit
                    lslb:4                        ;shift left 4 times for BCD
                    orb       BCD060Time+1
                    stb       BCD060Time+1
                    xgdx                          ;get tens digit into D
                    stb       BCD060Time
                    jsr       Display060

UDSkip060           tst       cQM                 ; see if QMile perf shite needs to be displayed
                    beq       UDSkipQM
                    clr       cQM
                    ldb       sQMTime
                    clra
                    ldx       #10
                    idiv                          ;get tenths digit
                    stb       BCDQMTime+1
                    xgdx
                    ldx       #10
                    idiv                          ;get ones digit
                    lslb:4                        ;shift left 4 times for BCD
                    orb       BCDQMTime+1
                    stb       BCDQMTime+1
                    xgdx                          ;get tens digit into D
                    stb       BCDQMTime

                    ldb       sQMSpeed
                    clra
                    ldx       #10
                    idiv                          ;get ones digit
                    stb       BCDQMSpeed+1
                    xgdx
                    ldx       #10
                    idiv                          ;get tens digit
                    lslb:4                        ;shift left 4 times for BCD
                    orb       BCDQMSpeed+1
                    stb       BCDQMSpeed+1
                    xgdx                          ;get hundreds digit into D
                    stb       BCDQMSpeed

                    jsr       DisplayQMile

UDSkipQM            rts

;*******************************************************************************

InitVariables       proc

          ; reset all variable memory to 0

                    ldx       #MyVars_Begin
Loop1@@             clr       ,x
                    inx
                    cmpx      #MyVars_End
                    blo       Loop1@@

          ; read odometer, TripMeter from EEPROM

                    ldx       #EEPROM             ; address of on-MCU EEPROM
                    ldy       #Odometer
                    ldb       #9                  ; grab 9 bytes
Loop2@@             lda       ,x
                    sta       ,y
                    inx
                    iny
                    decb
                    bne       Loop2@@

          ; setup LCD for white background

                    lda       #$ff
                    sta       LCDBkColor
                    rts

;*******************************************************************************

InitDisplay         proc
                    jsr       InitLCD

                    bsr       ClearLCDAddrs

                    ldx       #IMGSIZE            ; clear both LCD displays
Loop1@@             lda       #$0c
                    ldb       #$ff                ; reset display byte
                    jsr       OutLCD1             ; send it to LCD
                    lda       #$0c
                    ldb       #$ff                ; reset display byte
                    jsr       OutLCD2             ; send it to LCD

                    dex
                    bne       Loop1@@

          ; Decompress the images, display them on the LCDs

                    ldx       #ImgBuf1            ; clear out logo buffer
                    ldy       #54*60
                    lda       #$ff
Loop2@@             sta       ,x
                    inx
                    dey
                    bne       Loop2@@

                    clrd                          ; setup the decompression pointer
                    std       LCDAddress
                    sta       DecompState         ; setup decompression state (logo)
                    lda       #30
                    sta       DecompCount         ; setup logo counter
                    jsr       Decompress

                    jsr       DisplayImages
                    rts

;*******************************************************************************

ClearLCDAddrs       proc
                    ldd       #$0a00              ; Clear Cursor Addresses for both LCDs A=$0A, B=$00
                    jsr       OutLCD1
                    ldd       #$0b00              ; A = $0B, B = $00
                    jsr       OutLCD1
                    ldd       #$0a00              ; A = $0A, B = $00
                    jsr       OutLCD2
                    ldd       #$0b00              ; A = $0B, B = $00
                    jsr       OutLCD2
                    rts

;*******************************************************************************

InitSystem          proc

          ; Init Options Register

                    lda       #$93                ; A/D activated, IRQ level-sensitive, COP deactivated
                    sta       OPTION              ; init option register

          ; Interrupt vectors stored in EEPROM

          ; Setup Pulse Accumulator

                    lda       #$53
                    sta       PACTL
                    lda       #$40
                    sta       TFLG2               ; clear real-time interrupt flag
                    sta       TMSK2

          ; Deactivate SCI & SPI, set data direction for port D

                    lda       #$20
                    sta       DDRD
                    clr       SPCR
                    clr       SCCR2
                    rts

;*******************************************************************************

ClearPMDisplay      proc

          ; initialize performance counters to blank

                    lda       #$0a
                    sta       BCD060Time
                    sta       BCDQMTime
                    sta       BCDQMSpeed
                    lda       #$aa
                    sta       BCD060Time+1
                    sta       BCDQMTime+1
                    sta       BCDQMSpeed+1

          ; Display it

                    jsr       DisplayPMOff

                    jsr       Display060
                    jsr       DisplayQMile

                    sei
                    ldd       #1841
                    std       LCDAddress
                    ldb       #56                 ; 56 lines
Loop@@              pshb

                    lda       #$0a                ; Set Cursor Address low order
                    ldb       LCDAddress+1
                    jsr       OutLCD2
                    lda       #$0b                ; Set cursor address high order
                    ldb       LCDAddress
                    jsr       OutLCD2

                    lda       #19                 ; 19 bytes wide
Inner@@             psha

                    lda       #$0c
                    ldb       LCDBkColor          ; reset display byte
                    jsr       OutLCD2             ; send it to LCD

                    pula
                    deca
                    bne       Inner@@

                    ldd       LCDAddress
                    addd      #30                 ; go to next line
                    std       LCDAddress

                    pulb
                    decb
                    bne       Loop@@

                    cli
                    rts

;*******************************************************************************

DisplaySpeed        proc
                    lda       sSpeed
                    cmpa      #150                ; max analog speedo at 150mph
                    blo       Skip@@
                    lda       #150
Skip@@              sta       newSpeed

                    clra
                    ldb       oldSpeed
                    lsld:2
                    addd      #SinCos
                    xgdx
                    ldb       oldSpeed
                    lsrb:3
                    andb      #$fe
                    abx

                    lda       ,x                  ; get cos (x) value
                    ldb       #181
                    mul
                    adda      #90
                    sta       LineX2

                    lda       1,x                 ; get sin (x) value
                    ldb       #181
                    mul
                    adda      #38
                    sta       LineY2

                    lda       #148
                    sta       LineX1
                    lda       #96
                    sta       LineY1

                    jsr       ClrLine             ; erase old line

                    clra
                    ldb       newSpeed
                    lsld:2
                    addd      #SinCos
                    xgdx
                    ldb       newSpeed
                    stb       oldSpeed            ; save this so we can erase this line next time
                    lsrb:3
                    andb      #$fe
                    abx

                    lda       ,x                  ; get cos (x) value
                    ldb       #181
                    mul
                    adda      #90
                    sta       LineX2

                    lda       1,x                 ; get sin (x) value
                    ldb       #181
                    mul
                    adda      #38
                    sta       LineY2

                    lda       #148
                    sta       LineX1
                    lda       #96
                    sta       LineY1

                    jsr       SetLine             ; draw new line

                    ldx       #124                ; one's digit
                    lda       BCDSpeed+1
                    anda      #$0f                ; isolate low-order nibble
                    jsr       DrawLCDDigit

                    ldx       #121                ; ten's digit
                    lda       BCDSpeed+1
                    lsra:4                        ;shift right by four to get binary value from BCD
                    jsr       DrawLCDDigit

                    tst       BCDSpeed            ; set the leading one if speed >= 100mph
                    jeq       SetLCDLeading1
                    jmp       ClearLCDLeading1

;*******************************************************************************
; A = Temperature (0 = 25C, 48 = 145C)

DrawTemp            proc
                    ldx       #BarGraph

                    clr       ,x
                    clr       1,x
                    clr       2,x
                    clr       3,x
                    clr       4,x
                    clr       5,x
                    ldb       #$ff
Loop1@@             bita      #$f8
                    beq       Cont1@@
                    stb       ,x
                    inx
                    suba      #8
                    bra       Loop1@@

Cont1@@             tsta
                    beq       Cont2@@
                    clrb
Loop2@@             sec
                    rolb
                    deca
                    bne       Loop2@@
                    stb       ,x                  ; now [BarGraph] is a binary bit mask

Cont2@@             lda       #$88
                    sta       BGByte

                    sei                           ;don't allow interrupts
                    ldx       #181                ; starting address on LCD panel
                    stx       LCDAddress
                    ldy       #22                 ; 22 lines
MainLoop@@          ldx       #BarGraph
                    lda       ,x
                    anda      BGByte
                    eora      LCDBkColor
                    jsr       OutLCD2Byte         ; this sets the LCD Address

                    lda       #5                  ; 5 more bytes
Loop3@@             psha

                    lda       #$0c
                    inx
                    ldb       ,x
                    andb      BGByte
                    eorb      LCDBkColor
                    jsr       OutLCD2

                    pula
                    deca
                    bne       Loop3@@

                    ldd       LCDAddress          ; go to next line
                    addd      #30
                    std       LCDAddress

                    ror       BGByte
                    lda       BGByte
                    bita      #$f0
                    bne       Cont@@
                    lda       #$88
                    sta       BGByte

Cont@@              dey
                    bne       MainLoop@@
                    cli                           ;reenable interrupts
                    rts

;*******************************************************************************
; A = fuel level (0 = empty, 48 = full)

DrawFuel            proc
                    ldx       #BarGraph

                    ldb       LCDBkColor
                    stb       ,x
                    stb       1,x
                    stb       2,x
                    stb       3,x
                    stb       4,x
                    stb       5,x
                    comb
Loop1@@             bita      #$f8
                    beq       Cont1@@
                    stb       ,x
                    inx
                    suba      #8
                    bra       Loop1@@

Cont1@@             tsta
                    beq       Cont2@@
                    clrb
Loop2@@             sec
                    rolb
                    deca
                    bne       Loop2@@
                    eorb      LCDBkColor
                    stb       ,x

Cont2@@             sei
                    ldx       #1471               ; starting address on LCD panel
                    stx       LCDAddress
                    ldy       #22                 ; 22 lines
Loop3@@             ldx       #BarGraph
                    lda       ,x
                    jsr       OutLCD2Byte
                    lda       #$0c
                    ldb       1,x
                    jsr       OutLCD2
                    lda       #$0c
                    ldb       2,x
                    jsr       OutLCD2
                    lda       #$0c
                    ldb       3,x
                    jsr       OutLCD2
                    lda       #$0c
                    ldb       4,x
                    jsr       OutLCD2
                    lda       #$0c
                    ldb       5,x
                    jsr       OutLCD2
                    ldd       LCDAddress
                    addd      #30
                    std       LCDAddress
                    dey
                    bne       Loop3@@
                    cli
                    rts

;*******************************************************************************

SetHiBeam           proc
                    sei
                    ldx       #HiBeamImg
                    ldd       #137
                    std       LCDAddress

                    ldb       #7                  ; 7 lines
Loop@@              pshb                          ;save loop counter

                    lda       ,x                  ; first byte
                    eora      LCDBkColor
                    jsr       OutLCD1Byte
                    inx

                    ldb       ,x                  ; second byte
                    eorb      LCDBkColor
                    lda       #$0c
                    jsr       OutLCD1
                    inx

                    ldb       ,x                  ; third byte
                    eorb      LCDBkColor
                    lda       #$0c
                    jsr       OutLCD1
                    inx

                    ldd       LCDAddress
                    addd      #30
                    std       LCDAddress

                    pulb
                    decb
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

ClearHiBeam         proc
                    sei
                    ldd       #137
                    std       LCDAddress

                    ldb       #7                  ; 7 lines
Loop@@              pshb                          ;save loop counter

                    lda       LCDBkColor          ; first byte
                    jsr       OutLCD1Byte

                    lda       #$0c
                    ldb       LCDBkColor          ; second byte
                    jsr       OutLCD1

                    lda       #$0c
                    ldb       LCDBkColor          ; third byte
                    jsr       OutLCD1

                    ldd       LCDAddress
                    addd      #30
                    std       LCDAddress

                    pulb
                    decb
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

DisplayPMOff        proc
                    ldx       #PMOffImg
                    clra
                    bra       DisplayPMode

;*******************************************************************************

DisplayPMRace       proc
                    ldx       #PMRaceImg
                    lda       #$ff

;*******************************************************************************

DisplayPMode        proc
                    eora      LCDBkColor
                    sta       DPMLCDInvert
                    sei
                    ldy       #85
                    sty       LCDAddress

                    ldb       #10                 ; 10 lines
Loop@@              pshb                          ;save loop counter

                    lda       ,x                  ; first byte
                    eora      DPMLCDInvert
                    jsr       OutLCD2Byte
                    inx

                    lda       #$0c
                    ldb       ,x                  ; second byte
                    eorb      DPMLCDInvert
                    jsr       OutLCD2
                    inx

                    lda       #$0c
                    ldb       ,x                  ; third byte
                    eorb      DPMLCDInvert
                    jsr       OutLCD2
                    inx

                    lda       #$0c
                    ldb       ,x
                    eorb      DPMLCDInvert
                    jsr       OutLCD2
                    inx

                    ldd       LCDAddress
                    addd      #30
                    std       LCDAddress

                    pulb
                    decb
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

DisplayOdometer     proc
                    ldx       #3525
                    lda       Odometer
                    lsra:4
                    jsr       DrawSmallDigit
                    ldx       #3526
                    lda       Odometer
                    anda      #$0f
                    jsr       DrawSmallDigit

                    ldx       #3527
                    lda       Odometer+1
                    lsra:4
                    jsr       DrawSmallDigit
                    ldx       #3528
                    lda       Odometer+1
                    anda      #$0f
                    jsr       DrawSmallDigit

                    ldx       #3529
                    lda       Odometer+2
                    lsra:4
                    jsr       DrawSmallDigit
                    ldx       #3530
                    lda       Odometer+2
                    anda      #$0f
                    jsr       DrawSmallDigit
                    rts

;*******************************************************************************

DisplayTripMeter    proc
                    ldx       #2821
                    lda       TripMeter
                    lsra:4
                    jsr       DrawSmallDigit
                    ldx       #2822
                    lda       TripMeter
                    anda      #$0f
                    jsr       DrawSmallDigit

                    ldx       #2823
                    lda       TripMeter+1
                    lsra:4
                    jsr       DrawSmallDigit
                    ldx       #2824
                    lda       TripMeter+1
                    anda      #$0f
                    jsr       DrawSmallDigit
                    rts

;*******************************************************************************

DisplayVoltage      proc
                    ldx       #6692
                    lda       BCDVoltage
                    anda      #$0f
                    jsr       DrawBigDigit

                    ldx       #6693
                    lda       BCDVoltage+1
                    lsra:4
                    jsr       DrawBigDigit
                    ldx       #6695
                    lda       BCDVoltage+1
                    anda      #$0f
                    jsr       DrawBigDigit
                    rts

;*******************************************************************************

DisplayDigRPM       proc
                    ldx       #1471
                    lda       BCDRPM
                    lsra:4
                    jsr       DrawBigDigit
                    ldx       #1472
                    lda       BCDRPM
                    anda      #$0f
                    bsr       DrawBigDigit

                    ldx       #1473
                    lda       BCDRPM+1
                    lsra:4
                    bsr       DrawBigDigit
                    ldx       #1474
                    lda       BCDRPM+1
                    anda      #$0f
                    bsr       DrawBigDigit
                    rts

;*******************************************************************************

Display060          proc
                    ldx       #4606
                    lda       BCD060Time
                    anda      #$0f
                    jsr       DrawSmallDigit

                    ldx       #4607
                    lda       BCD060Time+1
                    lsra:4
                    jsr       DrawSmallDigit
                    ldx       #4608
                    lda       BCD060Time+1
                    anda      #$0f
                    jsr       DrawSmallDigit
                    rts

;*******************************************************************************

DisplayQMile        proc
                    ldx       #5116
                    lda       BCDQMTime
                    anda      #$0f
                    jsr       DrawSmallDigit

                    ldx       #5117
                    lda       BCDQMTime+1
                    lsra:4
                    bsr       DrawSmallDigit
                    ldx       #5118
                    lda       BCDQMTime+1
                    anda      #$0f
                    bsr       DrawSmallDigit

                    ldx       #5416
                    lda       BCDQMSpeed
                    anda      #$0f
                    bsr       DrawSmallDigit

                    ldx       #5417
                    lda       BCDQMSpeed+1
                    lsra:4
                    bsr       DrawSmallDigit
                    ldx       #5418
                    lda       BCDQMSpeed+1
                    anda      #$0f
                    bsr       DrawSmallDigit
                    rts

;*******************************************************************************
; This works for either LCD module
; X = LCD offset of digit (>= IMGSIZE for LCD #2)
; A = binary digit (0-9)

DrawBigDigit        proc
                    tab
                    lslb:4
                    ldy       #BigDigitBits
                    aby

                    sei
                    cpx       #IMGSIZE
                    bhs       Second@@
                    ldb       #15                 ; fifteen lines high
Loop1@@             stx       LCDAddress
                    lda       ,y
                    eora      LCDBkColor
                    jsr       OutLCD1Byte
                    iny
                    aix       #30                 ; go to next line
                    decb
                    bne       Loop1@@
                    cli
                    rts

Second@@            aix       #-IMGSIZE
                    ldb       #15                 ; fifteen lines high
Loop2@@             stx       LCDAddress
                    lda       ,y
                    eora      LCDBkColor
                    jsr       OutLCD2Byte
                    iny
                    aix       #30                 ; go to next line
                    decb
                    bne       Loop2@@
                    cli
                    rts

;*******************************************************************************
; This works for either LCD module
; X = LCD offset of digit (>= IMGSIZE for LCD #2)
; A = binary digit (0-9)

DrawSmallDigit      proc
                    tab
                    lslb:3
                    ldy       #DigitBits
                    aby

                    sei
                    cpx       #IMGSIZE
                    bhs       Second@@
                    ldb       #7                  ; seven lines high
Loop1@@             stx       LCDAddress
                    lda       ,y
                    eora      LCDBkColor
                    jsr       OutLCD1Byte
                    iny
                    aix       #30                 ; go to next line
                    decb
                    bne       Loop1@@
                    cli
                    rts

Second@@            aix       #-IMGSIZE
                    ldb       #7                  ; seven lines high
Loop2@@             stx       LCDAddress
                    lda       ,y
                    eora      LCDBkColor
                    jsr       OutLCD2Byte
                    iny
                    aix       #30                 ; go to next line
                    decb
                    bne       Loop2@@
                    cli
                    rts

;*******************************************************************************
; Note this is only for LCD #1
; X = LCD offset of digit
; A = binary digit (0-9)

?                   macro     Counter
                    mset      0
          #ifparm ~#1~ = 14
                    mset      0,inx:2
          #endif
                    ldb       #~#1~
Loop$$$             lda       ,y
                    ora       ,x
                    sta       ,x
                    inx
                    ~text~
                    iny
                    decb
                    bne       Loop$$$
                    endm

;-------------------------------------------------------------------------------

DrawLCDDigit        proc
                    stx       SLDOffset
                    tab                           ;b = LCD digit
                    ldx       #LCDDigitSegs
                    abx                           ;x = pointer to digit information
                    lda       ,x
                    sta       SLDDigit

                    ldb       #96                 ; clear out the digit buffer
                    ldx       #SLDDigitBuf
Clear@@             clr       ,x
                    inx
                    decb
                    bne       Clear@@

                    lda       #1
                    bita      SLDDigit
                    beq       Skip@@

                    ldx       #SLDDigitBuf
                    ldy       #LCDSegA
                    ldb       #9
Copy@@              lda       ,y
                    sta       ,x
                    inx
                    iny
                    decb
                    bne       Copy@@

Skip@@              lsr       SLDDigit
                    lda       #1
                    bita      SLDDigit
                    beq       Skip1@@

                    ldx       #SLDDigitBuf+5
                    ldy       #LCDSegBC
                    @?        #14

Skip1@@             lsr       SLDDigit
                    lda       #1
                    bita      SLDDigit
                    beq       Skip2@@

                    ldx       #SLDDigitBuf+50
                    ldy       #LCDSegBC
                    @?        #14

Skip2@@             lsr       SLDDigit
                    lda       #1
                    bita      SLDDigit
                    beq       Skip3@@

                    ldx       #SLDDigitBuf+84
                    ldy       #LCDSegD
                    @?        #9

Skip3@@             lsr       SLDDigit
                    lda       #1
                    bita      SLDDigit
                    beq       Skip4@@

                    ldx       #SLDDigitBuf+48
                    ldy       #LCDSegEF
                    @?        #14

Skip4@@             lsr       SLDDigit
                    lda       #1
                    bita      SLDDigit
                    beq       Skip5@@

                    ldx       #SLDDigitBuf+3
                    ldy       #LCDSegEF
                    @?        #14

Skip5@@             lsr       SLDDigit
                    lda       #1
                    bita      SLDDigit
                    beq       Skip6@@

                    ldx       #SLDDigitBuf+42
                    ldy       #LCDSegG
                    @?        #9

Skip6@@             sei
                    ldd       SLDOffset           ; now dump the LCD Digit buffer out to the LCD
                    std       LCDAddress

                    ldb       #32                 ; 32 lines
                    ldx       #SLDDigitBuf
Loop@@              pshb                          ; save loop counter

                    lda       ,x                  ; first byte
                    eora      LCDBkColor
                    bsr       OutLCD1Byte
                    inx

                    lda       #$0c
                    ldb       ,x                  ; second byte
                    eorb      LCDBkColor
                    jsr       OutLCD1
                    inx

                    lda       #$0c
                    ldb       ,x                  ; third byte
                    eorb      LCDBkColor
                    jsr       OutLCD1
                    inx

                    ldd       LCDAddress
                    addd      #30
                    std       LCDAddress

                    pulb
                    decb
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

SetLCDLeading1      proc
                    sei
                    ldx       #150
                    stx       LCDAddress

                    ldb       #29                 ; 29 iterations
                    ldx       #LCDLeading1
Loop@@              pshb                          ;save loop counter

                    lda       ,x
                    eora      LCDBkColor
                    bsr       OutLCD1Byte
                    inx

                    ldd       LCDAddress
                    addd      #30
                    std       LCDAddress

                    pulb
                    decb
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

ClearLCDLeading1    proc
                    sei
                    ldx       #150
                    stx       LCDAddress

                    ldb       #29                 ; 29 iterations
Loop@@              lda       LCDBkColor
                    bsr       OutLCD1Byte

                    aix       #30
                    stx       LCDAddress

                    decb
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

OutLCD1Byte         proc
                    pshd
                    lda       #$0a                ; Set Cursor Address low order
                    ldb       LCDAddress+1
                    jsr       OutLCD1
                    lda       #$0b                ; Set cursor address high order
                    ldb       LCDAddress
                    jsr       OutLCD1
                    pulb
                    lda       #$0c
                    jsr       OutLCD1
                    pulb
                    rts

;*******************************************************************************

OutLCD2Byte         proc
                    pshd
                    lda       #$0a                ; Set Cursor Address low order
                    ldb       LCDAddress+1
                    jsr       OutLCD2
                    lda       #$0b                ; Set cursor address high order
                    ldb       LCDAddress
                    jsr       OutLCD2
                    pulb
                    lda       #$0c
                    jsr       OutLCD2
                    pulb
                    rts

;*******************************************************************************

SetLine             proc
                    lda       LCDBkColor
                    bita      #$ff
                    beq       Skip@@
                    lda       #$0e
                    sta       LineColor
                    bra       DrawLine

Skip@@              lda       #$0f
                    sta       LineColor
                    bra       DrawLine

;*******************************************************************************

ClrLine             proc
                    lda       LCDBkColor
                    bita      #$ff
                    beq       Skip@@
                    lda       #$0f
                    sta       LineColor
                    bra       DrawLine

Skip@@              lda       #$0e
                    sta       LineColor

;*******************************************************************************

DrawLine            proc
                    lda       LineX2              ; make sure we go left to right
                    cmpa      LineX1
                    bhs       Skip@@
                    ldd       LineX1              ; swap start and end coords
                    ldx       LineX2
                    std       LineX2
                    stx       LineX1
Skip@@              lda       LineY1              ; calculate LCD Address for starting coord
                    ldb       #30
                    mul
                    xgdx                          ;x = LCD Address for start of line
                    ldb       LineX1              ; get X coord
                    lsrb:3                        ;shift right 3 times to divide by 8
                    abx                           ;now x = LCD Address for starting pixel
                    stx       LineLCDAddr
                    lda       LineX1              ; get X coord
                    anda      #7                  ; get bit #
                    sta       LineLCDBit

                    lda       LineX2
                    suba      LineX1              ; a = width of line
                    inca
                    sta       LWidth

                    lda       LineY2
                    suba      LineY1
                    bge       Skip2@@
                    nega
Skip2@@             inca
                    sta       LHeight             ; a = height of line
                    cmpa      LWidth
                    bhs       LineVertical

;*******************************************************************************

LineHorizontal      proc
                    ldx       #30                 ; assume top to bottom (y increment = 30 bytes)
                    ldb       LineY2
                    cmpb      LineY1              ; make sure this is the case
                    bhs       Skip@@
                    ldx       #-30                ; nope, y increment = -30 bytes
Skip@@              stx       LineYAdd            ; store y increment

;                   lda       LWidth              ;initialize Y increment counter
;                   lsra
;                   sta       LCounter

                    clr       LCounter

                    sei
                    clra
                    ldb       LWidth              ; interate X times
                    xgdy
Loop@@              lda       #$0a                ; Set Cursor Address low order
                    ldb       LineLCDAddr+1
                    jsr       OutLCD1
                    lda       #$0b                ; Set cursor address high order
                    ldb       LineLCDAddr
                    jsr       OutLCD1
                    lda       LineColor
                    ldb       LineLCDBit
                    jsr       OutLCD1             ; clear a point
                    inc       LineLCDBit          ; increment x by 1
                    lda       LineLCDBit
                    bita      #$08
                    beq       IncY@@
                    ldx       LineLCDAddr         ; increment LCD address by 1
                    inx
                    stx       LineLCDAddr
                    clr       LineLCDBit          ; bit = 0
IncY@@              lda       LCounter            ; get Y increment counter
                    adda      LHeight
                    bvs       Overflow@@
                    sta       LCounter
                    cmpa      LWidth
                    blo       Cont@@
Overflow@@          suba      LWidth
                    sta       LCounter
                    ldd       LineLCDAddr         ; increment Y
                    addd      LineYAdd
                    std       LineLCDAddr
Cont@@              dey
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

LineVertical        proc
                    ldx       #30                 ; assume top to bottom (y increment = 30 bytes)
                    ldb       LineY2
                    cmpb      LineY1              ; make sure this is the case
                    bhs       StoreY@@
                    ldx       #-30                ; nope, y increment = -30 bytes
StoreY@@            stx       LineYAdd            ; store y increment

;                   lda       LHeight             ;initialize Y increment counter
;                   lsra
;                   sta       LCounter
                    clr       LCounter

                    sei
                    clra
                    ldb       LHeight             ; interate Y times
                    xgdy
Loop@@              lda       #$0a                ; Set Cursor Address low order
                    ldb       LineLCDAddr+1
                    jsr       OutLCD1
                    lda       #$0b                ; Set cursor address high order
                    ldb       LineLCDAddr
                    jsr       OutLCD1
                    lda       LineColor
                    ldb       LineLCDBit
                    jsr       OutLCD1             ; clear a point

                    ldd       LineLCDAddr         ; increment Y
                    addd      LineYAdd
                    std       LineLCDAddr

                    lda       LCounter            ; get X increment counter
                    adda      LWidth
                    bvs       Overflow@@
                    sta       LCounter
                    cmpa      LHeight
                    blo       Cont@@
Overflow@@          suba      LHeight
                    sta       LCounter
                    inc       LineLCDBit          ; increment x by 1
                    lda       LineLCDBit
                    bita      #$08
                    beq       Cont@@
                    ldx       LineLCDAddr         ; increment LCD address by 1
                    inx
                    stx       LineLCDAddr
                    clr       LineLCDBit          ; bit = 0
Cont@@              dey
                    bne       Loop@@
                    cli
                    rts

;*******************************************************************************

Decompress          proc
                    ldx       #Huff_data          ; Setup huffman code extracting variables
                    stx       CPointer
                    lda       #8
                    sta       HuffBit

                    ldx       TotalRuns           ; Setup initial loop variables
                    stx       tRuns
                    clr       Color
                    lda       #8
                    sta       ImgBit

MainLoop@@          jsr       ReadHuffCode        ; returns Run Length in X
                    ldb       Color               ; get color in accum. b
                    cpx       #$100               ; skip color inversion
                    beq       Skip@@              ; if run length = 256
                    com       Color               ; invert color for next time
Skip@@              lda       ImgByte
Loop@@              asrb                          ; get color into carry bit
                    rora                          ; put into image byte
                    dec       ImgBit              ; -> Check to see if the image byte is full
                    bne       Cont@@

                    bsr       DecompOutByte

                    lda       #8                  ; reset ImgBit = 8
                    sta       ImgBit
Cont@@              dex
                    bne       Loop@@
                    sta       ImgByte             ; save these for next time

                    ldx       tRuns
                    dex
                    stx       tRuns
                    bne       MainLoop@@
                    rts

;*******************************************************************************

DecompOutByte       proc
                    tst       DecompState         ; see if we are decompressing the logo or the images
                    beq       Go@@

                    pshx                          ;we are decompressing the images
                    ldx       LCDAddress
                    inx
                    stx       LCDAddress          ; store incremented address
                    aix       #ImgBuf1-1
                    sta       ,x                  ; put in memory buffer
                    pulx
                    rts

Go@@                pshx                          ;we are decompressing the logo
                    ldx       LCDAddress
                    inx
                    stx       LCDAddress          ; store incremented addreses
                    aix       #ImgBuf1+29
                    coma
                    sta       ,x                  ; put in memory buffer
                    dec       DecompCount
                    bne       Done@@

                    xgdx
                    ldd       LCDAddress          ; end of line
                    addd      #30                 ; go to next 60-byte wide line
                    std       LCDAddress
                    xgdx
                    lda       #30                 ; reset DecompCount
                    sta       DecompCount
                    cpx       #54*60              ; are we at the end of the logo display?
                    blo       Done@@

                    pshd
                    jsr       DisplayLogo         ; display the logo animation
                    clr       LCDAddress          ; clear the LCDAddress pointer
                    clr       LCDAddress+1
                    inc       DecompState         ; go to state 1 (decompressing images)
                    puld

Done@@              pulx
                    rts

;*******************************************************************************

DisplayImages       proc
                    jsr       ClearLCDAddrs       ; Set both LCD cusor addresses to 0

                    ldx       #ImgBuf1
                    ldy       #ImgBuf2
Loop@@              lda       #$0c
                    ldb       ,x
                    eorb      LCDBkColor
                    jsr       OutLCD1

                    lda       #$0c
                    ldb       ,y
                    eorb      LCDBkColor
                    jsr       OutLCD2

                    inx
                    iny
                    cpx       #ImgBuf1+IMGSIZE
                    bne       Loop@@

          ; erase the tachometer band

                    ldx       #SinCos+618
Tacho@@             lda       ,x                  ; get cos (x) value
                    adda      #66
                    sta       LineX2
                    lda       ,x                  ; get cos (x) value
                    ldb       #228
                    mul
                    addb      #128
                    adca      #75
                    sta       LineX1

                    lda       1,x                 ; get sin (y) value
                    adda      #14
                    sta       LineY2
                    lda       1,x                 ; get sin (y) value
                    ldb       #228
                    mul
                    addb      #128
                    adca      #23
                    sta       LineY1
                    pshx
                    jsr       ClrLine
                    dec       LineY1
                    dec       LineY2
                    jsr       ClrLine
                    pulx
                    dex:2
                    cpx       #SinCos
                    bhs       Tacho@@

                    lda       #1                  ; set all changed flags so we update everything
                    sta       cSpeed
                    sta       cVoltage
                    sta       cTemp
                    sta       cFuel
                    sta       cRPM
                    sta       cTripMeter
                    sta       cOdometer

                    jsr       ClearPMDisplay      ; reset Performance mode display
                    rts

;*******************************************************************************

DisplayLogo         proc
                    lda       #31
                    sta       DecompCount         ; big loop counter
MainLoop@@
          ; Set cursor address for logo

                    ldd       #$0a56              ; Set Cursor Address low order A=$0A, B=$56
                    jsr       OutLCD1
                    ldd       #$0b04              ; Set cursor address high order A=$0B, B=$04
                    jsr       OutLCD1

          ; display logo image

                    ldx       #ImgBuf1            ; x = pointer to logo image in memory
                    lda       #54                 ; 54 rows
Image@@             ldb       #30                 ; 30 bytes per row
RowLoop@@           pshd
                    lda       #$0c
                    ldb       ,x
                    jsr       OutLCD1
                    puld
                    inx
                    decb
                    bne       RowLoop@@           ; end of row?
                    aix       #30
                    deca
                    bne       Image@@             ; end of image?

          ; move logo to the left by 8 pixels

                    clrb
                    ldx       #ImgBuf1+3239       ; 54*60-1
                    ldy       #54*60
Loop@@              ldb       ,x
                    sta       ,x
                    tba
                    dex
                    dey
                    bne       Loop@@

                    dec       DecompCount
                    bne       MainLoop@@
                    rts

;*******************************************************************************
; Returns next run length in X

ReadHuffCode        proc
                    clr       HCodeBits           ; 0 initial bits for huffman code
                    clr       HCode
                    clr       HCode+1

                    ldx       CPointer
                    lda       ,x                  ; a = current byte in huffman data stream
                    ldb       #8
                    subb      HuffBit
                    bitb      #$ff
                    beq       Loop@@

Shift@@             lsla                          ;shift A left once
                    decb
                    bne       Shift@@

Loop@@              rola                          ;shift 1 bit from huffman stream into huffman code
                    rol       HCode+1
                    rol       HCode

                    inc       HCodeBits           ; 1 more bit in the huffman code
                    dec       HuffBit
                    bne       Skip@@

                    lda       #8
                    sta       HuffBit

                    ldx       CPointer
                    inx
                    stx       CPointer

                    lda       ,x                  ; get next byte in huffman stream

Skip@@              psha                          ;save huffman byte
                    ldb       HCodeBits           ; get # of bits in code
                    lslb:2
                    clra
                    addd      #HCBitPtrs
                    xgdx

                    lda       2,x                 ; load # of huffman codes with this many bits
                    beq       NoSearch@@          ; skip search if there are 0 codes with this number of bits

                    ldx       ,x                  ; get pointer to first huffman code

                    aix       #HCTable

                    ldy       HCode               ; code to match is in Y
Compare@@           cpy       ,x
                    beq       Found@@

                    inx:3
                    deca
                    bne       Compare@@
NoSearch@@          pula
                    bra       Loop@@

Found@@             pula                          ;pull A off of stack
                    ldb       2,x                 ; get Run Length value
                    clra
                    xgdx
                    inx                           ;run length = X
                    rts

;*******************************************************************************
; 32.768 ms timer interrupt

Int30MS             proc
                    lda       sSpeed
                    sta       PMOldSpeed          ; save currently displayed speed

          ; get Pulse Accumulator Count and store in circular SpeedBuf buffer

                    clra
                    ldb       PACNT               ; get pulse accum. count in reg. B
                    sta       PACNT
                    std       PulseCount          ; store here for now

                    tba
                    ldb       SBufIdx
                    ldx       #SpeedBuf
                    abx
                    sta       ,x                  ; store most recent pulse count in buffer

                    ldb       #14                 ; number of pulse samples to accumulate
SL1@@               inx
                    cmpx      #SpeedBuf+31
                    blo       Skip@@

                    ldx       #SpeedBuf           ; reset to beginning of circular buffer
Skip@@              adda      ,x                  ; accumulate pulse count sample into A
                    decb
                    bne       SL1@@

                    sta       tSpeed0             ; save speed for most recent 1/2 second

                                                  ;accumulate -1s <= t <= -.5s
                    ldd       #15                 ; number of pulse samples to accumulate
SL2@@               inx
                    cmpx      #SpeedBuf+31
                    blo       Skip1@@

                    ldx       #SpeedBuf           ; reset to beginning of circular buffer
Skip1@@             adda      ,x                  ; accumulate pulse count sample into A
                    decb
                    bne       SL2@@

                    inx
                    cmpx      #SpeedBuf+31
                    blo       Skip2@@

                    ldx       #SpeedBuf
Skip2@@             ldb       ,x                  ; get oldest speed sample
                    incb                          ;round up
                    lsrb                          ;divide by 2
                    aba                           ;accumulate

                    sta       tSpeed1             ; save speed for 2nd most recent 1/2 second
                    nega
                    adda      tSpeed0             ; get tSpeed0-tSpeed1
                    asra
                    adda      tSpeed0
                    adda      tSpeed1             ; now A = predicted instantaneous speed

                    dec       SBufIdx             ; decrement index variable
                    bpl       Skip3@@
                    ldb       #30
                    stb       SBufIdx

Skip3@@             ldb       #230                ; multiplication factor
                    mul                           ;get speed in MPH
                    cmpa      #200                ; check for >= 200 mph
                    blo       Skip4@@
                    lda       #199                ; max out at 199
Skip4@@
          ; now a = calculated instantaneous speed value (in MPH)

                    ldx       #SFBuf
                    ldb       SFBufIdx
                    abx
                    sta       ,x                  ; store speed sample in ring buffer
                    sta       SFSample

                    incb                          ;advance ring buffer pointer
                    andb      #$0f                ; bit mask
                    stb       SFBufIdx
                    ldx       #SFBuf
                    abx

                    suba      ,x                  ; get differential
                    anda      #$fc                ; test high bits
                    beq       Skip5@@
                    coma
                    anda      #$fc
                    beq       Skip5@@
                    lda       SFSample
                    sta       sSpeed
                    inc       cSpeed              ; set voltage display update flag
                    clr       SFAvgTimer
                    bra       I30SpeedEnd

Skip5@@             ldx       #SFBuf
                    lda       #16                 ; 16-entry ring buffer
                    clry
SL3@@               ldb       ,x                  ; get sum of buffer entry values
                    inx
                    aby
                    deca
                    bne       SL3@@

                    xgdy                          ;get sum into D accumulator
                    lsrd:4                        ;now D is average
                    cmpb      sSpeed              ; check to see if average = displayed value
                    beq       I30SpeedN5
                    inc       SFAvgTimer
                    lda       SFAvgTimer
                    cmpa      #12
                    bne       I30SpeedEnd

                    stb       sSpeed
                    inc       cSpeed              ; update display

I30SpeedN5          clr       SFAvgTimer

I30SpeedEnd         lda       PMState             ; get state of performance mode
                    bne       I30PModeN1

          ; PMode state = 0, off

                    lda       PMOldSpeed          ; get currently displayed speed
                    jne       I30PModeEnd         ; then get outa here

                    lda       PulseCount+1        ; else get last pulse count
                    jeq       I30PModeEnd         ; then get outa here

                    clrx
                    stx       QMCount             ; clear quarter-mile counter
                    stx       PMRaceTime          ; clear race timer
                    clr       PMTrip060
                    inc       PMState             ; go to state 1 (test speed)
                    jmp       I30PModeEnd         ; get outa here

I30PModeN1          psha

                    ldd       PulseCount
                    addd      QMCount
                    std       QMCount             ; accumulate distance for quarter mile

                    ldd       PMRaceTime
                    incd                          ; add 1 tick to the clock
                    std       PMRaceTime

                    pula

                    cmpa      #1
                    bne       I30PMode2           ; * mode 1 = wait for 2 secs & check speed

                    ldd       PMRaceTime
                    cmpd      #61
                    blo       I30PModeEnd

                    lda       SFSample            ; get instantaneous speed (MPH)
                    cmpa      #15                 ; see if we are going 15
                    bhs       I30PMN1_1           ; if we are, go to race mode!

                    clr       PMState             ; otherwise go to mode 0
                    bra       I30PModeEnd

I30PMN1_1           lda       #2                  ; go to mode 2 (race!)
                    sta       PMState
                    jsr       ClearPMDisplay      ; clear graph, meters
                    jsr       DisplayPMRace       ; show race mode!

I30PMode2
          ; we are in race mode!

                    ldd       PMRaceTime          ; time since starting
                    cpd       #450
                    bhi       I30PM2N1            ; if > 450 then don't plot point

                    ldx       #3
                    idiv
                    tstb                          ;if remainder != 0
                    bne       I30PM2N1            ; then don't plot point

                    xgdx                          ;get time (tenths of a sec.) in B
                    lda       SFSample            ; A = instantaneous speed
                    jsr       I30PlotPoint        ; plot point on the graph

I30PM2N1            tst       PMTrip060           ; have we hit 60 before?
                    bne       I30PM2N2            ; if not

                    lda       SFSample            ; get current speed
                    cmpa      #60                 ; are we going 60 now?
                    blo       I30PM2N2            ; if so

                    ldd       PMRaceTime          ; get current time
                    ldx       #3
                    idiv                          ;div by 3 to get tenths of a sec
                    xgdx                          ;get time in B
                    stb       s060Time            ; store
                    inc       c060                ; update display
                    inc       PMTrip060           ; now we have hit 60

I30PM2N2            ldd       QMCount             ; get # of pulses since we started
                    cpd       #1000               ; have we hit the QM yet?
                    blo       I30PM2N3            ; if so

                    lda       SFSample            ; get current speed
                    sta       sQMSpeed            ; store

                    ldd       PMRaceTime          ; get current time
                    ldx       #3
                    idiv                          ;div by 3 to get tenths of a sec
                    xgdx                          ;get time in B
                    stb       sQMTime
                    inc       cQM                 ; update quarter-mile display
                    clr       PMState             ; go to state 0
                    jsr       DisplayPMOff        ; display race mode: off

I30PM2N3            ldd       PMRaceTime          ; get race time
                    cmpd      #765                ; have we been in race for > 25.5 seconds?
                    blo       I30PModeEnd         ; if so

                    clr       PMState             ; go to state 0
                    jsr       DisplayPMOff

I30PModeEnd         ldd       MileCount           ; get odometer pulse count
                    addd      PulseCount
                    cmpd      #4000
                    blo       I30N1

                    subd      #4000

                    psha                          ;save reg A

                    lda       Odometer+2          ; increment the odometer reading
                    adda      #1
                    daa
                    sta       Odometer+2

                    clra
                    adca      Odometer+1
                    daa
                    sta       Odometer+1

                    lda       Odometer
                    adca      #0
                    daa
                    sta       Odometer

                    inc       cOdometer           ; set display flag
                    pula

I30N1               std       MileCount

                    ldd       TripCount           ; get trip meter pulse count
                    addd      PulseCount
                    cmpd      #400
                    blo       I30N2

                    subd      #400

                    psha

                    lda       TripMeter+1         ; increment trip meter
                    adda      #1
                    daa
                    sta       TripMeter+1

                    clra
                    adca      TripMeter
                    daa
                    sta       TripMeter

                    inc       cTripMeter          ; set display flag

                    pula

I30N2               std       TripCount

                    lda       PORTD
                    bita      #$04                ; test for trip reset switch
                    bne       I30TripUp
                    inc       TripTimer
                    bne       I30TripN1
                    lda       #$ff
                    sta       TripTimer
                    bra       I30SkipTrip

I30TripN1           lda       TripTimer
                    cmpa      #90                 ; 4 seconds
                    bne       I30SkipTrip
                    lda       #$ff
                    sta       TripTimer
                    com       LCDBkColor          ; invert background/foreground color
                    jsr       InitLCD             ; re-initialize display
                    jsr       DisplayImages
                    clr       PACNT               ; clear pulse count so speedo isn't fucked
                    bra       I30SkipTrip

I30TripUp           lda       TripTimer
                    clr       TripTimer
                    cmpa      #$ff
                    beq       I30SkipTrip
                    cmpa      #4
                    blo       I30SkipTrip
                    clr       TripMeter
                    clr       TripMeter+1         ; clear trip meter
                    lda       #$01
                    sta       cTripMeter          ; update trip meter display

I30SkipTrip         lda       #$10
                    sta       ADCTL               ; start A/D conversion
                    nop:2                         ;blow away some time
I30Loop1            lda       ADCTL
                    bpl       I30Loop1            ; wait until conversion complete

          ; The Fuel and Temp guages will be updated at 1/4 the frequency
          ; of the other displays.

                    inc       SlowCount           ; see if we should update fuel&temp
                    lda       #$03
                    anda      SlowCount
                    bne       I30SkipSlow

          ; calculate fuel level reading

                    ldx       #FuelBuf
                    ldb       FBufIdx
                    abx                           ;x = pointer to fuel buffer to store sample
                    lda       ADR3
                    sta       ,x
                    incb                          ;move to next sample in buffer
                    andb      #$1f                ; 32-entry buffer
                    stb       FBufIdx

                    ldx       #FuelBuf
                    jsr       I30Avg32            ; get average value of samples in buffer
                    sta       FuelSample

                    tab
                    lda       #1
                    negb
                    sbca      #0                  ; D = 256-F
                    xgdx
                    lda       #116                ; scale factor
                    ldb       FuelSample
                    mul                           ;X = 256-F, D = 116*F
                    idiv
                    xgdx                          ;b = displayed fuel value
                    cmpb      sFuel
                    beq       I30FuelEnd
                    stb       sFuel
                    inc       cFuel
I30FuelEnd

          ; calculate temperature reading

                    ldx       #TempTable
                    ldb       ADR2
                    clra
I30TempLoop1        cmpb      ,x
                    bhs       I30TL1End
                    inca
                    inx
                    bra       I30TempLoop1

I30TL1End           ldx       #TempBuf            ; now 0 <= A <= 48 == linear temp [25C,145C]
                    ldb       TBufIdx
                    abx                           ;x = pointer to temp. buffer to store sample
                    sta       ,x
                    incb                          ;move to next sample in buffer
                    andb      #$1f                ; 32-entry buffer
                    stb       TBufIdx

                    ldx       #TempBuf
                    jsr       I30Avg32            ; get average value of samples in buffer in A

                    cmpa      sTemp
                    beq       I30TempEnd
                    sta       sTemp
                    inc       cTemp
I30TempEnd

I30SkipSlow

          ; This code below updates a 16-entry ring buffer
          ; with transformed Voltage samples.... Update display
          ; under these conditions:
          ; if (abs(sum(d_dt(VoltSamples)) / numSamples) > 0.5)
          ; sVoltage = VoltSamples[current]; updateVoltage
          ; else if (avg(VoltSamples) != sVoltage)
          ; timer++; if (timer > 20)
          ; sVoltage = avg(VoltSamples); updateVoltage
          ; else timer=0;

                    lda       ADR1                ; get voltage sample
                    ldb       #223
                    mul                           ;now a = 10ths of a volt
                    ldx       #VoltageBuf
                    ldb       VBufIdx
                    abx
                    sta       ,x                  ; store voltage sample in ring buffer
                    sta       VoltSample

                    incb                          ;advance ring buffer pointer
                    andb      #$0f                ; bit mask
                    stb       VBufIdx
                    ldx       #VoltageBuf
                    abx

                    suba      ,x                  ; get differential
                    anda      #$fc                ; test high bits
                    beq       I30VoltageN1
                    coma
                    anda      #$fc
                    beq       I30VoltageN1
                    lda       VoltSample
                    sta       sVoltage
                    inc       cVoltage            ; set voltage display update flag
                    clr       VoltAvgTimer
                    bra       I30VoltageEnd

I30VoltageN1        ldx       #VoltageBuf
                    lda       #16                 ; 16-entry ring buffer
                    clry
I30VoltLoop1        ldb       ,x                  ; get sum of buffer entry values
                    inx
                    aby
                    deca
                    bne       I30VoltLoop1

                    xgdy                          ;get sum into D accumulator
                    lsrd:4                        ;now D is average
                    cmpb      sVoltage            ; check to see if average = displayed value
                    beq       I30VoltageN2
                    inc       VoltAvgTimer
                    lda       VoltAvgTimer
                    cmpa      #20
                    bne       I30VoltageEnd

                    stb       sVoltage
                    inc       cVoltage            ; update display

I30VoltageN2        clr       VoltAvgTimer

I30VoltageEnd       lda       #$ff                ; set byte telling main loop that timer fired
                    sta       TimerFired

                    lda       #$40                ; reset timer interrupt flag
                    sta       TFLG2
                    rti                           ; exit

;*******************************************************************************
; Purpose:
; Input  : A = speed (MPH)
;        : B = time (tenths of a second)

I30PlotPoint        proc
                    pshb                          ;save B for now
                    cmpa      #100                ; if speed > 100
                    blo       Skip@@
                    lda       #100                ; speed = 100
Skip@@              ldb       #141
                    mul                           ;now a = y displacement on graph
                    nega
                    adda      #116                ; now A = Y coordinate for graph
                    ldb       #30                 ; 30 bytes per line
                    mul                           ;now D = offset to start of display line for point
                    xgdx                          ;put into X for now

                    pulb                          ;retrieve the time
                    addb      #80                 ; graph is 80 columns over
                    tba                           ;copy into A
                    lsrb:3                        ;now b = offset to byte int display line
                    abx                           ;add into display line offset for total offset
                    stx       LCDAddress          ; store here

                    anda      #7                  ; get bit # to set
                    psha                          ;save for a moment

                    lda       #$0a                ; Set Cursor Address low order
                    ldb       LCDAddress+1
                    jsr       OutLCD2
                    lda       #$0b                ; Set cursor address high order
                    ldb       LCDAddress
                    jsr       OutLCD2
                    lda       #$0f
                    adda      LCDBkColor          ; don't worry about it, I got it covered
                    pulb                          ;get bit # to set
                    jsr       OutLCD2             ; set a point
                    rts

;*******************************************************************************
; Purpose:
; Input  : X = pointer to buffer to average
; Output : A = average value

I30Avg32            proc
                    clry
                    lda       #32                 ; calculate average of all 32 samples
Loop@@              ldb       ,x
                    inx
                    aby
                    deca
                    bne       Loop@@
                    xgdy                          ;D = sum of 32 samples
                    lsld:3                        ;now a = average value
                    rts

;*******************************************************************************

IntPowerDown        proc

          ; Save Odometer, TripMeter in on-chip EEPROM

                    sei                           ; Make sure interrupts are turned off

                    ldb       #$0e                ; Erase first row of EEPROM ($b600-$b60f)
                    stb       PPROG               ; Set to Row Erase Mode
                    stb       EEPROM              ; Write any Data to any Address in Row
                    inc       PPROG               ; Turn on High Voltage
                    bsr       Delay10ms
                    clr       PPROG               ; Turn Off High Voltage and Set to Read

                    ldx       #EEPROM             ; Address to begin storing data
                    ldy       #Odometer
                    ldb       #9                  ; save 9 bytes
Loop@@              pshb                          ; save loop counter
                    lda       ,y
                    bsr       IPDProgram
                    inx
                    iny
                    pulb
                    decb
                    bne       Loop@@

                    bra       *                   ; wait for power to die

;*******************************************************************************

IPDProgram          proc
                    ldb       #$02
                    stb       PPROG               ; Set EELAT Bit (EEPGM = 0)
                    sta       ,x                  ; Store Data to EEPROM Address
                    inc       PPROG               ; Set EEPGM Bit (EELAT = 1)
                    bsr       Delay10ms
                    clr       PPROG               ; Turn Off High Voltage and Set to READ Mode
                    rts

;*******************************************************************************
; Purpose: Delay 10 msec
; Input  : None
; Output : None
; Note(s): It works for any MHz as MSEC is already adjusted accordingly.

                              #Cycles
Delay10ms           proc
                    pshx
                    ldx       #DELAY@@
                              #Cycles
Loop@@              dex
                    bne       Loop@@
                              #temp :cycles
                    pulx
                    rts

DELAY@@             equ       10*BUS_KHZ-:cycles-:ocycles/:temp

;*******************************************************************************
; Permanent Data Area

HiBeamImg           fcb       $00,$ee,$03,$00,$0f,$00,$80,$0f,$00
                    fcb       $80,$cf,$03,$80,$0f,$00,$00,$0f,$00
                    fcb       $00,$ee,$03
PMOffImg            fcb       $f0,$80,$19,$00,$0c,$43,$04,$00,$04,$42
                    fcb       $04,$00,$02,$44,$04,$00,$02,$e4,$1f,$00
                    fcb       $02,$44,$04,$00,$02,$44,$04,$00,$04,$42
                    fcb       $04,$00,$0c,$41,$04,$00,$f8,$e0,$0e,$00
PMRaceImg           fcb       $7e,$00,$00,$80,$c4,$00,$00,$80,$84,$00
                    fcb       $00,$80,$84,$00,$00,$80,$44,$70,$38,$8e
                    fcb       $34,$88,$26,$91,$24,$e0,$02,$9f,$44,$98
                    fcb       $02,$81,$84,$88,$06,$03,$0e,$f9,$3d,$9e
DigitBits           fcb       $1e,$21,$21,$21,$21,$21,$1e,0  ; 0
                    fcb       $08,$0c,$08,$08,$08,$08,$1c,0  ; 1
                    fcb       $1f,$20,$20,$1e,$01,$01,$3e,0  ; 2
                    fcb       $1f,$20,$20,$1e,$20,$20,$1f,0  ; 3
                    fcb       $21,$21,$21,$3e,$20,$20,$20,0  ; 4
                    fcb       $1f,$01,$01,$1f,$20,$20,$1f,0  ; 5
                    fcb       $1e,$01,$01,$1f,$21,$21,$1e,0  ; 6
                    fcb       $3f,$20,$10,$08,$04,$02,$01,0  ; 7
                    fcb       $1e,$21,$21,$1e,$21,$21,$1e,0  ; 8
                    fcb       $1e,$21,$21,$3e,$20,$20,$1f,0  ; 9
                    fcb       $00,$00,$00,$3f,$00,$00,$00,0  ; A (-)

BigDigitBits        fcb       $3e,$7f,$63,$63,$63,$63,$63,$63,$63  ; 0
                    fcb       $63,$63,$63,$63,$7f,$3e,0
                    fcb       $60,$70,$78,$78,$60,$60,$60,$60,$60  ; 1
                    fcb       $60,$60,$60,$60,$60,$60,0
                    fcb       $3e,$7f,$63,$63,$63,$60,$30,$38,$18  ; 2
                    fcb       $1c,$0c,$06,$07,$7f,$7f,0
                    fcb       $3e,$7f,$63,$63,$63,$60,$7c,$3c,$60  ; 3
                    fcb       $63,$63,$63,$63,$7f,$3e,0
                    fcb       $70,$78,$78,$6c,$6c,$6c,$64,$66,$66  ; 4
                    fcb       $66,$63,$ff,$ff,$60,$60,0
                    fcb       $7f,$7f,$03,$03,$03,$3f,$7f,$63,$60  ; 5
                    fcb       $63,$63,$63,$63,$7f,$3e,0
                    fcb       $3e,$7f,$63,$63,$03,$3f,$7f,$63,$63  ; 6
                    fcb       $63,$63,$63,$63,$7f,$3e,0
                    fcb       $7f,$7f,$60,$30,$30,$30,$18,$18,$1c  ; 7
                    fcb       $0c,$0c,$06,$06,$06,$03,0
                    fcb       $3e,$7f,$63,$63,$63,$63,$3e,$3e,$63  ; 8
                    fcb       $63,$63,$63,$63,$7f,$3e,0
                    fcb       $3e,$7f,$63,$63,$63,$63,$7f,$7e,$60  ; 9
                    fcb       $63,$63,$63,$63,$7f,$3e,0

LCDDigitSegs        fcb       $3f,6,$5b,$4f,$66,$6d,$7d,7,$7f,$6f

LCDSegA             fcb       $80,$ff,$1f,0,$ff,$0f,0,$fe,7
LCDSegBC            fcb       $40,$60,$70,$70,$70,$70,$70,$70,$70,$70
                    fcb       $70,$70,$60,$40
LCDSegD             fcb       0,$fe,7,0,$ff,$0f,$80,$ff,$1f
LCDSegEF            fcb       $20,$60,$e0,$e0,$e0,$e0,$e0,$e0,$e0,$e0
                    fcb       $e0,$e0,$60,$20
LCDSegG             fcb       0,$ff,$0f,$80,$ff,$1f,0,$ff,$0f
LCDLeading1         fcb       $40,$60,$70,$70,$70,$70,$70,$70,$70,$70
                    fcb       $70,$70,$60,$40,$00,$40,$60,$70,$70,$70
                    fcb       $70,$70,$70,$70,$70,$70,$70,$60,$40

TotalRuns           dw        5009

HCTable             fcb       $00,$00,$00,$00,$06,$01,$00,$04,$02,$00,$0a,$03
                    fcb       $00,$3d,$04,$00,$2c,$05,$00,$72,$06,$00,$79,$08
                    fcb       $00,$74,$0b,$00,$7e,$0c,$00,$5d,$2f,$00,$fb,$07
                    fcb       $00,$ff,$0a,$00,$b9,$0d,$00,$e1,$0e,$00,$e3,$0f
                    fcb       $00,$ed,$10,$00,$eb,$11,$00,$b7,$18,$00,$b8,$3a
                    fcb       $00,$fa,$e0,$01,$cc,$09,$01,$f3,$12,$01,$d8,$15
                    fcb       $01,$cd,$19,$01,$78,$1c,$01,$7a,$3b,$01,$68,$87
                    fcb       $01,$7b,$ee,$01,$c5,$ff,$03,$9c,$13,$03,$9d,$14
                    fcb       $03,$9e,$16,$03,$fa,$17,$03,$fb,$1a,$02,$d2,$1d
                    fcb       $03,$b2,$1e,$03,$b3,$20,$02,$d3,$28,$03,$9f,$29
                    fcb       $03,$a8,$2c,$02,$f3,$2d,$02,$f8,$31,$03,$b8,$34
                    fcb       $02,$d4,$37,$02,$d5,$38,$02,$d6,$40,$02,$d7,$44
                    fcb       $02,$d8,$45,$03,$f8,$4a,$03,$a9,$59,$02,$d9,$81
                    fcb       $03,$b9,$83,$02,$f9,$85,$02,$fa,$88,$03,$aa,$89
                    fcb       $02,$da,$8b,$03,$ba,$93,$03,$f9,$a5,$02,$fb,$ae
                    fcb       $03,$ab,$b3,$02,$db,$b6,$07,$76,$1b,$07,$77,$1f
                    fcb       $07,$78,$21,$05,$e5,$22,$07,$79,$23,$07,$7a,$24
                    fcb       $05,$f8,$25,$07,$7b,$26,$05,$f9,$27,$07,$7c,$2a
                    fcb       $07,$7d,$2b,$07,$7e,$33,$05,$fa,$39,$07,$7f,$3c
                    fcb       $05,$fb,$3e,$07,$80,$3f,$07,$81,$41,$07,$82,$42
                    fcb       $07,$83,$43,$07,$84,$48,$05,$fc,$4d,$05,$fd,$50
                    fcb       $05,$fe,$53,$05,$ff,$65,$07,$85,$66,$07,$86,$67
                    fcb       $07,$87,$69,$07,$00,$72,$07,$01,$73,$07,$02,$7c
                    fcb       $07,$03,$7f,$07,$04,$84,$07,$88,$91,$07,$05,$99
                    fcb       $07,$06,$af,$07,$07,$b5,$07,$10,$bd,$07,$11,$be
                    fcb       $07,$12,$c1,$07,$13,$c2,$0f,$12,$2e,$0f,$13,$30
                    fcb       $0f,$14,$32,$0f,$15,$3d,$0f,$16,$46,$0f,$17,$4c
                    fcb       $0f,$18,$4e,$0f,$19,$5d,$0f,$1a,$68,$0f,$1b,$6d
                    fcb       $0f,$1c,$6e,$0f,$1d,$75,$0f,$1e,$7a,$0f,$1f,$92
                    fcb       $0f,$80,$a3,$0f,$81,$d4,$0f,$82,$dc,$0f,$83,$df
                    fcb       $0f,$84,$e5,$0b,$c8,$e7,$0b,$c9,$ed,$1f,$0a,$35
                    fcb       $1f,$0b,$47,$1f,$0c,$49,$1f,$0d,$4b,$1f,$0e,$4f
                    fcb       $1f,$0f,$54,$1f,$10,$57,$1f,$11,$5a,$1f,$12,$5b
                    fcb       $1f,$13,$5c,$1f,$14,$60,$1f,$15,$62,$1f,$16,$63
                    fcb       $1f,$17,$6b,$1f,$18,$6f,$1f,$19,$70,$1f,$1a,$71
                    fcb       $1f,$1b,$76,$1f,$1c,$77,$1f,$1d,$79,$1f,$1e,$7d
                    fcb       $1f,$1f,$80,$1f,$20,$82,$1f,$21,$86,$1f,$22,$8d
                    fcb       $1f,$23,$90,$1f,$24,$94,$1f,$25,$95,$1f,$26,$96
                    fcb       $1f,$27,$98,$1f,$28,$9b,$1f,$29,$9e,$1f,$2a,$a1
                    fcb       $1f,$2b,$a8,$1f,$2c,$aa,$1f,$2d,$ad,$1f,$2e,$b2
                    fcb       $1f,$2f,$c5

HCBitPtrs           fcb       $00,$00,$00,0,$00,$00,$01,0,$00,$03,$00,0,$00,$03,$02,0
                    fcb       $00,$09,$01,0,$00,$0c,$00,0,$00,$0c,$02,0,$00,$12,$05,0
                    fcb       $00,$21,$0a,0,$00,$3f,$09,0,$00,$5a,$20,0,$00,$ba,$28,0
                    fcb       $01,$32,$15,0,$01,$71,$26,0,$01,$e3,$00,0,$01,$e3,$00,0

; Huffman encoded run-lengths for image data

Huff_data           fcb       $e2,$f1,$79,$ff,$3d,$eb,$de,$23,$a4,$ef,$1a,$4e
                    fcb       $21,$ab,$76,$6b,$37,$10,$d5,$bb,$be,$e4,$e2,$1a,$96,$7b
                    fcb       $ed,$59,$26,$fd,$ab,$57,$be,$d7,$29,$f2,$5d,$16,$6d,$a9
                    fcb       $34,$db,$6d,$ff,$5a,$a6,$da,$92,$b6,$db,$e4,$9e,$f2,$4d
                    fcb       $1e,$a2,$b3,$7e,$d5,$a2,$69,$a6,$a5,$6e,$eb,$37,$32,$6d
                    fcb       $eb,$7d,$b7,$b6,$6d,$5a,$c9,$34,$d3,$ef,$ba,$db,$72,$cd
                    fcb       $b7,$de,$e5,$b9,$6a,$d5,$ab,$34,$d1,$5b,$82,$ed,$ac,$db
                    fcb       $6d,$f7,$b9,$4b,$35,$6a,$d3,$6d,$55,$ee,$e0,$ba,$6a,$d3
                    fcb       $6d,$f7,$39,$6b,$35,$69,$36,$9a,$c5,$93,$e4,$fa,$b4,$da
                    fcb       $0d,$34,$d3,$4d,$52,$b6,$2c,$d0,$92,$cd,$37,$88,$b3,$56
                    fcb       $89,$2a,$ca,$b3,$dc,$aa,$da,$44,$a2,$aa,$be,$0f,$7b,$be
                    fcb       $13,$59,$be,$12,$56,$bc,$8b,$38,$bc,$5e,$2f,$84,$73,$1c
                    fcb       $6e,$63,$c0,$b9,$d7,$7b,$dd,$eb,$db,$f1,$b8,$5e,$fb,$9c
                    fcb       $cd,$2a,$aa,$ab,$db,$3c,$1d,$7d,$8e,$f2,$a7,$7b,$7a,$f2
                    fcb       $a5,$33,$ee,$45,$32,$99,$4f,$51,$de,$56,$92,$3e,$d1,$d9
                    fcb       $ba,$87,$af,$b5,$3c,$bc,$a2,$99,$4c,$a7,$ab,$f2,$b4,$9d
                    fcb       $11,$d9,$be,$eb,$13,$6d,$06,$24,$a2,$25,$8a,$79,$79,$45
                    fcb       $32,$99,$4f,$56,$69,$35,$46,$25,$69,$33,$54,$24,$96,$3d
                    fcb       $a5,$23,$6c,$a6,$a3,$25,$39,$28,$d1,$90,$89,$93,$6d,$87
                    fcb       $da,$9e,$5e,$55,$ec,$f6,$7b,$ec,$8c,$30,$8d,$93,$23,$32
                    fcb       $62,$89,$99,$9c,$b6,$50,$8a,$46,$66,$68,$d1,$1c,$94,$29
                    fcb       $19,$94,$c8,$88,$3e,$d4,$f2,$f2,$fb,$72,$72,$7a,$b2,$64
                    fcb       $72,$64,$54,$ca,$14,$c8,$8f,$f1,$42,$3d,$01,$24,$4f,$63
                    fcb       $92,$85,$20,$8a,$49,$48,$3e,$d4,$f2,$f2,$fb,$72,$72,$7a
                    fcb       $f5,$91,$9c,$99,$11,$99,$42,$99,$14,$e6,$0a,$11,$e8,$0c
                    fcb       $c8,$8f,$b5,$0a,$34,$56,$8a,$53,$d4,$05,$8f,$54,$8f,$28
                    fcb       $88,$88,$8b,$14,$d8,$c2,$23,$23,$36,$c3,$0a,$64,$66,$7a
                    fcb       $82,$84,$13,$6d,$9b,$44,$47,$b5,$2b,$13,$d4,$9a,$6a,$d3
                    fcb       $63,$ed,$ed,$47,$99,$aa,$aa,$aa,$c9,$59,$ec,$92,$cd,$b6
                    fcb       $6c,$d1,$5a,$6c,$96,$54,$a4,$8f,$51,$33,$24,$9f,$37,$97
                    fcb       $45,$3a,$dd,$1f,$2e,$7d,$bb,$77,$cd,$cc,$75,$7b,$d9,$fa
                    fcb       $b8,$bc,$5e,$2f,$17,$e0,$dd,$e1,$e5,$d8,$77,$77,$da,$fd
                    fcb       $7b,$6b,$6f,$7d,$e3,$b3,$ff,$73,$bb,$ef,$de,$bd,$cb,$5c
                    fcb       $22,$d6,$58,$8e,$34,$74,$76,$1c,$94,$f9,$be,$d7,$08,$b5
                    fcb       $96,$23,$8d,$1d,$1d,$85,$8b,$77,$da,$e3,$60,$ca,$92,$8d
                    fcb       $34,$22,$62,$24,$92,$16,$21,$b5,$72,$64,$6a,$84,$9a,$8c
                    fcb       $85,$8f,$ba,$92,$43,$48,$52,$42,$73,$e6,$46,$a1,$32,$19
                    fcb       $89,$b6,$26,$d9,$90,$4d,$65,$44,$79,$93,$11,$36,$66,$68
                    fcb       $d3,$58,$e4,$a1,$04,$c4,$61,$10,$99,$cf,$bd,$45,$08,$8b
                    fcb       $26,$44,$64,$46,$f4,$a5,$88,$8c,$f2,$28,$72,$44,$91,$14
                    fcb       $b1,$55,$47,$a5,$1e,$ed,$5e,$e4,$73,$ef,$51,$4d,$29,$eb
                    fcb       $32,$23,$22,$33,$d5,$2c,$44,$1c,$c6,$41,$c9,$19,$91,$14
                    fcb       $b1,$e8,$a1,$ea,$85,$82,$2c,$47,$3e,$f5,$14,$8f,$54,$db
                    fcb       $22,$32,$23,$20,$93,$d4,$56,$e6,$32,$08,$88,$36,$88,$92
                    fcb       $c4,$c5,$08,$25,$46,$66,$46,$45,$a7,$24,$95,$2b,$4a,$cf
                    fcb       $76,$de,$ed,$b6,$92,$1e,$fb,$ea,$2c,$98,$89,$26,$cc,$c9
                    fcb       $21,$f7,$0d,$52,$48,$69,$a2,$26,$9b,$5f,$a5,$2f,$db,$e5
                    fcb       $bb,$c5,$5f,$2c,$af,$15,$9c,$5e,$2f,$56,$bd,$af,$6b,$da
                    fcb       $f6,$bd,$af,$6b,$da,$f6,$bd,$ba,$97,$74,$ef,$8e,$a3,$77
                    fcb       $4e,$7c,$77,$c1,$dd,$3b,$eb,$be,$57,$8f,$bb,$e2,$f3,$77
                    fcb       $4e,$fa,$ef,$b3,$c7,$dc,$fe,$b9,$f9,$fc,$a9,$df,$bb,$2f
                    fcb       $94,$3c,$4a,$d3,$f9,$33,$be,$f8,$97,$74,$b4,$fb,$8f,$91
                    fcb       $af,$c5,$c7,$c8,$9d,$e6,$fd,$db,$4e,$ce,$d7,$be,$9d,$97
                    fcb       $84,$f9,$0a,$f1,$e2,$fc,$11,$fd,$df,$df,$90,$39,$bd,$9f
                    fcb       $1f,$78,$b6,$5f,$d5,$e2,$b7,$1e,$ab,$4b,$81,$b9,$eb,$7f
                    fcb       $3b,$8f,$79,$e6,$fc,$5d,$09,$99,$4e,$77,$fa,$e9,$93,$3c
                    fcb       $ee,$96,$fe,$1f,$31,$86,$53,$9c,$ec,$70,$e8,$0f,$bd,$c3
                    fcb       $cf,$78,$d0,$0c,$32,$9d,$7f,$d7,$1e,$30,$fb,$dc,$36,$ff
                    fcb       $73,$e2,$48,$4d,$54,$ed,$e7,$66,$e1,$c6,$1f,$7b,$87,$6e
                    fcb       $c7,$27,$8c,$a0,$a6,$53,$8f,$e7,$66,$e1,$c6,$16,$fb,$de
                    fcb       $72,$cd,$f1,$34,$14,$ca,$53,$99,$f3,$e5,$b8,$71,$1c,$dd
                    fcb       $67,$39,$c9,$e3,$88,$39,$7c,$f9,$9a,$e7,$76,$5b,$23,$73
                    fcb       $90,$78,$e2,$9c,$ba,$fa,$6b,$9d,$d9,$60,$09,$f3,$8f,$8c
                    fcb       $29,$67,$5f,$7f,$d8,$05,$7c,$f8,$02,$2c,$ed,$ad,$9f,$0a
                    fcb       $d3,$05,$9d,$7f,$1a,$49,$3a,$e2,$0e,$e3,$50,$39,$75,$f6
                    fcb       $77,$f7,$ef,$8d,$a6,$c3,$b8,$db,$47,$2e,$72,$fe,$af,$c7
                    fcb       $1c,$d4,$0b,$5e,$6e,$df,$87,$3b,$d7,$1d,$40,$0f,$07,$2b
                    fcb       $b7,$c6,$1d,$eb,$86,$cc,$d1,$6b,$eb,$38,$fc,$61,$de,$78
                    fcb       $7e,$f5,$fe,$e3,$bf,$8b,$cb,$c3,$cc,$d7,$fb,$8f,$c5,$dd
                    fcb       $e3,$87,$fe,$5f,$ee,$3f,$86,$bb,$3e,$1f,$db,$e1,$fc,$7f
                    fcb       $c3,$b3,$b9,$bf,$fc,$3f,$c3,$bb,$dc,$f8,$6e,$1f,$86,$3b
                    fcb       $2b,$9f,$0f,$c3,$e1,$1d,$95,$cf,$87,$e1,$f0,$8b,$4b,$8f
                    fcb       $c3,$76,$fc,$2d,$78,$fd,$a5,$ff,$9a,$e7,$c5,$97,$8f,$d2
                    fcb       $7c,$5a,$4b,$9f,$16,$77,$6f,$de,$ef,$8a,$cf,$6e,$6d,$87
                    fcb       $f7,$fb,$c6,$a7,$dd,$cd,$ae,$fe,$ff,$78,$68,$66,$2e,$6d
                    fcb       $77,$37,$f7,$87,$33,$02,$e7,$c1,$b9,$bd,$3c,$6d,$81,$bf
                    fcb       $78,$35,$bf,$ef,$1b,$60,$6b,$9f,$04,$b7,$e9,$91,$e1,$98
                    fcb       $13,$f7,$82,$7f,$5f,$80,$3c,$2b,$32,$ae,$7c,$0b,$fa,$e9
                    fcb       $18,$70,$0f,$de,$05,$fd,$7f,$fa,$0e,$03,$d3,$c0,$b9,$ee
                    fcb       $90,$07,$01,$fd,$6b,$39,$ef,$d1,$1c,$06,$e6,$d6,$76,$3b
                    fcb       $7e,$37,$db,$e0,$1d,$8e,$9b,$de,$01,$5b,$f5,$e2,$49,$11
                    fcb       $d6,$ec,$7f,$fb,$ef,$c6,$77,$bd,$2f,$0c,$cc,$d9,$ba,$dc
                    fcb       $ef,$4e,$53,$e3,$9f,$bb,$b3,$33,$30,$3a,$dc,$ef,$ff,$73
                    fcb       $d3,$bb,$49,$22,$3a,$dc,$ef,$ff,$73,$d2,$b7,$63,$53,$d4
                    fcb       $75,$ae,$75,$74,$ee,$7f,$67,$f4,$23,$d4,$75,$b8,$55,$ff
                    fcb       $7c,$f4,$db,$9e,$66,$7a,$8e,$b7,$0a,$bf,$ef,$9e,$98,$5a
                    fcb       $ae,$14,$e9,$7c,$fe,$0b,$55,$c2,$9f,$f6,$8e,$8d,$7e,$9f
                    fcb       $bd,$7f,$da,$3a,$77,$f7,$cf,$fe,$d1,$d3,$a5,$f1,$f3,$ff
                    fcb       $b4,$74,$ef,$ee,$be,$96,$8f,$d7,$a7,$5d,$cd,$f3,$fa,$f4
                    fcb       $eb,$ed,$f8,$f7,$5d,$e9,$d7,$ff,$27,$c7,$7b,$17,$a7,$5f
                    fcb       $ff,$03,$ab,$3a,$5e,$9d,$7f,$fd,$57,$4b,$d3,$b7,$a7,$c7
                    fcb       $d9,$0f,$d7,$0e,$de,$9c,$0f,$00,$3a,$5c,$3b,$7f,$ed,$96
                    fcb       $60,$e9,$70,$ed,$ff,$b6,$50,$0e,$97,$0e,$df,$fb,$64,$00
                    fcb       $e9,$70,$ed,$ff,$b6,$59,$1b,$a5,$c3,$b7,$ff,$23,$82,$74
                    fcb       $b8,$76,$ff,$b6,$1d,$cf,$4b,$87,$6f,$fe,$61,$dc,$f4,$b8
                    fcb       $76,$ff,$e6,$1d,$cf,$4b,$87,$6f,$fe,$61,$dc,$f4,$b8,$76
                    fcb       $ff,$e4,$70,$4e,$97,$0e,$df,$fb,$6b,$a5,$c3,$b7,$9c,$e0
                    fcb       $5d,$77,$e7,$76,$bb,$79,$ce,$05,$d7,$7e,$77,$6b,$b7,$fe
                    fcb       $da,$e9,$70,$ed,$ff,$b6,$ba,$5c,$3b,$7a,$75,$5f,$ae,$1d
                    fcb       $bd,$3a,$af,$d7,$0e,$bf,$fe,$ab,$a5,$e9,$d7,$ff,$d5,$74
                    fcb       $bd,$3a,$ff,$fa,$ae,$97,$a7,$5f,$ff,$55,$d3,$fa,$f7,$48
                    fcb       $d3,$b5,$d7,$d2,$d1,$fb,$9a,$a6,$66,$67,$1b,$af,$a5,$a3
                    fcb       $f7,$35,$4c,$cc,$ce,$37,$cf,$fe,$d1,$d3,$fb,$54,$8d,$3b
                    fcb       $5f,$3f,$fb,$47,$4f,$ed,$5a,$99,$f3,$7c,$ff,$ed,$1d,$3f
                    fcb       $b5,$08,$cf,$9a,$e5,$5d,$2f,$9f,$da,$76,$14,$cc,$cf,$9b
                    fcb       $f0,$9e,$6d,$ae,$66,$16,$a3,$f0,$aa,$f1,$94,$5d,$b3,$d5
                    fcb       $a8,$fc,$29,$f1,$4c,$de,$29,$29,$6a,$bf,$0b,$fe,$00,$91
                    fcb       $3b,$da,$9f,$77,$c5,$cb,$c2,$a0,$22,$2f,$37,$06,$40,$4d
                    fcb       $a7,$c1,$20,$23,$d7,$c1,$20,$44,$4e,$26,$bc,$97,$17,$8b
                    fcb       $dc,$af,$6b,$db,$db,$3a,$f5,$ab,$39,$6e,$17,$8f,$2c,$4d
                    fcb       $c6,$bd,$6b,$37,$0e,$f1,$e5,$8a,$71,$af,$43,$dd,$c6,$f0
                    fcb       $17,$c7,$95,$8a,$71,$af,$42,$87,$1b,$c0,$2e,$bc,$ac,$45
                    fcb       $4a,$32,$b4,$a3,$63,$69,$25,$12,$a4,$96,$0a,$1e,$cf,$6a
                    fcb       $49,$bb,$85,$d7,$95,$84,$88,$d0,$c8,$db,$41,$36,$cc,$8d
                    fcb       $33,$6c,$c8,$f5,$99,$94,$db,$68,$c8,$db,$b8,$5d,$79,$58
                    fcb       $e5,$ec,$7a,$88,$8f,$51,$15,$22,$32,$cf,$7d,$66,$65,$23
                    fcb       $29,$bd,$b5,$57,$5e,$56,$39,$39,$3d,$44,$47,$a8,$8d,$b2
                    fcb       $23,$2c,$79,$66,$65,$23,$29,$9d,$fd,$75,$e5,$63,$96,$b1
                    fcb       $ea,$6c,$8f,$51,$19,$11,$1b,$7b,$be,$d3,$45,$6c,$da,$36
                    fcb       $f7,$77,$0b,$af,$2f,$67,$2a,$d2,$49,$24,$91,$36,$58,$9b
                    fcb       $6a,$95,$53,$6d,$2a,$55,$9a,$b6,$ee,$17,$5e,$5d,$5a,$eb
                    fcb       $cb,$83,$42,$ea,$33,$2f,$b5,$2e,$a3,$32,$fb,$52,$ea,$56
                    fcb       $5f,$6a,$5d,$46,$65,$f6,$a5,$d4,$66,$70,$68,$5d,$46,$67
                    fcb       $56,$ba,$f2,$ea,$d7,$5e,$5d,$5a,$eb,$ca,$b7,$cd,$5b,$ba
                    fcb       $5d,$79,$46,$dc,$3a,$9b,$78,$85,$d7,$96,$52,$e5,$ea,$9e
                    fcb       $21,$75,$e5,$94,$fc,$b1,$4f,$10,$ba,$f2,$ca,$7e,$26,$53
                    fcb       $6e,$e2,$95,$a7,$f0,$ba,$f2,$ca,$7e,$db,$14,$db,$bf,$3d
                    fcb       $52,$fe,$5f,$1e,$59,$4a,$af,$54,$29,$6a,$3d,$52,$fa,$65
                    fcb       $3f,$28,$52,$d4,$f5,$32,$fa,$46,$5c,$ec,$8c,$9a,$d4,$14
                    fcb       $f8,$8f,$b6,$9a,$b7,$1d,$56,$ad,$df,$bd,$56,$9f,$10,$05
                    fcb       $a6,$fe,$37,$8e,$ab,$33,$33,$1c,$f3,$f8,$de,$3a,$8c,$66
                    fcb       $36,$3b,$0e,$26,$94,$66,$66,$76,$1c,$4d,$14,$cc,$cc,$ec
                    fcb       $38,$94,$96,$33,$27,$3c,$f8,$15,$bb,$e0,$56,$ef,$5e,$5e
                    fcb       $ac,$9c,$b7,$27,$c0,$4f,$e9,$ab,$ca,$b7,$2d,$66,$e5,$b8
                    fcb       $8b,$d7,$da,$87,$21,$ee,$a4,$70,$2f,$05,$eb,$ed,$43,$90
                    fcb       $a1,$f6,$e2,$2f,$5c,$94,$ce,$42,$85,$22,$49,$bb,$bd,$2b
                    fcb       $4b,$e5,$eb,$92,$99,$c9,$99,$93,$46,$46,$dd,$93,$d5,$2f
                    fcb       $a7,$ab,$15,$65,$8c,$cc,$a4,$6f,$6f,$2b,$d5,$2f,$a7,$ab
                    fcb       $1f,$6e,$4c,$cc,$a4,$67,$7b,$f5,$32,$fa,$7a,$b1,$f6,$e4
                    fcb       $9a,$29,$1b,$7b,$af,$28,$a5,$fc,$be,$3c,$a7,$b1,$cb,$da
                    fcb       $9b,$69,$a2,$6a,$db,$b2,$f5,$5a,$7f,$0b,$af,$2f,$f5,$fb
                    fcb       $de,$15,$75,$e5,$cc,$78,$0d,$e1,$57,$5e,$5c,$c7,$ca,$d7
                    fcb       $5e,$5d,$5a,$eb,$cb,$a9,$42,$66,$7f,$0b,$af,$2e,$a7,$31
                    fcb       $86,$7f,$0b,$af,$2e,$a4,$06,$19,$fc,$2e,$bc,$ba,$94,$26
                    fcb       $ab,$62,$8b,$a9,$59,$d4,$a0,$a6,$5b,$0a,$5d,$47,$ae,$a5
                    fcb       $05,$32,$d8,$99,$75,$24,$5f,$6a,$5d,$47,$ab,$ed,$4b,$a8
                    fcb       $f5,$88,$9f,$01,$45,$d4,$7a,$c0,$06,$fe,$57,$5e,$58,$01
                    fcb       $9f,$ca,$eb,$cb,$00,$33,$f9,$5d,$79,$62,$26,$7f,$2b,$af
                    fcb       $2f,$cf,$e5,$75,$e5,$f9,$fc,$ae,$bc,$bf,$3f,$95,$d7,$97
                    fcb       $e7,$f2,$ba,$f2,$fc,$fe,$6f,$8f,$2f,$cf,$a7,$e7,$d3,$f3
                    fcb       $e9,$f9,$c1,$fd,$ff,$d6,$ef,$cb,$6d,$9d,$16,$ef,$cb,$6c
                    fcb       $33,$6d,$16,$ef,$cb,$6e,$66,$19,$96,$ef,$cb,$6c,$66,$2b
                    fcb       $2d,$df,$96,$da,$98,$7a,$b7,$7e,$70,$79,$3d,$44,$fe,$9f
                    fcb       $9f,$4a,$89,$9f,$4a,$41,$9f,$29,$f0,$55,$01,$be,$94,$03
                    fcb       $3e,$95,$13,$3e,$9f,$9f,$4f,$cf,$a7,$e7,$d3,$f3,$e9,$f9
                    fcb       $f4,$fc,$fa,$7e,$7d,$3f,$3e,$9f,$9f,$4f,$cf,$a0,$36,$07
                    fcb       $d1,$00,$0f,$a2,$00,$1f,$40,$6d,$0f,$a0,$10,$1f,$40,$20
                    fcb       $3e,$9f,$9f,$4f,$cf,$a7,$e7,$d3,$f3,$e9,$f9,$c4,$b5,$e1
                    fcb       $f9,$c4,$b5,$e1,$f9,$f4,$fc,$fa,$7e,$7d,$3f,$3e,$9f,$9f
                    fcb       $4f,$cf,$a7,$e7,$d3,$cc,$3e,$9e,$43,$82,$f3,$62,$7b,$b2
                    fcb       $f0,$f2,$3b,$f3,$c5,$1e,$26,$fe,$d1,$c9,$97,$87,$91,$c1
                    fcb       $d1,$b4,$23,$5b,$bc,$ce,$1a,$56,$9d,$8a,$bc,$c9,$d4,$15
                    fcb       $cd,$19,$99,$87,$f6,$e6,$bd,$52,$dd,$e2,$60,$ea,$05,$e0
                    fcb       $66,$66,$6d,$6e,$e6,$bd,$52,$de,$af,$30,$75,$05,$7f,$68
                    fcb       $cc,$c8,$b7,$73,$7d,$4c,$ee,$cb,$cc,$1d,$42,$9f,$da,$b6
                    fcb       $8a,$e6,$ad,$21,$4b,$7a,$bc,$c9,$d4,$15,$6c,$39,$be,$ab
                    fcb       $4f,$97,$af,$6a,$00,$60,$00,$30,$ff,$c0

; SinCos table for analog tach & speedo

SinCos              fcb       $04,$6c,$03,$6b,$03,$6a,$03,$69,$03,$68,$02,$67,$02,$66
                    fcb       $02,$65,$02,$64,$01,$63,$01,$62,$01,$61,$01,$60,$01,$5f
                    fcb       $00,$5e,$00,$5d,$00,$5c,$00,$5b,$00,$5a,$00,$59,$00,$58
                    fcb       $00,$57,$00,$56,$00,$55,$00,$54,$00,$53,$00,$52,$00,$51
                    fcb       $00,$50,$00,$4f,$00,$4e,$00,$4d,$00,$4c,$00,$4b,$00,$4a
                    fcb       $00,$49,$00,$48,$00,$47,$00,$46,$00,$45,$01,$44,$01,$43
                    fcb       $01,$42,$01,$41,$01,$40,$02,$3f,$02,$3e,$02,$3d,$02,$3c
                    fcb       $03,$3b,$03,$3a,$03,$39,$03,$38,$04,$37,$04,$36,$04,$35
                    fcb       $05,$35,$05,$34,$06,$33,$06,$32,$06,$31,$07,$30,$07,$2f
                    fcb       $08,$2e,$08,$2d,$08,$2c,$09,$2b,$09,$2a,$0a,$2a,$0a,$29
                    fcb       $0b,$28,$0b,$27,$0c,$26,$0c,$25,$0d,$24,$0e,$24,$0e,$23
                    fcb       $0f,$22,$0f,$21,$10,$20,$10,$20,$11,$1f,$12,$1e,$12,$1d
                    fcb       $13,$1c,$14,$1c,$14,$1b,$15,$1a,$16,$19,$16,$19,$17,$18
                    fcb       $18,$17,$18,$17,$19,$16,$1a,$15,$1b,$15,$1b,$14,$1c,$13
                    fcb       $1d,$13,$1e,$12,$1e,$11,$1f,$11,$20,$10,$21,$0f,$22,$0f
                    fcb       $22,$0e,$23,$0e,$24,$0d,$25,$0d,$26,$0c,$27,$0c,$28,$0b
                    fcb       $28,$0b,$29,$0a,$2a,$0a,$2b,$09,$2c,$09,$2d,$08,$2e,$08
                    fcb       $2f,$07,$2f,$07,$30,$06,$31,$06,$32,$06,$33,$05,$34,$05
                    fcb       $35,$05,$36,$04,$37,$04,$38,$04,$39,$03,$3a,$03,$3b,$03
                    fcb       $3c,$02,$3d,$02,$3e,$02,$3f,$02,$40,$01,$41,$01,$42,$01
                    fcb       $43,$01,$44,$01,$45,$01,$46,$00,$47,$00,$48,$00,$49,$00
                    fcb       $49,$00,$4a,$00,$4b,$00,$4c,$00,$4d,$00,$4e,$00,$4f,$00
                    fcb       $50,$00,$51,$00,$52,$00,$53,$00,$54,$00,$55,$00,$56,$00
                    fcb       $57,$00,$58,$00,$59,$00,$5a,$00,$5b,$00,$5c,$00,$5d,$00
                    fcb       $5e,$01,$5f,$01,$60,$01,$61,$01,$62,$01,$63,$01,$64,$02
                    fcb       $65,$02,$66,$02,$67,$02,$68,$03,$69,$03,$6a,$03,$6b,$04
                    fcb       $6c,$04,$6d,$04,$6e,$05,$6f,$05,$70,$05,$71,$06,$72,$06
                    fcb       $73,$06,$74,$07,$74,$07,$75,$08,$76,$08,$77,$09,$78,$09
                    fcb       $79,$0a,$7a,$0a,$7b,$0b,$7b,$0b,$7c,$0c,$7d,$0c,$7e,$0d
                    fcb       $7f,$0d,$80,$0e,$81,$0e,$81,$0f,$82,$0f,$83,$10,$84,$11
                    fcb       $85,$11,$85,$12,$86,$13,$87,$13,$88,$14,$88,$15,$89,$15
                    fcb       $8a,$16,$8b,$17,$8b,$17,$8c,$18,$8d,$19,$8d,$19,$8e,$1a
                    fcb       $8f,$1b,$8f,$1c,$90,$1c,$91,$1d,$91,$1e,$92,$1f,$93,$20
                    fcb       $93,$20,$94,$21,$94,$22,$95,$23,$95,$24,$96,$24,$97,$25
                    fcb       $97,$26,$98,$27,$98,$28,$99,$29,$99,$2a,$9a,$2a,$9a,$2b
                    fcb       $9b,$2c,$9b,$2d,$9b,$2e,$9c,$2f,$9c,$30,$9d,$31,$9d,$32
                    fcb       $9d,$33,$9e,$34,$9e,$35,$9f,$35,$9f,$36,$9f,$37,$a0,$38
                    fcb       $a0,$39,$a0,$3a,$a0,$3b,$a1,$3c,$a1,$3d,$a1,$3e,$a1,$3f
                    fcb       $a2,$40,$a2,$41,$a2,$42,$a2,$43,$a2,$44,$a3,$45,$a3,$46
                    fcb       $a3,$47,$a3,$48,$a3,$49,$a3,$4a,$a3,$4b,$a3,$4c,$a3,$4d
                    fcb       $a3,$4e,$a3,$4f,$a3,$50,$a3,$51,$a3,$52,$a3,$53,$a3,$54
                    fcb       $a3,$55,$a3,$56,$a3,$57,$a3,$58,$a3,$59,$a3,$5a,$a3,$5b
                    fcb       $a3,$5c,$a3,$5d,$a3,$5e,$a2,$5f,$a2,$60,$a2,$61,$a2,$62
                    fcb       $a2,$63,$a1,$64,$a1,$65,$a1,$66,$a1,$67,$a0,$68,$a0,$69
                    fcb       $a0,$6a,$a0,$6b

; table for calculating temperature bar-graph length from input A/D sample

TempTable           fcb       208,197,186,175,163,152,141,131,121,112,103,95,88
                    fcb       81,75,69,64,59,55,51,47,44,41,38,35,33,31,29,27,25,24
                    fcb       22,21,20,19,18,17,16,15,14,13,13,12,12,11,10,10,9,0

;*******************************************************************************

                    @vector   Vrti,Int30MS
                    @vector   Virq,IntPowerDown
                    @vector   Vreset,Start

                    end       :s19crc
