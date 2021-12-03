;*******************************************************************************
; Benjamin T. Fenner, FE3471 Wayne State University
; This Program will simulate a simple clock with Hours(12) & Minutes(60) on 4 7-Segment Displays.
; Using pin PE7 to adjust time and alarm time.
; The user can change the alarm time by pressing PA0. If the user presses PA0 again The program will allow them to change the time.
; Each digit can be changed by using the Potentiometer. Once the desired digit is displayed press to advance to next digit.
; PM is the dot on the leftmost 7-Segment Display. Once PM/AM is selected the program will return you to regular clock.
; When the alarm time and clock time are equal the buzzer will activate. Press PA0 to turn alarm off. (Note changing time and..
; the alarm will not be active) Alarm automaticly turns off after 30 seconds.
; Version 06 -- 2017.12.14
;*******************************************************************************

BUS_KHZ             equ       2000

PD2                 equ       1<2
PD3                 equ       1<3
PD4                 equ       1<4
PD5                 equ       1<5

REGS                equ       $1000
PORTB               equ       REGS+$04
PORTD               equ       REGS+$08
DDRD                equ       REGS+$09
TCNT                equ       REGS+$0E
TOC3                equ       REGS+$1A
TOC5                equ       REGS+$1E
TCTL1               equ       REGS+$20
TCTL2               equ       REGS+$21
TMSK1               equ       REGS+$22
TFLG1               equ       REGS+$23
TMSK2               equ       REGS+$24
TFLG2               equ       REGS+$25
PACTL               equ       REGS+$26
ADCTL               equ       REGS+$30
ADR1                equ       REGS+$31

;*******************************************************************************
; Variables (why declared this way and not with RMB?)

counter             equ       $000C,2
alarm1              equ       $001C,1
alarm30             equ       $001D,2

;*******************************************************************************
                    #RAM      $0000
          ;-------------------------------------- ; List of Hex Values 0-9 for 7-Segment
Zero                fcb       $3F
One                 fcb       $06
Two                 fcb       $5B
Three               fcb       $4F
Four                fcb       $66
Five                fcb       $6D
Six                 fcb       $7D
Seven               fcb       $07
Eight               fcb       $7F
Nine                fcb       $6F
                    fcb       $0A,$00,$00,$00     ;what are these?

                    #RAM      $0010
          ;-------------------------------------- ; Time followed by alarm time
hh                  fcb       $06,$06
mm                  fcb       $3F,$3F
pm_am               fcb       $80
alarm_hh            fcb       $06,$06
alarm_mm            fcb       $3F,$06
alarm_pm_am         fcb       $80
                    fcb       $77,$78,$00,$00,$00,$00 ;what are these?

;*******************************************************************************
                    #ROM      $C000
;*******************************************************************************

Start               proc
                    ldx       #REGS
                    lds       #$8FFF              ; Load Stack
                    lda       #%11000011          ; configure PD2-PD5 as output
                    sta       PORTD
          ;-------------------------------------- ; Turns on A/D conversions
                    lda       #%00100111          ; Turns on PE7
                    sta       [ADCTL,x            ; this triggers the A/D
          ;-------------------------------------- ; Sets up PA0
                    lda       #1                  ; Let TCTL2 to accept a rising edge on PA0.
                    sta       [TCTL2,x
          ;-------------------------------------- ; Output Compare
                    ldd       TCNT                ; Loads REG D as the current time
                    std       TOC5                ; Saves REG D into the time keep REG TOC5 so interrupt can happen
                    lda       #$29                ; Loads REG A as 0010 1000
                    sta       TFLG1               ; Clears Flag OC5F & IC3F
                    lda       #$08
                    sta       TMSK1               ; Sets the 0C5I to allow intrupts
                    bra       Back

;*******************************************************************************

BackClr             proc
                    lda       #$01                ; Let TCTL2 to accept a rising edge on PA0. * PA0 Is reset to accpet a rising edge: PA0 1 in Bit 0
                    sta       TFLG1               ; Clear Flag at IC3F so a capture can be seen on a falling edge.
;                   bra       Back

;*******************************************************************************

Back                proc
Loop@@              cli                           ; Unmask IRQ Interrupts

                    lda       hh                  ; Load High Digit of Hour
                    sta       PORTB
                    lda       #PD5
                    sta       DDRD                ; Turn on PD5 7-Display
                    jsr       Delay

                    lda       hh+1                ; Load Low Digit of Hour
                    sta       PORTB
                    lda       #PD4
                    sta       DDRD                ; Turn on PD4 7-Display
                    bsr       Delay

                    lda       mm                  ; Load High Digit of Minute
                    sta       PORTB
                    lda       #PD3
                    sta       DDRD                ; Turn on PD3 7-Display
                    bsr       Delay

                    lda       mm+1                ; Load Low Digit of Minute
                    sta       PORTB
                    lda       #PD2
                    sta       DDRD                ; Turn on PD2 7-Display
                    bsr       Delay

                    lda       pm_am               ; PM/AM
                    sta       PORTB
                    lda       #PD4
                    sta       DDRD                ; Turn on PD5 7-Display
                    bsr       Delay
          ;--------------------------------------
          ; Following code will check to see if the alarm is on.
          ; i.e., the current time equals alarm time and then it
          ; will wait for the PAO to be pressed.
          ;--------------------------------------
                    lda       alarm1              ; Checks if on
                    bne       AlarmOn@@
                    jsr       AlarmCheck
                    bra       Forward

AlarmOn@@           brclr     [TFLG1,x,#1,Loop@@  ; This will check if the flag on TMSK1 bit 0 is flagged to 1.
                    clra                          ; Reset so alarm is off. $01 on, $00 off.
                    sta       alarm1
;                   clra                          ; Set OM3 and OL3 in TCTL1 to
                    sta       TCTL1               ; 01 so PA5 will toggle on each compare
?Back               bra       BackClr

;*******************************************************************************
; Following code will allow user to change time or alarm with the potentiometer.

Forward             proc
                    brclr     [TFLG1,x,#1,Back    ; This will check if the flag on TMSK1 bit 0 is flagged to 1.

                    lda       #$01                ; Let TCTL2 to accept a rising edge on PA0.
                    sta       [TFLG1,x            ; Clear Flag at IC3F so a capture can be seen.
                    pshy

                    ldy       #$001A              ; Set REG Y to Location for A in HEX
                    jsr       DisA1t1             ; Is a 5 second delay to give user time to press PA0 to...
          ;-------------------------------------- ; advance to Clock seting, Displays A1.
                    brclr     [TFLG1,x,#1,_1@@    ; This will check if the flag on TMSK1 bit 0 is flagged to 1.

                    ldy       #$001B              ; Set REG Y to Location for A in HEX
                    jsr       DisA1t1             ; Is a 5 second delay Displays t1.

                    ldy       #$0010
                    jsr       TimeSet             ; Will set time for the Clock
                    puly

                    bsr       DelayOneSec
                    bra       ?Back               ; Resets the PA0 Flag

_1@@                lda       #$01                ; Clear TCTL2
                    sta       [TFLG1,x            ; Set Flag at IC3F
                    ldy       #$0015
                    jsr       TimeSet             ; Will set time for the alarm
                    puly

                    bsr       DelayOneSec
                    bra       ?Back               ; Resets the PA0 Flag

                    swi

;*******************************************************************************
; This sub-program will look where the Hex digit is located so that the program...
; can return the next increment of Hex Digit. if $06 it would change to $5B

Convert             proc
                    clrx                          ; Load X as zero
Loop@@              ldb       ,x                  ; Load The Hex digits 1-9
                    inx                           ; Increase X by 1
                    cba                           ; Compare The Hex code in mem to REG A Hex Code
                    bne       Loop@@
                    rts

;*******************************************************************************
; Delay for 5ms so the 7-Segment Display has time to shine
                              #Cycles 6
Delay               proc
                    pshx
                    ldx       #DELAY@@
                              #Cycles
Loop@@              dex
                    bne       Loop@@
                              #temp :cycles
                    pulx
                    rts

DELAY@@             equ       3*BUS_KHZ-:cycles-:ocycles/:temp

;*******************************************************************************
; One second delay
                              #Cycles 6
DelayOneSec         proc
                    pshx
                    ldx       #DELAY@@
                              #Cycles
Loop@@              dex
                    bne       Loop@@
                              #temp :cycles
                    pulx
                    rts

DELAY@@             equ       BUS_KHZ-:cycles-:ocycles/:temp

;*******************************************************************************
; Low Hour and Low Minute set with potentiometer

LH                  proc
                    lda       #$01                ; Let TCTL2 to accept a rising edge on PA0.
                    sta       [TFLG1,x            ; Clear Flag at IC3F so a capture can be seen.
                    ldx       #REGS
Loop@@              ldb       [ADR1,x
                    cmpb      #25
                    bls       _0@@
                    cmpb      #50
                    bls       _1@@
                    cmpb      #75
                    bls       _2@@
                    cmpb      #100
                    bls       _3@@
                    cmpb      #125
                    bls       _4@@
                    cmpb      #150
                    bls       _5@@
                    cmpb      #175
                    bls       _6@@
                    cmpb      #200
                    bls       _7@@
                    cmpb      #225
                    bls       _8@@
                    cmpb      #255
                    bls       _9@@
_0@@                lda       Zero
                    bra       Save@@

_1@@                lda       One
                    bra       Save@@

_2@@                lda       Two
                    bra       Save@@

_3@@                lda       Three
                    bra       Save@@

_4@@                lda       Four
                    bra       Save@@

_5@@                lda       Five
                    bra       Save@@

_6@@                lda       Six
                    bra       Save@@

_7@@                lda       Seven
                    bra       Save@@

_8@@                lda       Eight
                    bra       Save@@

_9@@                lda       Nine
Save@@              sta       PORTB
                    brclr     [TFLG1,x,#1,Loop@@  ; This will check if the flag on TMSK1 bit 0 is flagged to 1.
                    rts

;*******************************************************************************
; High Minute Potentiometer sub 0-5

HM                  proc
                    lda       #PD3
                    sta       DDRD                ; Turn on PD3 7-Display
                    lda       #$01                ; Let TCTL2 to accept a rising edge on PA0.
                    sta       [TFLG1,x            ; Clear Flag at IC3F so a capture can be seen.
Loop@@              ldb       [ADR1,x
                    cmpb      #42
                    bls       _0@@
                    cmpb      #84
                    bls       _1@@
                    cmpb      #126
                    bls       _2@@
                    cmpb      #168
                    bls       _3@@
                    cmpb      #210
                    bls       _4@@
                    cmpb      #255
                    bls       _5@@
_0@@                lda       Zero
                    bra       Save@@

_1@@                lda       One
                    bra       Save@@

_2@@                lda       Two
                    bra       Save@@

_3@@                lda       Three
                    bra       Save@@

_4@@                lda       Four
                    bra       Save@@

_5@@                lda       Five
Save@@              sta       PORTB
                    brclr     [TFLG1,x,#1,Loop@@  ; This will check if the flag on TMSK1 bit 0 is flagged to 1.
                    rts

;*******************************************************************************
; Low Hour with High Hour being 1 Potentiometer sub 0-2

IfHH1               proc
                    lda       #PD4
                    sta       DDRD                ; Turn on PD4 7-Display
                    lda       #$01                ; Let TCTL2 to accept a rising edge on PA0.
                    sta       [TFLG1,x            ; Clear Flag at IC3F so a capture can be seen.
Loop@@              ldb       [ADR1,x
                    cmpb      #85
                    bls       _1@@
                    cmpb      #170
                    bls       _2@@
                    cmpb      #255
                    bls       _3@@
_1@@                lda       Zero
                    bra       Save@@

_2@@                lda       One
                    bra       Save@@

_3@@                lda       Two
Save@@              sta       PORTB
                    brclr     [TFLG1,x,#1,Loop@@  ; This will check if the flag on TMSK1 bit 0 is flagged to 1.
                    rts

;*******************************************************************************
; High Hour Set With Potentiometer

Hour                proc
                    lda       #PD5
                    sta       DDRD                ; Turn on PD5 7-Display
                    lda       #$01                ; Let TCTL2 to accept a rising edge on PA0.
                    sta       [TFLG1,x            ; Clear Flag at IC3F so a capture can be seen.
Loop@@              ldb       [ADR1,x
                    cmpb      #125
                    bls       _1@@
                    cmpb      #255
                    bls       _2@@
_1@@                lda       Zero
                    bra       Save@@

_2@@                lda       One
Save@@              sta       PORTB
                    brclr     [TFLG1,x,#1,Loop@@  ; This will check if the flag on TMSK1 bit 0 is flagged to 1.
                    rts

;*******************************************************************************
; PM or AM Set with Potentiometer

AMPM                proc
                    lda       #PD4
                    sta       DDRD                ; Turn on PD5 7-Display
                    lda       #$01                ; Let TCTL2 to accept a rising edge on PA0.
                    sta       [TFLG1,x            ; Clear Flag at IC3F so a capture can be seen.
Loop@@              ldb       [ADR1,x
                    cmpb      #125
                    bls       AM@@
                    cmpb      #255
                    bls       PM@@
AM@@                clra
                    bra       Save@@

PM@@                lda       #$80
Save@@              sta       PORTB
                    brclr     [TFLG1,x,#1,Loop@@  ; This will check if the flag on TMSK1 bit 0 is flagged to 1.
                    rts

;*******************************************************************************
; Lets user set time or alarm.

TimeSet             proc
                    jsr       Delay               ; Set AM or PM
                    bsr       AMPM
                    sta       4,y

                    jsr       Delay               ; Sets High Hour of Alarm
                    bsr       Hour
                    sta       ,y

                    lda       hh
                    cmpa      #6
                    bne       _1@@
          ;-------------------------------------- ; IF HH is 1 code.
                    jsr       Delay
                    jsr       IfHH1               ; If the High hour is 1 this will only allow Low Hour to be 0,1, or 2.
                    sta       1,y
                    bra       Min@@               ; Moves to the next digit.

_1@@                lda       #PD4                ; Sets Low hour of Alarm
                    sta       DDRD                ; Turn on PD3 7-Display
                    jsr       Delay
                    jsr       LH
                    sta       1,y

Min@@               jsr       Delay               ; Sets High minute of Alarm
                    jsr       HM
                    sta       2,y

                    lda       #PD2                ; Sets Low minute of Alarm
                    sta       DDRD                ; Turn on PD5 7-Display
                    jsr       Delay
                    jsr       LH
                    sta       3,y

                    jmp       DelayOneSec         ; TFLG1 has a 0 in bit 0. So it will not accept a rising edge.

;*******************************************************************************
; This will Display A1 or T1 so the user knows what is active for change.
; It is a 5 second delay.

DisA1t1             proc
                    pshb
                    pshx
                    ldb       #120
Loop@@              ldx       #65535
InnerLoop@@         dex

                    lda       ,y                  ; Load A in HEX
                    lda       #PD4
                    sta       DDRD                ; Turn on PD4 7-Display
                    jsr       Delay

                    lda       #$06                ; Load 1 in HEX
                    sta       PORTB
                    lda       #PD3
                    sta       DDRD                ; Turn on PD3 7-Display
                    jsr       Delay

                    bne       InnerLoop@@
                    decb
                    bne       Loop@@
                    pulx
                    pulb
                    rts

;*******************************************************************************
; This sub will check to see if the current time equals alarm time.

AlarmCheck          proc
                    lda       hh                  ; Load High Hour
                    cmpa      alarm_hh            ; Compare to Alarm High Hour
                    bne       Done@@
                    lda       hh+1                ; Load Low Hour
                    cmpa      alarm_hh+1          ; Compare to Alarm Low Hour
                    bne       Done@@
                    lda       mm                  ; Load High Minute
                    cmpa      alarm_mm            ; Compare to Alarm High Minute
                    bne       Done@@
                    lda       mm+1                ; Load Low Minute
                    cmpa      alarm_mm+1          ; Compare to Alarm Low Minute
                    bne       Done@@
                    lda       pm_am               ; Load AMPM
                    cmpa      alarm_pm_am         ; Compare to Alarm AMPM
                    bne       Done@@
                    lda       #1
                    sta       alarm1              ; Stores 1 into Mem for a check if alarm on.
                    pshx
                    clrx                          ; Resets the alarm30 to #0 for a 30second delay.
                    stx       alarm30
                    ldx       counter
                    cpx       #100
                    pulx
                    bhi       Done@@              ; Makes sure buzzer doesn't come back on after alarm is off, 1 min.
                    lda       #$20
                    sta       TFLG1
                    lda       #$10                ; Set OM3 and OL3 in TCTL1 to
                    sta       TCTL1               ; 01 so PA5 will toggle on each compare
Done@@              rts

;*******************************************************************************
                    #VECTORS                      ; Keeping Time Interrupts
;*******************************************************************************

                    org       $00D3               ; Clock Interrupt
                    jmp       TimerHandler

                    org       $00D9               ; Speaker Interrupt
                    jmp       SpeakerHandler

;*******************************************************************************
                    #ROM
;*******************************************************************************

;*******************************************************************************
; This interrupt will allow the speaker to be heard

SpeakerHandler      proc
                    ldd       TOC3                ; TOC3 is connected to PA5
                    addd      #12000
                    std       TOC3
                    lda       #$20                ; Sets Flag
                    sta       TFLG1
                    rti

;*******************************************************************************
; This interrupt will increment time 1 minute at a time

TimerHandler        proc
                    lda       #$08                ; Loads REG A as 0000 1000
                    sta       TFLG1               ; Clears Flag OC5F
                    sta       TMSK1               ; Sets the 0C5I to allow intrupts
                    cli                           ; Unmask IRQ Interrupts

                    lda       alarm1
                    cmpa      #1                  ; Checks if alarm is on.
                    bne       Off@@
                    ldx       alarm30
                    inx
                    stx       alarm30
                    cpx       #916                ; Check if 30 seconds have passed.
                    bne       Off@@
                    clra                          ; Turns off alarm.
                    sta       alarm1
                    clrx                          ; Resets alarm for future use.
                    stx       alarm30

Off@@               ldx       counter
                    inx
                    stx       counter
                    cpx       #1830
                    bne       _1@@

                    ldd       TCNT
                    addd      #61800              ; 60/0.03277 = 1830.942935; .942935*.03277/.0005m = 61800
                    std       TOC5
                    bra       AnRTI

_1@@                cpx       #1831
                    bne       AnRTI
                    clrx
                    stx       counter
          ;-------------------------------------- ; now 1 minute has passed. We need to increase minutes by 1.
                    lda       mm+1                ; Load Low Minute
                    jsr       Convert             ; Convert to a num from 7-Segment Hex
                    lda       ,x                  ; Load New Hex
                    sta       mm+1                ; Changes A to next Hex value
                    cmpa      #10                 ; compare is X = 0009
                    bne       AnRTI               ; branch if !=0 Branch back

                    lda       Zero                ; Load A as zero
                    sta       mm+1                ; Reset low minute to zero
                    lda       mm                  ; Load High Minute
                    jsr       Convert             ; Convert to a num from 7-Segment Hex
                    lda       ,x                  ; Load new Hex
                    sta       mm                  ; Changes A to next Hex value
                    cmpa      #$7D                ; If High Minute = 6
                    bne       AnRTI               ; If !=0 branch back
          ;-------------------------------------- ; If 60 Minutes Increase hour By 1
                    lda       Zero                ; Load A as zero
                    sta       mm                  ; Reset High minute to zero
                    lda       hh                  ; Loading High bit to see if It's a 0,1
                    jsr       Convert
                    cmpa      #6                  ; Compare to 1
                    bne       _4@@                ; Branch to bits 0-9 if less than 1
                    lda       hh+1                ; Load low Hour
                    jsr       Convert             ; Convert to a num from 7-Segment Hex
                    lda       ,x                  ; Load New Hex
                    sta       hh+1                ; Changes A to next Hex value

                    cmpa      #$5B                ; Seeing if AM/PM Changed
                    bne       _3@@
                    lda       pm_am
                    cmpa      #$80                ; Check if it is PM or not
                    bne       _2@@
                    clra                          ; Set to AM
                    sta       pm_am
                    bra       AnRTI

_2@@                lda       #$80                ; Set to PM
_3@@                cmpa      #$4F                ; If Low Hours = 3
                    bne       AnRTI               ; If !=0 branch back
                    lda       One                 ; Load A as 01:00
                    sta       hh+1                ; Reset Low Hour to 01:00
                    lda       Zero                ; Seting High Hour to 0
                    sta       hh
                    bra       AnRTI
          ;-------------------------------------- ; If High Hour <= 1
_4@@                lda       hh+1                ; Load low Hour
                    jsr       Convert             ; Convert to a num from 7-Segment Hex
                    lda       ,x                  ; Load New Hex
                    sta       hh+1                ; Changes A to next Hex value
                    cmpa      #10                 ; If Low Hours = 10
                    bne       AnRTI               ; If !=0 branch back
          ;-------------------------------------- ; If High Hour > 1
                    lda       Zero                ; Load A as zero
                    sta       hh+1                ; Reset Low Hour to zero
                    lda       hh                  ; Load High Hour
                    jsr       Convert             ; Convert to a num from 7-Segment Hex
                    lda       ,x                  ; Load New Hex
                    sta       hh                  ; Changes A to next Hex value
AnRTI               rti
