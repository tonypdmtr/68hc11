;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;*             SYSTEM FOR THE REGULATION OF SOLDERING IRON (WELLER)
;*
;* - read the iron's temperature (tied to A/D0)
;* - read the la consigne de temperature (tied to A/D1)
;* - drive the iron resistance (I/O PB0)
;*
;* - display the temperature (I/O PD2 to PD4)
;*
;*                       -+-+-+-+-+-+-+-+-+-+-+-+-
;*
;* The display is done thru a MC14499 by MOTOROLA, and then via a MAX7221
;* sends it to the BCD digits.
;*
;*       MC14499                 68HC811E2 PORT D
;*       -------                 ---------------
;*       DATA(pin  5)            PD2 (pin 22 of E)
;*       CLK (pin 13)            PD3 (pin 23 of E)
;*       /ENB(pin 12)            PD4 (pin 24 of E)
;*******************************************************************************

REGS                def       0

                    #ListOff
                    #Uses     exp-f1.inc          ;found in ASM11 distribution
                    #ListOn

          #ifdef PORTB
PORT                def       PORTB
          #endif
PORT                def       PORTA

;*******************************************************************************
; MC14499 constants

MC_DATA             equ       %00000100           ;value for pin PD2
MC_CLK              equ       %00001000           ;value for pin PD3
MC_ENB              equ       %00010000           ;value for pin PD4

; Static Relay constants

CHAUFFE             equ       1

;*******************************************************************************
                    #RAM
;*******************************************************************************

RTI_Cnt             rmb       1                   ;compteur du TIC/TAC du RTI

AN_0                rmb       1                   ;temperature actuelle du fer
AN_1                rmb       1                   ;consigne de temperature relevee sur AN0

Digit12             rmb       1                   ;digit 1 et 2 (centaines et dizaines)
Digit34             rmb       1                   ;digit 3 et 4 (unit‚ et d‚cimale)
Points              rmb       1                   ;point decimal des afficheurs

I_Degree            rmb       2

C_Time              rmb       1                   ;compteur du temps d'affichage de la consigne
Consigne            rmb       1                   ;consigne de regulation

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    @SetChipSelects

                    lda       #ADPU.              ;start the A/D converter
                    sta       OPTION

                    lda       #%00000011          ;RTI prescaler regle a 32.768ms
                                                  ; 00=  4.096 ms
                                                  ; 01=  8.192 ms
                                                  ; 10= 16.384 ms
                                                  ; 11= 32.768 ms
                    sta       PACTL

                    lda       #%01000011          ;Prescaler set to E/16, soit 8us, TOF … 524.3ms
                    sta       TMSK2               ;bit 7 … 1 autorise INT sur Timer Overflow
                                                  ;bit 6 … 1 autorise INT sur Real Time Int.

                    lda       #%00010000          ;Lance ‚chantillonage sur AN1/AN4
                    sta       ADCTL

                    bset      PORTD,MC_DATA+MC_CLK+MC_ENB ;PD2, PD3 and PD4 are 1 (high)
                    bset      DDRD,MC_DATA+MC_CLK+MC_ENB  ;PD2, PD3 and PD4 are outputs

          ; set variable

                    clr       PORT                ;port B a zero (d‚sactive toutes les sorties)
                    clr       RTI_Cnt             ;mise a zero du compteur de RTI
                    clr       C_Time              ;mise a zero compteur affichage consigne

                    lda       #-1
                    sta       Consigne

                    clr       Digit12
                    clr       Digit34

                    lda       #40
                    sta       Points

                    cli                           ;Allow interrupts

;*******************************************************************************
; Purpose: Main Loop

MainLoop            proc
Loop@@              bsr       AFFICHE
                    bsr       REGULE
                    bsr       WRITE_4_DIGIT
                    bra       Loop@@

;*******************************************************************************
; Purpose: Display the temperature or the consigne

AFFICHE             proc
                    ldb       Consigne            ;lecture de la consigne
                    lda       C_Time              ;Lecture de la tempo d'affichage consigne
                    bne       ADC_2_DEGREE        ;egale a zero, alors affiche consigne
                    ldb       AN_0                ;lecture de la temperature
                    bclr      Points,#%01010000
;                   bra       ADC_2_DEGREE

;*******************************************************************************
; Purpose: Conversion of A/D reading to degrees
; Input  : B = A/D READING
; Output : Digit12 and Digit34 mis a jour
; Destroy: A, B and CCR

ADC_2_DEGREE        proc
                    clra
                    ldx       #51                 ;*5/255 = 1/51
                    pshx                          ;sauve diviseur pour FDIV
                    idiv
                    stx       I_Degree
                    pulx
                    fdiv

                    lda       I_Degree+1          ;centaine et dizaine
                    asla:4
                    sta       Digit12
                    xgdx
                    ldb       #10
                    mul
                    ora       Digit12
                    sta       Digit12

                    lda       #10                 ;unit‚ et decimale
                    mul
                    asla:4
                    sta       Digit34
                    lda       #10
                    mul
                    ora       Digit34
                    sta       Digit34

                    lda       Digit12             ;efface le z‚ro inutile pour les centaines
                    cmpa      #$10
                    bhs       Done@@
                    ora       #$F0
                    sta       Digit12
Done@@              rts

;*******************************************************************************
; Regulation subroutine

REGULE              proc
                    lda       C_Time              ;on change la consigne?
                    bne       Off@@               ;oui, alors on coupe la chauffe!

                    lda       AN_0                ;lecture de la sonde
                    cmpa      Consigne            ;compare avec la consigne
                    bls       On@@                ;inferieure ou egale

Off@@               bclr      Points,#%10000000   ;etteint le temoin de chauffe sur digit 1
                    bclr      PORT,#CHAUFFE       ;coupe la chauffe
                    rts

On@@                bset      PORT,#CHAUFFE       ;active la chauffe
                    rts

;*******************************************************************************
; Purpose: Displayt the value on the four digits

WRITE_4_DIGIT       proc
                    bclr      PORTD,#MC_ENB       ;force ENB … l'‚tat bas (ENaBle)

                    clc                           ;CARRY = 0
                    lda       Points              ;Get the decimal points
                    bsr       Put_Byte_Raw

                    lda       Digit12
                    bsr       Put_Byte
                    lda       Digit34
                    bsr       Put_Byte

                    bset      PORTD,#MC_ENB       ;ENB … l'‚tat haut (m‚morise & affiche)
                    rts

;*******************************************************************************
; Purpose: Send a byte to the MC14499
;        : Send a byte, most-significant bit first, to the PORTD pins
; Input  : A = Byte to send
; Output : None

Put_Byte            proc
                    sec                           ;CARRY=1 (bit de fin de transmition)
;                   bra       Put_Byte_Raw

;*******************************************************************************

Put_Byte_Raw        proc
Loop@@              rola                          ;bit suivant dans CARRY
                    bcc       Zero@@              ;test si CARRY=0
                    bset      PORTD,#MC_DATA      ;non alors met SDA … 1
                    bra       Clock@@

Zero@@              bclr      PORTD,#MC_DATA      ;met DATA a 0
Clock@@             bclr      PORTD,#MC_CLK       ;force CLK low
                    nop                           ;tiny pause
                    bset      PORTD,#MC_CLK       ;force CLK high
                    cmpa      #%10000000          ;on a tout envoy‚?
                    clc                           ;CARRY = 0 (efface le bit qui vient d'ˆtre envoy‚)
                    bne       Loop@@              ;on n'a pas tout envoy‚ alors on continu
                    rts

;*******************************************************************************

RTI_Handler         proc
                    lda       RTI_Cnt
                    inca                          ;incremente le compteur de RTI
                    anda      #%111               ;compteur circulaire de 0 a 31=> 1.048 seconds
                    sta       RTI_Cnt
                    bne       Done@@              ;si pas eale a zero alors rend la main!

                    brclr     PORT,#$01,HeatOff@@
                    lda       Points              ;si on chauffe alors fait clignoter le point
                    eora      #%10000000          ;du digit 1
                    sta       Points

HeatOff@@           lda       C_Time
                    beq       GetTemp@@           ;deja a zero alors on passe!
                    deca
                    sta       C_Time

                    lda       Points              ;fait clignoter le point
                    eora      #%01010000          ;of digits 2 and 4
                    sta       Points

GetTemp@@           brclr     ADCTL,#CCF.,*       ;wait for end of A/D conversion
                    ldd       ADR1                ;A=ADR1, B=ADR2
                    std       AN_0                ;AN_0=A, AN_1=B

                    lda       #%00010000          ;Lance ‚chantillonage sur AN1/AN4
                    sta       ADCTL

          ; Verification du changement de consigne de regulation

                    lda       AN_1                ;lecture du potar de reglage de la consigne
                    anda      #%11111100          ;small filtering of unused bits
                    ldb       Consigne            ;lecture de la consigne actuelle
                    andb      #%11111100          ;small filtering of unused bits
                    cba                           ;if identical, exit!
                    beq       Done@@              ;Pas de changement de la consigne!

                    lda       AN_1
                    sta       Consigne            ;mise a jour de la consigne

                    lda       #5*4                ;number of seconds *4 for corresponding display
                    sta       C_Time

Done@@              lda       #%01000000
                    sta       TFLG2               ;efface le flag d'INT. de RTI

AnRTI               rti

;*******************************************************************************

                    @vector   Vsci                ;SCI
                    @vector   Vspi                ;SPI
                    @vector   Vpai                ;pulse accumulator input edge
                    @vector   Vpao                ;pulse accumulator overflow
                    @vector   Vrto                ;timer overflow

                    @vector   Vtoc5               ;timer output compare 5
                    @vector   Vtoc4               ;timer output compare 4
                    @vector   Vtoc3               ;timer output compare 3
                    @vector   Vtoc2               ;timer output compare 2
                    @vector   Vtoc1               ;timer output compare 1
                    @vector   Vtic3               ;timer input capture 3
                    @vector   Vtic2               ;timer input capture 2
                    @vector   Vtic1               ;timer input capture 1

                    @vector   Vrti,RTI_Handler    ;real time interupt
                    @vector   Virq                ;masquable interupt
                    @vector   Vxirq               ;non masquable interupt
                    @vector   Vswi                ;DISPATCH.MOD
                    @vector   Villop,Start        ;Illegal Opcode trap
                    @vector   Vcop,Start          ;COP failure
                    @vector   Vcmf,Start          ;CMF - Clock Monitor Failure
                    @vector   Vreset,Start        ;reset

;*******************************************************************************

                    end       :s19crc
