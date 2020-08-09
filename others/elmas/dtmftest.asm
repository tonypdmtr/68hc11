;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; Prog: DTMF Transceiver example
; 1. Wait for a ring
; 2. Pick up the line
; 3. Read DTMF tones
; 4. Show the received numbers
;*******************************************************************************

                    #ListOff
                    #Uses     811e2.inc
                    #ListOn

;*******************************************************************************
; DTMF DEFINITIONS
;*******************************************************************************

DTMF_STATUS_REG     def       $8001
DTMF_CONTROL_REG    def       $8001
DTMF_TRANSMIT_REG   def       $8000
DTMF_RECEIVE_REG    def       $8000

;*******************************************************************************
                    #RAM
;*******************************************************************************

; We need to remember the last values in the control registers
; since we can not read them

dtmf_cra            rmb       1                   ; Here is the last value of CRA
dtmf_crb            rmb       1                   ; Here is the last value of CRB
rt_counter          rmb       1                   ; RTI counter
delay_count         rmb       1

;*******************************************************************************
                    #ROM
;*******************************************************************************

Start               proc
                    lds       #STACKTOP
                    lda       #4                  ; Disable COP
                    sta       CONFIG

                    lda       #$3d                ; PortD bit 0 = output (Tel. Hook Switch Control)
                                                  ; PortD bit 1 = input (Ring Detector)
                                                  ; PortD bit 2 - 5 = outputs (7-segment control)
                    sta       DDRD
                    clr       PORTD               ; On hook

                    bsr       InitCM8880          ; Initialize CM8880

          ; Configure the Timer for Real-Time Interrupt Mode

                    clr       rt_counter
                    clr       PACTL               ; Select RTI interrupt rate
                    lda       #$40                ; enable interrupts
                    sta       TMSK2
                    cli                           ; enable the interrupts
;                   bra       RingWait

;*******************************************************************************
; Wait for a ring

RingWait            proc
                    lda       #2
Loop@@              bita      PORTD
                    beq       Loop@@
          ;-------------------------------------- ; Set the transceiver to the proper mode
                    clra
                    bsr       WriteControlRegA    ; set DTMF mode, disable Tone output
                    bsr       WriteControlRegB    ; Enable the Burst mode

                    bsr       Delay
          ;-------------------------------------- ; pick up the line
                    lda       #1
                    sta       PORTD               ; Off hook
;                   bra       DigitWait

;*******************************************************************************

DigitWait           proc
                    bsr       ReadDigit
                    cmpb      #1                  ; see if we got dial tone
                    beq       Done@@              ; yes, we have dial tone
                    cmpa      #11                 ; if '*' close connection
                    bne       ShowDigit
Done@@              clr       PORTD               ; On hook
                    bra       RingWait

;*******************************************************************************

ShowDigit           proc
                    cmpa      #10                 ; convert 10 to zero
                    bne       Skip@@
                    clra
Skip@@              asla:2
                    ora       #1                  ; keep connection on
                    sta       PORTD               ; show the digit
                    bra       DigitWait

;*******************************************************************************
; Purpose: Initialize the CM8880
; Input  : None
; Output : None
; Note(s):

InitCM8880          proc
                    bsr       ReadStatus          ; CM8880 initialization
                    clra
                    bsr       WriteControlRegA
                    bsr       WriteControlRegB
                    bsr       ReadStatus
                    bra       ReadDigit1

;*******************************************************************************
; Purpose: Reads the CM8880 Status Register
; Input  : None
; Output : A = Contents of the Status Register

ReadStatus          proc
                    lda       DTMF_STATUS_REG
                    rts                           ; CM8880 status reg steel in ACCA

;*******************************************************************************
; Purpose: Writes the CM8880 Control Register A
; Input  : A = Control Register A value
; Output : None

WriteControlRegA    proc
                    sta       dtmf_cra
                    sta       DTMF_CONTROL_REG
                    rts

;*******************************************************************************
; Purpose: Writes the CM8880 Control Register B
; Input  : A = Control Register B value
; Output : None

WriteControlRegB    proc
                    pshb
                    ldb       dtmf_cra
                    orb       #8                  ; set RSEL to access CRB
                    stb       DTMF_CONTROL_REG    ; Write to CRA
                    pulb
                    sta       dtmf_crb            ; Save the new content of CRB
                    sta       DTMF_CONTROL_REG    ; Write to CRB
                    rts

;*******************************************************************************
; Purpose: Reads the CM8880 Data Bus Reg
; Input  : None
; Output : A = Contents of the Data Bus Reg
;        : B = 1 if dial tone was detected, 0 otherwise

ReadDigit           proc
Loop@@              lda       DTMF_STATUS_REG
                    anda      #4
                    beq       Loop@@
;                   bra       ReadDigit1

;*******************************************************************************

ReadDigit1          proc
                    lda       DTMF_RECEIVE_REG
                    clrb                          ; no dial tone
                    rts                           ; CM8880 Data Bus Reg in ACCA

;*******************************************************************************
; Purpose: Writes the CM8880 Control Register A
; Input  : A = Data Bus Reg value
; Output : None

WriteDigit          proc
Loop@@              ldb       DTMF_STATUS_REG
                    andb      #2
                    beq       Loop@@
                    sta       DTMF_TRANSMIT_REG
                    rts

;*******************************************************************************
; Purpose: Delay
; Input  : None
; Output : None

Delay               proc
                    lda       #48
                    sta       delay_count
Loop@@              dec       delay_count
                    bne       Loop@@
                    rts

;*******************************************************************************
; Real-Time interrupt service routine
;*******************************************************************************

RTI_Handler         proc
                    inc       rt_counter
                    lda       #$40
                    sta       TFLG2               ; clear RTIF
                    rti

;*******************************************************************************
                    @vector   Vrti,RTI_Handler    ; Real-Time interrupt vector
                    @vector   Vreset,Start        ; Reset vector
;*******************************************************************************

                    end       :s19crc
