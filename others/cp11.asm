;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
; A serial command prompt shell for the 68HC11
;
; History
;   wep, 020120, Completed prompt with commands U, D, Q.
;     Removed some of the code for rxchar.
;*******************************************************************************
;*******************************************************************************
; Conventions
;
; IY points to the register base ($1000) to support efficient register access.
; Using IY is more space and time efficient for bit manipulation and testing
; of memory, but loads and stores are more efficient with extended addressing.
; We might want to consider using IX for register addressing, since this saves
; a pre-byte with every instruction. It depends on what else we need IX for.
;
; Currently, functions do not save registers to the stack.
;*******************************************************************************
                    #CaseOn
;*******************************************************************************
; Equates
;*******************************************************************************
; Registers

REGBS               def       $1000               ; start of registers
BAUD                def       $2B                 ; sci baud reg
SCCR1               def       $2C                 ; sci control1 reg
SCCR2               def       $2D                 ; sci control2 reg
SCSR                def       $2E                 ; sci status reg
SCDR                def       $2F                 ; sci data reg

PORTA               def       $00
PACTL               def       $26                 ; Port A control reg.
OC1M                def       $0C                 ; Port A alternate function reg.s
OC1D                def       $0D                 ; "
TCTL1               def       $20                 ; "

; Register fields and bit masks

SCCR2_TIE           def       $80                 ; transmit interrupt enable
SCSR_TDRE           def       $80                 ; transmit data register empty
SCSR_RDRF           def       $20                 ; receive data register full

LED_ON              equ       $01                 ; LED count enable
LED_UP              equ       $02                 ; LED count up

; Other equates

JMP_OP              equ       $7E                 ; Operand for JMP instruction
CARRIAGE_RET        equ       $0D
LINE_FEED           equ       $0A
ASCII_D             equ       $44
ASCII_U             equ       $55
ASCII_Q             equ       $51

;*******************************************************************************
; Stack
;
; The stack grows downward from the last byte before the pseudo-vector
; jump table.
;*******************************************************************************

STACK               equ       $00C3

;*******************************************************************************
; Uninitialized variables
;
; These will be placed in the first 256 bytes of internal RAM to support
; direct addressing. They must be explicitly initialized by the code.
;*******************************************************************************

                    #RAM
                    org       $0000

; Transmit buffer

.tx                 rmb       2                   ; address of next character to transmit
rx_buffer           rmb       16                  ; up to 16 bytes for commands
                    rmb       1                   ; plus one byte for null-termination
.rx                 rmb       2                   ; pointer to next write position

; LED control bits
;   bit 0: 0 = LEDs off, 1 = LEDs count
;   bit 1: 0 = count down, 1 = count up

led_ctrl            rmb       1                   ; LED control bits

;*******************************************************************************
; Vector jump table ($00C4)
;
; These jump commands must be installed at run time, since this memory is used
; by the talker program for download.
;*******************************************************************************

                    #RAM
                    org       $00C4

jmp_sci             rmb       3

;*******************************************************************************
; Entry point for program. Initialize stack and pseudo-vectors.
;*******************************************************************************

                    #ROM
                    org       $B600

Start               proc
                    lds       #STACK              ; set the stack pointer
                    ldy       #REGBS              ; use IY for accessing registers

                    lda       #JMP_OP             ; set the bootstrap mode SCI pseudo-vector
                    sta       jmp_sci             ; JSR intsruction
                    ldx       #SCI_Handler        ; address of interrupt handler
                    stx       jmp_sci+1

;*******************************************************************************
; Initialize port A for LED test.
;*******************************************************************************

                    lda       #$80                ; enable PA7 for output
                    sta       PACTL,y
                    clra                          ; disable PA alternate functions
                    sta       OC1M,y
                    sta       OC1D,y
                    sta       TCTL1,y
;                   bra       InitSCI

;*******************************************************************************
; Initialize SCI.
;   9600 baud at 8 MHz Extal.
;   Enable transmitter, receiver, and receive interrupt.
;*******************************************************************************

InitSCI             proc
                    lda       #$30
                    sta       BAUD,y              ; baud register
                    clr       SCCR1,y             ; (m=0, wake=0)

                    lda       #$2c
                    sta       SCCR2,y             ; enable (~TIE | RIE | TE | RE)

                    ldx       #NULLCHR            ; make .tx point to a permanent NULL
                    stx       .tx                 ; ...until TxStr is called

                    ldx       #rx_buffer          ; initialize .rx -> rx_buffer
                    stx       .rx
                    cli                           ; enable maskable interrupts

;*******************************************************************************
; Other initialization. Enable LEDs to count up.
;
; We don't transmit an initial banner or prompt, since our circuit comes up
; with the serial port disconnected.
;*******************************************************************************

                    bset      led_ctrl,#LED_UP+LED_ON  ; set LED control for counting up

;*******************************************************************************
; Main loop
;
; A simple, example foreground task. Just count on four LEDs.
; Serial processing is all interrupt driven.
;*******************************************************************************

test                proc
                    bsr       TestLED             ; count on the upper nibble of Port A
                    ldx       #$ffff              ; delay value
                    bsr       Delay
                    bra       test

;*******************************************************************************
; Subroutines
;*******************************************************************************

;*******************************************************************************
; LED test. On each pass, update the 4-bit value displayed in the upper
; nibble of port A. Behavior depends on the led_ctrl byte.
;*******************************************************************************

TestLED             proc
                    lda       PORTA,y             ; load current LED pattern

                    brset     led_ctrl,#LED_ON,On@@ ; test whether count enabled

                    clra                          ; if not enabled, clear PORTA
                    bra       Test@@

On@@                brclr     led_ctrl,#LED_UP,Down@@ ; test for up or down count

                    adda      #$10                ; increment upper nibble of A
                    bra       Test@@

Down@@              suba      #$10                ; decrement upper nibble of A
Test@@              sta       PORTA,y
                    rts

;*******************************************************************************
; Delay. Use the value in IX as a delay count.
;*******************************************************************************

Delay               proc
Loop@@              dex
                    bne       Loop@@
                    rts

;*******************************************************************************
; Transmit string
;
; Initiate transmission of a string. The string address is passed in IX.
; To initiate transmission, we only need to enable interrupts, since the
; empty transmit register will invoke an interrupt immediately. Use caution
; as to where IX points. Transmission will continue until a NULL is reached,
; so unexpected results will occur if IX points to dynamic data. Once
; transmission completes, the .tx will be pointed back to a permanent NULL.
;
; Uses IX.
;*******************************************************************************

TxStr               proc
                    stx       .tx                 ; save address of transmit string
                    bset      SCCR2,y,#SCCR2_TIE  ; enable int's for empty tx data register
                    rts

;*******************************************************************************
; Transmit character
;
; If transmit data register empty and *.tx != NULL, load the
; next character for transmission. If there are no more characters to transmit,
; disable SCCR2:TIE so that the empty tranmit register doesn't continue to
; invoke interrupts.
;
; Uses A, IX.
;*******************************************************************************

TxChr               proc
                    brclr     SCSR,y,#SCSR_TDRE,Done@@ ; return if TDR still full

                    ldx       .tx                 ; load U16 address of next character to IX
                    lda       ,x                  ; load A with next character
                    beq       StopTX@@            ; terminate transmit if NULL character

                    inx                           ; otherwise, increment .tx
                    stx       .tx                 ; ...and update

                    sta       SCDR,y              ; load character (still in A) for transmission
                    bra       Done@@

StopTX@@            bclr      SCCR2,y,#SCCR2_TIE  ; don't let empty TDRE cause interrupt

                    ldx       #NULLCHR            ; make .tx point to a permanent NULL
                    stx       .tx                 ; ...until TxStr is called again
Done@@              rts

;*******************************************************************************
; Receive character
;
; If receive data register empty, return.
; Otherwise, read and echo chr.
;
; PENDING! Add code to implement the following pseudo-code.
;
; If received character == <CR> or rx_buffer full,
;   reset rx_buffer,
;   call do_cmd to process command and display prompt
; else
;   put new character in rx_buffer,
;   update .rx++.
;*******************************************************************************

RxChr               proc
                    brclr     SCSR,y,#SCSR_RDRF,Done@@ ; return if RDR empty
                    lda       SCDR,y              ; read newly received character
                    sta       SCDR,y              ; echo char
          ;***** PENDING! Add code described in RxChr function header. *****
Done@@              rts

;*******************************************************************************
; do_cmd
;
; Invoke appropriate command here based on the contents of rx_buffer
; Currently, we only examine the first character of rx_buffer.
; Eventually, commands will contain arguments.
;*******************************************************************************

do_cmd              proc
                    lda       rx_buffer           ; load A with the first command character
                    beq       Prompt              ; for null string, just display prompt

; U: (Up) Set the led_ctrl:LED_UP bit for counting up; clear for counting down.

                    cmpa      #ASCII_U            ; check for "U" command
                    bne       do_cmd2
                    bset      led_ctrl,#LED_UP+LED_ON ; Set led_ctrl for counting up
                    bra       Prompt              ; valid command, display prompt

; D: (Down) Clear the led_ctrl:LED_UP byte for counting down.
do_cmd2
;***** PENDING! Handle "D" command. *****

; Q: (Quit) Turn off LEDs and don't count
do_cmd3
;***** PENDING! Handle "Q" command. *****

; Add future commands here. When a command is not recognized, execution falls
; through to the "BadCmd" label, and an appropriate message is displayed.

do_cmd4

;*******************************************************************************

BadCmd              proc
                    ldx       #BADPROMPT          ; Display possible cmds + prompt for invalid cmd
                    bsr       TxStr
                    rts                           ; ...and return

;*******************************************************************************
; Display next prompt here.

Prompt              proc
                    ldx       #GOODPROMPT         ; Display prompt for valid cmd
                    bra       TxStr               ; end of do_cmd

;*******************************************************************************
; Interrupt handlers
;*******************************************************************************
; SCI
SCI_Handler         proc
                    bsr       TxChr               ; call the transmit character routine
                    bsr       RxChr               ; call the receive character routine
                    rti

;******************************************************************************
; Constants
;
; Note that the bad prompt includes the good prompt, but adds a prefix.
;******************************************************************************

prompt              macro
                    mset      #
                    mstr      1
                    fcc       CARRIAGE_RET,LINE_FEED,~1~
                    endm

BADPROMPT           @prompt   Commands: U, D, Q   ; Response to invalid cmd = "\r\n??\r\nHC11> "
GOODPROMPT          @prompt   'HC11> '            ; Response to valid cmd = "\r\nHC11> "
NULLCHR             fcb       0                   ; null-termination and safe place for the TX ptr
