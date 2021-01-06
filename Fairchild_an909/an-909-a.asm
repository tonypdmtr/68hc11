;*******************************************************************************
; MC68HC11 to NM93CS06/46 interface code
; Revision A 7/28/93
; This assembly code implements the serial interface
; to the NM93CS06/46 serial EEPROM with Chip Security
; through the SPI interface and the rest of Port D.
; There are 10 commands:
; WRITE Protect Reg WRite (PRWR)
; READ Protect Reg ReaD (PRRD)
; WRite ENable (WrEn) Protect Reg ENable (PrEn)
; WRite DiSable (WRDS) Protect Reg DiSable (PRDS)
; WRite ALL (WRALL) Protect Reg CLeaR (PrClr)
; Each EEPROM command type is implemented as a
; subroutine call. Address is passed to the routine
; in XADDR, data in XDATLO and XDATHI. Data is
; returned from read operations in RDATLO and RDATHI.

                    #Uses     header.inc
                    #Uses     init.sub

MainLoop            proc
                    lda       #$aa                ; Main give examples of using
                                                  ; a few of the command routines
                    sta       XADDR               ; Load in transmit address
                    lda       #$33
                    sta       XDATLO              ; Load in low byte xmit dat
                    lda       #$cc
                    sta       XDATHI              ; Load in hi byte xmit data
                    bsr       WrEn                ; Call Write Enable
                    bsr       PrEn                ; Call Protect Register Enable
                    jsr       PrClr               ; Call Protect Register Clear
                    jsr       Write               ; Call Write
                    bsr       Read                ; Call Read
                                                  ; (read back memory)
                    bra       MainLoop

;*******************************************************************************

PrEn                proc
                    psha
                    pshb
                    pshx
                    pshy
                    jsr       WrPen               ; Subroutine Write Pending checks
                                                  ; whether a previous write is still
                                                  ; pending, and waits until it is
                                                  ; done if there is.
                    lda       #$23
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    clra
                    sta       EXTWR               ; No extra time after command
                    bra       Enb

WrEn                psha
                    pshb
                    pshx
                    pshy
                    jsr       WrPen
                    lda       #$21
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    clra
                    sta       EXTWR               ; No extra time after command
Enb                 lda       #$02
                    sta       MESSCT              ; Load message byte count
                    lda       #$30                ; Load last byte, op code=00
                    sta       XMESS0              ; Data is 00110000
                    lda       #1
                    sta       XMESS1              ; Load start bit
                    jmp       PstWr               ; Jump to Posted Write Routine

PrDs                psha
                    pshb
                    pshx
                    pshy
                    jsr       WrPen               ; Check Write still Pending?
                    lda       #$23
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    lda       #1
                    sta       EXTWR               ; Extra write time required
                    bra       Dis

WrDs                psha
                    pshb
                    pshx
                    pshy
                    jsr       WrPen               ; Check Write still Pending?
                    lda       #$21
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    clra
                    sta       EXTWR               ; No extra time after command
Dis                 lda       #$02
                    sta       MESSCT              ; Load message byte count
                    clra                          ; Load last byte, op code=00
                    sta       XMESS0              ; Data = 0000 0000
                    lda       #1
                    sta       XMESS1              ; Load start bit
                    jmp       PstWr               ; jump to Posted Write Routine

Read                psha
                    pshb
                    pshx
                    pshy
                    jsr       WrPen               ; Check Write still Pending?
                    lda       #$20
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    clra
                    sta       EXTWR               ; No extra time after command
                    lda       #$04
                    sta       MESSCT              ; Load message byte count
                    clra
                    sta       XMESS0              ; Load low byte data (don’t care)
                    sta       XMESS1              ; Load hi byte data (don’t care)
                    lda       XADDR
                    asla
                    anda      #$7e                ; Mask off bits 0 & 7 of address
                    sta       XMESS2              ; Load address byte
                    lda       #3
                    sta       XMESS3              ; Load start bit and MSB of op code
                    jmp       PollRd              ; Jump to Polled Read Routine

PrRd                psha
                    pshb
                    pshx
                    pshy
                    jsr       WrPen               ; Check Write still Pending?
                    lda       #$22
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    clra
                    sta       EXTWR               ; No extra time after command
                    lda       #$03
                    sta       MESSCT              ; Load message byte count
                    clra
                    sta       XMESS0              ; Load low byte (don’t care)
                    lda       XADDR
                    asla
                    anda      #$7e                ; Mask off bits 0 & 7 of address
                    sta       XMESS1              ; Load address byte
                    lda       #3
                    sta       XMESS2              ; Load start bit and MSB of op code
                    jmp       PollRd              ; Jump to Polled Read Routine

Write               psha
                    pshb
                    pshx
                    pshy
                    jsr       WrPen               ; Check Write still Pending?
                    lda       #$21
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    lda       #1
                    sta       EXTWR               ; Extra time after command
                    lda       #$04
                    sta       MESSCT              ; Load message byte count
                    lda       XDATLO
                    sta       XMESS0              ; Load low xmit data byte
                    lda       XDATHI
                    sta       XMESS1              ; Load high xmit data byte
                    lda       #$3f
                    ldb       #$40
                    anda      XADDR               ; Mask off bits 6 & 7 of address
                    sta       XMESS2
                    orb       XMESS2              ; Add op code=01 to address
                    stb       XMESS2              ; Load address byte
                    lda       #1
                    sta       XMESS3              ; Load start bit
                    bra       PstWr               ; Jump to Posted Write Routine

WrAll               psha
                    pshb
                    pshx
                    pshy
                    bsr       WrPen               ; Check if Write still Pending?
                    lda       #$21
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    lda       #1
                    sta       EXTWR               ; Extra time after command
                    lda       #$04
                    sta       MESSCT              ; Load message byte count
                    lda       XDATLO
                    sta       XMESS0              ; Load low xmit data byte
                    lda       XDATHI
                    sta       XMESS1              ; Load high xmit data byte
                    lda       #$10
                    sta       XMESS2              ; Load op code=00, rest 010000
                    lda       #1
                    sta       XMESS3              ; Load start bit
                    bra       PstWr               ; Jump to Posted Write Routine

PrWrt               psha
                    pshb
                    pshx
                    pshy
                    bsr       WrPen               ; Check if Write still Pending?
                    lda       #$23
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    lda       #1
                    sta       EXTWR               ; Extra time after command
                    lda       #$02
                    sta       MESSCT              ; Load message byte count
                    lda       #$3f
                    ldb       #$40
                    anda      XADDR               ; Mask off bits 6 & 7 of address
                    sta       XMESS0
                    orb       XMESS0              ; Add op code=01 to address
                    stb       XMESS0              ; Load address byte
                    lda       #1
                    sta       XMESS1              ; Load start bit
                    bra       PstWr               ; Jump to Posted Write Routine

PrClr               psha
                    pshb
                    pshx
                    pshy
                    bsr       WrPen               ; Check if Write still Pending?
                    lda       #$23
                    sta       PREPE               ; Set CS, PRE, and PE bits
                    lda       #1
                    sta       EXTWR               ; Extra time after command
                    lda       #$02
                    sta       MESSCT              ; Load message byte count
                    lda       #$ff
                    sta       XMESS0              ; Load address byte of all one’s
                    lda       #1
                    sta       XMESS1              ; Load start bit
                    bra       PstWr               ; Jump to Posted Write Routine

WrPen               lda       MESSCT              ; Write Pending Subroutine:
                    bne       WrPen               ; Check if previous message byte
                                                  ; count is 0 before preceding
                    lda       WRACTV              ; Check write still active even
                    bne       WrPen               ; though last message sent
                    rts                           ; Note that a bad write (ie. WrEn
                                                  ; has not been sent) will not return
                                                  ; a ready so this loop will never
                                                  ; stop... a timer (approx 10ms)
                                                  ; should be implemented to monitor
                                                  ; for this situation.
PstWr               lda       #$80
                    ora       SPCR
                    sta       SPCR                ; Enable SPI Interrupt
                    ldb       MESSCT              ; Message byte count in B
                    ldx       #XMESS0             ; Address of last message byte
                    lda       PORTD               ; Read Port D
                    ora       PREPE
                    sta       PORTD               ; Set CS, PRE, and PE lines of PORT D
                    decb                          ; decrement message indexing B
                    abx                           ; index address in X to mess byte
                    lda       ,x                  ; Load A with message byte data
                    sta       SPDR                ; Send data packet (start bit)
                    puly
                    pulx
                    pulb
                    pula
                    rts                           ; Return to main program...the
                                                  ; rest of the message is sent
                                                  ; by interrupt routine SpInt
SpInt               ldb       SPSR                ; Check that interrupt cause by
                    bpl       Done                ; SPIF (not mode error)
                    lda       SPDR                ; Clear SPIF interrupt
                    ldb       MESSCT              ; Load message byte count in B
                    beq       Twp                 ; Jump to post write poller
                    decb                          ; Decrement message count
                    stb       MESSCT              ; Store that 1 byte was sent
                    beq       SsOff               ; Branch if last byte was sent
                    ldx       #XMESS0             ; Load address of last byte in X
                    decb                          ; Decrement message index B
                    abx                           ; Index address in X to next byte
                    lda       ,x                  ; Load next message byte
                    sta       SPDR                ; Send message byte
                    bra       Done                ; Return to Main Program

SsOff               lda       PORTD
                    anda      #$df
                    sta       PORTD               ; Turn off CS
                    anda      #$fc
                    sta       PORTD               ; Turn off PRE, PE
                    lda       EXTWR               ; Zero out Extra Write time
                    beq       Off                 ; Jump to turn off interrupt
                    lda       PORTD
                    ora       #$20
                    sta       PORTD               ; Turn on CS
                    clra
Twp                 cmpa      #$ff                ; Check if write complete
                    bne       Send                ; No, send another idle byte
                    lda       PORTD
                    anda      #$df
                    sta       PORTD               ; Turn off CS
Off                 lda       #$7f
                    anda      SPCR
                    sta       SPCR                ; Disable SPI interrupt
                    clra
                    sta       WRACTV              ; Write complete
                    bra       Done

Send                lda       #1
                    sta       WRACTV              ; Set write active
                    clra
                    sta       SPDR                ; Send data
Done                rti                           ; Return to main program

PollRd              ldb       MESSCT              ; Load message byte count in B
                    ldx       #XMESS0             ; Load address of last byte in X
                    lda       PORTD
                    ora       PREPE
                    sta       PORTD               ; Set CS, PRE, and PE
                    abx                           ; Index into address past byte
SendX               dex                           ; Decrement back to byte
                    decb                          ; Decrement index B
                    lda       ,x                  ; Load next message byte
                    sta       SPDR                ; Send message byte
Wait                lda       SPSR                ; Load SPI Status register
                    bpl       Wait                ; Wait unit SPIF is set
                    lda       SPDR                ; Load in data to A
                    tstb                          ; Last message byte?
                    beq       StrLo
                    sta       RDATHI              ; Save byte as hi receive byte
                    bra       SendX               ; Send/receive next byte

StrLo               sta       RDATLO              ; Store lo receive byte
                    stb       MESSCT              ; Zero out message byte count
                    lda       #$dc
                    anda      PORTD
                    sta       PORTD               ; Turn off CS, PRE, PE
                    puly
                    pulx
                    pulb
                    pula
                    rts                           ; Return to Main Program
