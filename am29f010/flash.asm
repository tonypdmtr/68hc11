;*******************************************************************************
; 68HC11 Enhanced EVBU2 Am29F010 FLASH Programming Utility                     *
;  Copyright (C) 1998 Mark Schultz - All Rights Reserved                       *
;------------------------------------------------------------------------------*
; Author:          Mark Schultz <mschultz@jps.net>                             *
; Version:         0.05  Revised 14-Aug-98                                     *
;                  0.06  Revised 18-Jul-01 <tonyp@acm.org>                     *
;------------------------------------------------------------------------------*
; Assembled with Tony Papadimitriou's ASM11 v1.84+                             *
;                                                                              *
; Tested on modified Motorola EVBU2 with Am29F010 FLASH device mapped in HC11  *
; address space at $8000-$FFFF.                                                *
; FLASH address lines FA15 and FA16 driven by PB0 and PB1 provided by a        *
; 68HC24 PRU.                                                                  *
;*******************************************************************************
;Tabs:    11        21        31        41        51        61        71
;------------------------------------------------------------------------------
;Version History:
;
;0.01   11-Aug-98   - First test version
;0.02   12-Aug-98   - Modified all JMPs and JSRs to use relative forms.
;                   - Added "helper" branches to allow all-relative addressing.
;                   - Modified MSG routine to use subroutine return address as
;                     string pointer.
;0.03   13-Aug-98   - Changed all LSRD instructions to "DB $04" to work around
;                     object code generation bug in ASM11 v1.09beta.
;0.04   14-Aug-98   - Changed "DB $04" back to "LSRD" after obtaining v1.11beta
;                     version of ASM11 that has LSRD bug fixed.
;                   - Added comments pertaining to S-record structure and
;                     FLASH programming/erasure algorithms.
;                   - Various minor modifications to reduce code size.
;                     Code size for this version: $01FC bytes
;0.05   17-Aug-98   - Added instruction to disable bootloader ROM if active
;                     Code size for this version: $01FE bytes
;0.06   18-Jul-01   - No functional changes at all.
;                     Saved with tabs converted to spaces.
;                     Fixed loop bug in CharIn routine.
;                     Removed @ signs from variables, it's automatic.
;                     CharOut optimized to not use RegA for SCSR
;                     Used "repeaters" and ASM11 extra instructions
;------------------------------------------------------------------------------
;Abstract:
;
;This utility can be used to program a memory-mapped 128Kx8 5V-only FLASH ROM
;using a standard S-record file transmitted via the SCI.  The utility may be
;loaded and executed from anywhere in the HC11 address space without
;modification, as long as it is not run from the FLASH device being programmed.
;The utility is small enough to run out of the (512-byte) EEPROM present
;in most HC11 variants.
;------------------------------------------------------------------------------
;Usage notes:
;
;Unfortunately, given the size of this utility and it's RAM requirements, this
;program cannot be bootloaded and executed out of internal RAM unless run on a
;HC11 variant equipped with a large (600+ byte) internal RAM array.  Various
;resource ORG's will have to be changed if execution out of RAM is desired.
;The program IS small enough (barely!) to fit within the 512-byte EEPROM found
;on many HC11 variants.
;
;This utility has been written using all-relative addressing modes, allowing it
;to be executed from any location in the HC11 address space.  The program is
;ORG'd to location $2000 but may be relocated to any address without code
;modification, ORG change or re-assembly.
;
;As presently written, the program assumes that the FLASH ROM is connected to
;a HC11 configured to run in expanded mode, with the FLASH device mapped to
;addresses $8000-$FFFF.  FLASH address bits A15 and A16 are assumed to be
;driven with output port pins PB0 and PB1.  On the modified EVBU2 on which this
;utility was debugged and tested, PB0 and PB1 signals were provided by a 68HC24
;Port Recovery Unit (PRU).  Any HC11 output port may be used to drive FA15 and
;FA16 once appropriate assignments are made to the FA15 and FA16 port-definition
;constants.
;
;RAM locations $0000-$0007 are used to hold internal variables.  The 40 bytes
;following are used as a S-record data buffer, allowing for record sizes up
;to 90 bytes total.  The stack is set to the end of RAM for a E-series device
;($01FF).  Worst-case stack usage should be 7 bytes or less.  Interrupts are
;not utilitized, and disabled on startup.  The SCI is set to operate at 9600
;bps (w/8 MHz clock) using a 8-bit word size.  Most of the behaviors described
;above may changed by modifying the appropriate constants.
;
;This utility offers a number of helpful features to facilitate FLASH
;programming.  When started, the user is prompted (Sec:) to specify a sector
;number (a 16K block of the FLASH array) to be programmed, or optionally erased.
;Entering a value from 0 to 7 simply selects a destination sector, which is
;only relevant as far as how FLASH address bits A15 and A16 are set during
;programming.  A value of 8 to F will initiate erasure of sector (n-8);
;e.g. entering "B" causes sector 3 to be erased.  Entering Ctrl-E causes the
;entire array (all 128K bytes) to be erased.  If a erase operation is specified,
;the sector prompt "Sec:" will be re-issued to allow erasure of additional
;sectors until such time as a "normal" sector number (0..7) is entered.
;
;Note: Entries made at the "Sec:" prompt are executed immediately; it is not
;necessary to transmit <CR> after specifying a sector # to select or erase.
;
;An optional offset may be specified, which will be ADDED to all S-record
;addresses when the FLASH is programmed.  This is entered at the "Ofs:" prompt,
;which is generated after the "Sec:" prompt.  Since the final address
;calculation ignores any carry beyond 16 bits, a negative offset may be applied
;by specifying it's two's compliment value.  Entering "EC00" will effectively
;subtract $1400 from S-record addresses sent in the programming phase.
;
;Once the sector number and offset have been entered, the program enters the
;S-record interpreter, as indicated by the "Send!" prompt.  The program will
;generate a prompt character after each S-record is processed.  There are three
;possible prompts: "*" indicates that a record was processed without incident.
;"?" indicates a parsing or checksum error in the data sent.  If a "/" is sent,
;an error occured while attempting to program the FLASH device.  The S-record
;data sent to the interpreter is NOT echoed back to the user's terminal.
;
;The program will remain in the S-record interpreter mode using the previously
;specified settings until a <ESC> character ($1B) is received.  Reception of
;a end-of-data S-record (one that starts with "S9...") will NOT terminate the
;S-record interpreter/programmer.  Once <ESC> is received, the program will
;resume operation at the "Sec:" prompt.
;
;If the FLASH device to be programmed overlaps the internal ROM or EEPROM space,
;and the data addresses to be programmed fall within these overlapped areas,
;it is up to the user to de-map these resources by writing the appropriate
;value to the CONFIG register before starting this utility.  THIS PROGRAM DOES
;NOT MODIFY THE CONFIG REGISTER IN ANY WAY.  If overlapping internal resources
;are in the memory map a verify error will likely occur when programming is
;attempted.  It is permissible to leave internal resources enabled if the
;location(s) to be programmed do not overlap these resources.
;------------------------------------------------------------------------------
;Notes regarding coding style:
;
;To keep the code size small and to allow for use of relative jumps and
;subroutine calls throught, some "unusual" coding practices have been used.
;The ordering and position of subroutine and code blocks is somewhat unusual,
;even "spahetti-like" but has been done so to facilitate the use of relative
;addressing.  "Helper" branches are located throught the code which serve to
;extend the range of distant branches and subroutine calls.  Use of "helper"
;branches can be identified by looking at the label being used to call a given
;routine; all BRx/BSR's that employ "helper" branches use labels with a "$"
;or "$$" suffix.
;
;The <Msg> subroutine, used to transmit text strings via the SCI, employs a
;unusual but effective method to determine the address of the string which it
;is to send - it expects the NUL-terminated string which is to be sent to
;follow the BSR/JSR which calls the MSG routine.  Execution resumes with the
;code following the terminating NUL of the string sent.
;*******************************************************************************

                    #ListOff
                    #Uses     hc11e.inc
                    #Uses     chars.inc
                    #ListOn
                    #ExtraOn                      ;Allow ASM11's extra mnemonics

;-------------------------------------------------------------------------------
; Character constants

EraseAck            equ       '!'                 ; Erasure acknowledge character
ErrorChr            equ       '?'                 ; Bad entry character

EraAllChr           equ       $05                 ; Erase-all command character

SRecLead            equ       'S'                 ; Lead-in character for S-records

PromptChr           equ       '*'                 ; S-record prompt character
BadRecChr           equ       '?'                 ; Prompt/response if bad S-record
PgmErrChr           equ       '/'                 ; Prompt/response if flash program error

;-------------------------------------------------------------------------------
; Application-specific resource definitions
; These may be changed to conform to your design

FA15                equ       PORTB               ; FLASH ROM address line 15
FA15_               equ       PORTB_
FA15.               equ       PB0.
;FA15D_             equ       DDRB_               ;(DDR address if needed)
;FA15D              equ       DDRB

FA16                equ       PORTB               ; FLASH ROM address line 16
FA16_               equ       PORTB_
FA16.               equ       PB1.
;FA16D_             equ       DDRB_               ;(DDR address if needed)
;FA16D              equ       DDRB

MyBaud              equ       SCP1.|SCP0.         ; SCI baud rate (9600 @ 8 MHz)

BUFSIZE             equ       40                  ; Size of buffer to hold S-record data

STACK               equ       $01FF               ; Stack base (highest) address

;-------------------------------------------------------------------------------
; FLASH programming constants

FOffset             equ       $8000               ; Base address of FLASH ROM in memory map

FUnlkA1             equ       $5555               ; Write unlock address 1 \ Device addr's
FUnlkA2             equ       $2AAA               ; Write unlock address 2 | Add <FOffset>
FCmdA               equ       $5555               ; Command write address / for MCU addr

FUnlkV1             equ       $AA                 ; Write unlock value 1
FUnlkV2             equ       $55                 ; Write unlock value 2

FCmdID              equ       $90                 ; Command: Read chip IDs
FCmdPgm             equ       $A0                 ; Command: Program byte
FCmdErase           equ       $80                 ; Command: Sector or chip erase
FCmdReset           equ       $F0                 ; Command: Reset to read state

FCmdCEra            equ       $10                 ; Erase subcommand: Entire chip
FCmdSEra            equ       $30                 ; Erase subcommand: Single sector

;-------------------------------------------------------------------------------
; Variable declarations
; Location may be changed to suit your needs

;*******************************************************************************
                    #RAM                          ; Page 0 variables
;*******************************************************************************
                    org       $0000               ; Start of variable block

sector              rmb       1                   ; Sector # selected for programming
offset              rmb       2                   ; Offset to add to S-record addresses
numbuf              rmb       1                   ; Temporary used by numeric parser

rec_type            rmb       1                   ; S-record type field
length              rmb       1                   ; S-record length field
address             rmb       2                   ; S-record address field
checksum            rmb       1                   ; S-record running checksum
          ;-------------------------------------- ; Anywhere in RAM
sbuffer             rmb       BUFSIZE             ; S-record data field in binary format

;*******************************************************************************
                    #ROM
;*******************************************************************************
                    org       $2000               ; Change to desired load address

;*******************************************************************************
; Purpose: Initialization
;        : + Disable XIRQ & STOP
;        : + Set up stack
;        : + CPU in expanded bus mode
;        : + Set up SCI for 9600bps, 8 bit wordsize, multidrop features off

Start               proc
                    lda       #S.+X.+I.           ; Disable STOP and XIRQ, IRQs off
                    tap
                    lds       #STACK              ; Set up stack pointer

                    ldx       #IOBase             ; Point to I/O registers
                    bset      HPRIO_,x,#MDA.      ; Enable expanded bus mode
                    bclr      HPRIO_,x,#RBOOT.    ; Disable bootloader ROM

                    lda       #MyBaud             ; Set SCI baud rate
                    sta       BAUD_,x
                    lda       #TE.+RE.            ; Tx,Rx enabled, IRQs off
                    sta       SCCR2_,x
                    clr       SCCR1_,x            ; 8 bit wordsize

                    clrd                          ; D <- 0
                    std       offset              ; Default S-record address offset
                    sta       sector              ; Default sector # to program

;                   bclr      FA15D_,x,FA15.      ;(DDR initialization here if needed)
;                   bclr      FA16D_,x,FA16.      ;
                    bra       Main                ; Skip over GETNUM and MSG subroutines

;*******************************************************************************
; Purpose: Get (up to) 4-digit hex number w/entry echoback & editing
;
; X <- Integer representation of value entered
; A <- Last character received from SCI
;
; Note(s): Backspace may be used to delete digit(s) entered
; Entry is terminated when <CR> received
; <ESC> also terminates entry, returns w/carry set
; The terminating character (CR or ESC) is NOT echoed back.
;
; The backspacing routine does not echo a destructive
; backspace, and can become "confused" about the number of
; digits previously entered if the first digit(s) entered
; were 0's.  The accuracy of the returned result is not
; affected by these minor deficiencies.
;
; Stack usage:       7 bytes (including BSR/JSR)
; Destroyed:         B

GetNum              proc
                    clrx                          ; Zero number accumulator

Loop@@              bsr       GetHexD$            ; Get a digit
                    bcc       GetNum2             ; If valid, add to accumulator

                    cmpa      #ESC                ; Abort entry ?
                    beq       GetNum3             ; Yes, exit (carry set)

                    cmpa      #CR                 ; End of entry ?
                    beq       GetNum4             ; Yes, exit (carry clear)

                    cmpa      #BS                 ; Backspace ?
                    bne       Loop@@              ; No, invalid digit, ignore
          ;-------------------------------------- ; Backspace processing
                    inx                           ; (INX/DEX: 2-byte test for X=0)
                    dex                           ; Current entry = 0 ?
                    beq       Loop@@              ; If entry = 0, ignore backspace
                    bsr       CharOut$            ; Echo backspace

                    xgdx                          ; D <- Value so far
                    lsrd:4                        ; Shift 1 hex digit right
                    xgdx                          ; Put # back in storage
                    bra       Loop@@              ; Get next character
          ;-------------------------------------- ; Valid digit entered
GetNum2             cpx       #$1000              ; Room for another digit ?
                    bhs       Loop@@              ; No, ignore new digit
                    bsr       CharOut$            ; Echo digit received

                    xgdx                          ; D <- Value so far (X has digit)
                    lsld:4                        ; Shift 1 hex digit left (make room for new digit)
                    xgdx                          ; Put # back in storage, B <- new digit
                    abx                           ; Add new digit to accumulator
                    bra       Loop@@              ; Get next digit
          ;-------------------------------------- ; <ESC> received - abort entry
GetNum3             sec                           ; Flag aborted entry
                    rts                           ; Exit
          ;-------------------------------------- ; <CR> received - standard exit
GetNum4             clc                           ; Clear abort flag
                    rts                           ; Exit
          ;--------------------------------------
GetHexD$            bra       GetHexD$$           ; Help for GETHEXD call

;*******************************************************************************
; Purpose: Transmit zero-terminated string via SCI
;
; Note(s): String to be transmitted is retrieved from the code space
; immediately following the calling instruction.  The string
; should be terminated with a NUL.  Execution resumes at
; the point immediately following the terminating NUL of
; the string.

; Stack usage:       5 bytes (including BSR/JSR)
; Destroyed:         A,x

Msg                 proc
                    pulx                          ; Pop return address - start of string
Loop@@              lda       ,x                  ; Get char from string
                    beq       Done@@              ; Exit if end of string
                    bsr       CharOut$            ; Transmit character
                    inx                           ; Point to next character
                    bra       Loop@@              ; Continue transmission
Done@@              jmp       1,x                 ; Return to caller, skipping over string

;*******************************************************************************
; Prompt for sector select/erase
; If 0-7, set sector address & advance to offset prompt.
; If 8-F, erase sector (n-8) and re-issue sector prompt.
; If <ESC>, advance to offset prompt (use last selected/erased sector)
; If <Ctrl-E>, erase entire device & re-issue sector prompt.

Main                proc
                    bsr       Msg                 ; Transmit sector select/erase prompt
                    fcs       CR,LF,'Sec (+8=Era):'

                    bsr       GetHexD$$           ; Get single hex digit
                    bcc       Main1               ; Set or erase sector if valid digit

                    cmpa      #ESC                ; ESC entered ?
                    beq       Main6               ; Yes, exit sector set/erase mode

                    cmpa      #EraAllChr          ; "Erase all" character ?
                    beq       Main5               ; Yes, erase entire array

                    lda       #ErrorChr           ; Invalid entry - transmit error char
                    bra       Main4               ; Send char, return to sector prompt
;-------------------------------------------------------------------------------
GetNum$             bra       GetNum              ; Help for GETNUM call
;-------------------------------------------------------------------------------
; Valid sector address entered
; 0-7 sets sector only, 8-F erases sector (n-8).

Main1               bsr       CharOut             ; Echo entry

                    stb       sector              ; Save sector #
                    cmpb      #8                  ; Erase sector request ?
                    blo       Main6               ; No, advance to offset prompt

;*******************************************************************************
; Erase sector
;
; Sector erase procedure:
; 1. Write unlock sequence:
; Value <FUnlkV1> written to address <FUnlkA1>
; Value <FUnlkV2> written to address <FUnlkA2>
; These writes must occur with FA15=FA16=0
; 2. Write generic erase-enable command:
; Value <FCmdErase> written to address <FCmdA>
; 3. Write unlock sequence again (same as step 1).
; 4. Write "erase sector" command:
; Value <FCmdSEra> written to any address in the sector to be erased.
; (address bits FA14,FA15,FA16 relevant, others ignored)
;
; Note: Verification of erasure is not performed.

                    bsr       FUnlock             ; Send unlock sequence
                    lda       #FCmdErase          ; Erase command prebyte
                    sta       FCmdA+FOffset
                    bsr       FUnlock             ; Send unlock sequence again
                    tba                           ; A <- Sector address
                    bsr       FSector             ; Select sector to erase

                    ldx       #FOffset            ; Assume even sector
                    bita      #$01                ; Odd sector ?
                    beq       Main2               ; No
                    ldx       #FOffset+$4000      ; Yes, set address for odd sector

Main2               lda       #FCmdSEra           ; Sector erase command
                    sta       ,x                  ; Send to sector base address

Main3               lda       #EraseAck           ; Acknowledge erase request
Main4               bsr       CharOut
Main$$              bra       Main                ; Issue sector prompt again
;-------------------------------------------------------------------------------
GetHexD$$           bra       GetHexD             ; Help for GETHEXD call
CharOut$            bra       CharOut             ; Help for CHAROUT call
Msg$                bra       Msg                 ; Help for MSG call
;-------------------------------------------------------------------------------
; Erase entire FLASH array
;
; Entire device erase procedure:
; 1. Write unlock sequence:
; Value <FUnlkV1> written to address <FUnlkA1>
; Value <FUnlkV2> written to address <FUnlkA2>
; These writes must occur with FA15=FA16=0
; 2. Write generic erase-enable command:
; Value <FCmdErase> written to address <FCmdA>
; 3. Write unlock sequence again (same as step 1).
; 4. Write "erase device" command:
; Value <FCmdCEra> written to address <FCmdA>
;
; Note: Verification of erasure is not performed.

Main5               bsr       FUnlock             ; Send unlock sequence
                    lda       #FCmdErase          ; Erase command prebyte
                    sta       FCmdA+FOffset
                    bsr       FUnlock             ; Send unlock sequence again
                    lda       #FCmdCEra           ; Erase entire device command
                    sta       FCmdA+FOffset
                    bra       Main3               ; Echo "!" & issue sector prompt again
          ;-------------------------------------- ; Prompt for load address offset
Main6               bsr       Msg                 ; Send offset prompt
                    fcs       CR,LF,'Ofs:'

                    bsr       GetNum$             ; Get offset (4-digit hex)
                    bcs       Main7               ; Use default offset if <ESC> rec'd
                    stx       offset              ; Save offset entered
          ;--------------------------------------
          ; Send prompt to start sending S-records
          ; Jump to S-record read & program routine
          ;--------------------------------------
Main7               bsr       Msg$                ; Transmit S-record send prompt
                    fcs       CR,LF,'Send!',CR,LF
                    bra       Program$            ; Start S-record read & program routine

;*******************************************************************************
; Purpose: Read next available character from SCI receive queue
; Waits for character to be received before returning
; Ignores characters with SCI error bits set
;
; A <- Character received from SCI (MSB stripped)
;
; Stack usage:       3 bytes (including BSR/JSR)
; Destroyed:         Nothing

CharIn              proc
                    pshb                          ; Save B

Loop@@              ldab      SCSR                ; Get SCI status
                    bitb      #RDRF.              ; Data ready ?
                    beq       Loop@@              ; No, wait

                    lda       SCDR                ; Get character received
                    bitb      #NF.+FE.            ; Any errors ?
                    bne       Loop@@              ; Yes, discard data (bug fix: WAS CharIn)

                    anda      #$7F                ; Strip MSbit
                    pulb                          ; Recover B
                    rts                           ; Exit

;*******************************************************************************
; Purpose: Transmit character via SCI transmitter
; Waits for TDRE before sending character
;
; A -> Character to send via SCI
;
; Stack usage:       2 bytes (including BSR/JSR)
; Destroyed:         Nothing

CharOut             proc
Loop@@              tst       SCSR                ; Check SCI status
                    bpl       Loop@@              ; Wait for transmit data register clear
                    sta       SCDR                ; Transmit
                    rts                           ; Exit

;*******************************************************************************
; Purpose: Send array write unlock sequence to FLASH
;
; Write <FUnlkV1> to <FUnlkA1>      ($AA to $5555)
; Write <FUnlkV2> to <FUnlkA2>      ($55 to $2AAA)
;
; Values specified above are device addresses.  The value
; <FOffset> is added to these addresses to account for the
; FLASH ROM's position in the HC11 memory map.
;
; Stack usage:       6 bytes (including BSR/JSR)
; Destroyed:         A

FUnlock             proc
                    clra                          ; A16=A15=0 for unlock commands
                    bsr       FSector
                    lda       #FUnlkV1            ; 1st unlock write cycle
                    sta       FOffset+FUnlkA1
                    lda       #FUnlkV2            ; 2nd unlock write cycle
                    sta       FOffset+FUnlkA2
                    rts                           ; Exit

;*******************************************************************************
; Purpose: Set FLASH A15 & A16 lines to requested sector address
;
; A -> Sector # (bits 1-2 relevant; all others ignored)
;
; This implementation assumes that the 128Kx8 FLASH ROM
; device is mapped such that a 32K page exists in the HC11
; address space at any given time.  Device address bits A15
; (FA15) and A16 (FA16) are driven by HC11 output ports.
; This routine accepts a FLASH sector number (0-7, specifying
; a 16K segment of the device) and uses the upper two bits
; of this value (bits 1 & 2) to drive the appropriate HC11
; output pins which in turn bank in the appropriate 32K
; FLASH memory block into the HC11 address space.
;
; Since the least significant bit (bit 0) of the sector number
; is not relevant to the bank switching scheme, it is ignored.
;
; Stack usage:       4 bytes (including BSR/JSR)
; Destroyed:         Nothing

FSector             proc
                    pshx                          ; Save X
                    ldx       #IOBase             ; Point to I/O register base address
                    bclr      FA15_,x,FA15.+FA16. ; Assume sector 0

                    bita      #$02                ; A15 = 1 ?
                    beq       A16@@               ; No: FA15 = 0
                    bset      FA15_,x,FA15.       ; Yes: FA15 = 1

A16@@               bita      #$04                ; A16 = 1 ?
                    beq       Done@@              ; No: FA16 = 0
                    bset      FA16_,x,FA16.       ; Yes: FA16 = 1

Done@@              pulx                          ; Restore X
                    rts                           ; Exit
;-------------------------------------------------------------------------------
Program$            bra       Program             ; Help for PROGRAM branch
Main$               bra       Main$$              ; Help for MAIN branch

;*******************************************************************************
; Purpose: Get single char from SCI, interpret as hex digit
;
; A <- Actual character received (converted to uppercase)
; B <- Integer value of digit received (0-15)
; Carry set on exit if digit was not valid hex
;
; Note:              Character read is NOT echoed back
;
; Stack usage:       5 bytes (including BSR/JSR)
; Destroyed:         Nothing

GetHexD             proc
                    bsr       CharIn              ; Get character from SCI

                    cmpa      #'a'                ; Lowercase?
                    blo       Go@@                ; No

                    cmpa      #'z'                ; Lowercase?
                    bhi       Go@@                ; No

                    suba      #'a'-'A'            ; Subtract lowercase offset

Go@@                tab                           ; B <- Character received

                    subb      #'0'                ; Subtract ASCII digit offset
                    blo       Fail@@              ; Exit if not valid digit

                    cmpb      #10                 ; Digit = 0..9 ?
                    blo       Done@@              ; Yes, exit now

                    subb      #'A'-'0'-10         ; Subtract alpha offset
                    blo       Fail@@              ; Exit if underflow (invalid digit)

                    cmpb      #10                 ; Valid digit ?
                    blo       Fail@@              ; No, exit

                    cmpb      #15                 ; Valid digit ?
                    bhi       Fail@@              ; No, exit

Done@@              clc                           ; Standard exit - no error
                    rts

Fail@@              sec                           ; Error exit
                    rts

;*******************************************************************************
; Purpose: Get 2-digit (byte) hex # from SCI
;
; A <- Last character received from SCI
; B <- Integer representation of value received
;
; Note:              Character(s) read are NOT echoed back
;
; Stack usage:       7 bytes (including BSR/JSR)
; Destroyed:         Nothing

GetHexB             proc
                    bsr       GetHexD             ; Get Most Significant Digit (MSD)
                    bcs       Done@@              ; Exit if error

                    lslb:4                        ; Move digit into MSD position
                    stb       numbuf              ; Save for later

                    bsr       GetHexD             ; Get Least Significant Digit (LSD)
                    bcs       Done@@              ; Exit if error

                    addb      numbuf              ; Add MSD*16 + LSD
                    stb       numbuf              ; Save result
                    addb      checksum            ; Add value to checksum
                    stb       checksum
                    ldab      numbuf              ; Recover converted value

                    clc                           ; No errors
Done@@              rts                           ; Exit
;-------------------------------------------------------------------------------
FUnlock$            bra       FUnlock             ; Help for FUNLOCK call
FSector$            bra       FSector             ; Help for FSECTOR call

;*******************************************************************************
; S-record read & programming routine
; S-record format reference:
; S10898184D61726BBC <CR>
; ^^^-^---^-------^- ^---
; ||| |   |       |  |____ End-of-line character, usually $0D.
; ||| |   |       |_______ 1's compliment 8-bit checksum (modulo 2^8)
; ||| |   |                Checksum based on 8-bit integer value of all
; ||| |   |                2-char pairs, NOT including "S" and rec_type.
; ||| |   |_______________ Data field: (length-3) 2-char ASCII HEX pairs
; ||| |___________________ Address field: base location to write data.
; |||_____________________ Length field: # of 2-char pairs including
; ||                       address field & checksum but not "S" lead-in
; ||                       or record type.
; ||______________________ Record type: 0=Header, 1=Data, 9=End.
; |_______________________ Lead-in character, uppercase "S" = $53

Program             proc
                    lda       #PromptChr          ; Transmit prompt character
Loop@@              bsr       CharOut
                    clr       checksum            ; Reset checksum counter
          ;-------------------------------------- ; Wait for starting "S"
WaitS@@             bsr       GetHexD             ; Get character (this does UC conversion)

                    cmpa      #SRecLead           ; S-record lead-in ?
                    beq       GetType@@           ; Yes, get record type

                    cmpa      #ESC                ; Escape ?
                    bne       WaitS@@             ; No, resume scan for leading "S"

                    bra       Main$               ; Abort: Restart everything
          ;-------------------------------------- ; Get record type
GetType@@           bsr       GetHexD             ; Get record type code
                    bcs       Abort@@             ; Abort if error
                    stb       rec_type            ; Save record type
          ;-------------------------------------- ; Get record length field
          ; Note: Record length specified in a S-record includes the 2 bytes
          ; for address plus 1 byte for checksum.  A "null" S-record (no data
          ; field) will have a specified length of 03.  The read and program
          ; loops expect length to be expressed in terms of the size of the
          ; data field only, plus 1.  Subtracting 2 from the length specified
          ; in the S-record will provide the proper length count for the read
          ; & program routines.
          ;--------------------------------------
                    bsr       GetHexB             ; Get record length
                    bcs       Abort@@             ; Abort if error
                    subb      #2                  ; Subtract 2 bytes for address
                    bls       Abort@@             ; Error if record length <= 0
                    stb       length              ; Save record length
          ;-------------------------------------- ; Get address field
                    bsr       GetHexB             ; Get address MSB
                    bcs       Abort@@             ; Abort if error
                    stb       address             ; Save address MSB

                    bsr       GetHexB             ; Get address LSB
                    bcs       Abort@@             ; Abort if error
                    stb       address+1           ; Save address LSB
          ;-------------------------------------- ; Get data field, save in buffer
                    ldx       #sbuffer            ; Reset save pointer
                    ldy       length              ; Save length count in X

GetData@@           dec       length              ; Decrement length counter
                    beq       CheckSum@@          ; If Length = 0, data field read complete

                    bsr       GetHexB             ; Get data byte
                    bcs       Abort@@             ; Abort if error
                    stb       ,x                  ; Save in buffer
                    inx                           ; Point to next location

                    bra       GetData@@           ; Get next data byte
          ;-------------------------------------- ; Get & verify checksum field
CheckSum@@          sty       length              ; Restore length count

                    bsr       GetHexB             ; Get checksum
                    bcs       Abort@@             ; Abort if error
                    lda       checksum            ; Get checksum accumulation
                    coma                          ; Should be $FF, inversion should set Z
                    beq       SetupFlash@@        ; If valid, program FLASH with data
          ;-------------------------------------- ; Bad record - send error prompt & restart
Abort@@             lda       #BadRecChr          ; Transmit "bad record" character
Loop@@@             bra       Loop@@              ; Restart S-record loader
          ;-------------------------------------- ; Set-up for FLASH programming operation
SetupFlash@@        lda       rec_type            ; Get S-record type code
                    cmpa      #1                  ; Type 1 data record ?
                    bne       Program             ; No, ignore it

                    ldd       offset              ; Get user-specified offset
                    addd      address             ; Add to S-record addr, ignore overflow
                    xgdx                          ; X <- Programming base address
                    ldy       #sbuffer            ; Y <- S-record data base address
          ;-------------------------------------- ; Program/verify FLASH with S-record data
          ; Programming algorithm for a Am29F010 FLASH device:
          ;
          ; 1. Write unlock sequence:
          ; Value <FUnlkV1> written to address <FUnlkA1>
          ; Value <FUnlkV2> written to address <FUnlkA2>
          ; These writes must occur with FA15=FA16=0
          ; 2. Write "program byte" command:
          ; Value <FCmdPgm> written to address <FCmdA>
          ; 3. Write desired value to desired address
          ; (<FSector> routine used to set address bits FA15 & FA16 properly)
          ; 4. Wait for delay/verification - see below
          ;--------------------------------------
FlashIt@@           dec       length              ; Decrement length counter
                    beq       Program             ; Done w/record if length count at 0

                    bsr       FUnlock$            ; Send unlock sequence
                    lda       #FCmdPgm            ; Send program command
                    sta       FOffset+FCmdA
                    lda       sector              ; Set sector address (A15 & A16)
                    bsr       FSector$
                    lda       ,y                  ; Get S-record data
                    sta       ,x                  ; Write to FLASH
                    tab                           ; Copy byte written to B
          ;--------------------------------------
          ; FLASH programming delay/verification procedure:
          ;
          ; 1. Read value from address just programmed
          ; 2. IF bit 7 of value read = bit 7 of value programmed THEN
          ; Programming complete, value written properly
          ; 3. IF bit 5 of value read = 0 THEN
          ; Write not complete, repeat starting at step 1
          ; 4. IF bit 7 of value read = bit 7 of value programmed THEN
          ; Programming complete, value written properly
          ; 5. Value not written properly (verify error) if read bit 5 = 1
          ; (indicating write cycle complete) and bit 7 read/data mismatch.
          ;
          ; This procedure is identical to the recommended procedure for data
          ; write verification as published in the Am29F010 data sheet.
          ;--------------------------------------
NextRec@@           tba                           ; Get S-record data
                    anda      #$80                ; Mask all except bit 7
                    eora      ,x                  ; DATA bit 7 = FLASH bit 7 ?
                    bpl       Cont@@              ; Yes, byte written OK
                    bita      #$20                ; FLASH bit 5 = 0 ?
                    beq       NextRec@@           ; Yes, write not complete
                    eorb      ,x                  ; Bit 7 data = Bit 7 FLASH ?
                    bmi       Fail@@              ; No, write error

Cont@@              inx                           ; Point to next FLASH address
                    iny                           ; Point to next S-record byte
                    bra       FlashIt@@           ; Resume programming
          ;-------------------------------------- ; FLASH programming error
Fail@@              lda       #PgmErrChr          ; Transmit programming error character
                    bra       Loop@@@             ; Resume S-record scan
