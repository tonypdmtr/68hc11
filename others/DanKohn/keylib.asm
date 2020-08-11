#ifmain ;-----------------------------------------------------------------------
                    org       *
#endif ;------------------------------------------------------------------------

;*************************************************************************
; Name:         Keypad Library
; Version:      1.0
; Date:         03/23/2002
; Author:       Dan Kohn
;

PORTD               equ       $1008               ; port D address
PDDR                equ       $1009               ; port D direction register
PORTE               equ       $100A               ; port E address
IO                  equ       $3E                 ; port D direction value
BUFSIZE             equ       20                  ; Keypad Buffer Size (in Bytes)
RET_KEY             equ       $2a                 ; Return key definition - '*' key
END_STR             equ       $04                 ; End of string flag definition

TABLE               fcb       $31,$32,$33,$41     ; keypad ASCII lookup table
                    fcb       $34,$35,$36,$42
                    fcb       $37,$38,$39,$43
                    fcb       $2A,$30,$23,$44

LASTKEY             rmb       1
KEYBUF              rmb       BUFSIZE

;*************************************************************************
; Name:         Get String
; Call By:      GET_STR
; Version:      1.0
; Date:         03/23/2002
; Author:       Dan Kohn
;
; Function:     To place a string of characters to KEYBUF and end the
;               string with a END_STR character
;
; Process:      1) Check for key press
;               2) Get key
;               3) Debounce keypad entry
;               4) If RET_KEY, add END_STR to KEYBUF and exit
;               5) If length of string > BUFSIZE-1 add END_STR and exit
;               6) Repeat steps 1-5 until either condition in step
;                  4 or 5 is met.
;
; Protocol:     Upon exit, string resides in KEYBUF with RET_KEY being
;               the last valid entry in buffer. All registers returned
;               to previous values.
;
;

GET_STR             proc
                    pshx                          ; Setup
                    psha
                    pshb

                    ldx       #KEYBUF
Loop@@              bsr       ANYKEY              ; Wait for key
                    beq       Loop@@
                    bsr       GETKEY              ; Get key
                    bsr       DBKEY
                    cmpa      #RET_KEY            ; if key is the RET_KEY, goto Done@@
                    beq       Done@@
                    tsta                          ; if value returned was $00 go back and get annother key
                    beq       Loop@@
                    sta       ,x                  ; Store ascii character in buffer
                    inx                           ; increment buffer location and
                    cpx       #KEYBUF+BUFSIZE-1   ; goto Done@@ if buffer full
                    bne       Loop@@              ; Loop back to ENT_TI to get next character

Done@@              lda       #END_STR            ; Instert end of string character into buffer
                    sta       ,x
                    pulb
                    pula
                    pulx
                    rts                           ; exit


;*************************************************************************
; Name:         Keypad Setup
; Call By:      KEYSET
; Version:      1.0
; Date:         03/23/2002
; Author:       Dan Kohn
;
; Function:     Set up PORTD For Keypad Entry by setting PDDR
;
; Protocol:     No Registers Changed upon RTS
;

KEYSET              proc
                    psha
                    lda       #IO
                    sta       PDDR
                    pula
                    rts

;*************************************************************************
; Name:         Any Key Pressed?
; Call By:      ANYKEY
; Version:      1.0
; Date:         03/23/2002
; Author:       Dan Kohn
;
; Function:     To determin if a key is being pressed
;
;
; Process:      Turn on all pins connected to keypad matrix (PORTD)
;               then read PORTE. If Port E pin 0 - 3 has any high
;               inputs, then a key is being pressed.
;
;                   PORT E -> PE0 PE1 PE2 PE3
;
;                    +-   PD2  1---2---3---A
;                    |         |   |   |   |
;                    |    PD3  4---5---6---B
;             PORTD -|         |   |   |   |
;                    |    PD4  7---8---9---C
;                    |         |   |   |   |
;                    +-   PD5  *---0---#---D
;
;
; Protocol:     CCR Z flag is set when NO key is being pressed
;               CCR Z Flag is NOT set when a key is being pressed

ANYKEY              proc
                    psha
                    lda       #$3C
                    sta       PORTD
                    lda       PORTE
                    anda      #$0F
                    pula
                    rts

;*************************************************************************
; Name:         GET KEY
; Call By:      GETKEY
; Version:      1.0
; Date:         03/23/2002
; Author:       Dan Kohn
;
; Function:     Get a key from the keypad and return the ASCII value
;               of that key.
;
; Process:      1) Turn on first bit of PORTD keypad pin (PD2) to scan
;                  first row of keyboard
;               2) READ PE0 -> PE3 and test bit for a HIGH. If a pin
;                  HIGH go to step ??
;
;                   PORT E -> PE0 PE1 PE2 PE3
;
;                    +-   PD2  1---2---3---A
;                    |         |   |   |   |
;                    |    PD3  4---5---6---B
;             PORTD -|         |   |   |   |
;                    |    PD4  7---8---9---C
;                    |         |   |   |   |
;                    +-   PD5  *---0---#---D
;
;               3) Turn off previous row and turn on next row.
;               4) If all 4 rows have been scaned, goto step 5, else
;                  repeat steps 2 -> 4.
;               5) Since all rows were scaned with no key being found
;                  set REG A to $00 and return.
;               6) Since a key was found, convert the key pressed to
;                  ASCII (using TABLE) and place the ASCII Character
;                  in REGISTER A and return.
;
;
; Protocol:     On Exit REGISTER A contains a $00 if no key pressed
;               or the ASCII Code of the key pressed.
;

GETKEY              proc
                    pshx
                    pshb
                    ldx       #$00                ; Reset Key Count
                    ldb       #$04                ; Turn on First Row of keys
RLoop@@             stb       PORTD
                    lda       #$01                ; Set up to check first column
CLoop@@             inx                           ; Increment key count (used for ASCII Lookup)
                    cmpa      PORTE               ; is key in active row, active column high
                    beq       Load@@              ; if it is goto Load@@
                    lsla                          ; set up next column to be tested
                    cmpa      #$08                ; if it is a valid column, goto CLoop@@
                    ble       CLoop@@
                    lslb                          ; set up next ROW to be tested
                    cmpb      #$20                ; if it is a valid ROW, goto RLoop@@
                    ble       RLoop@@

                    clra                          ; If all rows and columns have been tested
                    bra       Done@@              ; without success, set REGA = $00 and exit

Load@@              dex                           ; decement key count (so keycount goes from 0-15, not 1-16)
                    xgdx                          ; exchange REG D with REG X (prep for calc)
                    addd      #TABLE              ; Add keycount to start location of ASCII Lookup table
                    xgdx                          ; exchange REG D with REG X (so X points to ASCII equivelent of key pressed)
                    lda       ,x                  ; Load A with ASCII equivelent of key pressed

Done@@              pulb                          ; Exit
                    pulx
                    rts

;*************************************************************************
; Name:         Debounce Key Pad
; Call By:      DBKEY
; Version:      1.0
; Date:         03/24/2002
; Author:       Dan Kohn
;
; Function:     To prevent multiple returns of the same keypress (Debounce
;               the keypad via software)
;
; Process:      1) store the last key pressed
;               2) pause
;               3) re-read the keypad
;               4) if the new key is the same as the last key then
;                  goto step 2.
;               5) if the character is different or $00 then load
;                  register A with the value stored in step 1 and exit
;
; Protocol:     Returns the debounced key's ascii value in REG A
;
;

DBKEY               proc
                    pshx
                    sta       LASTKEY
MainLoop@@          ldx       #0100               ; Enter Delay Loop
Loop@@              dex
                    bne       Loop@@
                    bsr       GETKEY              ; Check Key Again
                    cmpa      LASTKEY             ; If Same As Last Key, Loop Again
                    beq       MainLoop@@
                    lda       LASTKEY             ; Restore Last Key
                    pulx
                    rts                           ; Return
