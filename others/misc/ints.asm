;*******************************************************************************
; Melody playing example by unknown author and vastly improved by tonyp@acm.org
;*******************************************************************************

REGS                equ       0                   ; zero-based registers for convenience
                    #Uses     mcu.inc

SPEAKER             pin       PORTA,5             ; The speaker port

TDRE                pin       SCSR,1              ; TDRE flag in SCSR
RDRF                pin       SCSR,0              ; RDRF flag in SCSR

;*******************************************************************************
                    #ROM
;*******************************************************************************

LAMB                fcs       'trerttttrrrrtuuutrerttttrrtre'
CHROMSC             fcs       'q2we4r5ty7u8i9op-[=]'
HALL                fcs       'qwertet5w5r2rqwertetiutetu'

?                   macro     ;key, frequency, and 1/4 second "period"
                    fcb       ~1~
                    dw        ~2~,~3~
                    endm

KEYMAP              @?        'q',220,758         ; A ;key, frequency, and 1/4 second "period"
                    @?        '2',233,715         ; A#
                    @?        'w',247,675         ; B
                    @?        'e',262,636         ; C
                    @?        '4',277,602         ; C#
                    @?        'r',294,567         ; D
                    @?        '5',311,536         ; D#
                    @?        't',330,505         ; E
                    @?        'y',349,478         ; F
                    @?        '7',370,450         ; F#
                    @?        'u',392,425         ; G
                    @?        '8',415,402         ; G#
                    @?        'i',440,379         ; A
                    @?        '9',466,358         ; A#
                    @?        'o',494,337         ; B
                    @?        'p',523,319         ; C
                    @?        '-',554,301         ; C#
                    @?        '[',587,284         ; D
                    @?        '=',622,268         ; D#
                    @?        ']',659,253         ; E
                    fcb       0                   ; Null termination character

PROMPT              fcs       CR,'Piano program - use QWERTY row to play notes',CR

;*******************************************************************************

Start               proc
                    lds       #STACKTOP
                    clr       XINIT
          ;-------------------------------------- ; Main Start
                    ldx       #PROMPT
                    bsr       PrintString         ; Print the promp string

Loop@@              bsr       GetChar             ; Get a character from the keyboard
                    bsr       PutChar
                    bsr       PlayTone            ; Play the tone

                    cmpa      #'a'
                    bne       Cont@@

                    ldx       #HALL
                    bsr       PlaySong

Cont@@              cmpa      #'s'
                    bne       Done@@

                    ldx       #LAMB
                    bsr       PlaySong

Done@@              bra       Loop@@              ; Loop to the top and continue playing

;*******************************************************************************
; Passed an ASCII character in RegA

PlayTone            proc
                    push                          ; for transparency
                    ldx       #KEYMAP             ; make X point to the KEYMAP
_1@@                ldb       ,x                  ; load B with the ASCII value
                    beq       Done@@              ; If end of table, no key match, exit routine
                    cba
                    beq       _2@@                ; If value are equal, skip rest of test to play tone
                    aix       #5                  ; point to the next character to compare
                    bra       _1@@                ; Branch until the end of table is reached
_2@@                ldd       1,x                 ; Load D with the frequency
                    lsrd:2                        ; Number of times to toggle the speaker in a 1/8 second
          ;--------------------------------------
Loop@@              ldy       3,x                 ; Load Y with the delay between toggles
_3@@                dey
                    bne       _3@@                ; Repeat until 0

                    psha
                    lda       SPEAKER             ; Load A with the speaker
                    eora      #@SPEAKER           ; Toggle the speaker bit
                    sta       SPEAKER             ; Store back into the speaker
                    pula

                    decd
                    bne       Loop@@              ; Branch until D is 0
          ;--------------------------------------
Done@@              pull                          ; return the stack to normal
                    rts                           ; return to the main program

;*******************************************************************************

PlaySong            proc
                    pshx
                    psha
Loop@@              lda       ,x                  ; Load A with the ith character of the string
                    beq       Done@@              ; Skips to end of subroutine if current character is null character
                    bsr       PlayTone
                    inx                           ; increments X to point to the next character
                    bra       Loop@@
Done@@              pula
                    pulx
                    rts

;*******************************************************************************
; Argument passed in RegA should be an ASCII value

PutChar             proc
                    brclr     TDRE,*              ; Wait for transmitter ready
                    sta       SCDR                ; Write A to the SCDR
                    rts

;*******************************************************************************
; ASCII value is returned in RegA

GetChar             proc
                    brclr     RDRF,*              ; Wait for character
                    lda       SCDR                ; Load A with what's in the SCDR (should be the pressed key)
                    rts

;*******************************************************************************

PrintString         proc
                    pshx
                    psha
Loop@@              lda       ,x                  ; Load A with the ith character of the string
                    beq       Done@@              ; Skips to end of subroutine if current character is null character
                    bsr       PutChar
                    inx                           ; increments X to point to the next character
                    bra       Loop@@
Done@@              pula
                    pulx
                    rts

;*******************************************************************************
