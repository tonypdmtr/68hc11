;*******************************************************************************
                    #Uses     lcd_2.asm
;*******************************************************************************

OctOut              proc
                    psha                          ; save it
                    rola:3                        ; put it to carry and around sometimes
                    anda      #%00000011          ; unneeded off
                    adda      #'0'                ; make it ascii
                    bsr       WriteLCD            ; and out
                    pula                          ; get it back
                    psha                          ; store it again
                    lsra:3                        ; now other bits down
                    anda      #%00000111          ; cut off
                    adda      #'0'                ; ... ascii
                    bsr       WriteLCD            ; ... out
                    pula                          ; for the last time
                    anda      #%00000111
                    adda      #'0'
                    bra       WriteLCD
