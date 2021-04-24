;*******************************************************************************
; Test TRACE function under PCBUGF1 talker
; To use with PCBUG11, follow these commands (starting from DOS):
;
; ASPISYS <ENTER>
; LOADS TEST <ENTER>
; RM
; -> 2 <ENTER>
; T <ENTER>
; ...
;*******************************************************************************

                    #ROM      $2000

Start               proc
                    lds       #$1FFF

                    ldd       #$1111
                    ldx       #$2222
                    ldy       #$3333

                    bsr       Subroutine
                    bra       *

;*******************************************************************************

Subroutine          proc
                    nop
                    rts

                    end       Start
