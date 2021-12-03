;*******************************************************************************
; AccA increments lights
; AccB = short delay
; AccY = long delay
; AccX -> stack
;*******************************************************************************

lights              equ       $1404
switch              equ       $1403
toc2                equ       $1018
toc3                equ       $101A
tcnt                equ       $100E
tflg1               equ       $1023
tmsk1               equ       $1022
shortdelay          equ       100
longdelay           equ       5
interuptdelay       equ       20000

;*******************************************************************************
                    #RAM      0
;*******************************************************************************

flashflag           rmb       1

                    #VECTORS  $00D9               ; interrupt jump table
                    jmp       ISR_Handler         ; causes interrupt when OC3 is triggered

;*******************************************************************************
                    #ROM      $D000
;*******************************************************************************

Start               proc
                    lds       #$DFFF
                    ldb       #shortdelay
                    ldy       #longdelay
                    lda       #$20
                    sta       tmsk1               ; start interrupts in OC3
                    ldd       tcnt
                    addd      #interuptdelay
                    std       toc3
                    clra
                    sta       flashflag
                    cli
;                   bra       MainLoop

;*******************************************************************************
; Increment lights once per second

MainLoop            proc
                    tst       flashflag
                    bne       allflash
Cont                staa      lights
                    bra       MainLoop

;*******************************************************************************
; Flash all lights

allflash            proc
                    pshd
                    ldb       #50
Loop@@              clra
                    sta       lights
                    bsr       Delay
                    lda       #$FF
                    sta       lights
                    bsr       Delay
                    decb
                    bne       Loop@@
                    clra
                    sta       flashflag
                    puld
                    bra       Cont

;*******************************************************************************
; Delay program

Delay               proc
                    pshd
                    ldd       tcnt
                    std       toc2
                    pshx
                    lda       #$40
                    sta       tflg1
                    ldx       #tflg1
                    brclr     ,x,$40,*            ; wait until timer flag is set
                    pulx
                    puld
                    rts

;*******************************************************************************

ISR_Handler         proc
                    ldd       tcnt
                    addd      #interuptdelay
                    std       toc3                ; reset interupt counter
                    tsx                           ; transfer stack ponter to X so variables can be changed in the stack
                    lda       2,x
                    ldb       1,x
                    ldy       5,x                 ; load accumulators back from stack
                    decb
                    bne       _@@                 ; don't increment lights if not 1 second
                    inca
                    ldb       #shortdelay
                    dey
                    bne       _@@                 ; not 5 seconds, please wait longer
                    psha
                    lda       #$01
                    sta       flashflag
                    pula
                    ldy       #longdelay
_@@                 sta       2,x
                    stb       1,x
                    sty       5,x
                    lda       #$20
                    sta       tflg1
                    rti
