;*******************************************************************************
; 68HC11.ASM
; This file is a list of all permutations of 68HC11 instructions
; Written by Kenneth A. Kuhn, March 30, 1994
; Reformatted and adapted to ASM11 by Tony G. Papadimitriou <tonyp@acm.org>
;*******************************************************************************

                    #ROM      *
          ;-------------------------------------- ; Define symbol equates
ADDR_DIR            equ       $F8                 ; An 8-bit address (upper 8 bits = 0)
ADDR_EXT            equ       $A45C               ; A 16-bit address
BYTE_DATA           equ       $5C                 ; 8-bit data
WORD_DATA           equ       $CFE4               ; 16-bit data
OFFSET              equ       $7A                 ; 8-bit positive offset from X or Y address

          ;--------------------------------------
          ; Load/Store/Transfer Instructions
          ;-------------------------------------- ; Clear
                    clr       ADDR_EXT            ; *ADDR_EXT = 0
                    clr       OFFSET,X            ; *(X + OFFSET) = 0
                    clr       OFFSET,Y            ; *(Y + OFFSET) = 0
                    clra                          ; A = 0
                    clrb                          ; B = 0
          ;-------------------------------------- ; Load registers
                    lda       #BYTE_DATA          ; A = BYTE_DATA
                    lda       ADDR_DIR            ; A = *ADDR_DIR
                    lda       ADDR_EXT            ; A = *ADDR_EXT
                    lda       OFFSET,X            ; A = *(X + OFFSET)
                    lda       OFFSET,Y            ; A = *(Y + OFFSET)
                    ldb       #BYTE_DATA          ; B = BYTE_DATA
                    ldb       ADDR_DIR            ; B = *ADDR_DIR
                    ldb       ADDR_EXT            ; B = *ADDR_EXT
                    ldb       OFFSET,X            ; B = *(X + OFFSET)
                    ldb       OFFSET,Y            ; B = *(Y + OFFSET)
                    ldd       #WORD_DATA          ; D = WORD_DATA
                    ldd       ADDR_DIR            ; D = *ADDR_DIR
                    ldd       ADDR_EXT            ; D = *ADDR_EXT
                    ldd       OFFSET,X            ; D = *(X + OFFSET)
                    ldd       OFFSET,Y            ; D = *(Y + OFFSET)
                    ldx       #WORD_DATA          ; X = WORD_DATA
                    ldx       ADDR_DIR            ; X = *ADDR_DIR
                    ldx       ADDR_EXT            ; X = *ADDR_EXT
                    ldx       OFFSET,X            ; X = *(X + OFFSET)
                    ldx       OFFSET,Y            ; X = *(Y + OFFSET)
                    ldy       #WORD_DATA          ; Y = WORD_DATA
                    ldy       ADDR_DIR            ; Y = *ADDR_DIR
                    ldy       ADDR_EXT            ; Y = *ADDR_EXT
                    ldy       OFFSET,X            ; Y = *(X + OFFSET)
                    ldy       OFFSET,Y            ; Y = *(Y + OFFSET)
          ;-------------------------------------- ; Store registers
                    sta       ADDR_DIR            ; *ADDR_DIR = A
                    sta       ADDR_EXT            ; *ADDR_EXT = A
                    sta       OFFSET,X            ; *(X + OFFSET) = A
                    sta       OFFSET,Y            ; *(Y + OFFSET) = A
                    stb       ADDR_DIR            ; *ADDR_DIR = B
                    stb       ADDR_EXT            ; *ADDR_EXT = B
                    stb       OFFSET,X            ; *(X + OFFSET) = B
                    stb       OFFSET,Y            ; *(Y + OFFSET) = B
                    std       ADDR_DIR            ; *ADDR_DIR = D
                    std       ADDR_EXT            ; *ADDR_EXT = D
                    std       OFFSET,X            ; *(X + OFFSET) = D
                    std       OFFSET,Y            ; *(Y + OFFSET) = D
                    sts       ADDR_DIR            ; *ADDR_DIR = SP
                    sts       ADDR_EXT            ; *ADDR_EXT = SP
                    sts       OFFSET,X            ; *(X + OFFSET) = SP
                    sts       OFFSET,Y            ; *(Y + OFFSET) = SP
                    stx       ADDR_DIR            ; *ADDR_DIR = X
                    stx       ADDR_EXT            ; *ADDR_EXT = X
                    stx       OFFSET,X            ; *(X + OFFSET) = X
                    stx       OFFSET,Y            ; *(Y + OFFSET) = X
                    sty       ADDR_DIR            ; *ADDR_DIR = Y
                    sty       ADDR_EXT            ; *ADDR_EXT = Y
                    sty       OFFSET,X            ; *(X + OFFSET) = Y
                    sty       OFFSET,Y            ; *(Y + OFFSET) = Y
          ;-------------------------------------- ; Transfer registers
                    tab                           ; B = A
                    tba                           ; A = B
                    tap                           ; CCR = A
                    tpa                           ; A = CCR
                    tsx                           ; X = SP
                    tsy                           ; Y = SP
                    txs                           ; SP = X
                    tys                           ; SP = Y
          ;-------------------------------------- ; Exchange registers
                    xgdx                          ; Exchange contents of D with X
                    xgdy                          ; Exchange contents of D with Y
          ;-------------------------------------- ; Stack
                    psha                          ; SP-- = A
                    pshb                          ; SP-- = B
                    pshx                          ; SP-- = X.low, *SP-- = X.high
                    pshy                          ; SP-- = Y.low, *SP-- = Y.high

                    pula                          ; A = *(++SP)
                    pulb                          ; B = *(++SP)
                    pulx                          ; X.high = *(++SP), X.low = *(++SP)
                    puly                          ; Y.high = *(++SP), Y.low = *(++SP)
          ;--------------------------------------
          ; Arithmetic/Math Instructions
          ;-------------------------------------- ; Add
                    aba                           ; A = A + B
                    abx                           ; X = X + B
                    aby                           ; Y = Y + B
                    adca      #BYTE_DATA          ; A = A + BYTE_DATA + carry_flag
                    adca      ADDR_DIR            ; A = A + *ADDR_DIR + carry_flag
                    adca      ADDR_EXT            ; A = A + *ADDR_EXT + carry_flag
                    adca      OFFSET,X            ; A = A + *(X + OFFSET) + carry_flag
                    adca      OFFSET,Y            ; A = A + *(Y + OFFSET) + carry_flag
                    adcb      #BYTE_DATA          ; B = B + BYTE_DATA + carry_flag
                    adcb      ADDR_DIR            ; B = B + *ADDR_DIR + carry_flag
                    adcb      ADDR_EXT            ; B = B + *ADDR_EXT + carry_flag
                    adcb      OFFSET,X            ; B = B + *(X + OFFSET) + carry_flag
                    adcb      OFFSET,Y            ; B = B + *(Y + OFFSET) + carry_flag
                    adda      #BYTE_DATA          ; A = A + BYTE_DATA
                    adda      ADDR_DIR            ; A = A + *ADDR_DIR
                    adda      ADDR_EXT            ; A = A + *ADDR_EXT
                    adda      OFFSET,X            ; A = A + *(X + OFFSET)
                    adda      OFFSET,Y            ; A = A + *(Y + OFFSET)
                    addb      #BYTE_DATA          ; B = B + BYTE_DATA
                    addb      ADDR_DIR            ; B = B + *ADDR_DIR
                    addb      ADDR_EXT            ; B = B + *ADDR_EXT
                    addb      OFFSET,X            ; B = B + *(X + OFFSET)
                    addb      OFFSET,Y            ; B = B + *(Y + OFFSET)
                    addd      #WORD_DATA          ; D = D + WORD_DATA
                    addd      ADDR_DIR            ; D = D + *ADDR_DIR
                    addd      ADDR_EXT            ; D = D + *ADDR_EXT
                    addd      OFFSET,X            ; D = D + *(X + OFFSET)
                    addd      OFFSET,Y            ; D = D + *(Y + OFFSET)
          ;-------------------------------------- ; Subtract
                    sba                           ; A = A - B
                    sbca      #BYTE_DATA          ; A = A - BYTE_DATA - carry_flag
                    sbca      ADDR_DIR            ; A = A - *ADDR_DIR - carry_flag
                    sbca      ADDR_EXT            ; A = A - *ADDR_EXT - carry_flag
                    sbca      OFFSET,X            ; A = A - *(X + OFFSET) - carry_flag
                    sbca      OFFSET,Y            ; A = A - *(Y + OFFSET) - carry_flag
                    sbcb      #BYTE_DATA          ; B = B - BYTE_DATA - carry_flag
                    sbcb      ADDR_DIR            ; B = B - *ADDR_DIR - carry_flag
                    sbcb      ADDR_EXT            ; B = B - *ADDR_EXT - carry_flag
                    sbcb      OFFSET,X            ; B = B - *(X + OFFSET) - carry_flag
                    sbcb      OFFSET,Y            ; B = B - *(Y + OFFSET) - carry_flag
                    suba      #BYTE_DATA          ; A = A - BYTE_DATA
                    suba      ADDR_DIR            ; A = A - *ADDR_DIR
                    suba      ADDR_EXT            ; A = A - *ADDR_EXT
                    suba      OFFSET,X            ; A = A - *(X + OFFSET)
                    suba      OFFSET,Y            ; A = A - *(Y + OFFSET)
                    subb      #BYTE_DATA          ; B = B - BYTE_DATA
                    subb      ADDR_DIR            ; B = B - *ADDR_DIR
                    subb      ADDR_EXT            ; B = B - *ADDR_EXT
                    subb      OFFSET,X            ; B = B - *(X + OFFSET)
                    subb      OFFSET,Y            ; B = B - *(Y + OFFSET)
                    subd      #WORD_DATA          ; D = D - WORD_DATA
                    subd      ADDR_DIR            ; D = D - *ADDR_DIR
                    subd      ADDR_EXT            ; D = D - *ADDR_EXT
                    subd      OFFSET,X            ; D = D - *(X + OFFSET)
                    subd      OFFSET,Y            ; D = D - *(Y + OFFSET)
          ;-------------------------------------- ; Multiply
                    mul                           ; (unsigned) D = A * B
          ;-------------------------------------- ; Divide
                    fdiv                          ; (unsigned) X = X / D, D = remainder
                    idiv                          ; (unsigned) X = D / X, D = remainder
          ;-------------------------------------- ; Negate (change sign)
                    neg       ADDR_EXT            ; *ADDR_EXT = -*ADDR_EXT
                    neg       OFFSET,X            ; *(X + OFFSET) = -*(X + OFFSET)
                    neg       OFFSET,Y            ; *(Y + OFFSET) = -*(Y + OFFSET)
                    nega                          ; A = -A
                    negb                          ; B = -B
          ;-------------------------------------- ; Increment
                    inc       ADDR_EXT            ; *ADDR_EXT = *ADDR_EXT + 1
                    inc       OFFSET,X            ; *(X + OFFSET) = *(X + OFFSET) + 1
                    inc       OFFSET,Y            ; *(Y + OFFSET) = *(Y + OFFSET) + 1
                    inca                          ; A = A + 1
                    incb                          ; B = B + 1
                    ins                           ; SP = SP + 1
                    inx                           ; X = X + 1
                    iny                           ; Y = Y + 1
          ;-------------------------------------- ; Decrement
                    dec       ADDR_EXT            ; *ADDR_EXT = *ADDR_EXT - 1
                    dec       OFFSET,X            ; *(X + OFFSET) = *(X + OFFSET) - 1
                    dec       OFFSET,Y            ; *(Y + OFFSET) = *(Y + OFFSET) - 1
                    deca                          ; A = A - 1
                    decb                          ; B = B - 1
                    des                           ; SP = SP - 1
                    dex                           ; X = X - 1
                    dey                           ; Y = Y - 1
          ;-------------------------------------- ; Compare
                    cba                           ; CCR flags = result of A - B
                    cmpa      #BYTE_DATA          ; CCR flags = result of A - BYTE_DATA
                    cmpa      ADDR_DIR            ; CCR flags = result of A - *ADDR_DIR
                    cmpa      ADDR_EXT            ; CCR flags = result of A - *ADDR_EXT
                    cmpa      OFFSET,X            ; CCR flags = result of A - *(X + OFFSET)
                    cmpa      OFFSET,Y            ; CCR flags = result of A - *(Y + OFFSET)
                    cmpb      #BYTE_DATA          ; CCR flags = result of B - BYTE_DATA
                    cmpb      ADDR_DIR            ; CCR flags = result of B - *ADDR_DIR
                    cmpb      ADDR_EXT            ; CCR flags = result of B - *ADDR_EXT
                    cmpb      OFFSET,X            ; CCR flags = result of B - *(X + OFFSET)
                    cmpb      OFFSET,Y            ; CCR flags = result of B - *(Y + OFFSET)
                    cpd       #WORD_DATA          ; CCR flags = result of D - WORD_DATA
                    cpd       ADDR_DIR            ; CCR flags = result of D - *ADDR_DIR
                    cpd       ADDR_EXT            ; CCR flags = result of D - *ADDR_EXT
                    cpd       OFFSET,X            ; CCR flags = result of D - *(X + OFFSET)
                    cpd       OFFSET,Y            ; CCR flags = result of D - *(Y + OFFSET)
                    cpx       #WORD_DATA          ; CCR flags = result of X - WORD_DATA
                    cpx       ADDR_DIR            ; CCR flags = result of X - *ADDR_DIR
                    cpx       ADDR_EXT            ; CCR flags = result of X - *ADDR_EXT
                    cpx       OFFSET,X            ; CCR flags = result of X - *(X + OFFSET)
                    cpx       OFFSET,Y            ; CCR flags = result of X - *(Y + OFFSET)
                    cpy       #WORD_DATA          ; CCR flags = result of Y - WORD_DATA
                    cpy       ADDR_DIR            ; CCR flags = result of Y - *ADDR_DIR
                    cpy       ADDR_EXT            ; CCR flags = result of Y - *ADDR_EXT
                    cpy       OFFSET,X            ; CCR flags = result of Y - *(X + OFFSET)
                    cpy       OFFSET,Y            ; CCR flags = result of Y - *(Y + OFFSET)
          ;-------------------------------------- ; Test
                    tst       ADDR_EXT            ; CCR flags = result of *ADDR_EXT - 0
                    tst       OFFSET,X            ; CCR flags = result of *(X + OFFSET) - 0
                    tst       OFFSET,Y            ; CCR flags = result of *(Y + OFFSET) - 0
                    tsta                          ; CCR flags = result of A - 0
                    tstb      CCR                 ; flags = result of B - 0
          ;-------------------------------------- ; BCD adjust
                    daa                           ; Adjust A for BCD result after ADA, ADD, ADC
          ;--------------------------------------
          ; Logical Instructions
          ;-------------------------------------- ; AND
                    anda      #BYTE_DATA          ; A = A & BYTE_DATA
                    anda      ADDR_DIR            ; A = A & *ADDR_DIR
                    anda      ADDR_EXT            ; A = A & *ADDR_EXT
                    anda      OFFSET,X            ; A = A & *(X + OFFSET)
                    anda      OFFSET,Y            ; A = A & *(Y + OFFSET)
                    andb      #BYTE_DATA          ; B = B & BYTE_DATA
                    andb      ADDR_DIR            ; B = B & *ADDR_DIR
                    andb      ADDR_EXT            ; B = B & *ADDR_EXT
                    andb      OFFSET,X            ; B = B & *(X + OFFSET)
                    andb      OFFSET,Y            ; B = B & *(Y + OFFSET)
          ;-------------------------------------- ; OR
                    ora       #BYTE_DATA          ; A = A | BYTE_DATA
                    ora       ADDR_DIR            ; A = A | *ADDR_DIR
                    ora       ADDR_EXT            ; A = A | *ADDR_EXT
                    ora       OFFSET,X            ; A = A | *(X + OFFSET)
                    ora       OFFSET,Y            ; A = A | *(Y + OFFSET)
                    orb       #BYTE_DATA          ; B = B | BYTE_DATA
                    orb       ADDR_DIR            ; B = B | *ADDR_DIR
                    orb       ADDR_EXT            ; B = B | *ADDR_EXT
                    orb       OFFSET,X            ; B = B | *(X + OFFSET)
                    orb       OFFSET,Y            ; B = B | *(Y + OFFSET)
          ;-------------------------------------- ; Exclusive OR
                    eora      #BYTE_DATA          ; A = A ^ BYTE_DATA
                    eora      ADDR_DIR            ; A = A ^ *ADDR_DIR
                    eora      ADDR_EXT            ; A = A ^ *ADDR_EXT
                    eora      OFFSET,X            ; A = A ^ *(X + OFFSET)
                    eora      OFFSET,Y            ; A = A ^ *(Y + OFFSET)
                    eorb      #BYTE_DATA          ; B = B ^ BYTE_DATA
                    eorb      ADDR_DIR            ; B = B ^ *ADDR_DIR
                    eorb      ADDR_EXT            ; B = B ^ *ADDR_EXT
                    eorb      OFFSET,X            ; B = B ^ *(X + OFFSET)
                    eorb      OFFSET,Y            ; B = B ^ *(Y + OFFSET)
          ;-------------------------------------- ; Complement bits
                    com       ADDR_EXT            ; *ADDR_EXT = ~*ADDR_EXT
                    com       OFFSET,X            ; *(X + OFFSET) = ~*(X + OFFSET)
                    com       OFFSET,Y            ; *(Y + OFFSET) = ~*(Y + OFFSET)
                    coma                          ; A = ~A
                    comb                          ; B = ~B
          ;-------------------------------------- ; Bit test
                    bita      #BYTE_DATA          ; CCR flags = result of A & BYTE_DATA
                    bita      ADDR_DIR            ; CCR flags = result of A & *ADDR_DIR
                    bita      ADDR_EXT            ; CCR flags = result of A & *ADDR_EXT
                    bita      OFFSET,X            ; CCR flags = result of A & *(X + OFFSET)
                    bita      OFFSET,Y            ; CCR flags = result of A & *(Y + OFFSET)
                    bitb      #BYTE_DATA          ; CCR flags = result of B & BYTE_DATA
                    bitb      ADDR_DIR            ; CCR flags = result of B & *ADDR_DIR
                    bitb      ADDR_EXT            ; CCR flags = result of B & *ADDR_EXT
                    bitb      OFFSET,X            ; CCR flags = result of B & *(X + OFFSET)
                    bitb      OFFSET,Y            ; CCR flags = result of B & *(Y + OFFSET)
          ;--------------------------------------
          ; Shift/Rotate Instructions
          ; (Note: any bit shifted out goes to carry flag)
          ;-------------------------------------- ; Arithmetic shift left
                    asl       ADDR_EXT            ; *ADDR_EXT = *ADDR_EXT * 2
                    asl       OFFSET,X            ; *(X + OFFSET) = *(X + OFFSET) * 2
                    asl       OFFSET,Y            ; *(Y + OFFSET) = *(Y + OFFSET) * 2
                    asla                          ; A = A * 2
                    aslb                          ; B = B * 2
                    asld                          ; D = D * 2
          ;-------------------------------------- ; Logical shift left
                    lsl       ADDR_EXT            ; *ADDR_EXT = *ADDR_EXT * 2
                    lsl       OFFSET,X            ; *(X + OFFSET) = *(X + OFFSET) * 2
                    lsl       OFFSET,Y            ; *(Y + OFFSET) = *(Y + OFFSET) * 2
                    lsla                          ; Same as ASLA
                    lslb                          ; Same as ASLB
                    lsld                          ; Same as ASLD
          ;-------------------------------------- ; Arithmetic shift right
                    asr       ADDR_EXT            ; (signed) *ADDR_EXT = *ADDR_EXT / 2
                    asr       OFFSET,X            ; (signed) *(X + OFFSET) = *(X + OFFSET) / 2
                    asr       OFFSET,Y            ; (signed) *(Y + OFFSET) = *(Y + OFFSET) / 2
                    asra                          ; (signed) A = A / 2
                    asrb                          ; (signed) B = B / 2
          ;-------------------------------------- ; Logical shift right
                    lsr       ADDR_EXT            ; (unsigned) *ADDR_EXT = *ADDR_EXT / 2
                    lsr       OFFSET,X            ; (unsigned) *(X + OFFSET) = *(X + OFFSET) / 2
                    lsr       OFFSET,Y            ; (unsigned) *(Y + OFFSET) = *(Y + OFFSET) / 2
                    lsra                          ; (unsigned) A = A / 2
                    lsrb                          ; (unsigned) B = B / 2
                    lsrd                          ; (unsigned) D = D / 2
          ;-------------------------------------- ; Rotate left thru carry
                    rol       ADDR_EXT            ; *ADDR_EXT = *ADDR_EXT * 2 + carry_flag
                    rol       OFFSET,X            ; *(X + OFFSET) = *(X + OFFSET) * 2 + carry_flag
                    rol       OFFSET,Y            ; *(Y + OFFSET) = *(Y + OFFSET) * 2 + carry_flag
                    rola                          ; A = A * 2 + carry_flag
                    rolb                          ; B = B * 2 + carry_flag
          ;-------------------------------------- ; Rotate right thru carry
                    ror       ADDR_EXT            ; *ADDR_EXT = *ADDR_EXT / 2, b7 = carry_flag
                    ror       OFFSET,X            ; *(X + OFFSET) = *(X + OFFSET) / 2, b7 = c_flag
                    ror       OFFSET,Y            ; *(Y + OFFSET) = *(Y + OFFSET) / 2, b7 = c_flag
                    rora                          ; A = A / 2, b7 = carry_flag
                    rorb                          ; B = B / 2, b7 = carry_flag
          ;--------------------------------------
          ; Branch Instructions
          ;-------------------------------------- ; Label for branch instructions
                    bcc       *                   ; Branch if carry_flag clear
                    bhs       *                   ; Branch if unsigned compare >= (BCC)

                    bcs       *                   ; Branch if carry_flag set
                    blo       *                   ; Branch if lower

                    beq       *                   ; Branch if zero flag set
                    bne       *                   ; Branch if zero flag not set

                    bge       *                   ; Branch if signed compare >=
                    bgt       *                   ; Branch if signed compare >
                    ble       *                   ; Branch if signed compare <=
                    blt       *                   ; Branch if signed compare <

                    bhi       *                   ; Branch if unsigned compare >
                    bls       *                   ; Branch if unsigned compare <=

                    bmi       *                   ; Branch if minus
                    bpl       *                   ; Branch if positive (or zero)

                    bvc       *                   ; Branch if overflow flag is clear
                    bvs       *                   ; Branch if overflow flag is set

                    bra       *                   ; Branch always

                    brn       *                   ; Branch never (a two-byte NOP)
                    nop                           ; No Operation
          ;--------------------------------------
          ; Test address with BYTE_DATA and branch only if all bits
          ; specified by a 1 in BYTE_DATA are 0 in the tested byte
          ;--------------------------------------
                    brclr     ADDR_DIR,#BYTE_DATA,*
                    brclr     OFFSET,X,#BYTE_DATA,*
                    brclr     OFFSET,Y,#BYTE_DATA,*
          ;--------------------------------------
          ; Test address with BYTE_DATA and branch only if all bits
          ; specified by a 1 in BYTE_DATA are 1 in the tested byte
          ;--------------------------------------
                    brset     ADDR_DIR,#BYTE_DATA,*
                    brset     OFFSET,X,#BYTE_DATA,*
                    brset     OFFSET,Y,#BYTE_DATA,*

                    jmp       ADDR_EXT            ; Jump to address at *ADDR_EXT
                    jmp       OFFSET,X            ; Jump to address at *(X + OFFSET)
                    jmp       OFFSET,Y            ; Jump to address at *(Y + OFFSET)
          ;--------------------------------------
          ; Subroutine Instructions
          ;--------------------------------------
                    bsr       *                   ; Save PC on stack and branch to subroutine
                                                  ; *SP-- = PC.low
                                                  ; *SP-- = PC.high
                                                  ; Save PC on stack and jump to subroutine at address given by operand address
                                                  ; *SP-- = PC.low
                                                  ; *SP-- = PC.high
                    jsr       ADDR_DIR            ; *ADDR_DIR
                    jsr       ADDR_EXT            ; *ADDR_EXT
                    jsr       OFFSET,X            ; *(X + OFFSET)
                    jsr       OFFSET,Y            ; *(Y + OFFSET)
                    rts                           ; Return from subroutine as follows:
                                                  ; PC.high = *(++SP)
                                                  ; PC.low = *(++SP)
          ;--------------------------------------
          ; Interrupt Processing Instructions
          ;--------------------------------------
                    rti                           ; Return from interrupt as follows:
                                                  ; CCR = *(++SP)
                                                  ; B = *(++SP)
                                                  ; A = *(++SP)
                                                  ; X.high = *(++SP)
                                                  ; X.low = *(++SP)
                                                  ; Y.high = *(++SP)
                                                  ; PC.high = *(++SP)
                                                  ; PC.low = *(++SP)
                    swi                           ; Software interrupt as follows:
                                                  ; PC = PC + 1
                                                  ; *SP-- = PC.low
                                                  ; *SP-- = PC.high
                                                  ; *SP-- = Y.low
                                                  ; *SP-- = Y.high
                                                  ; *SP-- = X.low
                                                  ; *SP-- = X.high
                                                  ; *SP-- = A
                                                  ; *SP-- = B
                                                  ; *SP-- = CCR
                                                  ; I bit of CCR = 1
                                                  ; PC = SWI vector
                    wai                           ; Wait for interrupt as follows:
                                                  ; PC = PC + 1
                                                  ; *SP-- = PC.low
                                                  ; *SP-- = PC.high
                                                  ; *SP-- = Y.low
                                                  ; *SP-- = Y.high
                                                  ; *SP-- = X.low
                                                  ; *SP-- = X.high
                                                  ; *SP-- = A
                                                  ; *SP-- = B
                                                  ; *SP-- = CCR
                                                  ; Wait for an interrupt to occur which has not
                                                  ; been masked.  Then branch to the appropriate
                                                  ; interrupt vector.
          ;--------------------------------------
          ; System Instructions
          ;--------------------------------------
                    stop                          ; Halt system clocks and go to minimum power
                                                  ; standby mode if S bit in CCR is clear.
                                                  ; Otherwise execute a NOP.  Recovery from STOP is
                                                  ; by RESET, XIRQ, or IRQ signal lines.
                    !test                         ; Only used when processor is in test mode
          ;--------------------------------------
          ; Bit manipulation Instructions
          ;-------------------------------------- ; Clear all bits at address that are specified by 1's in #BYTE_DATA
                    bclr      ADDR_DIR,#BYTE_DATA
                    bclr      OFFSET,X,#BYTE_DATA
                    bclr      OFFSET,Y,#BYTE_DATA
          ;-------------------------------------- ; Set all bits at address that are specified by 1's in #BYTE_DATA
                    bset      ADDR_DIR,#BYTE_DATA
                    bset      OFFSET,X,#BYTE_DATA
                    bset      OFFSET,Y,#BYTE_DATA
          ;--------------------------------------
          ; Condition Code Register Instructions
          ;--------------------------------------
                    clc                           ; Clear carry flag
                    cli                           ; Clear interrupt mask
                    clv                           ; Clear overflow flag
                    sec                           ; Set carry flag
                    sei                           ; Set interrupt mask
                    sev                           ; Set overflow flag
