;*******************************************************************************
;* Program   : BSORT.ASM
;* Programmer: Tony Papadimitriou <tonyp@acm.org>
;* Purpose   : Simple bubble sort example
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;* Status    : Public Domain
;*******************************************************************************

#ifdef ?
  #Hint +--------------------------------------------------+
  #Hint | Available conditionals (for use with -Dx option) |
  #Hint +--------------------------------------------------+
  #Hint | ASCENDING: Sort in ascending order (default is descending)
  #Hint +---------------------------------------------------
  #Fatal Run ASM11 -Dx (where x is any of the above)
#endif

RAM                 equ       $0100               ;beginning of variable space
ROM                 equ       $F000               ;beginning of code space
STACKTOP            equ       RAM-1               ;top of stack
Vreset              equ       $FFFE               ;reset vector

;*******************************************************************************
                    #RAM      RAM
;*******************************************************************************
          ;--------------------------------------
          ; RAM is initialized during S19 load
          ; (good for simulator use only)
          ;--------------------------------------
Samples             fcb       18,205,15,62,230,27,246,155

;*******************************************************************************
                    #ROM      ROM
;*******************************************************************************

BubbleSort          proc
                    ldb       #::Samples          ;B = number of elements to check
NextPass@@          ldy       #Samples-1          ;Y -> byte array to sort (minus one)
                    aby                           ;Y -> last array element for current pass
                    ldx       #Samples            ;X -> byte array to sort
Loop@@              lda       ,x                  ;compare array[N]
                    cmpa      1,x                 ;with array[N+1]
          #ifdef ASCENDING
                    #Hint     Ascending order sort
                    blo       Cont@@              ;(sort in ascending order)
          #else
                    #Hint     Descending order sort (define ASCENDING for ascending)
                    bhi       Cont@@              ;(sort in descending order)
          #endif
          ;-------------------------------------- ;swap the two array elements
                    pshb
                    ldb       1,x                 ;B = array[N+1]
                    sta       1,x                 ;array[N+1] = A
                    stb       ,x                  ;array[N] = B
                    pulb
          ;--------------------------------------
Cont@@              inx                           ;X -> next array element
                    pshy
                    tsy
                    cpx       ,y                  ;past the current end?
                    puly
                    blo       Loop@@              ;if not, stay in same pass
                    decb                          ;one less element to check
                    bne       NextPass@@          ;start a new pass
                    rts

;*******************************************************************************

Start               proc
                    lds       #STACKTOP
                    bsr       BubbleSort
                    bra       *

;*******************************************************************************
                    #VECTORS
;*******************************************************************************
                    org       Vreset
                    dw        Start

                    end       Start
