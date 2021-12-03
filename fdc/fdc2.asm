;*******************************************************************************
;* Language  : Motorola/Freescale/NXP 68HC11 Assembly Language (aspisys.com/ASM11)
;*******************************************************************************
;Dear Tony,
;
;Here is the program "fdc2.src"; we used the smc FDC9268 control ic.
;
;Best Regards,
;
;Denis Forde
;*******************************************************************************
;Note: In memory of Denis Forde who died on Sunday, May 8, 2005.
;(https://notices.irishtimes.com/acknowledgement/forde/2371077)
;
;FORDE (Denis) (Died May 8, 2005) (Ringaskiddy and Cork) -
;On the occasion of his 60th Birthday, the family of Denis
;(retired, Forde Electronics Ltd.) would like to thank all those
;who sympathised with us on our sad loss; those who visited
;our house, attended the removal and funeral, sent floral
;tributes, cards and letters. Please accept this
;acknowledgement as a token of our appreciation.
;
;Published in the Irish Times on 20th August 2005
;*******************************************************************************
;      FDC2.SRC
;
;      HD63B03YP ASSEMBLY CODE FOR FLOPPY DISK DRIVE
;
;      COPYRIGHT (C) 1994 BY FORDE ELECTRONICS LTD.
;
;      DATE: 27 April 1994
;
;*******************************************************************************
;           HITACHI 6303Y MPU I/O PORTS
;*******************************************************************************
; PORT 2 ($0003) --------------------------------------
;   9 ICAPT     PORT2-0
;  10 OCR1      PORT2-1
;  11 SCLK      PORT2-2
;  12 RDATA     PORT2-3
;  13 TDATA     PORT2-4
;  14 OCR2      PORT2-5
;  15 PORT26    PORT2-6
;  16 PORT27    PORT2-7
; PORT 5 ($0015) --------------------------------------
;  17 IRQ1      PORT5-0 FDC 1 Interrupt
;  18 IRQ2      PORT5-1
;  19 MR        PORT5-2 MFM/FM mode
;  20 HALT      PORT5-3 MINI floppy mode
;  21 IS for P6 PORT5-4 MOTOR ON signal
;  22 OS for P6 PORT5-5 High Density disk signal
;  23 PORT56    PORT5-6 FDC RESET signal
;  24 PORT57    PORT5-7 FDC Terminal Count signal
; PORT 6 ($0017)---------------------------------------
;  25 PORT60    PORT6-0
;  26 PORT61    PORT6-1
;  27 PORT62    PORT6-2
;  28 PORT63    PORT6-3
;  29 PORT64    PORT6-4
;  30 PORT65    PORT6-5
;  31 PORT66    PORT6-6
;  32 PORT67    PORT6-7
;*******************************************************************************
;       HITACHI 6303Y MPU REGISTERS
;*******************************************************************************

;*******************************************************************************
; Macro(s) to allow use under the 68HC11 (added by <tonyp@acm.org>)
; Eventually replaced original instructions with 68HC11 equivalents:
; AIM -> BCLR w/ inverted mask
; OIM -> BSET w/ same mask
; TIM -> BRSET (if followed by BNE) or BRCLR (if followed by BEQ)
; A single TIM (not followed by any conditional branch instruction) was used to
; simply set the flags, so I left it alone, and it uses the following TIM macro,
; instead.
;*******************************************************************************
                    #ListOff
                    #Uses     exp-f1.inc
                    #ListOn

                    #Macro

;*******************************************************************************
; Macros
;*******************************************************************************

; Test Immediate

TIM                 macro     Memory,#Immediate
                    psha
                    lda       ~1~
                    anda      ~2~
                    pula
                    endm

;*******************************************************************************
ESC                 def       27                  ; ASCII ESC character
SECTOR_SIZE         def       512                 ; fixed sector size
FNAME_SIZE          def       11                  ; filename size (8+3 format)
;*******************************************************************************

P2DDR               equ       $0001               ; Port 2 Data Direction (0-7)
PORT2               equ       $0003               ; "  2 Data (0-7)
RP5CR               equ       $0014               ; RAM/Port 5 Control Reg.
P5DDR               equ       $0020               ; Port 5 Data Direction
PORT5               equ       $0015               ; "  5 Data (0-7)
P6CSR               equ       $0021               ; Port 6 CSR for Parallel H'shake
P6DDR               equ       $0016               ; "  6 Data Direction (0-7)
;PORT6              equ       $0017               ; "  6 Data (0-7)

TCSR1               equ       $0008               ; Timer 1 CSR 1
TCSR2               equ       $000F               ; "   1 CSR 2
OCR1                equ       $000B               ; 000C "   1 Output Comp. Reg.1  P21
OCR2                equ       $0019               ; 001A "   1 Output Comp. Reg.2  P25
;ICR                equ       $000D               ; 000E "   1 Input Capture Reg.  P20
;FRC                equ       $0009               ; 000A "   1 Free Runn. Up Cntr @ E~

TCSR3               equ       $001B               ; Timer 2 CSR 3
TCONR               equ       $001C               ; "   2 Time Cnst.Reg. SIOorP26
;T2CNT              equ       $001D               ; "   2 Up Counter @ E/n or P27

RMCR                equ       $0010               ; Serial Rate/Mode Control Reg.
TRCSR1              equ       $0011               ; "    Transmit/Receive CSR 1
TRCSR2              equ       $001E               ; "    Transmit/Receive CSR 2
RDR                 equ       $0012               ; "    Receive  Data Reg.
TDR                 equ       $0013               ; "    Transmit Data Reg.

MAXFAT              equ       2880                ; fat entries max (+1 for easy checking)

;*******************************************************************************
;       FLOPPY DISK DRIVE CONTROLLER ADDRESSES
;*******************************************************************************

FDCMSR              def       $4000               ; FDC Master Status Register
FDCDR               def       $4001               ; FDC Data Register

;*******************************************************************************
                    #RAM     ;$0060               ;BUFFER AND VARIABLES DEFINITIONS
;*******************************************************************************
MyVars_Begin
          ;-------------------------------------- ; FDD parameter variables
MLTITRK             rmb       1                   ; multi track access variable
MFM                 rmb       1                   ; FM or MFM mode variable
SKIP                rmb       1                   ; skip sector variable
DRIVE               rmb       1                   ; drive number variable
US0                 rmb       1                   ; unit select 0 variable
US1                 rmb       1                   ; unit select 1 variable
HEAD                rmb       1                   ; head number variable
TRACK               rmb       1                   ; track number variable
SECTOR              rmb       1                   ; sector number variable
EOTSEC              rmb       1                   ; end of track sector number variable
GAP3                rmb       1                   ; gap 3 length variable
DTL                 rmb       1                   ; data field length variable
SRT                 rmb       1                   ; step rate time
HUT                 rmb       1                   ; head unload time
HLT                 rmb       1                   ; head load time
SECBYTES                                          ; Missing variable (added by <tonyp@acm.org>).  Is it the same as SECBYTS?
SECBYTS             rmb       1                   ; bytes per sector variable
TRKSCS              rmb       1                   ; sectors per track variable
SDTRKS              rmb       1                   ; tracks per side variable
DMA                 rmb       1                   ; DMA or non DMA mode variable
FDCCMND             rmb       1                   ; FDC command number variable
          ;-------------------------------------- ; FILE HANDLE STRUCTURE
FNAME               rmb       FNAME_SIZE          ; 8.3 filename buffer
FSIZE               rmb       4                   ; file size variable
FPTR                rmb       3                   ; file pointer variable
BOF                 rmb       2                   ; beginning of file sector variable
EOF                 rmb       2                   ; end of file sector variable
OLDEOF              rmb       2                   ; old end of file sector variable
FDIRNM              rmb       1                   ; file directory entry number variable
CENTRY              rmb       2                   ; current fat entry variable
NENTRY              rmb       2                   ; next fat entry variable
CENUM               rmb       2                   ; current fat entry number variable
LFFAT               rmb       2                   ; last free fat location variable
NFBS                rmb       2                   ; number of free bytes left in sector var
CTRLCD              rmb       1                   ; control code file access mode flag
FHANDLE             rmb       1                   ; File handle code var
          ;-------------------------------------- ; THE OTHERS
APNDF               rmb       1
ABSSEC              rmb       2                   ; absolute sector variable
LASTHD              rmb       1                   ; last head accessed variable
ERRNUM              rmb       1                   ; errnum variable

PAGENUM             rmb       2                   ; current 5 sector data block off disk
SECNUM              rmb       2                   ; sector number within pagenum
SECOFF              rmb       2                   ; byte offset within secnum
SECAREA             rmb       1                   ; flag to denote which part of the disk is being accessed
PTRADR              rmb       3                   ; File pointer address variable
BADDR               rmb       2                   ; buffer address within in DATA buffer
BSIZE               rmb       2                   ; handy block size variable
BSTEMP              rmb       2                   ; temporary bsize variable
DBLOCK              rmb       1                   ; data block number variable
PP                  rmb       2                   ; partial product for math
TEMPA               rmb       1                   ; some handy temp regs
TEMPB               rmb       1                   ; keep together with TEMPA
TEMPD               rmb       2
TEMPX               rmb       2
PTRX                rmb       2                   ; buffer pointer variable
ENDX                rmb       2
BUFFX               rmb       2
SRCX                rmb       2
DSTX                rmb       2
MVCOUNT             rmb       2
INTCNT              rmb       1                   ; interrupt counter for formating
FATNUM              rmb       2                   ; FAT number variable
FDRNUM              rmb       2                   ; File Dir. number variable
NXTFAT              rmb       2                   ; next fat number variable
NXTFDR              rmb       1                   ; next file dir variable
NXTFNM              rmb       1                   ; next filename variable
EXIST               rmb       1                   ; file existence flag
S1ERR               rmb       1                   ; serial comms error flag
WFLAG               rmb       1                   ; #'W' to write
COUNTER             rmb       2
INITF               rmb       1                   ; flag to denote disk init status, defualt = 0,
DSKCHNG             rmb       1                   ; gets set if the disk is changed or missing
DSKPRS              rmb       1                   ; gets set if a disk is in the drive
DISKIN              rmb       1

MyVars_End
;*******************************************************************************
                    #XRAM                         ; THE BUFFERS
;*******************************************************************************
MyXVars_Begin

BUFFER              rmb       513                 ; 513 byte sector content buffer
FILEDR              rmb       32                  ; 32 byte file dir content buffer
SBUFF               rmb       12                  ; serial comms buffer
PRMBUF              rmb       9                   ; parameter buffer
RESBUF              rmb       7                   ; results buffer
FRMTBUF             rmb       4                   ; format params buffer
SECBUF              rmb       20                  ; sector number buffer
BPBBUF              rmb       0512                ; BPB buffer
BPBEND              rmb       1
FATBUF              rmb       4608                ; FAT buffer
FATEND              rmb       1
FDIRBUF             rmb       7168                ; File Directory buffer
FDIREND             rmb       1
DATA                rmb       2560                ; the data buffer - 5 sectors worth
TOPRAM              rmb       1                   ; last byte of ram used

MyXVars_End

;*******************************************************************************
;       FBUG3Y MACROS
;*******************************************************************************

COLD                equ       $F000
WARM                equ       $F003
GETCH               equ       $F006
PUTCH               equ       $F009
OUTHL               equ       $F230
OUTHR               equ       $F234

;*******************************************************************************
                    #ROM     ;$8000
;*******************************************************************************

;*******************************************************************************
;       INITIALIZE ALL REGISTERS
;*******************************************************************************

STOP                proc
                    bsr       STOP0
                    cli                           ; Enable all IRQs
                    rts

;*******************************************************************************

STOP0               proc
                    sei                           ; first Disable all IRQs
                    bsr       INITP2              ; Initialize Port 2
                    bsr       INITP5              ; "      Port 5 & RAM
                    bsr       INITP6              ; "      Port 6  (//H'shake)
                    bsr       INITT1              ; "      Timer 1 (OCI,ICI)
                    jsr       INITS               ; "      serial port
                    jsr       RSTFDC              ; reset disk controller
                    lda       DBUGF
                    anda      #%00000001
                    bne       Done@@
                    jmp       CLRRAM
Done@@              equ       :AnRTS

                    fcc       'Copyright (c) 1994 Forde Electronics Ltd.'

;*******************************************************************************
;       INITIALIZE PORT 2
;*******************************************************************************
;       PORT 2 DDR ($0001) & DATA REG. ($0003)
; ICAPT 0 = 1  OUT-
; OCR1  1 = 1  OUT - Write Serial RTS
; SCLK  2 = 0  IN  - Read     "   CTS or Sync CLK
; RDATA 3 = 0  IN  - Read     "   Rx
; TDATA 4 = 1  OUT - Write    "   Tx
; OCR2  5 = 1  OUT -
; T2OUT 6 = 1  OUT -
; T2CLK 7 = 1  OUT -
;*******************************************************************************

INITP2              proc
                    lda       #%11110011
                    sta       P2DDR               ; Port 2 DDR
                    lda       #%11110011
                    sta       PORT2               ; Port 2 Data Reg.
                    rts

;*******************************************************************************
;       INITIALIZE PORT 5
;*******************************************************************************
;       RAM / PORT5 CNTL. REG. ($0014)
; IRQ1  0 = 0  Disable IRQ1 on P50 (FOR DEVELOPMENT ONLY
; IRQ2  1 = 0  Disable IRQ2 on P51
; MRE   2 = 0  Disable Memory Ready
; HLTE  3 = 0  Disable HALT on P53
; AMRE  4 = 0  Disable Auto Memory Ready
; STBYF 5 = 1  No STANBY Mode
; RAME  6 = 1  Enable On-Chip RAM
; STBYP 7 = 1  Flag for Vcc Retention
;
;       PORT5 DDR ($0020) & DATA REG. ($0015)
; IRQ1  0 = 0  IN  - FDC 1 Interrupt
; IRQ2  1 = 1  OUT -
; MR    2 = 0  IN  - MFM/FM mode
; HALT  3 = 1  OUT - MINI floppy mode
; IS    4 = 1  OUT - MOTOR ON signal
; OS    5 = 0  IN  - DISKCHANGE SIGNAL
; PORT5 6 = 1  OUT - FDC RESET signal
;   "   7 = 1  OUT - FDC Terminal Count signal
;*******************************************************************************

INITP5              proc
                    lda       #%11100000
                    sta       RP5CR               ; RAM / Port5 Cntl. Reg.
                    lda       #%11011010
                    sta       P5DDR               ; Port 5 DDR
                    lda       #%11010010
                    sta       PORT5               ; Port 5 Data Reg.
                    rts

;*******************************************************************************
; ENABLE IRQ1

IRQON               proc
                    lda       #%11100001
                    sta       RP5CR               ; RAM / Port5 Cntl. Reg.
                    rts

;*******************************************************************************
; DISABLE IRQ1

IRQOFF              proc
                    lda       #%11100000
                    sta       RP5CR               ; RAM / Port5 Cntl. Reg.
                    rts

;*******************************************************************************
;       INITIALIZE PORT 6
;*******************************************************************************
;
;       PORT 6 CNTL./STATUS REG. (Parallel H'shk) $0021
; ....  0 = 1  Not Used
; ....  1 = 1   "   "
; ....  2 = 1   "   "
; IS L  3 = 0  Disable IS LATCH on P54
; OSS   4 = 0  Output Strobe Select on P55 (0=Reading)
; OSE   5 = 0  Disable OS on P55
; ISIRQ 6 = 0  Disable IRQ on IS
; ISFLG 7 = X  Flag: IRQ1 of IS
;
;       PORT 6 DDR ($0016) & DATA REG. ($0017)
; PORT6 0 = 1  OUT -
;   "   1 = 1  OUT -
;   "   2 = 1  OUT -
;   "   3 = 1  OUT -
;   "   4 = 1  OUT -
;   "   5 = 1  OUT -
;   "   6 = 1  OUT -
;   "   7 = 1  OUT -
;*******************************************************************************

INITP6              proc
                    lda       #%00000111
                    sta       P6CSR               ; Port 6 Control/Status Reg.
                    lda       #%11111111
                    sta       P6DDR               ; Port 6 DDR
                    lda       #%11111111
                    rts

;*******************************************************************************
;       INITIALIZE TIMER 1
;*******************************************************************************
;
;       TIMER 1 CNTL./STATUS REGISTER-1 ($0008)
; OLVL1 0 = X  Output Level on P21 for OCR1
; IEDG  1 = X  Input Edge on P20 for ICR
; ETOI  2 = 1  Enable  Timer Overflow Int.
; EOCI1 3 = 1  Enable  Output Compare Int.1
; EICI  4 = 0  Disable Input Capture Int.
; TOF   5 = X  Flag: Timer Overflow
; OCF1  6 = X  Flag: Output Compare FRC = OCR1     #####
; ICF   7 = X  Flag: Input Capture on P20          #####
;
;       TIMER 1 CNTL./STATUS REGISTER-2 ($000F)
; OE1   0 = 0  Disable OLVL1 to P21
; OE2   1 = 0  Disable OLVL2 to P25
; OLVL2 2 = 0  Output Level on P25 for OCR2
; EOCI2 3 = 0  Disable Output Compare Int.2 (0.1 S Cntr)
; ....  4 = 0  not used
; OCF2  5 = 0  Flag: Output Compare FRC = OCR2
; OCF1  6 = 0  Flag: Output Compare FRC = OCR1     #####
; ICF   7 = 0  Flag: Input Capture on P20          #####
;*******************************************************************************

INITT1              proc
                    lda       #%00000000
                    sta       TCSR1               ; Timer-1 CSR-1 ($0011)
                    lda       #%00000000
                    sta       TCSR2               ; Timer-1 CSR-2 ($001E)
                    clrx
                    stx       OCR1                ; Timer-1 OCR-1 ($000B)
                    stx       OCR2                ; Timer-1 OCR-2 ($0019) 100 mS
                    rts

;*******************************************************************************
;       INITIALIZE SERIAL-1 (MPU) - AS PER MONEYCHANGER
;*******************************************************************************
;
;       TIMER 2 CNTL / STATUS REG.-3 ($001B)
; CKS0  0 = 0  Input Clock Select : 00 = E~
; CKS1  1 = 0    "     "     "    : 01 = E~/128
; TOS0  2 = 0  Timer 2 Output Select
; TOS1  3 = 0    "   "   "      " 00 = Output Inhibited
; T2E   4 = 1  Timer 2 Enable, allow T2CNT to count
; ....  5 = 1  not used
; ECMI  6 = 0  Disable Counter Match Interrupt (CMI)
; CMF   7 = X  Flag: Counter Match
;              Set by T2CNT = TCONR. to Clear write 0.
;
;       RATE / MODE CONTROL REG. ($0010)
; SS0   0 = 0  Baud Rate from Timer 1 /N
; SS1   1 = 0    "    "    "    "   " /N
; CC0   2 = 1  Clock Control/Format Select
; CC1   3 = 0    101: 7 Bit Async, Internal Clk.
; CC2   4 = 1         Clock on P22 not used
; SS2   5 = 1  Baud Rate from Timer 2 /N
; ....  6 = 1  not used
; ....  7 = 1  not used
;
;       TRANSMIT / RCV. CNTL./STATUS REG.1 ($0011)
; WU    0 = 0  Disable Wake Up
; TE    1 = 1  Enable  Tx on P24
; TIE   2 = 0  Disable Transmit Interrupt (toggled ON)
; RE    3 = 0  Disable Rx on P23           ==========
; RIE   4 = 1  Enable  Receive  Interrupt
; TDRE  5 = X  Flag: Tx Data Reg. Empty           #####
; ORFE  6 = X  Flag: Overrun/Framing Error        #####
; RDRF  7 = X  Flag: Rx Data Reg. Full            #####
;
;       TRANSMIT / RCV. CNTL./STATUS REG.2 ($001E)
; SBL   0 = 0  One Stop Bit
; EOP   1 = 0  Even Parity
; PEN   2 = 1  Enable Parity
; ....  3 = 1  not used
; PER   4 = X  Flag: Parity Error
; TDRE  5 = X  Flag: Tx Data Reg. Empty           #####
; ORFE  6 = X  Flag: Overrun/Framing Error        #####
; RDRF  7 = X  Flag: Rx Data Reg. Full            #####
;*******************************************************************************
; Count = 2MHz/32*BAUD     - 1
;       = (10^6 / 16*BAUD) - 1
;       = (62,500/BAUD)    - 1

BAUD1T              fcb        3,207              ; 300 Baud Rate
                    fcb        6,103              ; 600 "   "
                    fcb       12,51               ; 1200 "   "
                    fcb       24,25               ; 2400 "   "
                    fcb       48,12               ; 4800 "   "
                    fcb       96,5                ; 9600 "   "

;*******************************************************************************
; Initialize Serial-1 Port (A = Baud Rate)

INTS1P              proc
                    psha
                    lda       TRCSR2              ; Transmit/Receive CSR-2
                    lda       RDR                 ; clear old Receive flags
                    clra                          ; and Disable Serial I/O
                    sta       TRCSR1              ; Transmit/Receive CSR-1

                    lda       #%11110100          ; 7 data bits, use Timer 2
                    sta       RMCR                ; Rate Mode Control Register

                    lda       #%00110000          ; E~ as clock, no CMI Interrupt
                    sta       TCSR3               ; Timer 2 CSR-3

                    pula                          ; A = Baud Rate
                    ldx       #BAUD1T             ; Baud Rate Table
_1@@                cmpa      ,x
                    beq       _2@@
                    inx:2
                    cpx       #BAUD1T+12
                    bne       _1@@
                    ldx       #BAUD1T+4           ; default is 1200 baud
_2@@                ldb       1,x
                    stb       TCONR               ; Timer 2 Time Const. Reg.

                    lda       #%00001100          ; 1 stop bit, Even parity
                    sta       TRCSR2              ; Transmit/Receive CSR-2

                    lda       #%00010010          ; Enable Tx,RIE; Rx,TIE OFF
                    sta       TRCSR1              ; Transmit/Receive CSR-1
                    rts                           ; Bit 2 = TIE, 1 = TE

;*******************************************************************************
;       Subroutine to INITIALIZE SERIAL-1 (MPU)
;*******************************************************************************

INITS1              proc
                    clra                          ; first disable SIO
                    sta       TRCSR1              ; Transmit / Receive CSR-1
                    sta       S1ERR               ; no Error
                    lda       #2
                    bra       INTS1P              ; Initialize Serial-1 Port

;*******************************************************************************
;       INITIALIZE SERIAL TRANSMIT/RECEIVE
;*******************************************************************************
;  AS PER LSI CARD
;       TIMER 2 CNTL / STATUS REGISTER-3 ($001B)
; CKS0  0 = 0  Input Clock Select: <00=E~>,   01=E/8
; CKS1  1 = 0    "     "     "   : 01=E~/128, 11=EXT
; TOS0  2 = 0  Timer 2 Output Select
; TOS1  3 = 0    "   "   "      " 00 = Output Inhibited
; T2E   4 = 1  Timer 2 Enable, allow T2CNT to count
; ....  5 = 1  not used
; ECMI  6 = 0  Disable Counter Match Interrupt (CMI)
; CMF   7 = X  Flag: Counter Match
;              Set by T2CNT = TCONR, to Clear write 0.
;
;       RATE / MODE CONTROL REGISTER ($0010)
; SS0   0 = 0  Baud Rate from Timer-1 /N (n = 16, <128>
; SS1   1 = 0    "    "    "     "    /N (1024, 4096)
; CC0   2 = 1  Clock Control & Format Select
; CC1   3 = 0    001: 8 Bit Async, Internal Clk.
; CC2   4 = 0         Clock on P22 not used
; SS2   5 = 0  Baud Rate from Timer-1
; ....  6 = 1  not used
; ....  7 = 1  not used
;
;       TRANSMIT / RCV. CNTL./STATUS REGISTER-1 ($0011)
; WU    0 = 0  Disable Wake Up
; TE    1 = 1  Enable  Tx on P24
; TIE   2 = 0  Disable Transmit Interrupt
; RE    3 = 1  Enable  Rx on P23
; RIE   4 = 0  Disable Receive  Interrupt
; TDRE  5 = X  Flag: Tx Data Reg. Empty           #####
; ORFE  6 = X  Flag: Overrun/Framing Error        #####
; RDRF  7 = X  Flag: Rx Data Reg. Full            #####
;
;       TRANSMIT / RCV. CNTL./STATUS REGISTER-2 ($001E)
; SBL   0 = 0  One Stop Bit
; EOP   1 = 0  Even Parity (not used)
; PEN   2 = 0  Disable Parity
; ....  3 = 1  not used
; PER   4 = X  Flag: Parity Error (not used)
; TDRE  5 = X  Flag: Tx Data Reg. Empty           #####
; ORFE  6 = X  Flag: Overrun/Framing Error        #####
; RDRF  7 = X  Flag: Rx Data Reg. Full            #####
;
; *-----------------------------------------------------
;                   XTAL:       8 MHz
;  SS2  SS1  SS0     E:         2 MHz
; ------------------------------------------------------
;   0    0    0     E/16         8 uS  (12.5 K byte/sec)
;   0    0    1     E/128       64 uS  (1.56 K byte /sec)
;   0    1    0     E/1024     512 uS
;   0    1    1     E/4096    2048 uS
;*******************************************************************************

INITS               proc
                    lda       TRCSR2              ; Transmit/Receive CSR-2
                    lda       RDR                 ; clear old Receive flags
                    clra                          ; and Disable Serial I/O
                    sta       TRCSR1              ; Transmit/Receive CSR-1
                    clr       S1ERR               ; no Error
                    lda       #%11000100          ; 8 data bits, use Timer-1
                    sta       RMCR                ; Rate & Mode Control Register
                    lda       #%00110000          ; E~ as clock, no CMI Interrupt
                    sta       TCSR3               ; Timer-2 CSR-3
                    lda       #%00001000          ; 1 stop bit, No parity
                    sta       TRCSR2              ; Transmit/Receive CSR-2
                    lda       #%00001010          ; Enable Tx,Rx; RIE & TIE = OFF
                    sta       TRCSR1              ; Transmit/Receive CSR-1
                    rts

;*******************************************************************************
;       FDC CONTROL FUNCTION ROUTINES
;*******************************************************************************

;*******************************************************************************
;       ROUTINE TO SEE IF THE FDC IS READY FOR COMMANDS
;*******************************************************************************
;  PARAMETERS : -----
;
;  MODIFIES   : A
;
;  CALLS      : -----
;
;  RETURNS    : returns when FDC is ready
;*******************************************************************************

CMNDRDY             proc
                    lda       FDCMSR              ; read main status register
                    anda      #%11000000          ; bit 6 = data in(0)/out(1) of fdc
                    cmpa      #%10000000          ; bit 7 = rqst for master
                    bne       CMNDRDY
                    rts

;*******************************************************************************
;       ROUTINE TO SEE IF THE FDC IS READY WITH RESULTS
;*******************************************************************************
;  PARAMETERS : -----
;
;  MODIFIES   : A
;
;  CALLS      : -----
;
;  RETURNS    : returns when FDC is ready
;*******************************************************************************

RESRDY              proc
                    lda       FDCMSR              ; read main status register
                    anda      #%11000000          ; bit 6 = data in(0)/out(1) of fdc
                    cmpa      #%11000000          ; bit 7 = rqst for master
                    bne       RESRDY
                    rts

;*******************************************************************************
;       ROUTINE TO SEND THE PARAMETERS TO THE FDC
;*******************************************************************************
;  PARAMETERS : PRMBUF, B (num params)
;
;  MODIFIES   : A, B
;
;  CALLS      : CMNDRDY, WAIT12
;
;  RETURNS    : FDCCMND
;*******************************************************************************

SNDPRMS             proc
                    pshx
                    ldx       #PRMBUF             ; extract command number
                    lda       ,x
                    anda      #%00011111
                    sta       FDCCMND             ; save the command number
Loop@@              bsr       CMNDRDY             ; wait till FDC is ready
                    lda       ,x
                    sta       FDCDR               ; give next param
                    jsr       WAIT12              ; 12 uSec delay neccessary
                    inx
                    decb
                    bne       Loop@@              ; go again
                    pulx
                    rts

;*******************************************************************************
;       ROUTINE TO GET THE RESULTS FROM THE FDC
;*******************************************************************************
;  PARAMETERS : B (num results)
;
;  MODIFIES   : A, B, X
;
;  CALLS      : RESRDY, WAIT12
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

GETRSLT             proc
                    ldx       #RESBUF
Loop@@              bsr       RESRDY              ; wait till FDC is ready
                    lda       FDCDR
                    sta       ,x                  ; get next result
                    jsr       WAIT12              ; 12 uSec delay neccessary
                    inx
                    decb
                    bne       Loop@@              ; go again
                    rts

;*******************************************************************************
;       ROUTINE TO SETUP THE PARAMETERS FOR A FUNCTION
;*******************************************************************************
;  PARAMETERS : MLTITRK, MFM, SKIP, USO, US1, HEAD,
;             TRACK, SECTOR, SECBYTS, EOTSEC, GAP3, DTL
;
;  MODIFIES   : B, X
;
;  CALLS      : ------
;
;  RETURNS    : PRMBUF is setup with the above values
;*******************************************************************************

SETPRMS             proc
                    ldx       #PRMBUF             ; set up parameter buffer
                    clrb
                    orb       MLTITRK             ; multi track access
                    orb       MFM                 ; FM or MFM
                    orb       SKIP                ; skip sector
                    stb       ,x                  ; 1st parameter done
                    clrb
                    orb       US0                 ; which unit
                    orb       US1
                    orb       HEAD                ; which head
                    stb       1,x                 ; 2nd parameter done
                    ldb       TRACK               ; which track
                    stb       2,x                 ; 3rd parameter done
                    ldb       HEAD                ; which head
                    lsrb:2
                    stb       3,x                 ; 4th parameter done
                    ldb       SECTOR              ; which sector
                    stb       4,x                 ; 5th paramater done
                    ldb       SECBYTS             ; how many bytes per sector
                    stb       5,x                 ; 6th parameter done
                    ldb       EOTSEC              ; what's the last sector on the track
                    stb       6,x                 ; 7th parameter done
                    ldb       GAP3                ; gap 3 length
                    stb       7,x                 ; 8th parameter done
                    ldb       DTL                 ; data field length
                    stb       8,x                 ; 9th parameter done
                    rts

;*******************************************************************************
;  ROUTINE TO SET THE CORRECT INTERRUPT JUMP ADDRESS
;*******************************************************************************
;  PARAMETERS : FDCCMND IN PRMBUF
;
;  MODIFIES   : --------
;
;  CALLS      : -------
;
;  RETURNS    : X = address of interrupt service routine
;*******************************************************************************

SETJMP              proc
                    lda       PRMBUF              ; extract command number
                    anda      #%00011111

                    ldx       #IRQRD
                    cbeqa     #%00000110,:AnRTS   ; READ DATA COMMAND

                    ldx       #IRQWR
                    cbeqa     #%00000101,:AnRTS   ; WRITE DATA COMMAND

                    ldx       #IRQF0
                    cbeqa     #%00001101,:AnRTS   ; FORMAT TRACK COMMAND

                    ldx       #IRQSNS             ; unknown interrupt source
                    rts

;*******************************************************************************
;       ROUTINE TO EXECUTE A COMMAND ON THE FDC
;*******************************************************************************
;  PARAMETERS : BSIZE, PRMBUF, A (num results), B (num
;               PTRX, ENDX.                      params)
;  MODIFIES   : A, B, X, ERRNUM
;
;  CALLS      : PCMND, SETJMP, SNDPRMS, IRQSERVICE,
;               GETRSLTS, ERROR
;  RETURNS    : results in RESBUF
;*******************************************************************************

DOCMND              proc
                    psha                          ; save num results
                    pshb                          ; save num params

                    jsr       PCMND               ; print command name if debugging

                    bsr       SETJMP
                    pulb
                    jsr       SNDPRMS             ; send the parameters

                    bclr      PORT5,#%10000000    ; ensure terminal count is inactive
                    jsr       ,x                  ; jump to correct service depending on command

                    pulb                          ; retrieve num results
                    pshb                          ; save for testing
                    bsr       GETRSLTS            ; get the results

                    pulb                          ; retrieve num results
                    clr       ERRNUM              ; => error value for FDC function
                    jmp       ERROR

;*******************************************************************************
;       LOW LEVEL ROUTINE TO SEEK A TRACK
;*******************************************************************************
;  PARAMETERS : US0, US1, HEAD, TRACK
;
;  MODIFIES   : B, X, A
;
;  CALLS      : PCMND, SNDPRMS, IRQSNS
;
;  RETURNS    : NONE, BUT AN INTERRUPT IS GENERATED
;*******************************************************************************

SEEK                proc
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00001111
                    stb       ,x                  ; 1st parameter done
                    clrb
                    orb       US0                 ; which unit
                    orb       US1
                    orb       HEAD                ; which head
                    stb       1,x                 ; 2nd parameter done
                    ldb       TRACK               ; which track
                    stb       2,x                 ; 3rd parameter done

                    jsr       PCMND               ; print command name if debugging

                    ldb       #3
                    jsr       SNDPRMS             ; send the parameters
                    jmp       IRQSNS              ; get result

;*******************************************************************************
;       LOW LEVEL ROUTINE TO RECALLIBRATE THE FDD
;*******************************************************************************
;  PARAMETERS : US0, US1
;
;  MODIFIES   : B
;
;  CALLS      : PCMND, SNDPRMS, IRQSNS
;
;  RETURNS    : NONE, BUT AN INTERRUPT IS GENERATED
;*******************************************************************************

RECAL               proc
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00000111
                    stb       ,x                  ; 1st parameter done
                    clrb
                    orb       US0                 ; which unit
                    orb       US1
                    stb       1,x                 ; 2nd parameter done

                    jsr       PCMND               ; print command name if debugging

                    ldb       #2
                    jsr       SNDPRMS             ; send the parameters
                    jmp       IRQSNS              ; get result

;*******************************************************************************
;       LOW LEVEL ROUTINE TO SENSE THE INTERRUPT STATUS
;*******************************************************************************
;  PARAMETERS : ------
;
;  MODIFIES   : B, X, A
;
;  CALLS      : PCMND, SNDPRMS, GETRSLT, ERROR
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

SNSINT              proc
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00001000
                    stb       ,x

                    jsr       PCMND               ; print command name if debugging

                    ldb       #1
                    jsr       SNDPRMS             ; send the parameters
                    ldb       #2
                    jsr       GETRSLT             ; get the results

                    clr       ERRNUM
                    jmp       ERROR               ; => error value for FDC function

;*******************************************************************************
;       LOW LEVEL ROUTINE TO SPECIFY AN FDD PARAMETER
;*******************************************************************************
;  PARAMETERS : SRT, HUT, HLT, DMA
;
;  MODIFIES   : B, X, A
;
;  CALLS      : PCMND, SNDPRMS
;
;  RETURNS    : NONE
;*******************************************************************************

SPECIFY             proc
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00000011
                    stb       ,x                  ; 1st parameter done
                    clrb
                    orb       SRT                 ; step rate time
                    orb       HUT                 ; head unload time
                    stb       1,x                 ; 2nd parameter done
                    clrb
                    orb       DMA                 ; dma mode or not
                    orb       HLT                 ; head load time
                    stb       2,x                 ; 3rd parameter done

                    jsr       PCMND               ; print command name if debugging

                    ldb       #3
                    jmp       SNDPRMS             ; send the parameters

;*******************************************************************************
;       LOW LEVEL ROUTINE TO SENSE THE DRIVE STATUS
;*******************************************************************************
;  PARAMETERS : HEAD, US0, US1
;
;  MODIFIES   : B, A, X
;
;  CALLS      : PCMND, SNDPRMS, GETRSLTS, ERROR
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

SNSDRV              proc
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00000100
                    stb       ,x
                    clrb
                    orb       US0                 ; which unit
                    orb       US1
                    orb       HEAD                ; which head
                    stb       1,x                 ; 2nd parameter done

                    jsr       PCMND               ; print command name if debugging

                    ldb       #2
                    jsr       SNDPRMS             ; send the parameters
                    ldb       #1
                    jsr       GETRSLTS            ; get the results

                    clr       ERRNUM              ; => error value for FDC function
                    jmp       ERROR

;*******************************************************************************
;       LOW LEVEL ROUTINE TO READ FROM A DISK
;*******************************************************************************
;  PARAMETERS : MT, MF, SK, HD, US0, US1, C, H, R, N,
;               EOT, GPL, DTL, PTRX, ENDX
;  MODIFIES   : B, X, A
;
;  CALLS      : SETPRMS, DOCMND
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

RDLOW               proc
                    jsr       SETPRMS             ; set up the parameter buffer
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00000110
                    orb       ,x
                    stb       ,x
                    lda       #7                  ; 7 results
                    ldb       #9                  ; 9 params
                    jmp       DOCMND              ; read the data

;*******************************************************************************
;       LOW LEVEL ROUTINE TO READ DELETED DATA FROM A DISK
;*******************************************************************************
;  PARAMETERS : MT, MF, SK, HD, US0, US1, C, H, R, N,
;               EOT, GPL, DTL, PTRX, ENDX
;  MODIFIES   : B, X, A
;
;  CALLS      : SETPRMS,DOCMND
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

RDDLTD              proc
                    jsr       SETPRMS             ; set up the parameter buffer
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00001100
                    orb       ,x
                    stb       ,x
                    lda       #7                  ; 7 results
                    ldb       #9                  ; 9 params
                    jmp       DOCMND

;*******************************************************************************
;       LOW LEVEL ROUTINE TO READ A TRACK ON A DISK
;*******************************************************************************
;  PARAMETERS : MF, HD, US0, US1, C, H, R, N, EOT, GPL
;               DTL, PTRX, ENDX
;  MODIFIES   : B, X, A
;
;  CALLS      : SETPRMS, DOCMND
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

RTRACK              proc
                    clr       MLTITRK             ; mltitrk funtion not allowed
                    clr       SKIP                ; skip function not allowed
                    jsr       SETPRMS             ; set up the parameter buffer
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00000010
                    orb       ,x
                    stb       ,x
                    lda       #7                  ; 7 results
                    ldb       #9                  ; 9 params
                    jmp       DOCMND

;*******************************************************************************
;       LOW LEVEL ROUTINE TO READ A DISK ID
;*******************************************************************************
;  PARAMETERS : HEAD, US0, US1
;
;  MODIFIES   : B, A, X
;
;  CALLS      : PCMND, MTRON, SNDPRMS, GETRSLT, MTROFF,
;               ERROR
;  RETURNS    : results in RESBUF
;*******************************************************************************

RDID                proc
                    clr       MLTITRK             ; mltitrk funtion not allowed
                    clr       SKIP                ; skip function not allowed
                    ldb       #%01000000
                    stb       MFM                 ; FM or MFM
                    jsr       SETPRMS             ; set up the parameter buffer
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00001010
                    orb       ,x
                    stb       ,x

                    jsr       PCMND               ; print command name if debugging

                    jsr       MTRON
                    ldb       #2
                    jsr       SNDPRMS             ; send the parameters
                    ldb       #7
                    jsr       GETRSLTS            ; get the results
                    jsr       MTROFF

                    clr       ERRNUM              ; => error value for FDC function
                    jmp       ERROR

;*******************************************************************************
;       LOW LEVEL ROUTINE TO WRITE TO A DISK
;*******************************************************************************
;  PARAMETERS : MT, MF, HD, US0, US1, C, H, R, N ,EOT,
;               GPL, DTL, PTRX, ENDX
;  MODIFIES   : B, A, X
;
;  CALLS      : SETPRMS, DOCMND
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

WRLOW               proc
                    clr       SKIP                ; skip function not allowed
                    jsr       SETPRMS             ; set up the parameter buffer
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00000101
                    orb       ,x
                    stb       ,x
                    lda       #7                  ; 7 results
                    ldb       #9                  ; 9 params
                    jmp       DOCMND

;*******************************************************************************
;       LOW LEVEL ROUTINE TO WRITE DELETED DATA
;                 TO A DISK
;*******************************************************************************
;  PARAMETERS : MT, MF, HD, US0, US1, C, H, R, N ,EOT,
;               GPL, DTL, PTRX, ENDX
;  MODIFIES   : B, A, X
;
;  CALLS      : SETPRMS, DOCMND
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

WRDLTD              proc
                    clr       SKIP                ; skip function not allowed
                    jsr       SETPRMS             ; set up the parameter buffer
                    ldx       #PRMBUF             ; set the correct command
                    ldb       #%00001001
                    orb       ,x
                    stb       ,x
                    lda       #7                  ; 7 results
                    ldb       #9                  ; 9 params
                    jmp       DOCMND

;*******************************************************************************
;       LOW LEVEL ROUTINE TO FORMAT A TRACK ON A DISK
;*******************************************************************************
;  PARAMETERS : MF, HD, US0, US1, N, SC, GPL
;
;  MODIFIES   : B, A, X
;
;  CALLS      : DOCMND
;
;  RETURNS    : results in RESBUF
;*******************************************************************************

FRMTTRK             proc
                    ldb       HEAD
                    pshb
                    aslb:2
                    stb       HEAD
                    clr       MLTITRK             ; mltitrk funtion not allowed
                    clr       SKIP                ; skip function not allowed
                    ldx       #PRMBUF             ; set up parameter buffer
                    ldb       #%01000000
                    stb       MFM                 ; FM or MFM
                    orb       #%00001101
                    stb       ,x                  ; 1st parameter done
                    clrb
                    orb       US0                 ; which unit
                    orb       US1
                    orb       HEAD                ; which head
                    stb       1,x                 ; 2nd parameter done
                    ldb       SECBYTS             ; how many bytes per sector
                    stb       2,x                 ; 3rd parameter done
                    ldb       TRKSCS              ; how many sectors per track
                    stb       3,x                 ; 4th parameter done
                    ldb       GAP3                ; gap 3 length
                    stb       4,x                 ; 5th parameter done
                    ldb       #$F6                ; initial data pattern fill byte
                    stb       5,x                 ; 6 parameter done

                    pulb
                    stb       HEAD
                    lda       #7                  ; 7 results
                    ldb       #6                  ; 6 params
                    jmp       DOCMND

;*******************************************************************************
;       IRQ SERVICE ROUTINES
;*******************************************************************************

QIRQ                proc
                    tim       PORT5,#01           ; check for IRQ
                    rts

;*******************************************************************************
;         SERVICE READ DATA INTERRUPT
;*******************************************************************************

IRQRD               proc
                    ldx       PTRX
                    brset     PORT5,#01,*         ; wait for interrupt (it goes LO)
                    lda       FDCMSR              ; check first interrupt for end of execution
                    anda      #%00100000
                    bne       _1@@
                    lda       #$FF                ; read next byte from data bus
                    sta       DBUGF               ; save it in the next location
                    rts                           ; point to the next location

Loop@@              brset     PORT5,#01,*         ; wait for interrupt (it goes LO)
_1@@                lda       FDCDR               ; read next byte from data bus
                    sta       ,x                  ; save it in the next location
                    inx                           ; point to the next location
                    cpx       ENDX
                    bne       Loop@@              ; are all bytes read yet ?

                    bset      PORT5,#%10000000    ; issue a terminal count
                    stx       PTRX
                    rts

;*******************************************************************************
;       SERVICE WRITE DATA INTERRUPT
;*******************************************************************************

IRQWR               proc
                    ldx       PTRX
                    brset     PORT5,#01,*         ; wait for interrupt
                    lda       FDCMSR              ; check first interrupt for end of execution
                    anda      #%00100000
                    bne       _1@@
                    lda       #$FF                ; get next byte from buffer
                    sta       DBUGF               ; write it to disk
                    rts                           ; point to the next location

Loop@@              brset     PORT5,#01,*         ; wait for interrupt
_1@@                lda       ,x                  ; get next byte from buffer
                    sta       FDCDR               ; write it to disk
                    inx                           ; point to the next location
                    cpx       ENDX
                    bne       Loop@@              ; are all bytes written yet ?

                    bset      PORT5,#%10000000    ; issue a terminal count
                    stx       PTRX
                    rts

;*******************************************************************************
;          SERVICE FORMAT A TRACK INTERRUPT
;*******************************************************************************

IRQF0               proc
                    lda       SECTOR
                    ldb       SECBYTES
                    xgdx
                    lda       TRACK
                    ldb       HEAD

                    brset     PORT5,#01,*         ; wait for TRACK interrupt
                    sta       FDCDR               ; send current track number to fdc
                    brclr     PORT5,#01,*         ; wait for interrupt to go away

                    brset     PORT5,#01,*         ; wait for HEAD interrupt
                    stb       FDCDR               ; send current head number to fdc
                    brclr     PORT5,#01,*         ; wait for interrupt to go away

                    xgdx
                    brset     PORT5,#01,*         ; wait for SECTOR interrupt
                    sta       FDCDR               ; send current sector number to fdc
                    brclr     PORT5,#01,*         ; wait for interrupt to go away

                    brset     PORT5,#01,*         ; wait for BYTES PER SECTOR interrupt
                    stb       FDCDR               ; send current bytes per sector number to fdc

                    inc       SECTOR              ; inc current sector value
                    lda       TRKSCS
                    inca
                    cmpa      SECTOR              ; check if all sectors are formated
                    bne       IRQF0

                    bset      PORT5,#%10000000    ; issue a terminal count
                    rts

;*******************************************************************************
;          SERVICE AN UNKNOWN INTERRUPT
;*******************************************************************************

IRQSNS              proc
                    brset     PORT5,#01,*         ; wait for interrupt (it goes low)
                    jmp       SNSINT              ; unknown interrupt - better see what it is

;*******************************************************************************
;       MISCELLANEOUS ROUTINES
;*******************************************************************************

;*******************************************************************************
;      ROUTINE TO TURN THE FDD MOTOR ON/OFF
;*******************************************************************************

MTROFF              proc
                    bclr      PORT5,#%00010000    ; motor off
                    rts

;*******************************************************************************

MTRON               proc
                    bset      PORT5,#%00010000    ; motor on
                    lda       #25                 ; wait 500mSecs
                    jmp       DLY1

;*******************************************************************************
;         ROUTINE TO ROTATE D TO THE RIGHT
;*******************************************************************************

RORD                proc
                    lsrd
                    bcc       Done@@
                    ora       #80
Done@@              rts

;*******************************************************************************
;       ROUTINE TO ROTATE D TO THE LEFT
;*******************************************************************************

ROLD                proc
                    asld
                    bcc       Done@@
                    orb       #1
Done@@              rts

;*******************************************************************************
;     ROUTINE TO COMPARE 2 DOUBLE BYTE WORDS
;*******************************************************************************
; TEMPD and D are compared for equality.
; Returns : the condition code register is set in the usual
;           manner (this allows us to use normal conditional
;           branching instructions)
;*******************************************************************************

;*******************************************************************************
;        ROUTINE TO COMPARE TWO FILENAMES
;*******************************************************************************
;  PARAMETERS : Master filename must be in FNAME
;               Trial filename must be pointed to SRCX
;
;  MODIFIES   : A, B, X,TEMPX, TEMPA, SRCX
;
;  CALLS      : -----
;
;  RETURNS    : TEMPA = 00 for a match
;                 "   = FF for no match
;*******************************************************************************

CMPFNM              proc
                    ldx       #FNAME
                    stx       TEMPX               ; pointer to FNAME
                    clr       TEMPA               ; assume the filenames match
                    ldb       #FNAME_SIZE         ; compare FNAME_SIZE bytes max
Loop@@              ldx       TEMPX
                    lda       ,x                  ; get next byte from master FILENAME
                    inx                           ; point to next byte of master FILENAME
                    stx       TEMPX
                    ldx       SRCX
                    cmpa      ,x                  ; get next byte of trial filename
                    bne       NotSame@@           ; quit if they're not the same
                    inx                           ; point to next byte of trial filename
                    stx       SRCX
                    decb
                    bne       Loop@@              ; continue until end of master FILENAME
                    rts                           ; they match !!

NotSame@@           lda       #$FF                ; no match return value
                    sta       TEMPA
                    rts

;*******************************************************************************
;  ROUTINE TO COMPARE THE NEXT FILENAME TO FNAME
;*******************************************************************************
;  PARAMETERS : filename to check against the master filename
;               should be in the filename field of FILEDR
;
;  MODIFIES   : SRCX
;
;  CALLS      : CMPFNM
;
;  RETURNS    : A = 00 for match, A = FF for no match
;*******************************************************************************

CMPNFNM             proc
                    ldx       #FILEDR             ; point to trial filename
                    stx       SRCX
                    bsr       CMPFNM              ; compare the two filenames
                    lda       TEMPA
                    rts

;*******************************************************************************
;       ROUTINE TO CONVERT A FAT ENTRY OFFSET TO A FAT LOCATION WITHIN BUFFER
;*******************************************************************************
;  PARAMETERS : fat entry number is passed through FATNUM
;
;  MODIFIES   : A, B
;
;  CALLS      : ----
;
;  RETURNS    : The location of this entry within the FAT
;               is returned through TEMPX
; *--------------------------------------------------------
; FAT LOCATION = (FATNUM * 3)/2 + #FATBUF
;*******************************************************************************

FNUMLOC             proc
                    ldd       FATNUM              ; fat location = fatnum * 3/2
                    addd:2    FATNUM
                    lsrd
                    addd      #FATBUF             ; return as absolute value
                    std       TEMPX
                    rts

;*******************************************************************************
;     BUFFER MANIPULATION ROUTINES
;*******************************************************************************
; *--------------------------------------------------------
;  ROUTINE TO MOVE B BYTES OF DATA FROM SRCX TO DSTX
; *--------------------------------------------------------
;  PARAMETERS : SRCX   = source address
;               DSTX   = destination address
;               B      = count of bytes to move
;
;  MODIFIES   : A, B, X, SRCX, DSTX
;
;  CALLS      : --------
;
;  RETURNS    : data is moved from SRCX to DSTX
;*******************************************************************************

MTOSB               proc
                    ldx       #SBUFF              ; moves the data from SRCX to SBUFF
                    stx       DSTX
                    bra       MOVE

                    ldx       #FDIRBUF            ; moves the data from SRCX to FDIR
                    stx       DSTX
                    ldb       #32                 ; 32 bytes in a file dir
                    bra       MOVE

                    ldx       #DATA               ; moves the data from SRCX to BUFFER
                    stx       DSTX                ; count set by user
Loop@@              ldx       SRCX                ; moves the data from SRCX to DSTX
                    lda       ,x                  ; fetch next byte
                    inx                           ; point to next source
                    stx       SRCX
                    ldx       DSTX                ; point to next destination
                    sta       ,x                  ; store the next byte
                    inx                           ; move on to next destination
                    stx       DSTX
                    decb                          ; update the counter
                    bne       Loop@@              ; repeat if block not finished
                    rts

MOVE                equ       Loop@@

;*******************************************************************************

DMOVE               proc
Loop@@              std       MVCOUNT             ; save counter value
                    ldx       SRCX                ; moves the data from SRCX to DSTX
                    lda       ,x                  ; fetch next byte
                    inx                           ; point to next source
                    stx       SRCX
                    ldx       DSTX                ; point to next destination
                    sta       ,x                  ; store the next byte
                    inx                           ; move on to next destination
                    stx       DSTX
                    ldd       MVCOUNT
                    decd
                    bne       Loop@@              ; repeat if block not finished
                    rts

;*******************************************************************************
; ROUTINE TO CLEAR A REGION OF MEMORY
;*******************************************************************************
;  PARAMETERS : X   = source address
;               B   = count of bytes to move
;
;  MODIFIES   : A, B, X, BSIZE
;
;  CALLS      : -------
;
;  RETURNS    : X to X+B is filled with 00
;*******************************************************************************

CLRBUF              proc
                    ldx       #BUFFER             ; special routine to clear BUFFER
Loop@@              clr       ,x                  ; clear current byte
                    inx                           ; move on to next byte
                    cpx       #FILEDR
                    bne       Loop@@              ; repeat if block not finished
                    rts

;*******************************************************************************

CLRBPB              proc
                    ldx       #BPBBUF             ; special routine to clear BPB
Loop@@              clr       ,x                  ; clear current byte
                    inx                           ; move on to next byte
                    cpx       #BPBEND
                    bne       Loop@@              ; repeat if block not finished
                    rts

;*******************************************************************************

CLRFAT              proc
                    ldx       #FATBUF             ; special routine to clear FAT
Loop@@              clr       ,x                  ; clear current byte
                    inx                           ; move on to next byte
                    cpx       #FATEND
                    bne       Loop@@              ; repeat if block not finished
                    rts

;*******************************************************************************

CLRFDR              proc
                    ldx       #FDIRBUF            ; special routine to clear FDIR
Loop@@              clr       ,x                  ; clear current byte
                    inx                           ; move on to next byte
                    cpx       #FDIREND
                    bne       Loop@@              ; repeat if block not finished
                    rts

;*******************************************************************************

CLRDTA              proc
                    ldx       #DATA               ; special routine to clear DATA
Loop@@              clr       ,x                  ; clear current byte
                    inx                           ; move on to next byte
                    cpx       #TOPRAM
                    bne       Loop@@              ; repeat if block not finished
                    rts

;*******************************************************************************

CLRSBF              proc
                    ldx       #SBUFF              ; clears the serial buffer
                    ldb       #12
                    bra       CLRXB

;*******************************************************************************

CLRPRM              proc
                    ldx       #PRMBUF             ; clears the parameter buffer
                    ldb       #9
                    bra       CLRXB

;*******************************************************************************

CLRRES              proc
                    ldx       #RESBUF             ; clears the results buffer
                    ldb       #7
                    bra       CLRXB

;*******************************************************************************

CLRSEC              proc
                    ldx       #SECBUF             ; clears the sector buffer
                    ldb       #20
;                   bra       CLRXB

;*******************************************************************************
; Clears B bytes from X to X + B

CLRXB               proc
Loop@@              clr       ,x                  ; clear current byte
                    inx                           ; move on to next byte
                    decb                          ; update the counter
                    bne       Loop@@              ; repeat if block not finished
                    rts

;*******************************************************************************
;  ROUTINE TO CLEAR ALL RAM                             -
;*******************************************************************************

CLRRAM              proc
                    ldx       #MyVars_Begin
Loop@@              clr       ,x
                    inx
                    cpx       #MyVars_End
                    blo       Loop@@
          ;--------------------------------------
                    ldx       #MyXVars_Begin
Loop@@@             clr       ,x
                    inx
                    cpx       #MyXVars_End
                    blo       Loop@@@
                    rts

;*******************************************************************************
;  A SIMPLE DELAY ROUTINE
;*******************************************************************************
;  PARAMETERS : B = delay as a multiple of 20 clicks
;
;  MODIFIES   : A, B
;
;  CALLS      : ----
;
;  RETURNS    : Program execution is suspended for B*20 clicks
;*******************************************************************************

WAIT12              proc
                    bsr       WAIT1               ; 7.5uS
                    nop:4                         ; 9.5
                    rts                           ; 12uS

;*******************************************************************************

W400                proc
                    ldb       #40                 ; 400uS delay
                    bra       WAIT

;*******************************************************************************

W200                proc
                    ldb       #20                 ; 200uS delay
Loop@@              bsr       WAIT1               ; 15
                    decb                          ; 1
                    bne       Loop@@              ; 3
                    rts                           ; 5 = 20.T = 10uSecs

WAIT                equ       Loop@@

;*******************************************************************************

WAIT1               proc
                    nop:5                         ; 5
                    rts                           ; 5

;*******************************************************************************

W20MS               proc
                    lda       #100                ; 20mS delay routine
Loop@@              bsr       W200                ; wait 200uS
                    deca
                    bne       Loop@@
                    rts

;*******************************************************************************

DELAY               proc
                    lda       #50                 ; DELAY 1 SEC
Loop@@              psha                          ; delay 20 ms approx
                    bsr       W20MS               ; wait a while
                    pula
                    deca
                    bne       Loop@@
                    rts

DLY1                equ       Loop@@

;*******************************************************************************
;       MATH ROUTINES
;*******************************************************************************
; --------------------------------------------------------
;     ROUTINE TO MULTIPLY 2 DOUBLE BYTE WORDS
; --------------------------------------------------------
;  PARAMETERS : (TEMPD) * (TEMPA, TEMPB)
;
;  MODIFIES   : PP, B, A, X
;
;  CALLS      : -----
;
;  RETURNS    : (msb) TEMPA, TEMPB, TEMPD (lsb)
; --------------------------------------------------------
; PP     MULTIPLIER      MULTPLICAND
; 0 1       2  3           4 5
; PP     TEMPA,TEMPB     TEMPD
; --------------------------------------------------------

MLTPLY              proc
                    ldx       #PP
                    ldb       #16                 ; multiply doubles giving quad
                    clr       PP                  ; PP(ms) = 0
                    clr       PP+1

Loop@@              lda       3,x                 ; test ls bit of multiplicand
                    asra
                    bcc       Cont@@              ; if bit = 1
                    lda       1,x                 ; add multiplicand to PP (ms)
                    adda      5,x                 ; LS
                    sta       1,x
                    lda       ,x
                    adca      4,x                 ; MS
                    sta       ,x                  ; possible carry after addition

Cont@@              ror       ,x                  ; right shift PP (ms)
                    ror       1,x                 ; right shift pp (ls)
                    ror       2,x
                    ror       3,x                 ; right shift multiplicand

                    decb
                    bne       Loop@@

                    ldd       TEMPA
                    std       TEMPD
                    ldd       PP
                    std       TEMPA
                    rts

;*******************************************************************************
; ROUTINE TO DIVIDE A QUAD BY A DOUBLE
;*******************************************************************************
;  PARAMETERS : TEMPA = divisor, TEMPD TEMPX = dividend
;
;  MODIFIES   : X, A, B
;
;  CALLS      : -----
;
;  RETURNS    : TEMPX = quotient, TEMPD = remainder
;*******************************************************************************
;    DIVISOR            DIVIDEND
;    0     1        2  3        4  5
;  TEMPA,TEMPB      TEMPD,      TEMPX
;                  REMAINDER   QUOTIENT
;*******************************************************************************

DIV4B2              proc
                    ldx       #TEMPA
                    ldb       #17
                    stb       PP
Loop@@              ldd       2,x
                    subd      ,x
                    bcc       _1@@
_@@                 clc
                    bra       _2@@

_1@@                std       2,x
                    sec
_2@@                rol       5,x
                    rol       4,x
                    dec       PP
                    beq       :AnRTS
                    rol       3,x
                    rol       2,x
;                   bcs       _@@
                    bra       Loop@@

;*******************************************************************************
;      ERROR HANDLING ROUTINE
;*******************************************************************************
;  PARAMETERS : RESBUF, FDCCMND, ERRNUM (0 = FDC,
;                                        !0 = OTHER)
;  MODIFIES   :
;
;  CALLS      : PERROR
;
;  RETURNS    : -----
;*******************************************************************************

ERROR               proc
                    lda       ERRNUM
                    beq       _1@@                ; test for FDC error
                    nop
                    rts

_1@@                jsr       PRSLT
                    lda       FDCCMND
                    cmpa      #%00000100          ; check for sense drive status command
                    jeq       _3@@

                    lda       RESBUF              ; extract errors for status reg. 0
                    anda      #%11000000          ; extract interrupt code
                    clrb                          ; interrupt code error
                    jsr       PERROR
                    lda       RESBUF
                    anda      #%00100000          ; extract seek end bit - 1 => OK
                    bne       _2@@
                    ldb       #$02                ; seek code error
                    jsr       PERROR
_2@@                lda       RESBUF
                    anda      #%00010000          ; extract equipment check bit
                    ldb       #$03                ; equipment check code error
                    jsr       PERROR
                    lda       RESBUF
                    anda      #%00001000          ; extract not ready bit
                    ldb       #$04                ; not ready code error
                    jsr       PERROR

                    lda       FDCCMND
                    cbeqa     #%00001000,:AnRTS   ; check for sense interrupt status command

                    lda       RESBUF+1            ; extract errors for status reg. 1
                    anda      #%10000000          ; extract end of track code
                    ldb       #$10                ; end of track code error
                    jsr       PERROR
                    lda       RESBUF+1
                    anda      #%00100000          ; extract data error code bit
                    ldb       #$11                ; data error code error
                    jsr       PERROR
                    lda       RESBUF+1
                    anda      #%00010000          ; extract over run bit
                    ldb       #$12                ; over run code error
                    jsr       PERROR
                    lda       RESBUF+1
                    anda      #%00000100          ; extract no data bit
                    ldb       #$13                ; no data code error
                    jsr       PERROR
                    lda       RESBUF+1
                    anda      #%00000010          ; extract not writable bit
                    ldb       #$15                ; not writable code error
                    jsr       PERROR
                    lda       RESBUF+1
                    anda      #%00000001          ; extract missing address mark bit
                    ldb       #$16                ; missing address mark code error
                    jsr       PERROR

                    lda       RESBUF+2            ; extract errors for status reg. 2
                    anda      #%01000000          ; extract control mark code
                    ldb       #$20                ; control mark code error
                    jsr       PERROR
                    lda       RESBUF+2
                    anda      #%00100000          ; extract data error in data field code bit
                    ldb       #$21                ; data error in data field code error
                    jsr       PERROR
                    lda       RESBUF+2
                    anda      #%00010000          ; extract wrong track bit
                    ldb       #$22                ; wrong track code error
                    jsr       PERROR
                    lda       RESBUF+2
                    anda      #%00001000          ; extract scan equal hit bit
                    ldb       #$23                ; scan equal hit error code
                    jsr       PERROR
                    lda       RESBUF+2
                    anda      #%00000100          ; extract scan not satisfied bit
                    ldb       #$24                ; scan not satisfied error code
                    jsr       PERROR
                    lda       RESBUF+2
                    anda      #%00000010          ; extract bad track bit
                    ldb       #$25                ; bad track code error
                    jsr       PERROR
                    lda       RESBUF+2
                    anda      #%00000001          ; extract missing address mark in data field bit
                    ldb       #$26                ; missing address mark in data field code error
                    jmp       PERROR

_3@@                lda       RESBUF              ; extract errors for status reg. 3
                    anda      #%10000000          ; extract fault code
                    ldb       #$30                ; fault code error
                    jsr       PERROR
                    lda       RESBUF
                    anda      #%01000000          ; extract write protected bit
                    ldb       #$31                ; write protected code error
                    jsr       PERROR
                    lda       RESBUF
                    anda      #%00100000          ; extract ready bit
                    ldb       #$32                ; ready code error
                    jsr       PERROR
                    lda       RESBUF
                    anda      #%00010000          ; extract track 0 bit
                    ldb       #$33                ; track 0 code error
                    jsr       PERROR
                    lda       RESBUF
                    anda      #%00001000          ; extract two sided bit
                    ldb       #$34                ; two sided code error
                    jsr       PERROR
                    lda       RESBUF
                    anda      #%00000100          ; extract head address bit
                    ldb       #$35                ; head address code error
                    jmp       PERROR

;*******************************************************************************
;       DISK ACCESS ROUTINES  (LOW LEVEL)
;*******************************************************************************

;*******************************************************************************
;       ROUTINE TO READ BSIZE BYTES FROM A SECTOR
;*******************************************************************************
;  PARAMETERS : ABSSEC, PTRX, BSIZE
;
;  MODIFIES   : A, B
;
;  CALLS      : STOTRK, TRKTHD, TRKSEC, SPEC, SEEK,
;               RDLOW
;  RETURNS    : SECTOR contents at PTRX
;*******************************************************************************
; the motor should be on before calling this routine
;*******************************************************************************

RBSIZE              proc
                    clr       MLTITRK             ; single sided access only
                    lda       #%00100000          ; bit 5
                    sta       SKIP                ; skip deleted data
                    lda       #%01000000          ; bit 6
                    sta       MFM                 ; 1 = MFM, 0 = FM
                    lda       #1
                    sta       US0                 ; select first drive
                    clr       US1

                    jsr       STOTRK              ; get track number from abs sector number
                    jsr       TRKTHD              ; get head number from track number
                    jsr       TRKSEC              ; get physical track sector from abs sector
                    lda       #2
                    sta       SECBYTES
                    lda       SECTOR
                    inca
                    sta       EOTSEC
                    lda       #$1B
                    sta       GAP3
                    lda       #$FF
                    sta       DTL
                    ldd       PTRX
                    addd      BSIZE
                    std       ENDX

                    jsr       SPEC
                    jsr       SEEK
                    jsr       SPEC
                    jmp       RDLOW

;*******************************************************************************
;       ROUTINE TO READ A SECTOR
;*******************************************************************************
;  PARAMETERS : SECTOR, PTRX
;
;  MODIFIES   : A, B
;
;  CALLS      : RBSIZE
;
;  RETURNS    : SECTOR contents at PTRX
;*******************************************************************************
; the motor should be on before calling this routine
;*******************************************************************************

RSECTOR             proc
                    ldd       #SECTOR_SIZE        ; read a full sector
                    std       BSIZE
                    bra       RBSIZE

;*******************************************************************************
;       ROUTINE TO READ B CONTIGUOUS SECTORS
;*******************************************************************************
;  PARAMETERS : ABSSEC, B, PTRX
;
;  MODIFIES   : A, B
;
;  CALLS      : RSECTOR, MTRON, MTROFF
;
;  RETURNS    : B sectors of data at PTRX
;*******************************************************************************

RBSECS              proc
                    pshb
                    jsr       MTRON
                    pulb
Loop@@              pshb
                    bsr       RSECTOR             ; read it
                    ldd       ABSSEC
                    incd
                    std       ABSSEC
                    pulb                          ; are they all read
                    decb
                    bne       Loop@@
                    jmp       MTROFF

;*******************************************************************************
;       ROUTINE TO READ B NONCONTIGUOUS SECTORS
;*******************************************************************************
;  PARAMETERS : B, PTRX, SECBUF
;
;  MODIFIES   : A, B, X, ABSSEC
;
;  CALLS      : RSECTOR, MTRON, MTROFF
;
;  RETURNS    : B sectors of data at PTRX
;*******************************************************************************

RBNSEC              proc
                    pshb
                    jsr       MTRON
                    pulb
                    ldx       #SECBUF
Loop@@              pshb
                    pshx
                    ldd       ,x                  ; get next sector num to be read
                    std       ABSSEC
                    bsr       RSECTOR             ; read it
                    pulx                          ; point to next sector num entry
                    inx:2
                    pulb                          ; are they all read
                    decb
                    bne       Loop@@
                    jmp       MTROFF

;*******************************************************************************
;      ROUTINE TO WRITE BSIZE BYTES TO A SECTOR
;*******************************************************************************
;  PARAMETERS : ABSSEC, PTRX, BSIZE
;
;  MODIFIES   : A, B
;
;  CALLS      : STOTRK, TRKTHD, TRKSEC, SPEC, SEEK,
;               WRLOW
;  RETURNS    : ------
;*******************************************************************************
; the motor should be on before calling this routine
;*******************************************************************************

WBSIZE              proc
                    clr       MLTITRK             ; single sided access only
                    lda       #%01000000          ; bit 6
                    sta       MFM                 ; 1 = MFM, 0 = FM
                    lda       #1
                    sta       US0                 ; select first drive
                    clr       US1

                    jsr       STOTRK              ; get track number from abs sector number
                    jsr       TRKTHD              ; get head number from track number
                    jsr       TRKSEC              ; get physical track sector from abs sector
                    lda       #2
                    sta       SECBYTES
                    lda       SECTOR
                    inca
                    sta       EOTSEC
                    lda       #$1B
                    sta       GAP3
                    lda       #$FF
                    sta       DTL
                    ldd       PTRX
                    addd      BSIZE
                    std       ENDX

                    jsr       SPEC
                    jsr       SEEK
                    jsr       SPEC
                    jmp       WRLOW

;*******************************************************************************
;       ROUTINE TO WRITE A SECTOR
;*******************************************************************************
;  PARAMETERS : ABSSEC, PTRX
;
;  MODIFIES   : A, B
;
;  CALLS      : WBSIZE
;
;  RETURNS    : ----
;*******************************************************************************
; the motor should be on before calling this routine
;*******************************************************************************

WSECTOR             proc
                    ldd       #SECTOR_SIZE        ; read a full sector
                    std       BSIZE
                    bra       WBSIZE

;*******************************************************************************
;       ROUTINE TO WRITE B CONTIGUOUS SECTORS
;*******************************************************************************
;  PARAMETERS : ABSSEC, B, PTRX
;
;  MODIFIES   : A, B
;
;  CALLS      : WSECTOR, MTRON, MTROFF
;
;  RETURNS    : ------
;*******************************************************************************

WBSECS              proc
                    pshb
                    jsr       MTRON
                    pulb
Loop@@              pshb
                    bsr       WSECTOR             ; read it
                    ldd       ABSSEC
                    incd
                    std       ABSSEC
                    pulb                          ; are they all read
                    decb
                    bne       Loop@@
                    jmp       MTROFF

;*******************************************************************************
;       ROUTINE TO WRITE TO B NONCONTIGUOUS SECTORS
;*******************************************************************************
;  PARAMETERS : B, PTRX, SECBUF
;
;  MODIFIES   : A, B, X, ABSSEC
;
;  CALLS      : WSECTOR, MTRON, MTROFF
;
;  RETURNS    :  -----
;*******************************************************************************

WBNSEC              proc
                    pshb
                    jsr       MTRON
                    pulb
                    ldx       #SECBUF
Loop@@              pshb
                    pshx
                    ldd       ,x                  ; get next sector num to be read
                    std       ABSSEC
                    bsr       WSECTOR             ; read it
                    pulx                          ; point to next sector num entry
                    inx:2
                    pulb                          ; are they all read
                    decb
                    bne       Loop@@
                    jmp       MTROFF

;*******************************************************************************
;       DISK ACCESS ROUTINES (OBJECT LEVEL)
;*******************************************************************************

;*******************************************************************************
;       ROUTINE TO READ THE BPB
;*******************************************************************************
;  PARAMETERS : ------
;
;  MODIFIES   : PTRX, ABSSEC, X
;
;  CALLS      : RSECTOR, MTRON, MTROFF, PBPB
;
;  RETURNS    : BPB at BPBBUF
;*******************************************************************************

RBPB                proc
                    clr       ABSSEC
                    clr       ABSSEC+1
                    ldx       #BPBBUF
                    stx       PTRX
                    jsr       MTRON
                    jsr       RSECTOR
                    jsr       MTROFF
                    jmp       PBPB

;*******************************************************************************
;       ROUTINE TO READ THE FAT
;*******************************************************************************
;  PARAMETERS : ------
;
;  MODIFIES   : A, B, X, PTRX, ABSSEC
;
;  CALLS      : RBSECS, FRDFAT, PFAT
;
;  RETURNS    : FAT at FATBUF
;*******************************************************************************

RFAT                proc
                    ldd       #1
                    std       ABSSEC
                    ldx       #FATBUF
                    stx       PTRX
                    ldb       #9
                    jsr       RBSECS
                    jsr       FRDFAT
                    jmp       PFAT

;*******************************************************************************
;       ROUTINE TO READ THE FILE DIRECTORY
;*******************************************************************************
;  PARAMETERS : ------
;
;  MODIFIES   : A, B, X, PTRX, ABSSEC
;
;  CALLS      : RBSECS, PFDIR
;
;  RETURNS    : FILE DIRECTORY at FDIRBUF
;*******************************************************************************

RFDIR               proc
                    ldd       #19                 ; and read the file dir
                    std       ABSSEC
                    ldx       #FDIRBUF
                    stx       PTRX
                    ldb       #14
                    jsr       RBSECS
                    jmp       PFDIR

;*******************************************************************************
;    ROUTINE TO READ THE NEXT 5 SECTORS OF DATA FROM CURRENT PAGENUM
;*******************************************************************************
;  PARAMETERS : PAGENUM
;
;  MODIFIES   : A, B, X, PTRX, ABSSEC
;
;  CALLS      : RBSECS
;
;  RETURNS    : DATA at DATA
;*******************************************************************************

RDATA               proc
                    ldd       PAGENUM             ; ABSSEC = PAGENUM * 5
                    decd
                    std       PAGENUM
                    addd:4    PAGENUM
                    addd      #33
                    std       ABSSEC
                    ldd       PAGENUM
                    incd
                    std       PAGENUM
                    ldx       #DATA
                    stx       PTRX
                    ldb       #5
                    jmp       RBSECS

;*******************************************************************************
;       ROUTINE TO WRITE THE BPB
;*******************************************************************************
;  PARAMETERS : BPB IN BPBBUF
;
;  MODIFIES   : PTRX, ABSSEC, X
;
;  CALLS      : RSECTOR, MTRON, MTROFF
;
;  RETURNS    : BPB at BPBBUF
;*******************************************************************************

WBPB                proc
                    clr       ABSSEC
                    clr       ABSSEC+1
                    ldx       #BPBMAP
                    stx       PTRX
                    jsr       MTRON
                    jsr       WSECTOR
                    jmp       MTROFF

;*******************************************************************************
;       ROUTINE TO WRITE THE FAT
;*******************************************************************************
;  PARAMETERS : FAT at FATBUF
;
;  MODIFIES   : A, B, X, PTRX, ABSSEC
;
;  CALLS      : WBSECS, DOSFAT
;
;  RETURNS    : ----
;*******************************************************************************

WFAT                proc
                    jsr       DOSFAT
                    ldd       #1
                    std       ABSSEC
                    ldx       #FATBUF
                    stx       PTRX
                    ldb       #9
                    jsr       WBSECS
                    ldd       #10
                    std       ABSSEC
                    ldx       #FATBUF
                    stx       PTRX
                    ldb       #9
                    jmp       WBSECS

;*******************************************************************************
;       ROUTINE TO WRITE THE FILE DIRECTORY
;*******************************************************************************
;  PARAMETERS : FILE DIRECTORY at FDIRBUF
;
;  MODIFIES   : PTRX, B, ABSSEC, A, X
;
;  CALLS      : WBSECS
;
;  RETURNS    : --------
;*******************************************************************************

WFDIR               proc
                    ldd       #19
                    std       ABSSEC
                    ldx       #FDIRBUF
                    stx       PTRX
                    ldb       #14
                    jmp       WBSECS

;*******************************************************************************
;    ROUTINE TO WRITE THE NEXT 5 SECTORS OF DATA TO CURRENT PAGENUM
;*******************************************************************************
;  PARAMETERS : PAGENUM
;
;  MODIFIES   : PTRX, B, ABSSEC, A, X
;
;  CALLS      : WBSECS
;
;  RETURNS    : --------
;*******************************************************************************

WDATA               proc
                    ldd       PAGENUM             ; ABSSEC = PAGENUM * 5
                    decd
                    std       PAGENUM
                    addd:4    PAGENUM
                    addd      #33
                    std       ABSSEC
                    ldd       PAGENUM
                    incd
                    std       PAGENUM
                    ldx       #DATA
                    stx       PTRX
                    ldb       #5
                    jmp       WBSECS

;*******************************************************************************
;       LOW LEVEL ROUTINE TO WRITE TO A FILE
;*******************************************************************************
;  PARAMETERS : BSIZE, PTRADR
;
;  MODIFIES   : A, B, X, SRCX, DSTX, PAGENUM, BADDR
;
;  CALLS      : BLKADR, MOVE, WDATA, RDATA, CHKSUM, UDFSZ
;
;  RETURNS    : FSIZE is incremented by the number of bytes written
;               BSIZE = numbytes written
;*******************************************************************************

WFLOW               proc
                    ldd       BSIZE
                    std       BSTEMP              ; save bsize

                    jsr       BLKADR              ; get block address and check if bsize is ok
                    beq       _2@@
                    ldd       BSIZE               ; bsize was too big
                    beq       _1@@                ; check bsize does not equal zero
                    ldx       #BUFFER
                    stx       SRCX
                    ldx       BADDR
                    stx       DSTX                ; write permissable block size
                    ldd       BSIZE
                    jsr       DMOVE

_1@@                bsr       WDATA               ; write the current 8k block
                    ldd       PAGENUM
                    incd
                    std       PAGENUM
                    jsr       RDATA               ; read the required 8k block

                    ldd       #DATA               ; reset baddr
                    std       BADDR

                    ldd       #BUFFER             ; point to next byte to be written
                    addd      BSIZE
                    std       SRCX

                    ldd       BSTEMP              ; reset bsize
                    subd      BSIZE
                    std       BSIZE
                    bra       _3@@

_2@@                ldd       BSTEMP
                    std       BSIZE
                    ldx       #BUFFER             ; write from start of buffer
                    stx       SRCX
_3@@                ldx       BADDR
                    stx       DSTX                ; write remainder of block
                    ldd       BSIZE
                    jsr       DMOVE
          ;-------------------------------------- ; update file size field
                    ldd       BSTEMP
                    std       BSIZE
                    jmp       UDFSZ

;*******************************************************************************
;       MID LEVEL ROUTINE TO WRITE TO A FILE
;*******************************************************************************
;  PARAMETERS : CTRLCD (P, N, A), BSIZE
;
;  MODIFIES   : A
;
;  CALLS      : WPTR, APPEND
;
;  RETURNS    : ----
;*******************************************************************************

WFMID               proc
                    lda       #'P'                ; check ctrl cd values
                    cmpa      CTRLCD
                    beq       _@@                 ; write from file ptr

                    lda       #'N'
                    cmpa      CTRLCD
_@@                 jeq       WPTR                ; write next block

                    lda       #'A'
                    cbeqa     CTRLCD,APPEND       ; append to end of file
                    rts

;*******************************************************************************
;      ROUTINE TO APPEND TO A FILE
;*******************************************************************************
;  PARAMETERS : BSIZE
;
;  MODIFIES   : A, B, FPTR, PTRADR, ADDR3,2,1, NFBS
;
;  CALLS      : PNTRADR, MOVE, UDEOF, WFLOW, UDFPTR, FFFAT,
;
;  RETURNS    : ----
;*******************************************************************************

APPEND              proc
                    ldd       BSIZE               ; save bsize first
                    std       BSTEMP
Loop@@              ldd       FSIZE+1             ; set file pointer to file size
                    std       FPTR                ; i.e. point to eof
                    lda       FSIZE+3
                    sta       FPTR+2

                    lda       EOFFLG
                    beq       _1@@
          ;-------------------------------------- ; INCLUDE THIS IF $1A EOF IS TO BE APPENDED TO FILES
                    ldd       FPTR+1              ; dec file pntr by one so that EOF gets over written
                    decd
                    std       FPTR+1
                    bcc       _1@@
                    dec       FPTR

_1@@                jsr       PNTRADR             ; convert to address
          ;-------------------------------------- ; check for enough room in sector
                    ldd       NFBS
                    subd      BSTEMP              ; check if there is enough room left in current EOFS
                    bcc       _2@@
          ;-------------------------------------- ; not enough room in current sector
                    ldd       BSTEMP
                    pshd
                    ldd       NFBS
                    std       BSIZE               ; set bsize to num bytes available
                    jsr       WFLOW               ; fill remainder of sector
                    puld
                    subd      BSIZE               ; sub amount written thus far
                    std       BSTEMP              ; save updated bsize

                    ldd       #BUFFER             ; put remainder of block at start of buffer
                    std       DSTX                ; srcx is still set from wflow
                    addd      BSIZE
                    std       SRCX
                    ldd       BSTEMP              ; set num bytes to be moved
                    jsr       DMOVE

                    jsr       UDFPTR              ; and update the file pointer
                    jsr       FFFAT               ; find another sector
                    cmpa      #$FF
                    jeq       FW85                ; FAT ERROR -----> ACCESS DENIED AND REBOOT
                    jsr       UDEOF               ; up date new EOF location for the file
                    bra       Loop@@

_2@@                ldd       BSTEMP
                    std       BSIZE               ; save the block size
                    pshd
                    jsr       WFLOW               ; write the block
                    puld
                    std       BSIZE
                    jsr       UDFPTR              ; and update the file pointer

                    lda       EOFFLG
                    beq       :AnRTS
          ;-------------------------------------- ; INCLUDE THIS IF $1A EOF IS TO BE APPENDED TO FILES
                    ldd       FSIZE+2             ; sub 1 from fsize to adjust for decrementing
                    decd                          ; the file pointer above
                    std       FSIZE+2
                    bcc       :AnRTS
                    ldd       FSIZE
                    decd
                    std       FSIZE
                    rts                           ; and quit

;*******************************************************************************
;      ROUTINE TO OVERWRITE A FILE
;*******************************************************************************
;  PARAMETERS : BSIZE
;
;  MODIFIES   : A, B, FPTR, PTRADR, ADDR3,2,1, NFBS
;
;  CALLS      : PNTRADR, MOVE, UDFPTR, WFLOW, UDFPTR, FFFAT,
;
;  RETURNS    : ----
;*******************************************************************************

WPTR                proc
                    ldd       BSIZE
                    std       BSTEMP
Loop@@              jsr       PNTRADR             ; convert to address

                    ldd       NFBS
                    subd      BSTEMP              ; check if there is enough room left in current EOFS
                    bcc       _1@@

                    ldd       BSTEMP
                    pshd
                    ldd       NFBS
                    std       BSIZE
                    jsr       WFLOW               ; write permissable block size
                    puld
                    subd      BSIZE               ; and up date num bytes to be written
                    std       BSTEMP

                    ldd       #BUFFER             ; put remainder of block at start of buffer
                    std       DSTX                ; srcx is still set from wflow
                    addd      BSIZE
                    std       SRCX
                    ldd       BSTEMP              ; set num bytes to be moved
                    jsr       DMOVE

                    jsr       UDFPTR              ; and up date the file pointer position
                    bra       Loop@@              ; go again

_1@@                ldd       BSTEMP
                    std       BSIZE
                    pshd
                    jsr       WFLOW               ; write block
                    puld
                    std       BSIZE
                    jmp       UDFPTR              ; and up date the file pointer

;*******************************************************************************
;       LOW LEVEL ROUTINE TO READ A FILE
;*******************************************************************************
;  PARAMETERS : BSIZE, PTRADR
;
;  MODIFIES   : A, B, X, SRCX, DSTX, PAGENUM, BADDR
;
;  CALLS      : BLKADR, MOVE, RDATA
;
;  RETURNS    : Outputs BSIZE bytes to moneychanger
;               BSIZE = numbytes read
;*******************************************************************************

RFLOW               proc
                    ldd       BSIZE
                    beq       :AnRTS
                    std       BSTEMP              ; save bsize

                    jsr       BLKADR              ; get block address and see if bsize is ok
                    beq       _1@@
                    ldd       BSIZE               ; bsize was too big
                    beq       :AnRTS              ; check bsize does not equal zero
                    ldx       BUFFX
                    stx       DSTX
                    ldx       BADDR
                    stx       SRCX                ; read permissable block size
                    ldd       BSIZE
                    jsr       DMOVE

                    ldd       PAGENUM
                    incd
                    std       PAGENUM
                    jsr       RDATA               ; read the required 8k block
                    ldd       #DATA               ; reset baddr
                    std       BADDR
                    ldd       BSIZE
                    addd      BUFFX
                    std       DSTX                ; point to next byte to be read
                    ldd       BSTEMP
                    subd      BSIZE
                    beq       :AnRTS
                    std       BSIZE
                    bra       _2@@

_1@@                ldd       BSTEMP
                    subd      BSIZE               ; set bsize to remainder of block
                    addd      BUFFX
                    std       DSTX
_2@@                ldx       BADDR
                    stx       SRCX                ; read remainder of block
                    ldd       BSIZE
                    jmp       DMOVE

;*******************************************************************************
; Missing code stubs
;*******************************************************************************

GETRSLTS            equ       GETRSLT

;*******************************************************************************
;       MID LEVEL ROUTINE TO READ A FILE
;*******************************************************************************
;  PARAMETERS : CTRLCD (N, P), BSIZE
;
;  MODIFIES   : A, B, BSIZE, SECTOR, FPTR, ADDR3,2,1, PTRADR,
;               NFBS
;  CALLS      : PNTRADR, RFLOW, UDFPTR
;
;  RETURNS    : -------
;*******************************************************************************

RFMID               proc
                    lda       #'P'                ; check ctrl codes first
                    cbeqa     CTRLCD,_1@@         ; read from file ptr

                    lda       #'N'
                    cjnea     CTRLCD,FR85         ; illegal CTRLCD, ----> access denied, REBOOT

                    ldd       #SECTOR_SIZE
                    std       BSIZE               ; set block size to 128(?) bytes

_1@@                ldx       #BUFFER
                    stx       BUFFX

Loop@@              ldd       BSIZE               ; save bsize
                    jeq       :AnRTS
                    std       BSTEMP
                    jsr       PNTRADR             ; convert to address

                    ldd       NFBS
                    subd      BSTEMP              ; check if there is enough room left in sector
                    bcc       _2@@

                    ldd       BSTEMP
                    pshd
                    ldd       NFBS
                    std       BSIZE
                    pshd                          ; save new bsize
                    jsr       RFLOW               ; read permissable block size
                    puld                          ; retrieve new bsize
                    std       BSIZE
                    addd      BUFFX
                    std       BUFFX
                    jsr       UDFPTR              ; and up date the file pointer position
                    puld                          ; retrieve old bsize
                    subd      BSIZE               ; and up date num bytes to be written
                    std       BSIZE
                    bra       Loop@@              ; go again

_2@@                ldd       BSTEMP
                    std       BSIZE
                    pshd
                    jsr       RFLOW               ; read block
                    puld
                    std       BSIZE
                    jmp       UDFPTR              ; and up date the file pointer

;*******************************************************************************
;  ROUTINE TO GET PHYSICAL TRACK SECTOR FROM ABSSEC
;*******************************************************************************
;  PARAMETERS : ABSSEC
;
;  MODIFIES   : A, B
;
;  CALLS      : DIV4B2
;
;  RETURNS    : SECTOR
;*******************************************************************************

TRKSEC              proc
                    ldb       TRKSCS              ; set up params for division
                    clra
                    std       TEMPA
                    ldd       ABSSEC
                    std       TEMPX
                    clr       TEMPD
                    clr       TEMPD+1
                    jsr       DIV4B2
                    ldd       TEMPD               ; get remainder
                    incb                          ; sector numbers start at 1
                    stb       SECTOR              ; sector is one byte only
                    rts

;*******************************************************************************
;  ROUTINE TO GET PHYSICAL TRACK NUMBER FROM ABSSEC
;*******************************************************************************
;  PARAMETERS : ABSSEC
;
;  MODIFIES   : A, B
;
;  CALLS      : DIV4B2
;
;  RETURNS    : TRACK
;*******************************************************************************

STOTRK              proc
                    ldb       TRKSCS              ; set up params for division
                    clra
                    asld                          ; *2
                    std       TEMPA               ; divisor
                    ldd       ABSSEC
                    std       TEMPX
                    clr       TEMPD
                    clr       TEMPD+1             ; dividend
                    jsr       DIV4B2
                    ldd       TEMPX               ; get result
                    stb       TRACK               ; track is one byte only
                    rts

;*******************************************************************************
;  ROUTINE TO GET PHYSICAL HEAD NUMBER FROM TRACK
;*******************************************************************************
;  PARAMETERS : TRACK
;
;  MODIFIES   : A, B
;
;  CALLS      : ----
;
;  RETURNS    : HEAD
;*******************************************************************************

TRKTHD              proc
                    clra
                    ldb       TRKSCS              ; set up params for division
                    asld                          ; *2
                    std       TEMPA               ; divisor
                    ldd       ABSSEC
                    std       TEMPX
                    clr       TEMPD
                    clr       TEMPD+1             ; dividend
                    jsr       DIV4B2
                    ldd       TEMPD               ; get remainder
                    std       TEMPX
                    clr       TEMPD
                    clr       TEMPD+1             ; dividend
                    clra
                    ldb       TRKSCS              ; set up params for division
                    std       TEMPA               ; divisor
                    jsr       DIV4B2
                    ldd       TEMPX
                    aslb:2                        ; get head bit to correct position
;                   eorb      #%00000100          ; toggle head value
                    stb       HEAD
                    rts

;*******************************************************************************
;       ROUTINE TO CALCULATE THE FILE POINTER ADDRESS
;*******************************************************************************
;  PARAMETERS : filepointer value is passed through FPTR
;
;  MODIFIES   : X, A, B, TEMPD, TEMPX, TEMPA, TEMPB, SECTOR,
;               FATNUM
;
;  CALLS      : DIV4B2, GENTRY, SECADR
;
;  RETURNS    : SECNUM & SECOFF are set to corresponding
;               disk address.
;*******************************************************************************

PNTRADR             proc
                    ldx       BSIZE               ; preserve bsize
                    pshx
          ;-------------------------------------- ; get num sectors to file pointer position
                    clra
                    ldb       FPTR                ; fptr / 512 = sector + offset
                    std       TEMPD
                    ldd       FPTR+1
                    std       TEMPX
                    ldd       #SECTOR_SIZE
                    std       TEMPA
                    jsr       DIV4B2
                    ldd       TEMPD               ; save offset to secoff
                    std       SECOFF
                    ldd       TEMPX
                    std       SECNUM              ; use SECNUM as a temp counter

                    ldd       #SECTOR_SIZE        ; get num bytes to end of sector
                    subd      TEMPD
                    std       NFBS

                    ldd       BOF                 ; trace through FAT until the Nth entry is found
                    beq       Fail@@              ; no fat entry can = 00
                    std       FATNUM              ; where N = value now stored in PTRADR
Loop@@              ldd       SECNUM
                    beq       Done@@              ; finished ?
                    jsr       GENTRY              ; get the fat entry

                    ldd       FATNUM
                    beq       Fail@@              ; no fat entry can = 00
                    std       TEMPD
                    ldd       #$0FFF
                    cmpd      TEMPD
                    beq       Fail@@              ; IF fat entry = FFFh we've gone too far

                    ldd       SECNUM              ; dec counter
                    decd
                    std       SECNUM
                    bra       Loop@@

Done@@              ldd       FATNUM              ; save last entry read to sector
                    subd      #2                  ; adjustment for correct addressing
                    std       SECNUM
                    pulx
                    stx       BSIZE               ; retrieve bsize
                    rts

Fail@@              jmp       FR85                ; FAT ERROR ---> ACCESS DENIED, REBOOT

;*******************************************************************************
;  ROUTINE TO UPDATE THE FILE POINTER TO THE LAST
;        LOCATION ACCESSED ON THE DISK
; --------------------------------------------------------
;  PARAMETERS : FPTR and BSIZE
;
;  MODIFIES   : ----
;
;  CALLS      : ----
;
;  RETURNS    : FPTR+BSIZE
;*******************************************************************************

UDFPTR              proc
                    ldd       BSIZE
                    addd      FPTR+1              ; update file pointer location
                    std       FPTR+1
                    bcc       Done@@
                    inc       FPTR
Done@@              rts

;*******************************************************************************
; ROUTINE CONVERT PTRADR TO A DATA BLOCK OFFSET ADDRESS
;*******************************************************************************
;  PARAMETERS : PTRADR, WFLAG
;
;  MODIFIES   : A, B, TEMPX, TEMPD, TEMPA, TEMPB
;
;  CALLS      : DIV4B2, WDATA, RDATA, CHKSUM, CHKBLK
;
;  RETURNS    : BADDR = data block address
;               PAGENUM = page containing that(file pointer)
;               address
; --------------------------------------------------------
; PAGENUM = SECNUM/5
; BADDR   = SECNUM mod 5 + #DATA
;*******************************************************************************

BLKADR              proc
                    ldx       BSIZE
                    pshx
                    ldd       SECNUM
                    std       TEMPX
                    clrd
                    std       TEMPD
                    ldd       #5
                    std       TEMPA
                    jsr       DIV4B2              ; SECNUM/5

                    ldd       #SECTOR_SIZE        ; BADDR = [(SECNUM MODE 5) * 512] + SECOFF
                    std       TEMPA
                    jsr       MLTPLY
                    ldd       TEMPD
                    addd      SECOFF
                    addd      #DATA               ; add data buffer location to offset to get correct
                    std       BADDR               ; baddr value

                    ldd       TEMPX               ; get the pagenumber of this block
                    incd                          ; inc by one for correct answer(pages start @ 1 not 0)r
                    subd      PAGENUM
                    beq       Done@@
                    ldx       TEMPX
                    pshx
                    lda       WFLAG               ; are we reading or writing
                    cmpa      #'W'
                    bne       _@@

                    jsr       WDATA               ; new page

_@@                 pulx
                    xgdx
                    incd                          ; adjustment for correct addressing
                    std       PAGENUM             ; save new pagenumber
                    jsr       RDATA
Done@@              pulx
                    stx       BSIZE
;                   bra       CHKBLK

;*******************************************************************************
; ROUTINE TO CHECK THAT BSIZE WONT PUT US BEYOND THE END
;  OF THE DATA BUFFER
;*******************************************************************************
;  PARAMETERS : BSIZE, BADDR
;
;  MODIFIES   : A, B
;
;  CALLS      : ----
;
;  RETURNS    : A = 00 if ok,
;               A = FF if not and BSIZE is set to numbytes
;                      to end of data block
;*******************************************************************************

CHKBLK              proc
                    ldd       #TOPRAM             ; get num bytes to end of block from BADDR
                    subd      BADDR
                    subd      BSIZE               ; is bsize bigger than numbytes to end of block
                    bcc       Done@@

                    ldd       #TOPRAM             ; bsize too big => reset it
                    subd      BADDR
                    std       BSIZE
                    lda       #$FF
                    rts

Done@@              clra
                    rts

;*******************************************************************************
;       ROUTINE TO GET A FAT ENTRY
;*******************************************************************************
;  PARAMETERS : entry number to get = FATNUM
;
;  MODIFIES   : A, B, X
;
;  CALLS      : -----
;
;  RETURNS    : FATNUM returns with fat entry contents
;               TEMPA = FF if there are no more entries
;                       in the FAT
;*******************************************************************************

GENTRY              proc
                    clr       TEMPA               ; reset TEMPA first
                    ldx       FATNUM              ; convert entry number to entry location
                    pshx                          ; save fatnum value
                    ldd       FATNUM              ; ENTRY LOCATION = (FATNUM/2) + FATNUM + #FAT
                    lsrd                          ; FATNUM/2
                    addd      FATNUM              ; +FATNUM

                    addd      #FATBUF             ; add offset to start of fat
                    xgdx
                    cpx       #FATEND             ; check if we are at the end of FAT
                    beq       Fail@@              ; we are => error
                    puld
                    lsrd                          ; check for even or odd location
                    bcs       Odd?@@              ; ODD ?
          ;-------------------------------------- ; EVEN!
                    ldd       ,x                  ; get the location contents
                    lsrd:4                        ; fix it up
                    std       FATNUM              ; save it
                    rts
          ;-------------------------------------- ; ODD!
Odd?@@              ldd       ,x                  ; get location contents
                    anda      #$0F                ; fix it up
                    std       FATNUM              ; save it
                    rts

Fail@@              puld                          ; no more entries error
                    lda       #$FF
                    sta       TEMPA
                    rts

;*******************************************************************************
; ROUTINE TO READ THE NEXT FILE DIR TO FILEDR BUFFER
;*******************************************************************************
;  PARAMETERS : NXTFDR (0 - 40) = file dir number to read
;
;  MODIFIES   : A, B, TEMPA, TEMPB, TEMPD, SRCX, DSTX
;
;  CALLS      : MOVE
;
;  RETURNS    : File dir is returned in FILEDR buffer
;               NXTFDR is incremented to next file dir number
;*******************************************************************************

RNFDIR              proc
                    lda       #$29
                    cmpa      NXTFDR              ; check to see if we have read the last one
                    bne       _1@@
                    clr       NXTFDR              ; must reset the file dir vars
_1@@                clra                          ; Offset into file dir buffer = (32*nxtfdr)+ #FDIR
                    ldb       NXTFDR
                    asld:5                        ; 32 = 2 to the 5 => shift 5 times
                    addd      #FDIRBUF            ; = (32*NXTFDR)+#FDIR
                    std       SRCX                ; move file dir to FILEDR
                    ldx       #FILEDR
                    stx       DSTX
                    ldb       #$20
                    jsr       MOVE
                    lda       NXTFDR
                    sta       FDIRNM              ; Set file dir num for file handle
                    inc       NXTFDR              ; inc NXTFDR
                    rts

;*******************************************************************************
;       LOW LEVEL FILE MANIPULATION ROUTINES
;*******************************************************************************
; --------------------------------------------------------
;  ROUTINE TO CONVERT FAT TO A FORDE COMAPTIBLE FORM
; --------------------------------------------------------
;  PARAMETERS : Fat entries must be in the FAT buffer
;
;  MODIFIES   : FAT buffer, A, B, X
;
;  CALLS      : ROLD
;
;  RETURNS    : FORDE form fat at #FAT
; *--------------------------------------------------------
; DOS fat looks like the following :
;        12 34 56 .. .. .. ..
; FORDE fat looks like the following :
;        41 25 63 .. .. .. ..
; This routine performs the transition.
; See notes at end of file for further FAT information.
; *--------------------------------------------------------
; FRDFAT       pointer = #FAT (i.e. n = 0)
;                    |
;                    |----------------------<-------|
;              byte n to B, byte n+1 to A           |
;                    |                              |
;              clear high nibble of A               |
;                    |                              |
;              rotate D left by 4                   |
;                    |                              |
;              save D to stack                      |
;                    |                              |
;              byte n+1 to A                        |
;              byte n+2 to B                        |
;                    |                              |
;              rotate D left by 4                   ^
;                    |                              |
;              clear high nibble of A               |
;                    |                              |
;              save D as bytes n+1, n+2             |
;                    |                              ^
;              retrieve D from stack                |
;                    |                              |
;              save A as byte N                     |
;                    |                              |
;              OR B with byte N+1                   |
;                    |                              |
;              save as byte N+1                     |
;                    |                              |
;              pointer += 3 (i.e. n += 3)           |
;                    |                NO            |
;              pointer at end of FAT ------->-------|
;                    |
;                    | YES
;                   END
;*******************************************************************************

FRDFAT              proc
                    ldx       #FATBUF
          ;-------------------------------------- ; FIX UP NEXT 3 BYTES
Loop@@              ldb       ,x                  ; 1st byte ok
                    lda       1,x
                    anda      #$0F                ; prepare 2nd byte
                    clc
                    jsr:4     ROLD                ; complete final form
                    pshd                          ; save final form
                    ldd       1,x
                    clc
                    jsr:4     ROLD                ; complete final form
                    anda      #$0F                ; fix up first byte
                    std       1,x                 ; SAVE FIXED BYTE (3rd ONE)
                    pula                          ; retrieve 1st byte
                    sta       ,x                  ; SAVE FIXED BYTE (1st one)
                    pulb                          ; retrieve 2nd byte
                    orb       1,x
                    stb       1,x                 ; SAVE FIXED BYTE (2nd one)
                    inx:3                         ; point to next 3 bytes
                    cpx       #FATEND             ; check for END OF FAT
                    bne       Loop@@
                    rts

;*******************************************************************************
;  ROUTINE TO CONVERT FAT TO A DOS COMAPTIBLE FORM
;*******************************************************************************
;  PARAMETERS : fat entries must be in the FAT buffer
;
;  MODIFIES   : --
;
;  CALLS      : FRDFAT
;
;  RETURNS    : DOS form of FAT at #FAT
;*******************************************************************************
; FORDE fat looks like the following :
;        41 25 63 .. .. .. ..
; DOS fat looks like the following :
;        12 34 56 .. .. .. ..
; This routine performs the transition.
; See notes at end of file for further FAT information.
;*******************************************************************************

DOSFAT              proc
                    bsr:4     FRDFAT
                    bra       FRDFAT

;*******************************************************************************
;       ROUTINE TO UPDATE A FILES EOF LOCATION
;*******************************************************************************
;  PARAMETERS : current EOF in EOFS, free fat location in FATNUM
;
;  MODIFIES   : A, B, X, TEMPX
;
;  CALLS      : FNUMLOC, WFAT, RFAT
;
;  RETURNS    : FAT is updated and stored in ram
;*******************************************************************************

UDFAT               proc
                    ldd       OLDEOF              ; get old eof location
                    std       FATNUM
          ;-------------------------------------- ; update old location
                    lsrd                          ; check for an even or odd location first
                    bcs       _1@@
                    jsr       FNUMLOC             ; convert to location address in FAT buffer
                    ldx       TEMPX
                    ldd       EOF                 ; reset FATNUM variable
                    std       FATNUM
                    asld:4                        ; format for correct storage
                    sta       ,x                  ; update the old EOF location
                    orb       #$0F                ; with a pointer to the new EOF location
                    andb      1,x
                    stb       1,x
                    bra       _2@@

_1@@                jsr       FNUMLOC             ; its an odd location
                    ldx       TEMPX
                    ldd       EOF
                    std       FATNUM              ; reset FATNUM variable
                    ora       #$F0
                    anda      ,x                  ; update the old EOF location
                    sta       ,x
                    stb       1,x
          ;-------------------------------------- ; NOW MARK NEW EOF LOCATION
_2@@                ldd       FATNUM
                    lsrd                          ; check for even or odd entry number
                    bcs       _3@@                ; its an odd location
                    jsr       FNUMLOC             ; convert to location address in fat buffer
                    ldx       TEMPX
                    ldd       #$FFF0              ; save EOF marker there
                    sta       ,x                  ; update the new EOF location
                    orb       1,x
                    stb       1,x
                    rts

_3@@                jsr       FNUMLOC             ; convert to location address in fat buffer
                    ldx       TEMPX
                    ldd       #$0FFF              ; save EOF marker there
                    ora       ,x                  ; update the new EOF location
                    sta       ,x
                    stb       1,x
                    rts

;*******************************************************************************
;       ROUTINE TO FIND A FREE FAT LOCATION
;*******************************************************************************
;  PARAMETERS : FAT contents must be present in the FAT
;               buffer.
;  MODIFIES   : A, B, TEMPD
;
;  CALLS      : GENTRY
;
;  RETURNS    : FATNUM, TEMPA = FF for error
;*******************************************************************************

FFFAT               proc
                    ldd       LFFAT               ; start at the last free location found
                    std       TEMPD
Loop@@              ldd       TEMPD
                    std       FATNUM
                    jsr       GENTRY              ; get the next FAT entry
                    lda       TEMPA
                    cmpa      #$FF                ; check if last entry was read
                    jeq       FR85                ; FAT ERROR -----> ACCESS DENIED, REBOOT
                    ldd       FATNUM
                    beq       Done@@              ; check if it`s free
                    ldd       TEMPD               ; inc fat location number
                    incd
                    std       TEMPD
                    ldd       #MAXFAT             ; Have we come to the end of the FAT
                    cmpd      TEMPD
                    bne       Loop@@              ; go again
;                   clr       TEMPD
;                   clr       TEMPD+1
;                   ldd       LFFAT               ; have we checked all locations
;                   cmpd      TEMPD
;                   bne       Loop@@              ; go again
;                   bra       FR85                ; FAT ERROR -----> ACCESS DENIED, REBOOT
Done@@              ldd       TEMPD
                    std       LFFAT               ; set last free fat location
                    std       FATNUM              ; set correct FAT number
                    rts

;*******************************************************************************
;       ROUTINE TO EXPAND A FILES SPACE IN THE FAT
;*******************************************************************************
;  PARAMETERS : -------
;
;  MODIFIES   : A, B, X, TEMPX
;
;  CALLS      : FNUMLOC, WFAT, RFAT
;
;  RETURNS    : FAT is updated in ram
;*******************************************************************************

UDEOF               proc
                    bsr       FFFAT               ; find free fat location
                    lda       TEMPA               ; any found ?
                    cbeqa     #$FF,Done@@
                    ldd       EOF
                    std       OLDEOF
                    ldd       FATNUM
                    std       EOF
                    jmp       UDFAT               ; update the fat in ram
Done@@              equ       :AnRTS

;*******************************************************************************
;       ROUTINE TO FIND A FILE
;*******************************************************************************
;  PARAMETERS : File name to search for is passed through
;               FNAME
;  MODIFIES   : A, NXTFDR
;
;  CALLS      : RNFDIR, CMPNFNM
;
;  RETURNS    : EXIST = 00 if file is found
;                 "   = FF if file is not found.
;*******************************************************************************

FFILE               proc
                    lda       #$FF
                    sta       EXIST               ; assume file not found
                    clr       NXTFDR              ; start at the start
Loop@@              jsr       RNFDIR              ; read the first file dir
                    jsr       CMPNFNM             ; compare next file name
                    tsta                          ; A = 00 for match
                    beq       Found@@
                    lda       #$29                ; see it was the last one
                    cbnea     NXTFDR,Loop@@       ; check if any file dirs left to be read
                    rts

Found@@             clr       EXIST               ; file found
                    rts

;*******************************************************************************
;       ROUTINE TO FIND THE END OF A FILE
;*******************************************************************************
;  PARAMETERS : first cluster field is passed through BOF
;
;  MODIFIES   : A, B, TEMPD,FATNUM
;
;  CALLS      : GENTRY
;
;  RETURNS    : last cluster of file is returned in EOF
;               EOF = 0000 if EOF mark not found
;*******************************************************************************

FEOF                proc
                    ldd       BOF                 ; get start of file sector number
                    std       TEMPD
                    ldd       #$0FFF
                    cmpd      TEMPD               ; check if its the EOF location
                    beq       _2@@
                    ldd       #$0FF8
                    cmpd      TEMPD               ; check if its the last cluster in file chain marker
                    beq       _2@@
Loop@@              ldd       TEMPD               ; preserve fatnum value on stack
                    jeq       FR85                ; FAT ERROR -----> ACCESS DENIED, REBOOT
                                                  ; no fat entry can = 00
                    pshd
                    std       FATNUM
                    jsr       GENTRY              ; get that FAT entry
                    ldd       FATNUM
                    beq       _4@@
                    std       TEMPD
                    ldd       #$0FFF
                    cmpd      TEMPD               ; check if its the EOF location
                    beq       _3@@
                    ldd       #$0FF8
                    cmpd      TEMPD               ; check if its the last cluster in file chain marker
                    beq       _3@@
                    puld                          ; clear the stack
                    bra       Loop@@              ; try again

_2@@                ldd       BOF                 ; EOF = BOF condition
                    std       EOF
                    rts

_3@@                puld                          ; EOF found
                    std       EOF
                    rts

_4@@                puld
                    clr       EOF
                    clr       EOF+1
                    rts

;*******************************************************************************
; ROUTINE TO UPDATE THE FILE DIR  AS STORED ON THE
;               DISK
;*******************************************************************************
;  PARAMETERS : FSIZE,FDIRNM
;
;  MODIFIES   : A, B, X, TEMPA, TEMPB, TEMPD
;
;  CALLS      : ----
;
;  RETURNS    : ------
;*******************************************************************************

UDFDIR              proc
                    ldb       FDIRNM              ; FDIR location = FDIRNUM * 32 + #FDIR
                    clra
                    asld:5                        ; 32 = 2 to the 5 => shift 5 times
                    addd      #FDIRBUF
                    addd      #$001C              ; add offset to file size field
                    xgdx                          ; point to filesize field

                    ldd       FSIZE+2             ; save filesize in dos form
                    stb       ,x
                    sta       1,x
                    ldd       FSIZE
                    stb       2,x
                    sta       3,x
                    rts

;*******************************************************************************
; ROUTINE TO UPDATE THE FILE SIZE AS STORED IN RAM
;*******************************************************************************
;  PARAMETERS : FSIZE
;
;  MODIFIES   : A, B
;
;  CALLS      : ------
;
;  RETURNS    : FSIZE
;*******************************************************************************

UDFSZ               proc
                    lda       CTRLCD
                    cmpa      #'A'
                    bne       Done@@
                    ldd       FSIZE+2             ; convert to DOS form first
                    addd      BSIZE
                    std       FSIZE+2             ; from FPTR = FSIZE-1
                    bcc       Done@@
                    ldd       FSIZE
                    incd
                    std       FSIZE
Done@@              rts

;*******************************************************************************
;       INITIALIZATION ROUTINES
;*******************************************************************************
;
; --------------------------------------------------------
; ROUTINE TO INIT THE SYSTEM ON POWER UP
; --------------------------------------------------------
;  PARAMETERS : -----
;
;  MODIFIES   : PAGENUM, A
;
;  CALLS      : STOP0, RBPB, RFAT, RFDIR, RDATA
;
;  RETURNS    : the following buffers are initialized
;               with data from the disk :
;               BPB  -- disk address 00 00 1c
;               FAT  --  "     "     00 00 9c
;               FDIR --  "     "     00 06 9c
;               DATA --  "     "     00 0b 80
;*******************************************************************************

INITSYS             proc
                    jsr       STOP0               ; init all regs
                    jmp       CHKDSK              ; check if a disk is in the drive first

;*******************************************************************************

INITDSK             proc
                    jsr       RESET
                    jsr       RBPB                ; read the BPB
                    jsr       RFAT                ; read in the complete fat
                    jsr       RFDIR               ; read in the complete file dir
                    ldd       #1
                    std       PAGENUM             ; read in the first data block
                    jsr       RDATA
                    clr       FHANDLE
                    rts

;*******************************************************************************
;  ROUTINE TO INITIALIZE THE FILE HANDLE
;*******************************************************************************
;  PARAMETERS : Files file dir. shopuld be in the FILEDR
;               buffer.
;  MODIFIES   : A, B, X, all file handle fields
;
;  CALLS      : MOVE, PNTRADR, FEOF
;
;  RETURNS    : file handle fields are initialized according
;               to the info found in a files file dir.
;               D = 00 if error finding EOF
;*******************************************************************************

IFHNDL              proc
                    ldx       #FILEDR             ; point to info source
                    stx       SRCX
                    ldx       #FNAME              ; point to destination
                    stx       DSTX
                    ldb       #FNAME_SIZE
                    jsr       MOVE                ; save the file name

                    ldx       #FILEDR             ; get the file size
                    ldd       $1C,x               ; get hi word
                    xgab                          ; convert to forde form
                    std       FSIZE+2
                    ldd       $1E,x               ; get low word
                    xgab                          ; convert to forde form
                    std       FSIZE

                    clr       FPTR                ; clear the file pointer
                    clr       FPTR+1
                    clr       FPTR+2

                    ldx       #FILEDR             ; get the first cluster field
                    ldd       $1A,x
                    xgab                          ; swap it to FORDE form
                    std       BOF                 ; save the beginning of file cluster value
                    std       CENTRY              ; set current entry = BOF
                    clrd
                    std       CENUM               ; set current entry num = 00

                    ldd       CENTRY              ; get next entry
                    std       FATNUM              ; get entry at BOF
                    jsr       GENTRY
                    lda       TEMPA               ; check if there was an entry there
                    cmpa      #$FF
                    bne       IFH1                ; if no entry clear next entry var
                    clr       NENTRY
                    clr       NENTRY+1
                    bra       IFH2

IFH1                ldd       FATNUM              ; else set next entry
                    std       NENTRY

IFH2                jsr       FEOF                ; find the end of file sector
                    ldd       FATNUM
                    beq       Done@@
                    clr       LFFAT
                    clr       LFFAT+1
                    inc       FHANDLE
Done@@              rts

;*******************************************************************************
;       SERIAL COMMUNICATIONS
;*******************************************************************************

;*******************************************************************************
; Send A to serial port
;*******************************************************************************

SNDESC              proc
                    lda       #ESC                ; send escape ctrl code
;                   bra       SENDA

;*******************************************************************************

SENDA               proc
Loop@@              ldb       TRCSR2              ; Transmit/Receive CSR-2
                    bitb      #%00100000
                    beq       Loop@@              ; wait for TDRE = 1
                    sta       TDR                 ; write A to Transmit Data Reg.
                    rts

;*******************************************************************************
; Receive serial port into A
;*******************************************************************************

RECESC              proc
Loop@@              bsr       RECVA               ; wait until escape ctrl code is read from serials
                    cmpa      #ESC
                    bne       Loop@@
                    rts

;*******************************************************************************
; CHECK IF CHAR TO BE READ FROM SERIAL PORT

QSERL               proc
                    ldb       TRCSR2              ; Transmit/Receive CSR-2
                    bitb      #%01000000          ; Overun / Framing error ?
                    bne       OVRRUN
                    tstb
                    rts

;*******************************************************************************
; WAIT TO GET A CHAR FROM THE SERIAL PORT

RECVA               proc
Loop@@              bsr       QSERL
                    bpl       Loop@@              ; wait for RDRF = 1 (bit 7)
;                   bra       GSERL

;*******************************************************************************
; GET AVAILABLE CHAR FROM THE SERIAL PORT
GSERL               proc
                    lda       RDR                 ; A = Read Data reg.
                    rts

;*******************************************************************************
;       Error
;*******************************************************************************

OVRRUN              proc
                    lda       #$FF                ; SERIAL ERROR errnum
                    sta       ERRNUM
                    lda       #25
Loop@@              psha                          ; wait for about 0.5 sec to clear serial buffer
                    jsr       W20MS
                    pula
                    deca
                    bne       Loop@@
                    jmp       ERROR               ; jump to error handler ------>

;*******************************************************************************
; ROUTINE TO TRANSMIT B BYTES FROM X OVER THE SERIAL LINK
;*******************************************************************************

SEROUT              proc
                    ldd       BSIZE
                    pshd
Loop@@              lda       ,x
                    bsr       SENDA
                    inx
                    ldd       BSIZE
                    decd
                    std       BSIZE
                    bne       Loop@@
                    puld
                    std       BSIZE
                    rts

;*******************************************************************************
; ROUTINE TO RECEIVE B BYTES TO X OVER THE SERIAL LINK
;*******************************************************************************

SERIN               proc
                    ldd       BSIZE
                    pshd
Loop@@              bsr       RECVA
                    sta       ,x
                    inx
                    ldd       BSIZE
                    decd
                    std       BSIZE
                    bne       Loop@@
                    puld
                    std       BSIZE
                    rts

;*******************************************************************************
;  ROUTINE TO OPEN A FILE
;*******************************************************************************
;  PARAMETERS : SBUFF should be set up as follows ---
;               CODE FILENAME EXT
;  MODIFIES   : A, B, TEMPD
;
;  CALLS      : -------
;
;  RETURNS    : SBUFF returns as follows -
;               CODE FILEHANDLE FILESIZE
;               CODE returns as follows
;               00 ok
;               01 invalid function
;               02 file not found
;               03 path not found
;               04 too many open files
;               05 Access denied
;               0C invalid access
;*******************************************************************************

FOPEN               proc
                    ldd       #12                 ; receive the info
                    std       BSIZE
                    ldx       #SBUFF
                    bsr       SERIN

                    jsr       CHKDSK              ; check disk in drive first
                    cmpb      #$FF
                    bne       FOPN85

                    lda       FHANDLE             ; check for file already open
                    bne       FOPN84              ; sorry file already open

                    ldx       #SBUFF+1            ; set up params for ffile
                    stx       SRCX
                    ldx       #FNAME
                    stx       DSTX
                    ldb       #FNAME_SIZE
                    jsr       MOVE
                    jsr       FFILE               ; search for the file
                    lda       EXIST
                    bne       FOPN82              ; file not found !!!

                    jsr       IFHNDL              ; intialize the file handle
                    lda       FHANDLE             ; return the file handle value and file size
                    sta       SBUFF+1
                    ldd       FSIZE
                    std       SBUFF+2
                    ldd       FSIZE+2
                    std       SBUFF+4
                    clr       SBUFF
                    bra       FOPN89
          ;-------------------------------------- ; ERROR HANDLING
FOPN81              lda       #1                  ; invalid function call error
                    sta       SBUFF
                    bra       FOPN89

FOPN82              lda       #2                  ; file not foound error
                    sta       SBUFF
                    bra       FOPN89

FOPN83              lda       #3                  ; path not found error
                    sta       SBUFF
                    bra       FOPN89

FOPN84              lda       #4                  ; too many open files error
                    sta       SBUFF
                    bra       FOPN89

FOPN85              lda       #5                  ; access denied error
                    sta       SBUFF
                    bra       FOPN89

                    rts

FOPN8C              lda       #$0C                ; invalid access error
                    sta       SBUFF

FOPN89              jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC
                    ldx       #SBUFF
                    ldd       #6
                    std       BSIZE
                    jsr       SEROUT
                    lda       SBUFF
                    beq       :AnRTS
                    sta       ERRNUM
                    jmp       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS

;*******************************************************************************
;  ROUTINE TO CLOSE A FILE
;*******************************************************************************
;  PARAMETERS : SBUFF should be set up as follows ---
;               FILEHANDLE
;  MODIFIES   : A, B, TEMPD
;
;  CALLS      : -------
;
;  RETURNS    : SBUFF returns as follows -
;               CODE
;               CODE returns as follows
;               00 ok
;               06 invalid handle
;*******************************************************************************

FCLOSE              proc
                    jsr       RECVA               ; get the file handle value
                    cmpa      FHANDLE
                    bne       FCLS8               ; invalid file handle value error

                    ldb       #37                 ; clear out the whole file handle
                    ldx       #FNAME
                    jsr       CLRXB
                    clr       SBUFF
                    bra       FCLS89

FCLS8               lda       #6                  ; INVALID FILE HANDLE ERROR CODE
                    sta       SBUFF
FCLS89              jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC
                    lda       SBUFF
                    jsr       SENDA
                    tsta
                    bne       :AnRTS
                    sta       ERRNUM
                    jmp       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS

;*******************************************************************************
;  ROUTINE TO MOVE THE FILE POINTER
;*******************************************************************************
;  PARAMETERS : SBUFF should be set up as follows ---
;               CODE FILEHANDLE OFFSET
;               code may have the following values :
;               00 offset from BOF
;               01 offset from current position (forward)
;               03 offset from current position (reverse)
;               03 offset from EOF
;  MODIFIES   : A, B, TEMPD
;
;  CALLS      : -------
;
;  RETURNS    : FPTR is adjusted to required value
;               SBUFF returns as follows -- CODE FPTR
;               TEMPA returns as follows :
;               00 ok
;               01 invalid function
;               06 invalid handle
;*******************************************************************************

MFPTR               proc
                    ldd       #5                  ; read info first
                    std       BSIZE
                    ldx       #SBUFF
                    jsr       SERIN

                    lda       SBUFF+1             ; check for correct file handle value
                    cmpa      FHANDLE
                    jne       MFP86               ; invalid file handle error

                    lda       SBUFF               ; get the code
                    beq       MFPTR0              ; offset from bof
                    cbeqa     #1,MFPTR1           ; offset from current fptr (forward)
                    cbeqa     #2,MFPTR2           ; offset from fptr (reverse)
                    cjnea     #3,MFP81            ; invalid function call error
                    jmp       MFPTR3              ; offset from EOF
          ;-------------------------------------- ; OFFSET FROM BOF
MFPTR0              ldd       FSIZE+2             ; get num bytes from file pointer to EOF
                    subd      FPTR+1
                    std       SBUFF+6
                    bcc       MFPA
                    lda       FSIZE+1
                    deca
                    bra       MFPB

MFPA                lda       FSIZE+1
MFPB                suba      FPTR
                    sta       SBUFF+5

                    ldd       SBUFF+6             ; see if it's greater than amount required to be read
                    subd      SBUFF+3
                    bcc       MFP03               ; OK wont go beyond EOF
                    lda       SBUFF+5
                    deca
                    bcs       MFP02
                    suba      SBUFF+2
                    bcc       MFP03               ; OK wont go beyond EOF

MFP02               ldd       FSIZE+1             ; offset value too big
                    std       FPTR                ; set pointer to EOF
                    lda       FSIZE+3
                    sta       FPTR+2
                    bra       MFPTR4

MFP03               ldd       SBUFF+2             ; offset from bof
                    std       FPTR                ; filepointer = offset value
                    lda       SBUFF+4
                    sta       FPTR+2
                    bra       MFPTR4
          ;-------------------------------------- ; OFFSET FROM FPTR (FORWARD)
MFPTR1              ldd       FPTR+1              ; offset from current file pointer value
                    addd      SBUFF+3             ; file pointer += offset value
                    std       FPTR+1
                    bcc       MFP11
                    inc       FPTR
MFP11               lda       FPTR
                    adda      SBUFF+2
                    sta       FPTR

                    ldd       FSIZE+2             ; get num bytes from file pointer to EOF
                    subd      FPTR+1
                    bcc       MFPC
                    lda       FSIZE+1
                    deca
                    bcs       MFP14
                    bra       MFPD

MFPC                lda       FSIZE+1
MFPD                suba      FPTR
                    bcc       MFPTR4              ; OK wont go beyond EOF

MFP14               ldd       FSIZE+1             ; offset value too big
                    std       FPTR                ; set pointer to EOF
                    lda       FSIZE+3
                    sta       FPTR+2
                    bra       MFPTR4
          ;-------------------------------------- ; OFFSET FROM FPTR (REVERSE)
MFPTR2              ldd       FPTR+1              ; Back wards from current file pointer value
                    subd      SBUFF+3             ; file pointer -= offset value
                    std       FPTR+1
                    bcc       MFP21
                    inc       SBUFF+2
MFP21               lda       FPTR
                    suba      SBUFF+2
                    sta       FPTR
                    bcc       MFPTR4              ; check that we're not gone before the BOF
                    clr       FPTR
                    clr       FPTR+1
                    clr       FPTR+2
                    bra       MFPTR4
          ;-------------------------------------- ; OFFSET FROM EOF
MFPTR3              ldd       FSIZE+2             ; offset from eof
                    subd      SBUFF+3             ; file pointer = file size - offset value
                    std       FPTR+1
                    bcc       MFP31
                    inc       SBUFF+2
MFP31               lda       FSIZE+1
                    suba      SBUFF+2
                    sta       FPTR
                    bcc       MFPTR4              ; ensure offset wasn't greater than the file size
                    clr       FPTR
                    clr       FPTR+1
                    clr       FPTR+2

MFPTR4              clr       SBUFF
                    ldd       FPTR
                    std       SBUFF+1
                    lda       FPTR+2
                    sta       SBUFF+3
                    bra       MFP89
          ;-------------------------------------- ; ERROR HANDLING
MFP81               lda       #1                  ; invalid function
                    sta       SBUFF
                    bra       MFP89

                    rts

MFP86               lda       #6                  ; invalid handle
                    sta       SBUFF
MFP89               jsr       PROMPT              ; debuging prompt
                    ldd       #4
                    std       BSIZE
                    jsr       SNDESC
                    ldx       #SBUFF
                    jsr       SEROUT
                    lda       SBUFF
                    beq       :AnRTS
                    sta       ERRNUM
                    jmp       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS

;*******************************************************************************
;  ROUTINE TO RENAME A FILE
;*******************************************************************************
;  PARAMETERS : SBUFF should be set up as follows ---
;               original filename(11 bytes) new filename(11 bytes)
;  MODIFIES   : A, B, X, DSTX, SRCX, FNAME, TEMPA
;
;  CALLS      : FFILE, MOVE
;
;  RETURNS    : TEMPA returns with 1 of the following values --
;               00 - ok
;               02 - file not found
;               03 - path not found
;               05 - access denied
;               11 - not same device
;*******************************************************************************

FRNAME              proc
                    ldd       #22                 ; read in 2 file names
                    std       BSIZE
                    ldx       #BUFFER
                    jsr       SERIN

                    jsr       CHKDSK              ; check disk in drive first
                    cmpb      #$FF
                    bne       FRN85

                    ldx       #BUFFER             ; set up for find file function
                    stx       SRCX
                    ldx       #FNAME
                    stx       DSTX
                    ldb       #FNAME_SIZE
                    jsr       MOVE
                    jsr       FFILE               ; search for the file
                    lda       EXIST
                    bne       FRN82               ; file not found error

                    ldb       FDIRNM              ; calculate offset to correct file dir location
                    clra                          ; FDIR location = FDIRNM * 32 + #FDIR
                    asld:5                        ; 32 = 2 to the 5 => shift 5 times
                    addd      #FDIRBUF            ; D now holds correct location
                    std       DSTX
                    ldx       #BUFFER+FNAME_SIZE
                    stx       SRCX
                    ldb       #FNAME_SIZE
                    jsr       MOVE                ; File has now been renamed
                    jsr       WFDIR
                    ldd       #1
                    std       PAGENUM
                    clr       SBUFF
                    bra       FRN89
          ;-------------------------------------- ; ERROR HANDLING
FRN82               lda       #2                  ; file not found error
                    sta       SBUFF
                    bra       FRN89

FRN83               lda       #3                  ; path not found error
                    sta       SBUFF
                    bra       FRN89

FRN85               lda       #5                  ; access denied error
                    sta       SBUFF
                    bra       FRN89
                    rts

FRN8B               lda       #$0B                ; not the same device error (WAS: 0B, possibly mis-scanned)
                    sta       SBUFF
FRN89               jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC
                    lda       SBUFF
                    jsr       SENDA
                    tsta
                    beq       :AnRTS
                    sta       ERRNUM
                    jmp       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS

;*******************************************************************************
;  ROUTINE TO READ FROM A FILE
;*******************************************************************************
;  PARAMETERS : SBUFF should be set up as follows ---
;               FILEHANDLE NUMBYTES
;  MODIFIES   : A, B, TEMPD
;
;  CALLS      : -------
;
;  RETURNS    : SBUFF returns as follows -
;               CODE NUMBYTES
;               DATA bytes are returned in BUFFER
;               CODE returns as follows
;               00 ok
;               05 Access denied
;               06 invalid handle
;               NUMBTYES = num bytes read,
;                  "     = 00 => already at EOF
;                  "     < passed value => EOF encountered
;                          during read.
;*******************************************************************************

FREAD               proc
                    ldd       #3                  ; get params from moneychanger
                    std       BSIZE
                    ldx       #SBUFF
                    jsr       SERIN

                    jsr       CHKDSK              ; check disk in drive first
                    cmpb      #$FF
                    jne       FR85

                    lda       SBUFF               ; check for correct file handle
                    cmpa      FHANDLE
                    jne       FR86                ; illegal file handle

                    ldd       FSIZE+2             ; get num bytes from file pointer to EOF
                    subd      FPTR+1
                    std       SBUFF+6
                    bcc       FR1A
                    lda       FSIZE+1
                    deca
                    bra       FR1B

FR1A                lda       FSIZE+1
FR1B                suba      FPTR
                    sta       SBUFF+5

                    ldd       SBUFF+6             ; see if it's greater than amount required to be read
                    subd      SBUFF+1
                    bcc       FR1C                ; OK wont go beyond EOF
                    lda       SBUFF+5
                    suba      #1                  ; cant use deca cause it dont affect the carry
                    sta       SBUFF+5
                    bcc       FR1C                ; OK wont go beyond EOF
                    ldd       SBUFF+6             ; set num bytes = amount left
                    bra       FR1D

FR1C                ldd       SBUFF+1             ; get numbytes to be read
FR1D                std       TEMPX               ; divide it up into 512 byte sections
                    clr       TEMPD
                    clr       TEMPD+1
                    ldd       #SECTOR_SIZE
                    std       TEMPA
                    jsr       DIV4B2

                    ldd       TEMPX               ; set counter to num 512 byte blocks
                    std       COUNTER
                    ldd       TEMPD               ; save the remainder
                    psha
                    pshb
                    psha
                    pshb

                    jsr       SNDESC              ; let him known the data is comming
                    clra                          ; no errors
                    jsr       SENDA

FR2                 ldd       COUNTER             ; all blocks read yet ?
                    beq       FR3
                    decd                          ; update counter
                    std       COUNTER
                    ldd       #SECTOR_SIZE        ; Set block size
                    std       BSIZE
                    lda       #'P'
                    sta       CTRLCD
                    clr       WFLAG
                    jsr       RFMID               ; read next block of data from the file

                    lda       DBUGF               ; dont echo if we are not debuging
                    anda      #%00000010
                    beq       FR21
                    ldx       #BUFFER
                    ldd       #SECTOR_SIZE
                    std       BSIZE
                    jsr       BUFOUT

FR21                ldx       #BUFFER
                    ldd       #SECTOR_SIZE
                    std       BSIZE
                    jsr       SEROUT              ; output next block of data to the moneychanger
                    bra       FR2                 ; any more to read ??

FR3                 pulb                          ; retrieve remainder
                    pula
                    subd      #0
                    beq       FR4
                    std       BSIZE
                    lda       #'P'
                    sta       CTRLCD
                    clr       WFLAG
                    jsr       RFMID               ; read last few bytes

                    lda       DBUGF               ; dont echo if we are not debuging
                    anda      #%00000010
                    beq       FR4
                    ldx       #BUFFER
                    pulb
                    pula
                    psha
                    pshb
                    std       BSIZE
                    jsr       BUFOUT

FR4                 ldx       #BUFFER
                    pulb
                    pula
                    subd      #0
                    beq       FR5
                    std       BSIZE
                    jsr       SEROUT              ; output last few bytes to moneychanger
FR5                 lda       SBUFF+1             ; output numbytes read
                    jsr       SENDA               ; hi byte
                    lda       SBUFF+2             ; low byte
                    jmp       SENDA
          ;-------------------------------------- ; ERROR HANDLING
FR85                jsr       SNDESC              ; access denied error
                    lda       #5
                    sta       ERRNUM
                    jsr       SENDA
                    clra                          ; no bytes read
                    jsr       SENDA
                    clra                          ; no bytes read
                    jsr       SENDA
                    jsr       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS
FR86                jsr       SNDESC              ; illegal file handle error
                    lda       #6
                    sta       ERRNUM
                    jsr       SENDA
                    clra                          ; no bytes read
                    jsr       SENDA
                    clra                          ; no bytes read
                    jsr       SENDA
                    jmp       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS

;*******************************************************************************
;  ROUTINE TO WRITE TO A FILE
;*******************************************************************************
;  PARAMETERS : SBUFF should be set up as follows ---
;               FILEHANDLE NUMBYTES(MAX = 512)
;               DATA bytes should also be located in BUFFER
;  MODIFIES   : A, B, TEMPD
;
;  CALLS      : -------
;
;  RETURNS    : SBUFF returns as follows -
;               CODE NUMBYTES
;               CODE returns as follows
;               00 ok
;               05 Access denied
;               06 invalid handle
;               NUMBTYES = num bytes written, if less than
;               passed NUMBYTES then disk is full and only
;               a partial write occured.
;*******************************************************************************

FWRITE              proc
                    jsr       RECVA               ; get file handle num
                    sta       SBUFF
                    jsr       RECVA               ; get num bytes
                    psha                          ; hi byte
                    sta       BSIZE
                    jsr       RECVA               ; get num bytes
                    psha                          ; save for later
                    sta       BSIZE+1             ; low byte
                    ldx       #BUFFER
                    jsr       SERIN               ; get the data bytes

                    jsr       CHKDSK              ; check disk in drive first
                    cmpb      #$FF
                    jne       FW85

                    lda       FHANDLE
                    cmpa      SBUFF
                    jne       FW86

                    lda       #'W'                ; set the write flag
                    sta       WFLAG

                    ldd       FSIZE+2             ; check if write will take us beyond the current EOF
                    subd      FPTR+1              ; get numbytes to EOF
                    std       SBUFF+1
                    bcc       FW1
                    lda       FSIZE+1             ; have to it this way 'cause FSIZE & FPTR must not
                    deca                          ; be changed
                    bra       FW2

FW1                 lda       FSIZE+1
FW2                 suba      FPTR
                    sta       SBUFF               ; numbytes to eof = sbuff0,1,2

                    ldd       SBUFF+1             ; check if BSIZE is greater than num bytes to EOF
                    subd      BSIZE
                    bcc       FW3                 ; bsize < numbytes to EOF => write from fptr

                    lda       #'P'                ; write from file pointer to EOF
                    sta       CTRLCD              ; set the write mode flag depending on fptr value
                    ldd       SBUFF+1             ; get correct bsize
                    beq       FW2A
                    std       BSIZE
                    jsr       WFMID               ; write the data to the file

FW2A                lda       #$FF
                    sta       APNDF               ; set append flag
                    lda       #'A'                ; append remainder to file
                    sta       CTRLCD              ; set the write mode flag depending on fptr value
                    pulb                          ; get correct bsize
                    pula
                    psha                          ; save again
                    pshb
                    subd      SBUFF+1             ; calculate amount left to append
                    std       BSIZE
          ;-------------------------------------- ; INCLUDE THIS IF $1A EOF IS TO BE APPENDED TO FILES
;                   addd      #BUFFER             ; put EOF marker at end of file
;                   xgdx
;                   lda       #$1A
;                   sta       ,x
;                   ldd       BSIZE
;                   incd
;                   std       BSIZE
          ;--------------------------------------
                    ldx       #BUFFER             ; move data to be appended to start of buffer
                    stx       DSTX
                    ldd       SBUFF+1
                    beq       FW2B
                    abx
                    dex
                    stx       SRCX
                    ldd       BSIZE
                    jsr       DMOVE
FW2B                jsr       WFMID               ; write the data to the file
                    bra       FW5

FW3                 clr       APNDF               ; clear the append flag
                    lda       #'P'                ; write new data from file pointer
                    sta       CTRLCD              ; set the write mode flag depending on fptr value
                    pulb                          ; get correct bsize
                    pula
                    psha                          ; save again
                    pshb
                    std       BSIZE
                    jsr       WFMID               ; write the data to the file

FW5                 jsr       WDATA
                    lda       APNDF               ; fdir and fat only updated if we are apending
                    beq       FW52

FW51                jsr       UDFDIR              ; only do these when appending
                    jsr       WFDIR
                    jsr       WFAT
                    lda       FHANDLE
                    psha
                    jsr       INITDSK
                    pula
                    sta       FHANDLE

FW52                jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC              ; return values
                    clra
                    jsr       SENDA               ; no errors
                    pula
                    sta       SBUFF
                    pula                          ; hi byte
                    jsr       SENDA               ; numbytes written
                    lda       SBUFF               ; low byte
                    jmp       SENDA
          ;-------------------------------------- ; ERROR HANDLING
FW85                jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC
                    lda       #5                  ; access denied error
                    sta       ERRNUM
                    jsr       SENDA
                    clra
                    jsr:2     SENDA
                    bsr       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS
;                   rts

FW86                jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC
                    lda       #6                  ; illegal file handle error
                    sta       ERRNUM
                    jsr       SENDA
                    clra
                    jsr:2     SENDA
                    bra       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS

;*******************************************************************************
;  ROUTINE TO GET THE AVAILABLE DISK SPACE ON A DISK
;*******************************************************************************
;  PARAMETERS : BPB should have been already read in and
;               a disk should be in the default drive
;  MODIFIES   : A, B, X, SBUFF
;
;  CALLS      : CHKDSK, FFFAT
;
;  RETURNS    : SBUFF is returned containing the following
;               data
;              1) CODE    ---  FF if no disk in drive
;                            00 if disk in drive
;              2) SECTORS/CLUSTER
;              3) CLUSTERS AVAILABLE
;              4) BYTES/SECTOR
;              5) CLUSTERS/DISK
;
;  NOTE       : free space =  2 * 3 * 4
;               total space = 4 * 2 * 5
;               items 2-5 become meaningless if CODE = FF
;*******************************************************************************

GDSPACE             proc
                    jsr       CHKDSK              ; check if a disk is in the drive first
                    cmpb      #$FF
                    bne       GDSP8               ; sorry no disk

                    ldx       #BPBBUF+$0B
                    lda       2,x                 ; SECTORS/CLUSTER
                    sta       SBUFF
                    ldd       ,x                  ; BYTES/SECTOR
                    xgab
                    std       SBUFF+3
                    ldd       8,x                 ; CLUSTERS/DISK
                    xgab
                    subd      #9                  ; 07 sector offset to data and 2 unused
                    std       SBUFF+5

                    jsr       FFFAT               ; get num sectors available
                    ldd       SBUFF+5
                    subd      LFFAT
                    addd      #2                  ; and 2 unused
                    std       SBUFF+1
                    jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC
                    clra
                    jsr       SENDA
                    ldd       #7
                    std       BSIZE
                    ldx       #SBUFF
                    jmp       SEROUT

GDSP8               jsr       PROMPT              ; debuging prompt
                    jsr       SNDESC
                    lda       #$FF                ; code for no disk present error
                    jsr       SENDA
                    ldd       #7
                    std       BSIZE
                    ldx       #SBUFF
                    jmp       SEROUT
                    rts

;*******************************************************************************
;       CRITICAL FILE ERROR HANDLER ROUTINE
;*******************************************************************************

FERROR              proc
                    lda       ERRNUM
                    beq       Done@@
                    lda       DBUGF               ; are we debuging ??
                    jeq       POWER               ; --------------------> POWER
                    lda       #'E'
                    jsr       $F009               ; OUTCH
                    lda       ERRNUM
                    jsr       $F23E               ; OUT ERROR NUMBER AS HEX VALUE
                    jmp       $F003               ; --------------------> FBUG WARM START
Done@@              equ       :AnRTS

;*******************************************************************************
;       SCREEN I\O FUNCTION ROUTINES
;*******************************************************************************

;*******************************************************************************
; PRINT AN ALPHA-NUMERIC CHAR

PCHAR               proc
                    cbeqa     #CR,Done@@          ; allow cr
                    cbeqa     #LF,Done@@          ; allow lf
                    cbeqa     #TAB,Done@@         ; allow tab
                    cbeqa     #ESC,Done@@         ; allow escape
                    cmpa      #' '                ; $20 <= ALPHA-NUMERIC <= $7E
                    bcs       Dot@@
                    cmpa      #$7E
                    bls       Done@@
Dot@@               lda       #'.'
Done@@              jmp       PUTCH

;*******************************************************************************
; PRINT A 2 BYTE HEX NUM. FOLLOWED BY A SPACE

P2HEX               proc
                    psha
                    jsr       OUTHL               ; print as a hex num
                    pula
                    jsr       OUTHR
          ;-------------------------------------- PRINT A BLANK SPACE
PSPC                lda       #' '
                    bra       PCHAR
          ;-------------------------------------- PRINT CARRAIGE RETURN, LINE FEED
CRLF                lda       #CR                 ; CR
                    bsr       PCHAR               ; print it
                    lda       #LF                 ; LF
                    bra       PCHAR               ; print it
          ;-------------------------------------- PRINT B BYTES FROM X TO THE SCREEN
PSTR                lda       ,x                  ; get next char
                    bsr       PCHAR               ; print it
                    inx
                    decb                          ; finished yet
                    bne       PSTR
                    rts

;*******************************************************************************
; PRINT THE BPB TO THE SCREEN

PBPB                proc
                    lda       DBUGF
                    anda      #%00010000
                    beq       Done@@
                    ldx       #BPBBUF
Loop@@              lda       ,x
                    bsr       PCHAR
                    inx
                    cpx       #BPBEND
                    bne       Loop@@
Done@@              rts

;*******************************************************************************
; PRINT THE FAT TO THE SCREEN

PFAT                proc
                    lda       DBUGF
                    anda      #%00001000
                    beq       Done@@
                    ldx       #FATBUF
Loop@@              lda       ,x
                    bsr       P2HEX
                    inx
                    cpx       #FATEND
                    bne       Loop@@
Done@@              rts

;*******************************************************************************
; PRINT THE FILE DIR. TO THE SCREEN

PFDIR               proc
                    lda       DBUGF
                    anda      #%00000100
                    beq       Done@@
                    ldx       #FDIRBUF
Loop@@              ldb       #32
                    bsr       PSTR
                    bsr       CRLF
                    cpx       #FDIREND
                    bne       Loop@@
Done@@              rts

;*******************************************************************************
; PRINT THE FILE DIR. TO THE SCREEN

PDATA               proc
                    lda       DBUGF
                    anda      #%00000010
                    beq       Done@@
                    ldx       #DATA
Loop@@              lda       ,x
                    jsr       PCHAR
                    inx
                    cpx       #TOPRAM
                    bne       Loop@@
Done@@              rts

;*******************************************************************************
; PRINT COMMAND NAME TO THE SCREEN

PCMND               proc
                    lda       DBUGF
                    anda      #%00100000
                    beq       Done@@
                    ldx       #PRMBUF             ; get the command number and print it to the screen
                    ldb       ,x
                    andb      #%00011111          ; extract command number
                    clra
                    std       TEMPD               ; multiply by 21
                    ldd       #CMNDTBL_ENTRY_SIZE
                    std       TEMPA
                    jsr       MLTPLY
                    ldd       TEMPD
                    addd      #CMNDTBL            ; add to start of command table to get the offset
                    xgdx
                    ldb       #CMNDTBL_ENTRY_SIZE
                    jsr       PSTR                ; print the command name
                    jmp       CRLF
Done@@              equ       :AnRTS

;*******************************************************************************
; PRINT B RESULTS TO THE SCREEN

PRSLT               proc
                    lda       DBUGF               ; check for test mode first
                    anda      #%01000000
                    beq       Done@@

                    lda       FDCCMND             ; set correct number for command
                    cmpa      #8                  ; sense interrupt
                    bne       PRS1
                    ldb       #2
                    bra       PRS3

PRS1                cmpa      #4                  ; sense drive
                    bne       PRS2
                    ldb       #1
                    bra       PRS3

PRS2                cbeqa     #7,Done@@           ; recalibrate
                    cbeqa     #$0F,Done@@         ; seek

                    ldb       #7                  ; all other commands
PRS3                ldx       #RESBUF
PRS4                lda       ,x                  ; get next result
                    jsr       P2HEX               ; print as a hex num
                    inx
                    decb
                    bne       PRS4
                    jmp       CRLF                ; new line
Done@@              equ       :AnRTS

;*******************************************************************************
;   ------   PRINT ERROR FOR STATUS REG B TO THE SCREEN ----
;               A != 00 IF THERE IS AN ERROR
;               Bhi = ERROR BLOCK, Blo = ERROR MESSAGE

PERROR              proc
                    tsta
                    beq       Done@@
                    psha
                    lda       DBUGF
                    bne       PERR1               ; debuging mode

                    pula                          ; not debuging
                    lda       FDCCMND             ; ignore (SNSIRQ) recallibration errors - sometimes takes
                    cmpa      #%00001000          ; two efforts to recalibrate
;                   cmpa      #%00000111          ; two efforts to recalibrate
                    beq       Done@@
                    jsr       SNDESC              ; call MC3
                    lda       #$FF
                    jsr       SENDA               ; to say that a serious error occured with the disk
                    jmp       POWER               ; reboot system

PERR1               pshb                          ; save error number
                    lsrb:4                        ; B = the error within the block
                    clra
                    std       TEMPD
                    ldd       #14*ERROR_MSG_SIZE  ; 14*60 chars in each error block
                    std       TEMPA
                    jsr       MLTPLY
                    ldd       #INTCDS
                    addd      TEMPD
                    xgdx                          ; X now holds start address of error block
                    pulb
                    pshx
                    andb      #%00001111          ; extract error within the block
                    clra
                    std       TEMPD
                    ldd       #2*ERROR_MSG_SIZE   ; WAS: 120 (POSSIBLY 2*ERR.. is wrong)
                    std       TEMPA
                    jsr       MLTPLY
                    pulx
                    xgdx                          ; X now points to correct error message
                    addd      TEMPD
                    xgdx                          ; X now points to correct error message
                    pula
                    tsta
                    beq       PERR2
                    ldb       #ERROR_MSG_SIZE     ; WAS: 60
                    abx
PERR2               ldb       #ERROR_MSG_SIZE     ; WAS: 60
                    jsr       PSTR                ; print the string
                    jmp       CRLF
Done@@              rts

;*******************************************************************************

PROMPT              proc
                    lda       DBUGF
                    beq       Done@@
                    lda       #'*'
                    jmp       $F009
Done@@              equ       :AnRTS

;*******************************************************************************

BUFOUT              proc
                    lda       DBUGF
                    beq       Done@@
Loop@@              lda       ,x
                    jsr       PCHAR
                    inx
                    ldd       BSIZE
                    decd
                    std       BSIZE
                    bne       Loop@@
Done@@              rts

;*******************************************************************************
;      TEST ROUTINES TO CALL THE FDC COMMANDS
;*******************************************************************************

RSTFDC              proc
                    bclr      PORT5,#%01000000    ; reset the FDC first
                    jsr       W400
                    bset      PORT5,#%01000000
                    brset     PORT5,#01,Done@@    ; check for irq = 0
                    jmp       IRQSNS              ; interrupt maybe generated if RDY is active during reset
Done@@              equ       :AnRTS

;*******************************************************************************

RESET               proc
                    bsr       RSTFDC
                    lda       #80                 ; now reset the FDD as well
                    sta       SDTRKS
                    lda       #18
                    sta       TRKSCS
                    lda       #1
                    sta       US0
                    clra
                    sta       US1
                    bsr       SPEC
                    jsr       RECAL
                    jmp       RECAL

;*******************************************************************************

SPEC                proc
                    lda       #$D0
                    sta       SRT                 ; HI NIBBLE ONLY VALID
                    lda       #1
                    sta       HUT                 ; LOW NIBBLE ONLY VALID
                    lda       #20                 ; 20mS head load time
                    sta       HLT                 ; BIT 1 TO 7 ONLY VALID
                    lda       #1
                    sta       DMA                 ; BIT 0 ONLY VALID
                    jmp       SPECIFY

;*******************************************************************************
; SENSE DRIVE STATUS

GETDRV              proc
                    lda       #%00000000
                    sta       HEAD
                    lda       #1
                    sta       US0
                    clra
                    sta       US1
                    bsr       SPEC
                    lda       #10
                    ldb       HEAD
                    bsr       SEEKTO
                    bsr       SPEC
                    jmp       SNSDRV

;*******************************************************************************
; READ ID

READID              proc
                    lda       #%00000000
                    sta       HEAD
                    lda       #1
                    sta       US0
                    clra
                    sta       US1
                    bsr       SPEC
                    lda       #10
                    ldb       HEAD
                    bsr       SEEKTO
                    bsr       SPEC
                    jmp       RDID

;*******************************************************************************
; SEEK TO TRACK A, SIDE B

SEEKTO              proc
                    sta       TRACK
                    stb       HEAD
                    lda       #1
                    sta       US0
                    clra
                    sta       US1
                    lda       HEAD
                    cbeqa     LASTHD,STO1
                    sta       LASTHD
                    bsr       RESET

STO1                bsr       SPEC
                    jmp       SEEK

;*******************************************************************************
;      ROUTINE TO FORMAT A DISK
;*******************************************************************************
;  PARAMETERS :
;  MODIFIES   :
;  CALLS      :
;  RETURNS    :
;*******************************************************************************

FORMAT              proc
                    lda       #1                  ; select first drive
                    sta       US0
                    clr       US1
                    lda       #2                  ; 512 bytes per sector
                    sta       SECBYTES
                    lda       #$54
                    sta       GAP3
                    clr       TRACK               ; start at the beginning
                    jsr       MTRON

FRMT1               clr       HEAD                ; NEXT TRACK, SIDE 0
                    jsr       SEEK
                    lda       #1
                    sta       SECTOR
                    jsr       FRMTTRK
                    lda       #1                  ; SAME TRACK, SIDE 1
                    sta       HEAD
                    jsr       SEEK
                    lda       #1
                    sta       SECTOR
                    jsr       FRMTTRK
                    lda       TRACK
                    inca
                    sta       TRACK
                    cmpa      #80
                    bne       FRMT1
                    jsr       MTROFF

                    jsr:2     RESET
                    ldb       #$FF
                    stb       $B001
                    jsr       WBPB                ; write the formatted bpb
                    jsr       CLRFAT
                    ldx       #FATMAP             ; format the fat
                    stx       SRCX
                    ldx       #FATBUF
                    stx       DSTX
                    ldb       #4
                    jsr       MOVE
                    jsr       WFAT                ; write the formatted fat
                    jsr       CLRFDR
                    ldx       #FDRMAP             ; format the FILEDIR
                    stx       SRCX
                    ldx       #FDIRBUF
                    stx       DSTX
                    ldb       #28
                    jsr       MOVE
                    jsr       WFDIR               ; write the formatted FILEDIR
                    jsr       SNDESC
                    clra
                    jmp       SENDA

;*******************************************************************************
;      ROUTINE TO TEST FOR A DISK IN THE DRIVE
;*******************************************************************************
;  PARAMETERS :  ---------
;
;  MODIFIES   :
;
;  CALLS      :
;
;  RETURNS    : B = 00 if disk in, B = $FF for no disk
;*******************************************************************************

CHKDSK              proc
                    ldb       #10                 ; read DISKCHANGE for a low level
                    lda       #$FF
                    sta       DISKIN              ; set count = 10, assume disk is in

CHK1                lda       PORT5
                    anda      #%00100000          ; bit 5 = diskchange line
                    bne       CHK2                ; if still high try again

                    clr       DISKIN              ; no disk present
                    lda       TRACK               ; see if stepping resets it
                    inca                          ; try stepping to next track
                    clrb
                    jsr       SEEKTO

CHK2                decb                          ; try 10 times
                    bne       CHK1

                    ldb       DISKIN
                    beq       CHK4                ; no disk in

                    lda       DSKPRS              ; disk is in now - but was it in last time
                    bne       CHK3                ; yes
                    lda       #1
                    sta       DSKCHNG             ; set disk changed flag
                    sta       DSKPRS              ; set disk present flag
                    clr       INITF               ; clear system inited falg
                    bra       CHK5

CHK3                clr       DSKCHNG             ; clear disk changed flag
                    lda       #1
                    sta       DSKPRS              ; set disk present flag
                    bra       CHK5

CHK4                lda       #1                  ; set disk changed flag
                    sta       DSKCHNG
                    clr       DSKPRS              ; clear disk present flag
                    clr       INITF               ; clear system inited flag

CHK5                lda       DSKPRS              ; ACT ON THE RESULTS
                    beq       CHK9                ; no disk in
                    lda       DSKCHNG
                    bne       CHK6                ; disk was changed
                    lda       INITF
                    bne       CHK9                ; disk already initialized
CHK6                jsr       INITDSK             ; read critical disk areas
                    lda       #1
                    sta       INITF
CHK9                ldb       DISKIN
                    rts

;*******************************************************************************
;      ROUTINE TO TEST THE FDD HEAD ACTION
;*******************************************************************************
;  PARAMETERS :  ---------
;
;  MODIFIES   :
;
;  CALLS      :
;
;  RETURNS    : ---------
;*******************************************************************************

HDTST               proc
                    lda       #2                  ; TRACK 0
Loop@@              clrb                          ; HEAD 0
                    suba      #2                  ; butterfly test - 0,3,1,4,2,5,......
                    psha
                    jsr       SEEKTO
                    pula
                    adda      #3
                    psha
                    clrb
                    jsr       SEEKTO
                    pula
                    cmpa      #80
                    bcs       Loop@@

                    clr       TEMPA
                    lda       #80
                    sta       TEMPB
Loop@@@             clrb
                    lda       TEMPA
                    jsr       SEEKTO
                    inc       TEMPA
                    clrb
                    lda       TEMPB
                    jsr       SEEKTO
                    dec       TEMPB
                    bne       Loop@@@
                    rts

;*******************************************************************************
;     BPB MAP, FAT MAP, FDIR MAP FOR FORMATTING
;*******************************************************************************

BPBMAP              fcb       $EB,$3C,$90,$4D,$53,$44,$4F,$53,$35,$2E,$30,$00
                    fcb       $02,$01,$01,$00,$02,$E0,$00,$40,$0B,$F0,$09,$00
                    fcb       $12,$00,$02,$00,$00,$00,$00,$00,$00,$00,$00,$00
                    fcb       $00,$00,$29,$F5,$14,$30,$36,$53,$4B,$59,$54,$45
                    fcb       $4C,$4C,$45,$52,$20,$20,$46,$41,$54,$31,$32,$20
                    fcb       $20,$20,$FA,$33,$C0,$8E,$D0,$BC,$00,$7C,$16,$07
                    fcb       $BB,$78,$00,$36,$C5,$37,$1E,$56,$16,$53,$BF,$3E
                    fcb       $7C,$B9,$0B,$00,$FC,$F3,$A4,$06,$1F,$C6,$45,$FE
                    fcb       $0F,$8B,$0E,$18,$7C,$88,$4D,$F9,$89,$47,$02,$C7
                    fcb       $07,$3E,$7C,$FB,$CD,$13,$72,$79,$33,$C0,$39,$06
                    fcb       $13,$7C,$74,$08,$8B,$0E,$13,$7C,$89,$0E,$20,$7C
                    fcb       $A0,$10,$7C,$F7,$26,$16,$7C,$03,$06,$1C,$7C,$13
                    fcb       $16,$1E,$7C,$03,$06,$0E,$7C,$83,$D2,$00,$A3,$50
                    fcb       $7C,$89,$16,$52,$7C,$A3,$49,$7C,$89,$16,$4B,$7C
                    fcb       $B8,$20,$00,$F7,$26,$11,$7C,$8B,$1E,$0B,$7C,$03
                    fcb       $C3,$48,$F7,$F3,$01,$06,$49,$7C,$83,$16,$4B,$7C
                    fcb       $00,$BB,$00,$05,$8B,$16,$52,$7C,$A1,$50,$7C,$E8
                    fcb       $92,$00,$72,$1D,$B0,$01,$E8,$AC,$00,$72,$16,$8B
                    fcb       $FB,$B9,$0B,$00,$BE,$E6,$7D,$F3,$A6,$75,$0A,$8D
                    fcb       $7F,$20,$B9,$0B,$00,$F3,$A6,$74,$18,$BE,$9E,$7D
                    fcb       $E8,$5F,$00,$33,$C0,$CD,$16,$5E,$1F,$8F,$04,$8F
                    fcb       $44,$02,$CD,$19,$58,$58,$58,$EB,$E8,$8B,$47,$1A
                    fcb       $48,$48,$8A,$1E,$0D,$7C,$32,$FF,$F7,$E3,$03,$06
                    fcb       $49,$7C,$13,$16,$4B,$7C,$BB,$00,$07,$B6,$03,$00
                    fcb       $50,$52,$51,$E8,$3A,$00,$72,$D8,$B0,$01,$E8,$54
                    fcb       $00,$59,$5A,$58,$72,$BB,$05,$01,$00,$83,$D2,$00
                    fcb       $03,$1E,$0B,$7C,$E2,$E2,$8A,$2E,$15,$7C,$8A,$16
                    fcb       $24,$7C,$8B,$1E,$49,$7C,$A1,$4B,$7C,$EA,$00,$00
                    fcb       $70,$00,$AC,$0A,$C0,$74,$29,$B4,$0E,$BB,$07,$00
                    fcb       $CD,$10,$EB,$F2,$3B,$16,$18,$7C,$73,$19,$F7,$36
                    fcb       $18,$7C,$FE,$C2,$88,$16,$4F,$7C,$33,$D2,$F7,$36
                    fcb       $1A,$7C,$88,$16,$25,$7C,$A3,$4D,$7C,$F8,$C3,$F9
                    fcb       $C3,$B4,$02,$8B,$16,$4D,$7C,$B1,$06,$D2,$E6,$0A
                    fcb       $36,$4F,$7C,$8B,$CA,$86,$E9,$8A,$16,$24,$7C,$8A
                    fcb       $36,$25,$7C,$CD,$13,$C3,$0D,$0A,$4E,$6F,$6E,$2D
                    fcb       $53,$79,$73,$74,$65,$6D,$20,$64,$69,$73,$6B,$20
                    fcb       $6F,$72,$20,$64,$69,$73,$6B,$20,$65,$72,$72,$6F
                    fcb       $72,$0D,$0A,$52,$65,$70,$6C,$61,$63,$65,$20,$61
                    fcb       $6E,$64,$20,$70,$72,$65,$73,$73,$20,$61,$6E,$79
                    fcb       $20,$6B,$65,$79,$20,$77,$68,$65,$6E,$20,$72,$65
                    fcb       $61,$64,$79,$0D,$0A,$00,$49,$4F,$20,$20,$20,$20
                    fcb       $20,$20,$53,$59,$53,$4D,$53,$44,$4F,$53,$20,$20
                    fcb       $20,$53,$59,$53,$00,$00,$55,$AA

FATMAP              fcb       $F0,$FF,$FF,$00

FDRMAP              fcb       $53,$4B,$59,$54,$45,$4C,$4C,$45,$52,$20,$20,$28
                    fcb       $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$98,$6D
                    fcb       $9D,$1C,$00,$00

;*******************************************************************************
; MAIN LOOP
;*******************************************************************************

                    #ROM      $A000

POWER               proc
                    lds       #$005F              ; set up the stack
                    clr       INITF               ; assume no disk present
                    jsr       INITSYS             ; setup the 6303 cpu

                    jsr       PROMPT
LOOP                jsr       RECESC              ; wait for function request.
                    jsr       RECVA

                    cbeqa     #$36,GSPACE         ; check for get available disk space request
                    cbeqa     #$3D,OPENF          ; check for open file request
                    cbeqa     #$3E,CLOSEF         ; check for close file request
                    cbeqa     #$3F,READF          ; check for read file request
                    cbeqa     #$40,WRITEF         ; check for write file request
                    cbeqa     #$42,MPTRF          ; check for move file pointer request
                    cbeqa     #$56,RNAMEF         ; Check for a rename file request
                    cbeqa     #$05,FRMT           ; Check for a format disk request
;                   cbeqa     #$01,MAGRD          ; check for read mag card request

                    bra       POWER               ; RESET THE WHOLE SYSTEM ON RECEPTION
                                                  ; OF AN ILLEGAL CODE ????????????????
OPENF               jsr       FOPEN
                    bra       LOOP

CLOSEF              jsr       FCLOSE
                    bra       LOOP

MPTRF               jsr       MFPTR
                    bra       LOOP

RNAMEF              jsr       FRNAME
                    bra       LOOP

READF               jsr       FREAD
                    bra       LOOP

WRITEF              jsr       FWRITE
                    bra       LOOP

GSPACE              jsr       GDSPACE
                    bra       LOOP

FRMT                jsr       RESET
                    jsr       FORMAT
                    bra       LOOP

;MAGRD              jsr       RDMAG
;                   bra       LOOP

;*******************************************************************************
; MAIN LOOP
;*******************************************************************************

                    #ROM      $A500

POWER1              proc
                    jsr       STOP0
                    jsr:2     RESET               ; RESET THE DRIVE
                    ldx       #TRKTBL             ; SEEK THROUGH TRACK TABLE
Loop@@              pshx
                    lda       ,x
                    ldb       #%00000000          ; side 1
                    jsr       SEEKTO
                    pulx
                    inx
                    cpx       #TRKEND
                    bne       Loop@@
                    jmp       QUIT

;*******************************************************************************
; TEST READ AND WRITE OF EVERY BYTE OF THE DISK

                    #ROM      $A600

POWER2              proc
                    lds       #$005F              ; set up the stack
                    jsr       STOP0
                    jsr:2     RESET
                    ldd       #33
                    std       ABSSEC

A6001               ldd       ABSSEC              ; put some data in the data buffer
                    ldx       #BUFFER
A6002               std       ,x
                    inx:2
                    cpx       #BUFFER+512
                    bne       A6002

                    ldx       #BUFFER
                    stx       PTRX
                    jsr       MTRON
                    jsr       WSECTOR             ; save that data on the disk
                    jsr       CLRBUF

                    ldx       #BUFFER
                    stx       PTRX
                    jsr       RSECTOR             ; get the data back
                    jsr       MTROFF
                    ldx       #BUFFER

A6003               cpx       #BUFFER+512         ; compare it to original data
                    beq       A6004
                    lda       ,x
                    cmpa      ABSSEC
                    bne       A6ERR               ; print error and quit if not the same
                    inx
                    lda       ,x
                    inx
                    cbeqa     ABSSEC+1,A6003
                    bra       A6ERR               ; print error and quit if not the same

A6004               lda       #'G'                ; let him know it was alright
                    jsr       PCHAR
                    ldd       ABSSEC
                    jsr       P2HEX
                    tba
                    jsr       P2HEX
                    ldx       ABSSEC              ; go again for next sector
                    inx
                    stx       ABSSEC
                    cpx       #$0B40
                    bne       A6001
                    jmp       QUIT

A6ERR               lda       #'E'                ; print error
                    jsr       PCHAR
                    ldd       ABSSEC
                    jsr       P2HEX
                    tba
                    jsr       P2HEX
                    jsr       CRLF
                    ldx       #BUFFER
A6005               lda       ,x                  ; print the offending block
                    jsr       P2HEX
                    inx
                    cpx       #BUFFER+512
                    bne       A6005
                    bra       A6004

;*******************************************************************************
; TEST READ OF EVERY BYTE OF THE DISK

                    #ROM      $A700

POWER3              proc
                    lds       #$005F              ; set up the stack
                    jsr       STOP0
                    jsr:2     RESET
                    ldd       #33
                    std       ABSSEC

A7001               clr       DBUGF
                    jsr       CLRBUF
                    ldx       #BUFFER
                    stx       PTRX
                    jsr       MTRON
                    jsr       RSECTOR             ; get the data back
                    jsr       MTROFF
                    ldx       #BUFFER

A7004               jsr       CRLF
                    lda       #'G'                ; let him know it was alright
                    jsr       PCHAR
                    ldd       ABSSEC
                    jsr       P2HEX
                    tba
                    jsr       P2HEX
                    jsr       CRLF
                    ldx       #BUFFER
A7005               lda       ,x                  ; print the block
                    jsr       P2HEX
;                   jsr       PCHAR
                    inx
                    cpx       #BUFFER+512
                    bne       A7005

                    ldx       ABSSEC              ; go again for next sector
                    inx
                    stx       ABSSEC
                    cpx       #$0B40
                    bne       A7001
                    jmp       QUIT

;*******************************************************************************
; HEAD TEST FUNCTION

                    #ROM      $A800
                    jsr       STOP0
                    jsr       RESET               ; RESET THE DRIVE
                    jsr       CRLF
                    jsr       HDTST
                    jmp       QUIT

;*******************************************************************************
; FORMAT DISK FUNCTION

                    #ROM      $A900
                    jsr       STOP0
                    jsr       RESET               ; RESET THE DRIVE
                    jsr       CRLF
                    jsr       FORMAT
                    jmp       QUIT

;*******************************************************************************
; ???
                    #ROM      $AA00
                    jsr       STOP0
                    jsr       INITSYS
          ;-------------------------------------- ; OPEN A FILE FIRST -- FILENAME AT SBUFF+1
                    ldx       #SBUFF+1            ; set up params for ffile
                    stx       SRCX
                    ldx       #FNAME
                    stx       DSTX
                    ldb       #FNAME_SIZE
                    jsr       MOVE
                    jsr       FFILE               ; search for the file
                    lda       EXIST
                    bne       FOP82               ; file not found !!!

                    jsr       IFHNDL              ; intialize the file handle
                    lda       FHANDLE             ; return the file handle value and file size
                    sta       SBUFF+1
                    ldd       FSIZE
                    std       SBUFF+2
                    ldd       FSIZE+2
                    std       SBUFF+4
                    clr       SBUFF
                    bra       FOP89

FOP82               lda       #2                  ; file not found error
                    sta       SBUFF

FOP89               jsr       PROMPT              ; debuging prompt
                    lda       SBUFF
                    beq       FRD1
                    sta       ERRNUM
                    jsr       FERROR              ; MUST RESET WHOLE SYSTEM FOR ANY ERRORS
          ;-------------------------------------- ; NOW READ THE FILE
FRD1                ldd       FSIZE+2             ; get num bytes from file pointer to EOF
                    subd      FPTR+1
                    std       SBUFF+6
                    bcc       FRA
                    lda       FSIZE+1
                    deca
                    bra       FRB

FRA                 lda       FSIZE+1
FRB                 suba      FPTR
                    sta       SBUFF+5
                    ldd       #$5FFF
                    std       SBUFF+1             ; set num bytes to read

                    ldd       SBUFF+6             ; see if it's greater than amount required to be read
                    subd      SBUFF+1
                    bcc       FRC1                ; OK wont go beyond EOF
                    lda       SBUFF+5
                    suba      #1                  ; cant use deca cause it dont affect the carry
                    sta       SBUFF+5
                    bcc       FRC1                ; OK wont go beyond EOF
                    ldd       SBUFF+6             ; set num bytes = amount left
                    bra       FRD

FRC1                ldd       SBUFF+1             ; get numbytes to be read
FRD                 std       TEMPX               ; divide it up into 512 byte sections
                    clr       TEMPD
                    clr       TEMPD+1
                    ldd       #SECTOR_SIZE
                    std       TEMPA
                    jsr       DIV4B2

                    ldd       TEMPX               ; set counter to num 512 byte blocks
                    std       COUNTER
                    ldd       TEMPD               ; save the remainder
                    psha
                    pshb
                    psha
                    pshb

FRD2                ldd       COUNTER             ; all blocks read yet ?
                    beq       FRD3
                    decd                          ; update counter
                    std       COUNTER
                    ldd       #SECTOR_SIZE        ; Set block size
                    std       BSIZE
                    lda       #'P'
                    sta       CTRLCD
                    clr       WFLAG
                    jsr       RFMID               ; read next block of data from the file

                    ldx       #BUFFER
                    ldd       #SECTOR_SIZE
                    std       BSIZE
                    jsr       BUFOUT
                    bra       FRD2                ; any more to read ??

FRD3                pulb                          ; retrieve remainder
                    pula
                    subd      #0
                    beq       FRD4
                    std       BSIZE
                    lda       #'P'
                    sta       CTRLCD
                    clr       WFLAG
                    jsr       RFMID               ; read last few bytes

                    pulb
                    pula
                    std       BSIZE
                    ldx       #BUFFER
                    jsr       BUFOUT
                    bra       FCLS

FRD4                pulb
                    pula
FCLS                ldb       #37                 ; clear out the whole file handle
                    ldx       #FNAME
                    jsr       CLRXB
                    clr       SBUFF

QUIT                jsr       WARM

;*******************************************************************************
;       THE VARIOUS TABLES FOR TESTING ROUTINES
;*******************************************************************************

                    #DATA     $B000

EOFFLG              fcb       $00                 ; APPEND EOF MARK TO FILE FLAG
DBUGF               fcb       $00                 ; TEST MODE FLAG
                                                  ; BIT SIGNIFICANCE
                                                  ;  0  CLEAR ALL RAM ON POWER UP
                                                  ;  1  PRINT DATA
                                                  ;  2  PRINT FDIR
                                                  ;  3  PRINT FAT
                                                  ;  4  PRINT BPB
                                                  ;  5  PRINT COMMANDS
                                                  ;  6  PRINT RESULTS FROM COMMANDS
                                                  ;  7  PRINT ERRORS FROM STATUS REGS

CMNDTBL_ENTRY_SIZE  equ       22

?                   macro     MsgString
                    mreq      1:MsgString
                    mset      1,~@~
                    mset      1,~1.1.{CMNDTBL_ENTRY_SIZE}~
                    mstr      1
                    #temp     CMNDTBL_ENTRY_SIZE-{:1-2}
                    mset      2,~1.1.1~
          #ifnz :temp
                    mdo
                    mset      1,~1.1.{:1-1}~ ~2~
                    mloop     :temp
          #endif
                    fcc       ~1~
                    endm

                    #HideMacros

CMNDTBL             @?        INVALID                       ; 00
                    @?        INVALID                       ; 01
                    @?        READ A TRACK                  ; 02
                    @?        SPECIFY                       ; 03
                    @?        SENSE DRIVE STATUS            ; 04
                    @?        WRITE DATA                    ; 05
                    @?        READ DATA                     ; 06
                    @?        RECALIBRATE                   ; 07
                    @?        SENSE INTERRUPT STATUS        ; 08
                    @?        WRITE DELETED DATA            ; 09
                    @?        READ ID                       ; 0A
                    @?        INVALID                       ; 0B
                    @?        READ DELETED DATA             ; 0C
                    @?        FORMAT A TRACK                ; 0D
                    @?        INVALID                       ; 0E
                    @?        SEEK                          ; 0F
                    @?        INVALID                       ; 10
                    @?        SCAN EQUAL                    ; 11
                    @?        INVALID                       ; 12
                    @?        INVALID                       ; 13
                    @?        INVALID                       ; 14
                    @?        INVALID                       ; 15
                    @?        INVALID                       ; 16
                    @?        INVALID                       ; 17
                    @?        INVALID                       ; 18
                    @?        SCAN LOW OR EQUAL             ; 19
                    @?        INVALID                       ; 1A
                    @?        INVALID                       ; 1B
                    @?        INVALID                       ; 1C
                    @?        SCAN HIGH OR EQUAL            ; 1D
                    @?        INVALID                       ; 1E
                    @?        INVALID                       ; 1F , ALL OTHERS INVALID

                    #ShowMacros

TRKTBL              fcb       $00,$01,$10,$40,$30,$46,$32,$12,$05,$01,$00
TRKEND              fcb       $00

ERROR_MSG_SIZE      equ       60

;*******************************************************************************

?                   macro     MsgString
                    mreq      1:MsgString
                    mset      1,~@~
                    mset      1,~1.1.{ERROR_MSG_SIZE}~
                    mstr      1
                    #temp     {ERROR_MSG_SIZE}-{:1-2}
                    mset      2,~1.1.1~
          #ifnz :temp
                    mdo
                    mset      1,~1.1.{:1-1}~ ~2~
                    mloop     :temp
          #endif
                    fcc       ~1~
                    endm

                    #HideMacros

          ;--------------------------------------
          ; STATUS REGISTER 0 ERROR CODES
          ;-------------------------------------- ; INTERRUPT CODES
INTCDS              @?        ST0 NORMAL TERMINATION - COMMAND COMPLETED AND EXECUTED
                    @?        ST0 ABNORMAL TERMINATION - COMMAND STARTED BUT NOT COMPLETED
                    @?        ST0 ABNORMAL TERMINATION - INVALID COMMAND ISSUED
                    @?        ST0 ABNORMAL TERMINATION - READY, CHANGED DURING EXECUTION
          ;-------------------------------------- ; SEEK CODES
SEKCDS              @?        ST0 SEEK COMMAND NOT COMPLETED
                    @?        ST0 SEEK COMMAND COMPLETED
          ;-------------------------------------- ; EQUIPMENT CHECK CODES
EQPCDS              @?        ST0 NO FAULT SIGNAL - AND/OR TRACK 00 FOUND (RECALIBRATE OK)
                    @?        ST0 FAULT SIGNAL RECEIVED OR NO TRACK 0 AFTER 77 STEP PULSES
          ;-------------------------------------- ; NOT READY CODES
RDYCDS              @?        ST0 FDD READY - AND/OR LEGAL HEAD NUMBER
                    @?        ST0 FDD NOT READY OR SINGLE SIDED DRIVE ONLY (ILLEGAL HEAD)
          ;-------------------------------------- ; DUMMY CODE
DM1CDS              @?        ST0 DUMMY CODE DUMMY CODE DUMMY CODE
                    @?        ST0 DUMMY CODE DUMMY CODE DUMMY CODE
          ;-------------------------------------- ; DUMMY CODE
DM2CDS              @?        ST0 DUMMY CODE DUMMY CODE DUMMY CODE
                    @?        ST0 DUMMY CODE DUMMY CODE DUMMY CODE
          ;--------------------------------------
          ; STATUS REGISTER 1 ERROR CODES
          ;-------------------------------------- ; END OF TRACK CODES
EOTCDS              @?        ST1 SECTOR ON TRACK
                    @?        ST1 ATTEMPT TO ACCESS SECTOR BEYOND END OF TRACK
          ;-------------------------------------- ; DATA ERROR CODES
DTECDS              @?        ST1 NO CRC ERROR
                    @?        ST1 CRC ERROR IN ID FIELD OR DATA FIELD
          ;-------------------------------------- ; OVER RUN CODES
OVRCDS              @?        ST1 FDC SERVICED IN TIME
                    @?        ST1 FDC NOT SERVICED IN TIME => OVERRUN ERROR
          ;-------------------------------------- ; NO DATA CODES
NDTCDS              @?        ST1 DATA CODES OK
                    @?        ST1 CANNOT FIND SPECIFED SECTOR (READ, WRITE DELETED, SCAN)
                    @?        ST1 CANNOT READ ID FIELD WITHOUT ERROR ( READ ID )
                    @?        ST1 CANNOT FIND STARTING SECTOR ( READ A TRACK )
          ;-------------------------------------- ; NOT WRITABLE CODES
WRTCDS              @?        ST1 DISK NOT PROTECTED
                    @?        ST1 WRITE PROTECTED DISK IN DRIVE
          ;-------------------------------------- ; MISSING ADDRESS MARK CODES
MAMCDS              @?        ST1 NO MISSING ADDRESS MARKS
                    @?        ST1 IDAM, DAM, DDAM NOT FOUND - CHECK MAM IN DATA FIELD CODE
          ;--------------------------------------
          ; STATUS REGISTER 2 ERROR CODES
          ;-------------------------------------- ; CONTROL MARK CODES
CMKCDS              @?        ST2 NO DELETED DATA ADDRESS MARKS FOUND
                    @?        ST2 SECTOR CONTAINS A DELETED DATA ADDRESS MARK (READ, SCAN)
          ;-------------------------------------- ; DATA ERROR IN DATA FIELD
DEDCDS              @?        ST2 NO CRC ERROR IN DATA FIELD
                    @?        ST2 CRC ERROR IN DATA FIELD
          ;-------------------------------------- ; WRONG TRACK CODES
WTRCDS              @?        ST2 CORRECT TRACK LOCATED
                    @?        ST2 DISK TRACK != IDR TRACK
          ;-------------------------------------- ; SCAN EQUAL HIT CODES
SEHCDS              @?        ST2 SCAN EQUAL NOT SATISFIED
                    @?        ST2 SCAN EQUAL SATISFIED
          ;-------------------------------------- ; SCAN NOT SATISFIED CODES
SNSCDS              @?        ST2 FOUND SECTOR ON DISK MATCHING SCAN PARAMETERS
                    @?        ST2 CANNOT FIND SECTOR ON TRACK MATCHING SCAN PARAMETERS
          ;-------------------------------------- ; BAD TRACK CODES
BTRCDS              @?        ST2 TRACK NUMBER OK
                    @?        ST2 TRACK = FFh AND DOES NOT EQUAL TRACK VALUE IN IDR
          ;-------------------------------------- ; MISSING ADDRESS MARK IN DATA FIELD CODES
MDFCDS              @?        ST2 NO MISSING ADDRESS MARKS IN DATA FIELD
                    @?        ST2 DAM OR DDAM NOT FOUND
          ;--------------------------------------
          ; STATUS REGISTER 3 ERROR CODES
          ;-------------------------------------- ; FAULT LINE CODES
FLTCDS              @?        'ST3 FAULT LINE IS LOW    => NO FAULT'
                    @?        'ST3 FAULT LINE IS HIGH   => FAULT'
          ;-------------------------------------- ; WRITE PROTECTED CODES
WRPCDS              @?        'ST3 WRITE PROTECT LINE LOW       => NOT WRITE PROTECTED'
                    @?        'ST3 WRITE PROTECT LINE IS HIGH   => WRITE PROTECTED'
          ;-------------------------------------- ; READY CODES
REDCDS              @?        'ST3 READY LINE IS LOW   => FDD NOT READY'
                    @?        'ST3 READY LINE IS HIGH  => FDD IS READY'
          ;-------------------------------------- ; TRACK 0 CODES
TR0CDS              @?        'ST3 TRACK 00 LINE IS LOW  => HEAD NOT ON TRACK 00'
                    @?        'ST3 TRACK 00 LINE IS HIGH => HEAD ON TRACK 00'
          ;-------------------------------------- ; TWO SIDED CODES
TSDCDS              @?        'ST3 TWO SIDED LINE IS LOW  => SINGLE SIDED DISK'
                    @?        'ST3 TWO SIDED LINE IS HIGH => DOUBLE SIDED DISK'
          ;-------------------------------------- ; HEAD ADDRESS MARK CODES
HDACDS              @?        'ST3 HEAD ADDRESS LINE IS LOW   => HEAD 2 SELECTED'
                    @?        'ST3 HEAD ADDRESS LINE IS HIGH  => HEAD 1 SELECTED'
          ;-------------------------------------- ; DUMMY CODE
DM3CDS              @?        ST3 DUMMY CODE DUMMY CODE DUMMY CODE
                    @?        ST3 DUMMY CODE DUMMY CODE DUMMY CODE

                    #ShowMacros

;*******************************************************************************
; VECTORS
;*******************************************************************************

                    #DATA     ;$7FE0
                    fcc       'LSI1.1'            ; PROGRAM ID
                    @bcd      {:date},{:month},{:year\100} ; FILE CREATION DATE (WAS: $15,$03,$94)

                    fcb       $00                 ; ROM CHECKSUM BYTE

                    @vector   Virq,IRQSNS         ; IRQ1
                    @vector   Vswi,QUIT           ; SWI
                    @vector   Vreset,POWER1       ; RES

                    end       :s19crc

                    #Message  -FD option verification 10612 bytes, RAM: 15566, CRC: $A4EB
