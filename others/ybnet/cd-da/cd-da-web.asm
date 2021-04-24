;*******************************************************************************
; cd-da-web.asc                Mp3 F1 lt.
;                   MP3 CD Player with 68HC11F1FN 16MHz
;                               Ver 1 lt.
;          original code for PIC16Cxx by Leandro Gentili /1999
;                 adapted to 68HC11F1 by YB 2000
;
;                            Main routines
;
; prochaines versions :
;       - affichage des menus/playlist
;       - gestion MAS3507D & DAC3550/CS4334
;       - gestion mémoire FLASH
;
; URL : http://www.ybnet.fr.st
; E-MAIL : yb_net@yahoo.fr
;*******************************************************************************
; Config materielle :
;  -Affichage de message:
;               -LCD CONTROL: $1800
;                       -RS : D4
;                       -E  : D2
;                       -R/W: D3
;                             D0,D1,D5,D6,D7 libres
;               -LCD DATA: $1801 :D0...D7
;
;  -PCBUG11:    PD0/PD1: RS232
;
;  -CLAVIER :   -KEYBOARD: $1805
;                       -SW1  : D0      (Play/Pause)
;                       -SW2  : D1      (Rew/-)
;                       -SW3  : D2      (Fwr/+)
;                       -SW4  : D3      (Stop/eject)
;                       -SW5  : D4      (Mode/List)
;                               D5...D7=GND
;
;  -port IDE :
;               -DD0...DD7 : PORTA
;               -DD8...DD15: PORTG
;               -IDE commands  : $1802
;                       -DA0   : D0
;                       -DA1   : D1
;                       -DA2   : D2
;                       -CS0_  : D3
;                       -CS1_  : D4
;                       -DIOR_ : D5
;                       -DIOW_ : D6
;                       -RESET_: D7
;
;  -BUFFER : SRAM 32Ko (RAM1) : $2000...$9FFF
;  -PROG   : SRAM 8Ko  (RAM2) : $C000...$DFFF
;
;*******************************************************************************

;*******************************************************************************
;         Definition des adresses des registres HC11F1
;*******************************************************************************

DEB_REG             equ       $1000
PORTA               equ       DEB_REG
DDRA                equ       DEB_REG+1
PORTC               equ       DEB_REG+3
PORTB               equ       DEB_REG+4
PORTCL              equ       DEB_REG+5
DDRC                equ       DEB_REG+7
PORTD               equ       DEB_REG+8
DDRD                equ       DEB_REG+9
PORTE               equ       DEB_REG+$A

; specifique au HC11F1
PORTG               equ       DEB_REG+2
DDRG                equ       DEB_REG+3
CSCTL               equ       DEB_REG+$5D

;*******************************************************************************
;              Definition des variables MP3 F1
; rappel :
; EQU ne réserve pas d'espace mémoire, c'est juste une
;   équivalence dans l'écriture, le compilateur remplacera...
; RMB x : réserve x cases mémoire pour une variable.
;
; les variables sont mises en RAM interne aprés le TALKer de PCBUG
; La RAM est laissée libre de $00 à $01FF pour le talker(256octets)
;*******************************************************************************

DEB_RAM             equ       $0200               ; debut RAM interne libre (ATTENTION AU TALKER et SA PILE en $1FF)
DEB_PROG            equ       $C000               ; debut memoire RAM1 externe (8Kbytes)
DEB_EEPROM          equ       $FE00               ; debut memoire EEPROM (512bytes)
DEB_ROM             equ       $A000               ; debut ROM (8Kbytes) : BUFFALO v3.2 HC11F1 - 9600bps - 8MHz
DEB_BUFFER          equ       $2000               ; debut du BUFFER RAM interne libre de $329 à $3EF (198bytes)
OFFSET_H            equ       $03                 ; poids fort de l'adresse du BUFFER
OFFSET_L            equ       $29                 ; poids faible de l'adresse du BUFFER
LCD_CONTROL         equ       $1800               ; registre de commande de l'afficheur LCD (écriture)
LCD_DATA            equ       $1801               ; registre de donnees de l'afficheur LCD (écriture)
IDE_REG             equ       $1802               ; registre IDE (écriture)
KEYBOARD            equ       $1805               ; registre du clavier (lecture)

;*******************************************************************************
                    #RAM      DEB_RAM             ; variables maison
;*******************************************************************************

LCDINFO             rmb       1                   ; Infos sur l'affichage
                                                  ; bits 0 à 4 : longueur du message à afficher (16max)
                                                  ; bit 5:  affichage sur 0->1ère ligne 1->deuxième ligne
LONGUEUR            rmb       1                   ; longueur du texte à afficher
OFFSET              rmb       1                   ; décalage du curseur vers la droite
;*******************************************************************************
;    Miscellaneous
;*******************************************************************************
DEM                 equ       5
;*******************************************************************************
;   IDE
;          PORTE   e2 e1 e0
;                   0  1  0  RDn
;                   0  1  1  WRn
;*******************************************************************************
; PORTA   d7  d6  d5  d4  d3  d2  d1  d0
;        DD7 DD6 DD5 DD4 DD3 DD2 DD1 DD0

; PORTG   d7   d6   d5   d4   d3   d2   d1  d0
;        DD15 DD14 DD13 DD12 DD11 DD10 DD9 DD8

D_STATUS            equ       %00010111
DSTATUSA            equ       %00001110           ; D_STATUS_A
DCOMMAND            equ       D_STATUS
DCYL_H              equ       %00010101           ; D_CILYNDER_H
DCYL_L              equ       %00010100           ; D_CILYNDER_L
DSECTORN            equ       %00010011           ; D_SECTOR_N
DSECTORC            equ       %00010010           ; D_SECTOR_C
DDEVICEH            equ       %00010110           ; D_DEVICE_H
D_FEATURES          equ       %00010001
D_ERRORR            equ       D_FEATURES
D_DATA_R            equ       %00010000           ; %00010000
DDEVICEC            equ       DSTATUSA            ; D_DEVICE_C

;*******************************************************************************
; TASK REGISTERs ATA

STATUS_R            rmb       1
STATUS_A            rmb       1
COMMAND             rmb       1
CYLIND_H            rmb       1                   ; CILYNDER_H
CYLIND_L            rmb       1                   ; CILYNDER_L
SECTOR_N            rmb       1
SECTOR_C            rmb       1
DEVICE_H            rmb       1
FEATURES            rmb       1
ERRORR              rmb       1
DEVICE_C            rmb       1
DATA_RL             rmb       1
DATA_RH             rmb       1

;*******************************************************************************
; FLAGs
FLAG_CD             rmb       1
; FLAG_CD  d7 d6 d5 d4  d3  d2  d1   d0
;          xx DA RO ST Pau Pre LoEj Start
;                               0     0      Stop Disc
;                               0     1      Start Disc , read TOC
;                               1     0      Eject Disc
;                               1     1      Load Disc
;                           0   xx    xx     Allow Eject
;                           1   xx    xx     Prevent Eject
;                       0   xx  xx    xx     Pause
;                       1   xx  xx    xx     Resume
;                   0   xx  xx  xx    xx     CD charged
;                   1   xx  xx  xx    xx     No CD charged
;             0  1  xx  xx  xx  xx    xx     CD-ROM/MP3
;             1  0  xx  xx  xx  xx    xx     CD-DA
;             1  1  xx  xx  xx  xx    xx     CD-HYBRID
;

FLAGKEY             rmb       1                   ; FLAGKEY
; FLAGKEY   d7 d6 d5  d4   d3  d2  d1   d0
;            xx xx xx MODE STOP FRD REW PLAY

FLAGKEY1            rmb       1                   ; FLAGKEY1
; FLAGKEY1  d7 d6 d5  d4   d3  d2  d1   d0
;            xx xx xx MODE STOP FRD REW PLAY

FLAGSTATE           rmb       1                   ; FLAGSTATE
; FLAG_STATE d7 d6   d5   d4    d3   d2  d1  d0
;                        MODE  EJECT    REW PAUSE
PLAY                equ       4                   ; 0
REW                 equ       2                   ; 1
FRD                 equ       8                   ; 2
STOP                equ       1                   ; 3
MODE                equ       16                  ; 4
PAUSE               equ       4                   ; 0
EJECT               equ       1                   ; 3

;*******************************************************************************
; VARIABLES et CONSTANTES

COUNTER             rmb       1
COUNTER1            rmb       1
; STATUS REGISTER
BSY                 equ       7
DRDY                equ       6
DRQ                 equ       3
ERR                 equ       0
; ERROR REGISTER
ABRT                equ       2
; DEVICE CONTROL REGISTER
SRST                equ       2
nEIN                equ       1
; PKT COMMAND
BP0                 rmb       1
BP1                 rmb       1
BP2                 rmb       1
BP3                 rmb       1
BP4                 rmb       1
BP5                 rmb       1
BP6                 rmb       1
BP7                 rmb       1
BP8                 rmb       1
BP9                 rmb       1
BP10                rmb       1
BP11                rmb       1

ADR_RL              rmb       1
ADR_RH              rmb       1
ADR_WL              rmb       1
ADR_WH              rmb       1
TOPE                rmb       1
BYTERAM             rmb       1
START_M             rmb       1
START_S             rmb       1
START_F             rmb       1
END_M               rmb       1
END_S               rmb       1
END_F               rmb       1
LBA_1               rmb       1
LBA_2               rmb       1
LBA_3               rmb       1
LBA_4               rmb       1
TRANS_1             rmb       1
TRANS_2             rmb       1
TRANS_3             rmb       1
TRANS_4             rmb       1
INDEX               rmb       1
INDEX_LIST          rmb       1
TOTAL_SONG          rmb       1
TOTAL_LIST          rmb       1

CONT1               rmb       1
CONT2               rmb       1
TEMPW               rmb       1
TEMP1               rmb       1
TEMP2               rmb       1
TEMP3               rmb       1
TEMP4               rmb       1
TEMP5               rmb       1
BASEF               rmb       4

FLAG                rmb       1
ATAPIn              equ       0
JOULIET             equ       1
ACK                 equ       2
MANTIENE            equ       3
FINMP3              equ       4                   ; FINALMP3
FINMP31             equ       5                   ; FINALMP31
MARCA1              equ       6
MARCA2              equ       7

;*******************************************************************************
; BCD

COUNTER2            rmb       1
L_BYTE              rmb       1
DECENA              rmb       1
MILENA              rmb       1
DIG_3               rmb       1
DIG_2               rmb       1
DIG_1               rmb       1

;*******************************************************************************
; Table  ( des messages et des Iso9660 mark)

M_0                 fcc       ' ** MP3F1 lt ** '  ; 1
M_1                 fcc       ' No CD Charged  '  ;
M_2                 fcc       '  **  CD-DA  ** '  ; 2
M_3                 fcc       '  ** CD-MP3 **  '  ; 3
M_4                 fcc       '  ** CD-RW  **  '  ; 4 equivalent à CD-Hybrid ?
M_5                 fcc       'Tracks Found:   '  ; 5
M_9                 fcc       '                '  ; 1
M_10                fcc       'ATAPI Not Found '  ;
M_11                fcc       '   Door Open    '  ; 2
M_12                fcc       ' Please wait... '  ; attente
M_13                fcc       'Not implemented '  ; limitation version lt
M_14                fcc       '    #   /       '  ; affichage en mode lecture CD-DA

;*******************************************************************************
; logos PLAY |>, Pause ||, STOP

ICONES              fcb       $10,$18,$1C,$1E,$1E,$1C,$18,$10  ; PLAY |>
                    fcb       $1B,$1B,$1B,$1B,$1B,$1B,$1B,$1B  ; Pause ||
                    fcb       $00,$1F,$1F,$1F,$1F,$1F,$00,$00  ; STOP
ICO                 fcb       0,1,2               ; code du caractere

;*******************************************************************************
                    #ROM      DEB_PROG            ;        InI Port's
;*******************************************************************************

Start               proc
                    lds       #$3FF               ; Initialisation de la pile(1024octets)

                    lda       #%00011111
                    sta       IDE_REG             ; désactive tous les registres ATA
                    clra                          ; Initialisations du PORTG (bus en entree)
                    sta       DDRG
                    sta       CSCTL               ; desactive les chips selects internes
                    sta       PORTG
                    sta       DDRA                ; Initialisation du PORTA (bus en entree)

                    sta       LCD_CONTROL         ; Initialisation registres LCD
                    sta       LCD_DATA            ; Initialisation registres LCD
          ;-------------------------------------- ; Initialize variables
          ;-------------------------------------- ; ou utilisation de CLRA et STAA pour reduire
                    clr       FLAG                ; le nombre de cycles processeurs
                    clr       FLAGKEY
                    clr       FLAGKEY1
                    clr       FLAGSTATE
                    clr       FLAG_CD

                    jsr       ESPERA              ; tempo de 500us x 5
          ;--------------------------------------
          ; Initializations
          ;--------------------------------------
                    jsr       _INITAFF            ; INI_LCD : initialiusation de l'afficheur LCD
                    jsr       INICONES            ; stockage des icones en CG-RAM (LCD)
          ;-------------------------------------- ; reset maison du CDROM
                    lda       #%01111000          ; DA0=DA1=DA2=RESET_=0, CS0_=CS1_=DIOW_=DIOR_=1
                    sta       IDE_REG
                    nop
                    lda       #%11111000          ; DA0=DA1=DA2=0, CS0_=CS1_=DIOW_=DIOR_=RESET_=1
                    sta       IDE_REG

                    jsr       ESPERA
NO_ATAPI
                    jsr       INI_ATAPI

                    ldy       #FLAG
                    brclr     ,y,#%00000001,S_ATAPI ; ATAPIn=0 ->bit n°0

                    ldx       #M_10
                    lda       #%00001111          ; affichage du message sur la premiere ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    jsr       ESPERA1             ; tempo 40us
                    bra       NO_ATAPI            ; on attend que le lecteur soit détecté...
S_ATAPI
                    ldx       #M_0
                    lda       #%00001111          ; affichage du message sur la premiere ligne
                    sta       LCDINFO             ; LONGUEUR=16

                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche
DENUEVO
                    clr       BYTERAM
                    jsr       FULLBUFX

                    jsr       INI_CD

                    ldy       #FLAG_CD
                    brset     ,y,#%00010000,NOCD

                    ldy       #FLAG_CD
                    brclr     ,y,#%01000000,SICD

                    ldy       #FLAG_CD
                    brclr     ,y,#%00100000,SICD
NOCD
                    jsr       RD_KEY              ; pas de CD, verif de la touche Eject
                    jsr       STOP_PRESS

                    ldy       #FLAGKEY1
                    brclr     ,y,#STOP,NOCD
                    jmp       EJECT_RUTIN
SICD
                    jsr       SET_MAX_SPEED       ; CD chargé, on configure alors la vitesse max (MAX SPEED)
                    jsr       NUMERO_SONG         ; combien de pistes sur le CD : affiche le nombre de morceaux sur le support
; si le support est un CD-DA, c'est le nombre de pistes
; si le support est un CD-MP3, c'est le nombre d'objets situés à
                                                  ; la racine du CD-ROM.
                    lda       #8
                    sta       TRANS_1
                    lda       #$40
                    sta       TOPE
                    lda       #1
                    sta       INDEX

;*******************************************************************************
; The big loop
;*******************************************************************************

LOOP
                    ldy       #FLAG
                    bset      ,y,#%00001000      ; mantienne=3
                    clr       INDEX_LIST          ; Affiche la premiere Track
                    lda       #1
                    sta       INDEX
LOOPP
                    jsr       RD_KEY
                    jsr       REW_PRESS

                    ldy       #FLAGKEY1
                    brclr     ,y,#REW,NONE

                    dec       INDEX
                    bne       PUTA
                    lda       TOTAL_SONG
                    sta       INDEX
                    bra       PUTA
NONE
                    jsr       FRD_PRESS

                    ldy       #FLAGKEY1
                    brclr     ,y,#FRD,NO_ROTO

                    lda       INDEX
                    inca
                    cmpa      TOTAL_SONG
                    bge       PUTA

                    lda       #1
                    sta       INDEX
PUTA
;*******************************************************************************

NO_ROTO             proc
                    jsr       RD_KEY
                    jsr       MODE_PRESS

                    ldy       #FLAGKEY1
                    brclr     ,y,#MODE,NO_PRESS

                    jsr       CAL_BASE            ; donne l'adresse en RAM de la "base" de la piste
                                                  ; dont le numéro est spécifié par INDEX.
                    jsr       RD_RAM              ; on lit DATA_RH de la piste choisi

                    tst       BYTERAM
                    bne       DESELECT
SELECT
                    inc       INDEX_LIST

                    lda       INDEX_LIST
                    sta       BYTERAM

                    jsr       WR_RAM
                    bra       NO_PRESS

;*******************************************************************************

DESELECT            proc
                    lda       INDEX
                    sta       TEMP3

                    jsr       RD_RAM

                    lda       BYTERAM
                    sta       TEMP4
                    inc       TEMP4
                    clr       BYTERAM

                    jsr       WR_RAM
JIRA
                    lda       #1
                    sta       INDEX
JIRO
                    jsr       CAL_BASE

                    jsr       RD_RAM

                    lda       BYTERAM
                    cmpa      TEMP4
                    bne       ORTO

                    lda       TEMP4
                    deca
                    sta       BYTERAM
                    inc       TEMP4

                    jsr       WR_RAM
                    bra       JIRA
ORTO
                    lda       INDEX
                    cmpa      TOTAL_SONG
                    beq       NO_PRE

                    inc       INDEX
                    bra       JIRO
NO_PRE
                    lda       TEMP3
                    sta       INDEX
                    dec       INDEX_LIST

;*******************************************************************************

NO_PRESS            proc
                    jsr       ESPERA
                    jsr       RD_KEY
                    jsr       PLAY_PRESS

                    ldy       #FLAGKEY1
                    brset     ,y,#PLAY,PLAYRUT

                    jsr       STOP_PRESS

                    ldy       #FLAGKEY1
                    brclr     ,y,#STOP,SUITNOPRESS
                    jmp       EJECT_RUTIN
SUITNOPRESS
                    jsr       ESPERA1             ; tempo de 40us
                    jmp       LOOPP               ; grande boucle terminée.

;*******************************************************************************
; PLAY_rutin : aiguillage pour démarrer la lecture

PLAYRUT             proc
                    ldy       #FLAG
                    bclr      ,y,#%00001000       ; mantiene=3
                    jsr       ESPERA

                    lda       INDEX_LIST
                    tsta
                    bne       ADELA

                    lda       INDEX
                    sta       TEMP3
                    clr       INDEX
OTRASS
                    inc       INDEX_LIST
                    inc       INDEX

                    jsr       CAL_BASE
                    lda       INDEX_LIST
                    sta       BYTERAM
                    jsr       WR_RAM

                    lda       INDEX_LIST
                    cmpa      TOTAL_SONG
                    bne       OTRASS

                    lda       TEMP3
                    sta       INDEX_LIST
                    lda       TOTAL_SONG
                    sta       TOTAL_LIST
                    bra       DISP
ADELA
                    lda       INDEX_LIST
                    sta       TOTAL_LIST
                    lda       #1
                    sta       INDEX_LIST
DISP
                    ldx       #M_14
                    lda       #%00011111          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    jsr       ESPERA

                    ldb       TOTAL_SONG
                    jsr       BCD2
                    ldx       #DIG_3              ; affichage en BCD de nombre de pistes trouvées
                    lda       #%00010010          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=3
                    lda       #9                  ; OFFSET=9 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche
;                   bra       PLAYY

;*******************************************************************************
; PLAYY : gestion lecture selon MODE MP3 ou Audio

PLAYY               proc
                    clr       INDEX
OTRAS
                    inc       INDEX
                    bne       NOCERO

                    lda       #1
                    sta       INDEX_LIST
                    bra       OTRAS
NOCERO
                    jsr       CAL_BASE
                    jsr       RD_RAM

                    lda       BYTERAM
                    cmpa      INDEX_LIST
                    bne       OTRAS
          ;--------------------------------------
                    ldb       INDEX
                    jsr       BCD2

                    ldx       #DIG_3              ; affichage en BCD de nombre de pistes trouvées
                    lda       #%00010010          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=3
                    lda       #5                  ; OFFSET=7 caractères
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche
          ;--------------------------------------
                    ldy       #FLAGSTATE
                    brclr     ,y,#FRD,SUITNC1
                    bra       PLAY_RUTIN1
SUITNC1
                    brclr     ,y,#REW,SUITNC2
                    bra       PLAY_RUTIN1
SUITNC2
                    jsr       CAL_BASE

                    ldy       #FLAG_CD
                    brset     ,y,#%00100000,SUITNC3 ; lecture CD audio
                    jmp       PLAY_DA
SUITNC3
                    jmp       PLAY_MP3            ; lecture fichiers MP3

;*******************************************************************************

EJECT_RUTIN         proc
                    lda       #2
                    sta       FLAG_CD
                    jsr       ST_UNIT
                    jsr       ESPERA

                    ldx       #M_0
                    lda       #%00001111          ; affichage du message sur la premiere ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    ldx       #M_11
                    lda       #%00011111          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche
MIRAR
                    jsr       RD_KEY
                    jsr       STOP_PRESS

                    ldy       #FLAGKEY1
                    brclr     ,y,#STOP,MIRAR

                    ldx       #M_12
                    lda       #%00011111          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    lda       #3
                    sta       FLAG_CD
                    jsr       ST_UNIT
                    jsr       ESPERA

                    ldx       #M_9
                    lda       #%00011111          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche
                    jmp       DENUEVO

;*******************************************************************************
; gestion des MODEs et des touches pendant la lecture

PLAY_RUTIN1         proc
                    ldy       #FLAGSTATE
                    brclr     ,y,#REW,HENNA

                    inc       CONT1
                    bne       HENNA
                    inc       CONT2
                    bne       HENNA

                    ldy       #FLAGSTATE
                    bclr      ,y,#REW
                    jmp       PLAYY
HENNA
                    ldy       #FLAGSTATE
                    brclr     ,y,#FRD,HENNA1

                    inc       CONT1
                    bne       HENNA1

                    inc       CONT2
                    bne       HENNA1

                    ldy       #FLAGSTATE
                    bclr      ,y,#FRD
                    jmp       PLAYY
HENNA1
                    ldy       #FLAGSTATE
                    brclr     ,y,#MODE,HENNA2

                    brclr     ,y,#PAUSE,SUITH1
                    jsr       ESPERA11
SUITH1
                    inc       CONT1
                    bne       HENNA2

                    inc       CONT2
                    bne       HENNA2
                    bra       ESPECIAL
HENNA2
                    jsr       RD_KEY

                    ldy       #FLAGKEY1

                    jsr       PLAY_PRESS
                    brclr     ,y,#PLAY,SUITH21
                    bra       RUT_PLAY
SUITH21
                    jsr       STOP_PRESS
                    brclr     ,y,#STOP,SUITH22
                    jmp       RUT_STOP
SUITH22
                    jsr       MODE_PRESS
                    brclr     ,y,#MODE,HHHH

                    ldy       #FLAGSTATE
                    bset      ,y,#MODE

                    bra       ESPECIAL
HHHH
                    ldy       #FLAGSTATE
                    brclr     ,y,#MODE,NORMAL
                    bra       ESPECIAL
NORMAL
                    jsr       REW_PRESS
                    ldy       #FLAGKEY1
                    brclr     ,y,#REW,SUITN1
                    jmp       RUT_REW
SUITN1
                    jsr       FRD_PRESS
                    ldy       #FLAGKEY1
                    brclr     ,y,#FRD,SUITN2
                    jmp       RUT_FWD
SUITN2
;*******************************************************************************

ESPECIAL            proc
                    ldy       #FLAGSTATE
                    brclr     ,y,#PAUSE,_1@@
                    jmp       PLAY_RUTIN1
_1@@                brclr     ,y,#FRD,_2@@
                    jmp       PLAY_RUTIN1
_2@@                brclr     ,y,#REW,_3@@
                    jmp       PLAY_RUTIN1
_3@@
;*******************************************************************************

AUDII               proc
                    jsr       READ_SUB
                    lda       TEMP1
                    cmpa      #$13
                    jne       PLAY_RUTIN1
                    lda       INDEX_LIST
                    cmpa      TOTAL_SONG
                    jeq       RUT_STOP
                    jmp       RUT_FWD

;*******************************************************************************
; gestion de la lecture selon l'appui sur la touche play

RUT_PLAY            proc
;*** tempos pour limiter les effets de rebonds des touches ->à améliorer ***

                    jsr:4     BIGTEMPO            ; tempo 100ms*4=400ms

                    ldy       #FLAGSTATE
                    brset     ,y,#PAUSE,UNPAUSE

                    ldy       #FLAG_CD
                    brclr     ,y,#%00100000,MUSIC

                    ldx       #ICO
                    lda       #%000010000         ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=1

                    lda       #1                  ; OFFSET=1 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    ldy       #FLAGSTATE
                    bset      ,y,#PAUSE
                    jmp       PLAY_RUTIN1
MUSIC
                    ldx       #ICO+1
                    lda       #%000010000         ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=1

                    lda       #1                  ; OFFSET=1 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    ldy       #FLAGSTATE
                    bset      ,y,#PAUSE

                    ldy       #FLAG_CD
                    bclr      ,y,#%00001000

                    jsr       PAUSE_RESUME
                    jsr       ESPERA
;                   lda       #$44
;                   sta       BUFFERIIC+2
                    jmp       PLAY_RUTIN1

;*******************************************************************************

UNPAUSE             proc
                    ldy       #FLAG_CD
                    brclr     ,Y,#%00100000,MUSIC1

                    ldy       #FLAGSTATE
                    bclr      ,Y,#PAUSE
                    jmp       PLAY_RUTIN1
MUSIC1
                    ldx       #ICO
                    lda       #%000010000         ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=1

                    lda       #1                  ; OFFSET=1 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

;                   lda       #$4C
;                   sta       BUFFERIIC+2

                    ldy       #FLAGSTATE
                    bclr      ,y,#PAUSE

                    ldy       #FLAG_CD
                    bset      ,y,#%00001000

                    jsr       PAUSE_RESUME
                    jsr       ESPERA
                    jmp       PLAY_RUTIN1

;*******************************************************************************

RUT_STOP            proc
                    lda       INDEX
                    sta       TEMP3
                    lda       #1
                    sta       INDEX
                    clr       BYTERAM
PONGO0
                    jsr       CAL_BASE
                    jsr       WR_RAM
                    lda       INDEX
                    inca
                    sta       INDEX
                    cmpa      #49                 ; on initialise le BUFFER (play list)
                    bne       PONGO0

                    lda       TEMP3
                    sta       INDEX

                    ldy       #FLAGSTATE
                    bclr      ,y,#MODE

                    ldy       #FLAG_CD
                    brclr     ,y,#%00100000,MUSIC2
ST_13
                    jsr       RDSTATUS
                    lda       STATUS_R
                    anda      #%10001000
                    jeq       LOOP

                    jsr       RD_DATA_R
                    bra       ST_13
MUSIC2
                    ldx       #ICO+2
                    lda       #%000010000         ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=1

                    lda       #1                  ; OFFSET=1 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    ldy       #FLAG_CD
                    bclr      ,y,#%00000011

                    jsr       ST_UNIT
                    jsr       ESPERA
                    jmp       LOOP

;*******************************************************************************

RUT_REW             proc
                    jsr       BIGTEMPO            ; tempo 100ms
                    jsr       BIGTEMPO            ; tempo 100ms
                    jsr       BIGTEMPO            ; tempo 100ms

                    clr       CONT1
                    lda       #225
                    sta       CONT2
ST_11
                    jsr       RDSTATUS
                    lda       STATUS_R
                    anda      #%10001000
                    beq       PL1

                    jsr       RD_DATA_R
                    bra       ST_11
PL1
                    ldy       #FLAGSTATE
                    brclr     ,y,#REW,NUEVVA

                    dec       INDEX_LIST
                    beq       SUITEPL1
                    jmp       PLAYY
SUITEPL1
                    lda       TOTAL_LIST
                    sta       INDEX_LIST
                    jmp       PLAYY
NUEVVA
                    ldy       #FLAGSTATE
                    bset      ,y,#REW
                    jmp       PLAYY

;*******************************************************************************

RUT_FWD             proc
                    jsr       BIGTEMPO            ; tempo 100ms
                    jsr       BIGTEMPO            ; tempo 100ms
                    jsr       BIGTEMPO            ; tempo 100ms

                    clr       CONT1
                    lda       #225
                    sta       CONT2
                    inc       INDEX_LIST
ST_12
                    jsr       RDSTATUS
                    lda       STATUS_R
                    anda      #%10001000
                    beq       PL2

                    jsr       RD_DATA_R
                    bra       ST_12
PL2
                    ldy       #FLAGSTATE
                    bset      ,y,#FRD
                    jmp       PLAYY

;*******************************************************************************
; routines de lectures en MODE MP3 ou audio

; lecture MP3

PLAY_MP3            jmp       PLAY_RUTIN1         ; lecture des CD MP3 non prévue dans la version lt.(voir MP3F1v2)

;*******************************************************************************
; lecture CD AUDIO

PLAY_DA             proc
                    ldx       #ICO
                    lda       #%000010000         ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=1

                    lda       #1                  ; OFFSET=1 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    inc       ADR_RL
                    bne       KOLO
                    inc       ADR_RH
KOLO
                    jsr       RD_RAM
                    lda       BYTERAM
                    sta       START_M

                    inc       ADR_RL
                    bne       KOLO1
                    inc       ADR_RH
KOLO1
                    jsr       RD_RAM
                    lda       BYTERAM
                    sta       START_S

                    inc       ADR_RL
                    bne       KOLO2
                    inc       ADR_RH
KOLO2
                    jsr       RD_RAM
                    lda       BYTERAM
                    sta       START_F

                    inc       INDEX
                    jsr       CAL_BASE
                    dec       INDEX

                    inc       ADR_RL
                    bne       KOLO3
                    inc       ADR_RH
KOLO3
                    jsr       RD_RAM
                    lda       BYTERAM
                    sta       END_M

                    inc       ADR_RL
                    bne       KOLO4
                    inc       ADR_RH
KOLO4
                    jsr       RD_RAM
                    lda       BYTERAM
                    sta       END_S

                    inc       ADR_RL
                    bne       KOLO5
                    inc       ADR_RH
KOLO5
                    jsr       RD_RAM
                    lda       BYTERAM
                    sta       END_F

                    jsr       PLAY_AUDIO_MSF
                    bsr       ESPERA
                    jmp       PLAY_RUTIN1

;*******************************************************************************
; Delays
;*******************************************************************************

ESPERA11            proc
                    ldy       #FLAG_CD
                    brclr     ,y,#%00100000,TEMPO200
                    ldx       #80                 ; 300us
                    bra       TEMPO_2us
TEMPO200
                    ldx       #50                 ; 200us
                    bra       TEMPO_2us
ESPERA
                    ldx       #140                ; 500us
                    bra       TEMPO_2us
BIGTEMPO
                    ldx       #30000              ; 100ms
                    bra       TEMPO_2us
ESPERA1
                    ldx       #10                 ; 40us
                    brn       *
TEMPO_2us
                    dex                           ; Aprox. 3.5 MicroSec. (X=1): 3(BRA)+3(dex)+3(bne)+5(rts)=14cycles
                    bne       TEMPO_2us           ; 14cyclesx250ns=3.5us
                    rts

;*******************************************************************************
;
;             ATA/ATAPI-4 Routines
;
;     - Low level ATA/ATAPI Routines
;     - Low level Ram Routines
;
;*******************************************************************************

RDSTATUS            proc
                    ldb       #D_STATUS
                    jsr       RUTR
                    stb       STATUS_R
                    rts

;*******************************************************************************

RDSTATA             proc
                    ldb       #DSTATUSA
                    jsr       RUTR
                    stb       STATUS_A
                    rts

;*******************************************************************************

RD_SC               proc
                    ldb       #DSECTORC
                    jsr       RUTR
                    stb       SECTOR_C
                    rts

;*******************************************************************************

RD_SN               proc
                    ldb       #DSECTORN
                    jsr       RUTR
                    stb       SECTOR_N
                    rts

;*******************************************************************************

RD_CL               proc
                    ldb       #DCYL_L
                    jsr       RUTR
                    stb       CYLIND_L
                    rts

;*******************************************************************************

RD_CH               proc
                    ldb       #DCYL_H
                    jsr       RUTR
                    stb       CYLIND_H
                    rts

;*******************************************************************************

RD_ERRORR           proc
                    ldb       #D_ERRORR
                    jsr       RUTR
                    stb       ERRORR
                    rts

;*******************************************************************************

RD_DATA_R           proc
                    clra                          ; PORTG IN
                    sta       DDRG
                    sta       DDRA                ; PORTA IN

                    lda       #D_DATA_R           ; CS1_=1 et CS0=DA2=DA1=DA0=0
                    ora       #%11100000
                    sta       IDE_REG

                    anda      #%11011111          ; READ_IDE : DIOW_=1 et DIOR_=0 (on garde CS1_=1)
                    sta       IDE_REG             ; adresse du registre IDE

                    nop

                    lda       PORTA               ; Read DD0..DD7 IDE
                    sta       DATA_RL
                    lda       PORTG               ; Read DD8..DD15 IDE
                    sta       DATA_RH

                    lda       #%11111000          ; DA0=DA1=DA2=0, CS0_=CS1_=DIOW_=DIOR_=RESET_=1
                    sta       IDE_REG
                    rts

;*******************************************************************************

WR_COMMAND          proc
                    lda       COMMAND
                    sta       TEMPW
                    ldb       #DCOMMAND           ; ON PASSE LES PARAMETRES PAR B
                    jsr       RUTW
                    rts

;*******************************************************************************

WR_SC               proc
                    lda       SECTOR_C
                    sta       TEMPW
                    ldb       #DSECTORC           ; ON PASSE LES PARAMETRES PAR B
                    jsr       RUTW
                    rts

;*******************************************************************************

WR_SN               proc
                    lda       SECTOR_N
                    sta       TEMPW
                    ldb       #DSECTORN           ; ON PASSE LES PARAMETRES PAR B
                    bsr       RUTW
                    rts

;*******************************************************************************

WR_CL               proc
                    lda       CYLIND_L
                    sta       TEMPW
                    ldb       #DCYL_L
                    bsr       RUTW
                    rts

;*******************************************************************************

WR_CH               proc
                    lda       CYLIND_H
                    sta       TEMPW
                    ldb       #DCYL_H
                    bsr       RUTW
                    rts

;*******************************************************************************

WR_DC               proc
                    lda       DEVICE_C
                    sta       TEMPW
                    ldb       #DDEVICEC
                    bsr       RUTW
                    rts

;*******************************************************************************

WR_DH               proc
                    lda       DEVICE_H
                    sta       TEMPW
                    ldb       #DDEVICEH
                    bsr       RUTW
                    rts

;*******************************************************************************

WR_FEATURES         proc
                    lda       FEATURES
                    sta       TEMPW
                    ldb       #D_FEATURES
                    bsr       RUTW
                    rts

;*******************************************************************************

WR_DATA_R           proc
                    lda       #$FF                ; PORTG OUT
                    sta       DDRG
                    sta       DDRA                ; PORTA OUT

                    ldb       #D_DATA_R           ; CS1_=1 et CS0=0
                    orb       #%11100000
                    stb       IDE_REG             ; adresse du registre IDE

                    lda       DATA_RL
                    sta       PORTA               ; Write DD0..DD7 IDE
                    lda       DATA_RH
                    sta       PORTG               ; Write DD8..DD15 IDE

                    andb      #%10111111          ; WRITE_IDE : DIOW_=0 et DIOR_=1 (on garde CS1_=1)
                    stb       IDE_REG

                    lda       #%11111000          ; DA0=DA1=DA2=0, CS0_=CS1_=DIOW_=DIOR_=RESET_=1
                    sta       IDE_REG
                    rts

;*******************************************************************************

RUTR                proc
                    orb       #%11100000          ; on traite seulement les infos concernant le portB(du PIC)
                    stb       IDE_REG

                    clra                          ; PORTG IN
                    sta       DDRG
                    sta       DDRA                ; PORTA IN

                    andb      #%11011111          ; on rajoute les infos concernant le portE(DIOR_=0)
                    stb       IDE_REG

                    nop

                    ldb       PORTA
                    lda       #%11111000          ; DA0=DA1=DA2=0, CS0_=CS1_=DIOW_=DIOR_=RESET_=1
                    sta       IDE_REG
                    rts

;*******************************************************************************

RUTW                proc
                    orb       #%11100000          ; on garde seulement les infos concernant le portB(du PIC)
                    stb       IDE_REG

                    lda       #$FF                ; PORTG OUT
                    sta       DDRG
                    sta       DDRA                ; PORTA OUT

                    lda       TEMPW
                    sta       PORTA

                    andb      #%10111111          ; WRITE_IDE : DIOW_=0 et DIOR_=1
                    stb       IDE_REG

                    lda       #%11111000          ; DA0=DA1=DA2=0, CS0_=CS1_=DIOW_=DIOR_=RESET_=1
                    sta       IDE_REG
                    rts

;*******************************************************************************
; routine de lecture en SRAM
;*******************************************************************************

RD_RAM              proc
                    ldb       ADR_RL              ; ACCU D = ADR_RH & ADR_RL ou utiliser directement un accu 16bits
                    lda       ADR_RH

                    addb      #OFFSET_L           ; poids faible ADAPTATION RAM
                    adda      #OFFSET_H           ; poids fort

                    xgdx                          ; echange X et D
                    lda       ,x                  ; Read Byte anywhere in the 64Kbytes adresse range.
                    sta       BYTERAM             ; store into BYTERAM
                    rts

;*******************************************************************************

WR_RAM              proc
                    ldb       ADR_WL              ; ACCU D=ADR_RH & ADR_RL ou utiliser directement un accu 16bits
                    lda       ADR_WH

                    addb      #OFFSET_L           ; poids faible ADAPTATION RAM
                    adda      #OFFSET_H           ; poids fort

                    xgdx                          ; echange X et D
                    lda       BYTERAM
                    sta       ,x                  ; Write Byte anywhere in the 64Kbytes adresse range.
                    rts

;*******************************************************************************

ST1                 proc
Loop@@              jsr       RDSTATUS            ; Wait: Bsy =0 & Drq =0
                    lda       STATUS_R
                    anda      #%10001000
                    beq       Done@@
                    ldy       #STATUS_R
                    brclr     ,y,#%00000001,Loop@@
Done@@              rts

;*******************************************************************************

ST2                 proc
Loop@@              jsr       RDSTATUS            ; Wait: Bsy =0 & Drq =1
                    lda       STATUS_R
                    anda      #%10001000
                    suba      #%00001000
                    beq       Done@@
                    ldy       #STATUS_R
                    brclr     ,Y,#%00000001,Loop@@
Done@@              rts

;*******************************************************************************

RD_BLOCK            proc
                    jsr       RD_ERRORR
                    jsr       RD_SC
                    jsr       RD_SN
                    jsr       RD_CL
                    jsr       RD_CH
                    rts

;*******************************************************************************

WR_BLOCK            proc
                    jsr       WR_FEATURES
                    jsr       WR_SC
                    jsr       WR_SN
                    jsr       WR_CL
                    jsr       WR_CH
                    jsr       WR_COMMAND
                    rts

;*******************************************************************************
; PACKET PARAMETERS

WR_PKT              proc
                    lda       BP0
                    sta       DATA_RL

                    lda       BP1
                    sta       DATA_RH

                    jsr       WR_DATA_R

                    lda       BP2
                    sta       DATA_RL

                    lda       BP3
                    sta       DATA_RH

                    jsr       WR_DATA_R

                    lda       BP4
                    sta       DATA_RL

                    lda       BP5
                    sta       DATA_RH

                    jsr       WR_DATA_R

                    lda       BP6
                    sta       DATA_RL

                    lda       BP7
                    sta       DATA_RH

                    jsr       WR_DATA_R

                    lda       BP8
                    sta       DATA_RL

                    lda       BP9
                    sta       DATA_RH

                    jsr       WR_DATA_R

                    lda       BP10
                    sta       DATA_RL

                    lda       BP11
                    sta       DATA_RH

                    jsr       WR_DATA_R
                    rts

;*******************************************************************************

DEVICE_SELECT       proc
                    jsr       ST1
                    clr       DEVICE_H
                    jsr       WR_DH
                    jsr       ST1
                    rts

;*******************************************************************************

INI_ATAPI           proc
                    bsr       DEVICE_SELECT
                    lda       #%00000010
                    sta       DEVICE_C
                    jsr       WR_DC
                    jsr       ST1

                    jsr       RD_BLOCK
                    lda       CYLIND_L
                    cmpa      #$14
                    bne       N_ATAPI

                    lda       CYLIND_H
                    cmpa      #$EB
                    bne       N_ATAPI

                    ldy       #FLAG
                    bclr      ,y,#%00000001       ; atapin=0
                    rts

N_ATAPI             ldy       #FLAG
                    bset      ,y,#%00000001       ; atapin=0
                    rts

;*******************************************************************************
; ATA COMMANDs

DEVICE_RESET        proc
                    bsr       DEVICE_SELECT
                    clr       FEATURES
                    clr       SECTOR_C
                    clr       SECTOR_N
                    clr       CYLIND_L
                    clr       CYLIND_H
                    clr       DEVICE_H
                    lda       #$8
                    sta       COMMAND
                    jsr       WR_BLOCK
                    rts

;*******************************************************************************
; ATAPI Command Packet

WR_C_P              proc
                    jsr       ST1
                    clr       FEATURES            ; Byte count limit 16k
                    clr       SECTOR_C
                    clr       SECTOR_N
                    lda       #$FF
                    sta       CYLIND_L
                    sta       CYLIND_H
                    clr       DEVICE_H
                    lda       #$A0
                    sta       COMMAND
                    jsr       WR_BLOCK
                    rts

;*******************************************************************************
; ATAPI COMMANDs

READ10              proc
                    bsr       WR_C_P
                    jsr       ST2
                    lda       #$28
                    sta       BP0
                    clr       BP1
                    lda       LBA_1
                    sta       BP5
                    lda       LBA_2
                    sta       BP4
                    lda       LBA_3
                    sta       BP3
                    lda       LBA_4
                    sta       BP2
                    clr       BP6
                    lda       TRANS_2
                    sta       BP7
                    lda       TRANS_1             ; Read 8 blockes (16Kbytes)
                    sta       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    rts

;*******************************************************************************

PLAY_AUDIO_MSF      proc
                    bsr       WR_C_P
                    jsr       ST2
                    lda       #$47
                    sta       BP0
                    clr       BP1
                    clr       BP2
                    lda       START_M
                    sta       BP3
                    lda       START_S
                    sta       BP4
                    lda       START_F
                    sta       BP5
                    lda       END_M
                    sta       BP6
                    lda       END_S
                    sta       BP7
                    lda       END_F
                    sta       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    jsr       RDSTATA
                    jsr       ST1
                    rts

;*******************************************************************************

READ_TOC            proc
                    jsr       WR_C_P
                    jsr       ST2
                    lda       #$43
                    sta       BP0
                    lda       #$02
                    sta       BP1
                    clr       BP2
                    clr       BP3
                    clr       BP4
                    clr       BP5
                    clr       BP6
                    lda       #$10
                    sta       BP7
                    clr       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    jsr       RDSTATA
                    jsr       ST2
                    rts

;*******************************************************************************

REQUEST_SENSE       proc
                    jsr       WR_C_P
                    jsr       ST2
                    lda       #$03
                    sta       BP0
                    clr       BP1
                    clr       BP2
                    clr       BP3
                    lda       #$12
                    sta       BP4
                    clr       BP5
                    clr       BP6
                    clr       BP7
                    clr       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    jsr       RDSTATA
                    jsr       ST2

                    ldy       #STATUS_R
                    brclr     ,y,#%00000001,SUITE3 ; err=0
                    rts

SUITE3
          ;-------------------------------------- ; Byte Returned
                    jsr       RD_DATA_R           ; 0,1
                    jsr       RD_DATA_R           ; 2,3
                    lda       DATA_RL
                    anda      #%00001111
                    sta       TEMP1               ; Sense Key
                    jsr       RD_DATA_R           ; 4,5
                    jsr       RD_DATA_R           ; 6,7
                    jsr       RD_DATA_R           ; 8,9
                    jsr       RD_DATA_R           ; 10,11
                    jsr       RD_DATA_R           ; 12,13
                    lda       DATA_RL
                    sta       TEMP2               ; ASC
                    lda       DATA_RH
                    sta       TEMP3               ; ASCQ
                    jsr       RD_DATA_R           ; 14,15
                    jsr       RD_DATA_R           ; 16,17
                    rts

;*******************************************************************************

ST_UNIT             proc
                    jsr       WR_C_P
                    jsr       ST2
                    lda       #$1B
                    sta       BP0
                    clr       BP1
                    clr       BP2
                    clr       BP3
                    lda       FLAG_CD
                    sta       BP4
                    clr       BP5
                    clr       BP6
                    clr       BP7
                    clr       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    jsr       RDSTATA
                    jsr       ST1
                    rts

;*******************************************************************************

PAUSE_RESUME        proc
                    jsr       WR_C_P
                    jsr       ST2
                    lda       #$4B
                    sta       BP0
                    clr       BP1
                    clr       BP2
                    clr       BP3
                    clr       BP4
                    clr       BP5
                    clr       BP6
                    clr       BP7

                    clra
                    ldy       #FLAG_CD
                    brclr     ,y,#%00001000,SUITE4
                    inca
SUITE4
                    sta       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    jsr       RDSTATA
                    jsr       ST1
                    rts

;*******************************************************************************

READ_SUB            proc
                    jsr       WR_C_P
                    jsr       ST2
                    lda       #$42
                    sta       BP0
                    lda       #$02
                    sta       BP1
                    lda       #$40
                    sta       BP2
                    lda       #$01
                    sta       BP3
                    clr       BP4
                    clr       BP5
                    clr       BP6
                    clr       BP7
                    lda       #15
                    sta       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    jsr       RDSTATA
                    jsr       ST2
          ;-------------------------------------- ; Byte Returned
                    jsr       RD_DATA_R           ; 1
                    lda       DATA_RH
                    sta       TEMP1
                    jsr       RD_DATA_R
                    jsr       RD_DATA_R
                    jsr       RD_DATA_R
                    jsr       RD_DATA_R
                    jsr       RD_DATA_R
                    jsr       RD_DATA_R
                    jsr       RD_DATA_R
                    rts

;*******************************************************************************

SET_MAX_SPEED       proc
                    jsr       WR_C_P
                    jsr       ST2
                    lda       #$BB
                    sta       BP0
                    clr       BP1
                    clra                          ; Set Max Speed x1 (150 KB/s)
                    sta       BP2
                    lda       #$96
                    sta       BP3
                    clr       BP4
                    clr       BP5
                    clr       BP6
                    clr       BP7
                    clr       BP8
                    clr       BP9
                    clr       BP10
                    clr       BP11
                    jsr       WR_PKT
                    rts

;*******************************************************************************
;
;           Management of the CD Routines
;
;     - Check The initial Cd Status
;     - Read TOC
;     - Detect if CD-DA or CD-Mp3
;     - Get Total Songs number
;     - Table info of Songs in Ram (position in cd,name,length,etc)
;
;*******************************************************************************

SENSE               proc
                    jsr       REQUEST_SENSE

                    clra
                    cmpa      TEMP2
                    bne       SUITE5
                    rts
SUITE5
                    lda       #$28
                    cmpa      TEMP2
                    bne       SUITE6
                    rts
SUITE6
                    lda       #$29
                    cmpa      TEMP2
                    bne       SUITE7
                    rts
SUITE7
                    ldy       #FLAG_CD
                    bclr      ,y,#%00010000
                    lda       #$3A
                    cmpa      TEMP2
                    beq       SUITE82
                    rts
SUITE82
                    ldy       #FLAG_CD
                    bset      ,y,#%00010000
                    ldx       #M_1
                    lda       #%00011111          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jmp       MOSTRA              ; pas de CD chargé... à vérifier...

;*******************************************************************************

INI_CD              proc
                    clr       FLAG_CD
                    clr       TOTAL_SONG

                    bsr       SENSE
                    jsr       ESPERA

                    bsr       SENSE
                    jsr       ESPERA
HASTA_NOERROR
                    jsr       READ_TOC

                    ldy       #STATUS_R
                    brset     ,y,#%00000001,SUITE83 ; err=0
                    bra       DALE
SUITE83
                    bsr       SENSE

                    ldy       #FLAG_CD
                    brclr     ,y,#%00010000,HASTA_NOERROR
                    rts

;*******************************************************************************

DALE                proc
                    jsr       RD_DATA_R           ; decremente 2 données du TOC
                    lda       DATA_RH
                    sta       COUNTER
                    dec:2     COUNTER

                    jsr       RD_DATA_R           ; 2 données qui ne servent pas
                    clr       ADR_WL
                    clr       ADR_WH
                    lda       #1
                    sta       COUNTER2

DIFICIL
                    inc       ADR_WL              ; remplaçable par un compteur 16bits directement.
                    bne       KA0
                    inc       ADR_WH
          ;--------------------------------------
KA0
                    jsr       RD_DATA_R
                    lda       DATA_RH
                    anda      #%00000100
                    bne       CDROM
                    ldy       #FLAG_CD
                    bset      ,y,#%01000000
                    bra       VENDA

CDROM               ldy       FLAG_CD
                    bset      ,y,#%00100000

VENDA
                    jsr       RD_DATA_R
          ;--------------------------------------
                    jsr       RD_DATA_R           ; on met les "coordonées" MSF en RAM de chaque piste audio
                    lda       DATA_RH
                    sta       BYTERAM
                    jsr       WR_RAM

                    jsr       RD_DATA_R
                    inc       ADR_WL              ; remplaçable par un compteur 16bits directement.
                    bne       KA1
                    inc       ADR_WH

KA1
                    lda       DATA_RL
                    sta       BYTERAM
                    jsr       WR_RAM


                    inc       ADR_WL              ; remplaçable par un compteur 16bits directement.
                    bne       KA2
                    inc       ADR_WH
KA2
                    lda       DATA_RH
                    sta       BYTERAM
                    jsr       WR_RAM
          ;--------------------------------------
                    inc       ADR_WL              ; piste suivante
                    bne       SUITE71
                    inc       ADR_WH
          ;--------------------------------------
SUITE71
                    inc       COUNTER2
          ;--------------------------------------
                    inc       TOTAL_SONG

                    lda       COUNTER             ; decremente par 8 le compteur du TOC
                    suba      #8
                    sta       COUNTER
                    bne       DIFICIL
;SUITE72                                          ; mise en RAM des coordonnées/track/numero des pistes terminé
                    ldx       #M_9                ; efface 1ere ligne
                    lda       #%00001111          ; affichage du message sur la premiere ligne
                    sta       LCDINFO
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE

                    ldx       #M_2                ; MESSAGE "CD-DA"
                    lda       #%00001111          ; affichage du message sur la premiere ligne
                    sta       LCDINFO
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET

                    dec       TOTAL_SONG
                    ldy       #FLAG_CD
                    brclr     ,y,#%01000000,DATA_CD

                    brclr     ,y,#5,MOSTRA

                    ldx       #M_4                ; MESSAGE "CD-HYBRID"
;                   bset      ,Y,#%00010000       ; Hybrid
                    brn       *

;*******************************************************************************

MOSTRA              proc
                    jsr       _AFFICHAGE          ; On affiche "CD-DA, CD-MP3 ou CD-Hybrid"
                    jsr       ESPERA
                    rts

;*******************************************************************************

DATA_CD             proc
                    clr       TOTAL_SONG
                    ldx       #M_3                ; MESSAGE "CD-MP3"

                    bsr       MOSTRA
                    jsr       BIGTEMPO
                    jsr       BIGTEMPO
                    jsr       BIGTEMPO
                    jsr       BIGTEMPO

                    clra
                    sta       BYTERAM
                    bsr       FULLBUFX

                    ldx       #M_13               ; fonction non implémentée
                    lda       #%00011111          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    bsr       _AFFICHAGE          ; On affiche

                    ldy       #FLAG_CD            ; on ne lit pas les CD-MP3 dans la version lt.
                    bset      ,y,#%00010000
                    rts

;*******************************************************************************
;
;                       Ram Routines
;           - Move datas from IDE to RAM
;           - Fill RAM
;           - Calculate Source and Destination ram address
;
;*******************************************************************************

;*******************************************************************************
;        BUFFER : Ram (Internal Static Ram 1024 bytes)
;*******************************************************************************

FULLBUFX            proc
                    clr       ADR_WL              ; CHARGEMENT ADRESSE de base BUFFER en RAM interne HC11F1
                    clr       ADR_WH
Loop@@              jsr       WR_RAM

                    inc       ADR_WL
                    bne       Cont@@
                    inc       ADR_WH
          ;--------------------------------------
Cont@@              clra                          ; on exprime ici la taille du BUFFER
                    cmpa      ADR_WH              ; (ici il fait 198 octets soit 198/4=49pistes max...)
                    bne       Loop@@

                    lda       #$C6
                    cmpa      ADR_WL
                    bne       Loop@@
                    rts

;*******************************************************************************
; La routine CAL_BASE  donne l'adresse en RAM de la "base" de la piste
; correspondant au CD chargé.
; Le numéro de la piste est spécifié par INDEX. La routine donne alors ADR_WL,
; ADR_WH et ADR_RL,ADR_RH la premiere adresse correspondant au 4octets de chaque
; pistes réservés en RAM, on lit alors le numéro dans la playlist, ensuite
; DATA_RH, puis DATA_RL, DATA_RH de la piste choisi. (voir cartographie BUFFER).
;*******************************************************************************

CAL_BASE            proc
                    lda       INDEX
                    sta       TEMP1
                    dec       TEMP1
                    clr       TEMP2

                    clc                           ; multiplie par 4 car une piste occupe 4 octets en RAM...
                    rol       TEMP1
                    rol       TEMP2
                    clc
                    rol       TEMP1
                    lda       TEMP1
                    rol       TEMP2

                    sta       ADR_RL
                    lda       TEMP2
                    sta       ADR_RH

                    lda       ADR_RL
                    sta       ADR_WL
                    lda       ADR_RH
                    sta       ADR_WH
                    rts

;*******************************************************************************
;
;  Affichage de message sur la carte de développement 68HC11F1
;     avec extension entrée/sortie :
;               -LCD CONTROL : $1800    RS=D4  E=D2 R/W=D3
;               -LCD DATA :    $1801    (D0-D7)
;
;*******************************************************************************

_INITAFF            proc
                    lda       #%00111000          ; valide bus 8bits +double ligne
                    jsr       _VALC

                    lda       #%00001100          ; allume l'écran, curseur non visible  (controle)
                    jsr       _VALC
                    lda       #%00000110          ; décallage à droite du curseur
                    jsr       _VALC
                    lda       #%00000001          ; efface afficheur (DD-RAM)
                    jsr       _VALC

                    lda       #%00100000          ; efface CG-RAM adress
                    jsr       _VALC
                    rts

;*******************************************************************************

_AFFICHAGE          proc
                    psha
                    pshb
                    pshx
                    pshy
                    ldy       #LCDINFO
                    ldb       ,y                  ; charge la longueur du message
                    andb      #$0F
                    addb      #2
                    stb       LONGUEUR
                    brset     ,y,#$10,SUITE1      ; sur quelle ligne on affiche ?
          ;-------------------------------------- ; affichage sur la première ligne
                    lda       #%10000000          ; curseur à gauche, 1ère ligne
                    jsr       _VALC
Loop@@              lda       OFFSET
                    beq       AFF1
                    lda       #%00010100          ; déplacement du curseur vers la droite
                    bsr       _VALC
                    dec       OFFSET
                    bne       Loop@@
          ;--------------------------------------
AFF1                lda       ,x                  ; contenu de l'adresse pointée par X
                    dec       LONGUEUR            ; dans A
                    beq       FIN2                ; si LONGUEUR=0, fin du message
                    bsr       _VALD
                    inx
                    bra       AFF1
;                   bra       FIN2

;*******************************************************************************
;  affichage sur la deuxième ligne

SUITE1              proc
                    lda       #%11000000          ; curseur à gauche, 2ième ligne
                    bsr       _VALC
Loop@@              lda       OFFSET
                    beq       AFF1
                    lda       #%00010100          ; déplacement du curseur vers la droite
                    bsr       _VALC
                    dec       OFFSET
                    bne       Loop@@
          ;--------------------------------------
AFF2                lda       ,x                  ; idem avant
                    dec       LONGUEUR
                    beq       Done@@              ; si LONGUEUR=0, fin du message
                    bsr       _VALD
                    inx
                    bra       AFF2
FIN2
Done@@              puly
                    pulx
                    pulb
                    pula
                    rts

;*******************************************************************************
; gestion des caractéres personalisés
; stockage des icones dans la RAM du LCD

INICONES            proc
                    ldx       #ICONES
                    lda       #%01000000          ; pointe le 1er caractere en CG-RAM
                    bsr       SP1
                    ldb       #40                 ; compteur= 5 icones x 8 = 40 boucles
Loop@@              lda       ,x
                    bsr       SP2
                    inx
                    decb
                    bne       Loop@@
                    rts

;*******************************************************************************

SP1                 proc
                    sta       LCD_DATA            ; Validation des commandes
                    lda       #%00000000          ; E=0 RS=0
                    sta       LCD_CONTROL
                    lda       #%00000100          ; E=1 RS=0
                    sta       LCD_CONTROL
                    lda       #%00000000          ; E=0 RS=0
                    sta       LCD_CONTROL
                    lda       #160
Loop@@              deca
                    bne       Loop@@
                    rts

;*******************************************************************************

SP2                 proc
                    sta       LCD_DATA            ; Validation des donnees
                    lda       #%00010000          ; E=0 RS=1
                    sta       LCD_CONTROL
                    lda       #%00010100          ; E=1 RS=1
                    sta       LCD_CONTROL
                    lda       #%00010000          ; E=0 RS=1
                    sta       LCD_CONTROL
                    lda       #160
Loop@@              deca
                    bne       Loop@@
                    rts

;*******************************************************************************

_VALC               proc
                    sta       LCD_DATA            ; Validation des commandes
                    lda       #%00000000          ; E=0 RS=0
                    sta       LCD_CONTROL
                    lda       #%00000100          ; E=1 RS=0
                    sta       LCD_CONTROL
                    lda       #%00000000          ; E=0 RS=0
                    sta       LCD_CONTROL
                    bsr       TEMPO
                    rts

;*******************************************************************************

_VALD               proc
                    sta       LCD_DATA            ; Validation des donnees
                    lda       #%00010000          ; E=0 RS=1
                    sta       LCD_CONTROL
                    lda       #%00010100          ; E=1 RS=1
                    sta       LCD_CONTROL
                    lda       #%00010000          ; E=0 RS=1
                    sta       LCD_CONTROL
                    bsr       TEMPO
                    rts

;*******************************************************************************

TEMPO               proc                          ; tempo (20*60us)
                    pshx
                    ldx       #20
Loop@@              bsr       _TEMPO120
                    dex
                    bne       Loop@@
                    pulx
                    rts

;*******************************************************************************

_TEMPO120           proc
                    pshx
                    ldx       #33
Loop@@              dex
                    bne       Loop@@
                    pulx
                    rts

;*******************************************************************************

NUMERO_SONG         proc
                    ldx       #M_5                ; affichage de "Tracks found:"
                    lda       #%00011111          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=16
                    lda       #16
                    sta       LONGUEUR
                    clra                          ; OFFSET=0 caractère
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche

                    ldb       TOTAL_SONG
                    bsr       BCD2

                    ldx       #DIG_3              ; affichage en BCD de nombre de pistes trouvées
                    lda       #%00010010          ; affichage du message sur la deuxieme ligne
                    sta       LCDINFO             ; LONGUEUR=3
                    lda       #13                 ; OFFSET=14 caractères
                    sta       OFFSET
                    jsr       _AFFICHAGE          ; On affiche
                    rts

;*******************************************************************************
; Binary To BCD Conversion Routine
;*******************************************************************************

BCD2                proc
                    stb       L_BYTE              ; la donnée hexa à convertir est dans B

                    clra
                    ldx       #100
                    idiv
                    xgdx
                    addb      #$30
                    stb       DIG_3
                    xgdx
                    ldx       #10
                    idiv
                    addb      #$30
                    stb       DIG_1
                    xgdx
                    addb      #$30
                    stb       DIG_2
                    rts

;*******************************************************************************
; Keyboard Routine
; Scan Keys Pressed
;*******************************************************************************

RD_KEY              proc
                    clr       FLAGKEY1
                    clr       BYTERAM
                    lda       #1
                    sta       COUNTER

LOOP22
                    lda       KEYBOARD
                    sta       BYTERAM
                    rts

;*******************************************************************************

PLAY_PRESS          proc
                    ldy       #BYTERAM
                    brclr     ,y,#PLAY,NOAPRE1
                    bra       CHE1

NOAPRE1
                    ldy       #FLAGKEY
                    bclr      ,y,#PLAY
                    rts

CHE1
                    ldy       #FLAGKEY
                    brclr     ,y,#PLAY,SUITE21
                    rts

SUITE21
                    ldy       #FLAGKEY
                    bset      ,y,#PLAY
                    ldy       #FLAGKEY1
                    bset      ,y,#PLAY
                    rts

;*******************************************************************************

REW_PRESS           proc
                    ldy       #BYTERAM
                    brclr     ,y,#REW,NOAPRE2
                    bra       CHE2


NOAPRE2
                    ldy       #FLAG
                    brclr     ,y,#%00001000,PEPE  ; mantiene=3
                    lda       #15
                    sta       CONT1


PEPE
                    ldy       #FLAGKEY
                    bclr      ,y,#REW
                    rts

CHE2
                    ldy       #FLAGKEY
                    brclr     ,y,#REW,GOMA

                    ldy       #FLAG
                    brset     ,y,#%00001000,SUITECH2 ; mantiene=3
                    rts

SUITECH2
                    dec       CONT1
                    beq       SUITECH3
                    rts

SUITECH3
                    lda       #3
                    sta       CONT1
GOMA
                    ldy       #FLAGKEY
                    bset      ,y,#REW
                    ldy       #FLAGKEY1
                    bset      ,y,#REW
                    rts

;*******************************************************************************

FRD_PRESS           proc
                    ldy       #BYTERAM
                    brclr     ,y,#FRD,NOAPRE3
                    bra       CHE3

NOAPRE3
                    ldy       #FLAG
                    brclr     ,y,#%00001000,PEPE2 ; mantiene=3
                    lda       #15
                    sta       CONT1


PEPE2
                    ldy       #FLAGKEY
                    bclr      ,y,#FRD
                    rts

CHE3
                    ldy       #FLAGKEY
                    brclr     ,y,#FRD,GOMA1

                    ldy       #FLAG
                    brset     ,y,#%00001000,SUITECH4 ; mantiene=3
                    rts

SUITECH4            dec       CONT2
                    beq       SUITECH5
                    rts

SUITECH5            lda       #3
                    sta       CONT2
GOMA1               ldy       #FLAGKEY
                    bset      ,y,#FRD
                    ldy       #FLAGKEY1
                    bset      ,y,#FRD
                    rts

;*******************************************************************************

STOP_PRESS          proc
                    ldy       #BYTERAM
                    brclr     ,y,#STOP,NOAPRE4
                    bra       CHE4

NOAPRE4             ldy       #FLAGKEY
                    bclr      ,y,#STOP
                    rts

CHE4                ldy       #FLAGKEY
                    brclr     ,y,#STOP,SUITE22
                    rts

SUITE22             ldy       #FLAGKEY
                    bset      ,y,#STOP
                    ldy       #FLAGKEY1
                    bset      ,y,#STOP
                    rts

;*******************************************************************************

MODE_PRESS          proc
                    ldy       #BYTERAM
                    brclr     ,y,#MODE,NOAPRE5
                    bra       CHE5

NOAPRE5             ldy       #FLAGKEY
                    bclr      ,y,#MODE
                    rts

CHE5
                    ldy       #FLAGKEY
                    brclr     ,y,#MODE,SUITE23
                    rts

SUITE23             ldy       #FLAGKEY
                    bset      ,y,#MODE
                    ldy       #FLAGKEY1
                    bset      ,y,#MODE
                    rts
