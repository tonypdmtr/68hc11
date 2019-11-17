*************************************
* HC11_DRIFT
* Version 1.0
* Copyright Gert Gottschalk (drgert1@yahoo.com)
* Algorithm by Paul Mortfield (paul@BackyardAstronomer.com)
* You may copy this software for non commercial use,
* as long as you mention the original developer's copyright
*************************************


* Instructions:
*   1  running Buffalo
*   2  "load t" (enter)
*   3  send the file
*	HyperTerminal (transfer, send text file, all files)
*   4  wait for completion of file send
*   5  "md 2000" (enter) will display the memory containing the program
*   6  "go 2000" (enter) to run the program

*************************************
* Major switches for assembly control
*************************************

DEBUG		EQU	0
ST4_90		EQU	0
GGMODE		EQU	0

*************************************
* Fatal Error Codes
*************************************

*	01	rd_stat checksum error (10x)
*	02	ST4 fails to respond to reset
*	03	wr_stat $06 response failure
*	04	rd_xref checksum error (10x)
*	05	rd_yref checksum error (10x)
*	06	wr_xref $06 response failure
*	07	wr_yref $06 response failure
*	10	rd_stat response timeout error
*	11	wr_stat response timeout error
*	12	rd_xref response timeout error
*	13	rd_yref response timeout error
*	14	wr_xref response timeout error
*	15	wr_yref response timeout error

************************************************************************
* REGISTER (define registers within the CPU)
************************************************************************

PORTA		EQU	$00		; PORT A
DDRA		EQU	$01		; PORT A DDR
PORTG		EQU	$02		; PORT G
DDRG		EQU	$03		; PORT G DDR
PORTB		EQU	$04		; PORT B
PORTCL		EQU	$05		; PROT C LATCHED DATA REG.
DDRC		EQU	$07		; DATA DIRECTION REGISTER C
PORTD		EQU	$08		; PORT D
DDRD		EQU	$09		; DATA DIRECTION REGISTER D
PORTE		EQU	$0A		; PORT E
CFORC		EQU	$0B		; TIMER COMPARE FORCE REG.
OC1M		EQU	$0C		; O/P COMPARE 1 MASK REG.
OC1D		EQU	$0D		; O/P COMPARE 1 DATA REG
TCNT		EQU	$0E		; TIMER COUNT H
TCNTL		EQU	$0F		;             L
TIC1		EQU	$10		; TIMER O/P COMPARE 1 H
TIC1L		EQU	$11		;                   1 L
TIC2		EQU	$12		;                   2 H
TIC2L		EQU	$13		;                   2 L
TIC3		EQU	$14		;                   3 H
TIC3L		EQU	$15		;                   3 L
TOC1		EQU	$16		; TIMER O/P COMPARE	1 H
TOC1L		EQU	$17		;			1 L
TOC2		EQU	$18		;			2 H
TOC2L		EQU	$19		;			2 L
TOC3		EQU	$1A		;			3 H
TOC3L		EQU	$1B		;			3 L
TOC4		EQU	$1C		;			4 H
TOC4L		EQU	$1D		;			4 L
TOC5		EQU	$1E		;			5 H
TOC5L		EQU	$1F		;			5 L
TCTL1		EQU	$20		; TIMER CONTROL REG 1
TCTL2		EQU	$21		; TIMER CONTROL REG 2
TMSK1		EQU	$22		; TIMER MASK 1
TFLG1		EQU	$23		; TIMER FLAG 1
TMSK2		EQU	$24		; TIMER MASK 2
TFLG2		EQU	$25		; TIMER FLAG 2
PACTL		EQU	$26		; PULSE ACCUMULATOR CONT. REG.
PACNT		EQU	$27		; PULSE ACCUMULATOR COUNT REG.
SPCR		EQU	$28		;
SPSR		EQU	$29		;
SPDR		EQU	$2A		;
BAUD		EQU	$2B		; SCI BAUD REG
SCCR1		EQU	$2C		; SCI CONTROL 1 REG
SCCR2		EQU	$2D		; SCI CONTROL 2 REG
SCSR		EQU	$2E		; SCI STATUS REG
SCDR		EQU	$2F		; SCI DATA REG
ADCTL		EQU	$30		;
ADR1		EQU	$31		;
ADR2		EQU	$32		;
ADR3		EQU	$33		;
ADR4		EQU	$34		;
BPROT		EQU	$35		;
OPTION		EQU	$39		; OPTION REG
COPRST		EQU	$3A		; COP RESET REG
PPROG		EQU	REGBS+$3B	; EEPROM PROG REG
HPRIO		EQU	$3C		; HPRIO REG
INIT		EQU	$3D		; INIT
CONFIG		EQU	$3F		; CONFIG REG
CSSTRH		EQU	$5C		; CS CYCLE STRETCH
CSCTL		EQU	$5D		; CS CONTROL
CSGADR		EQU	$5E		; GENERAL PURPOSE CS (RAM)
CSGSIZ		EQU	$5F		; GENERAL PURPOSE CS SIZE

************************************************************************
* Constants (define references that do not change)
************************************************************************
RAM_SIO	EQU	$00C5		; Buffalo sio IRQ jmp address
RAM_RTI	EQU	$00EC		; Buffalo timer IRQ jmp address
REGBS	EQU	$1000		; start of CPU register block
LED	EQU	%10000000	; led output bit mask
DATA	EQU	%00000001	; LED port data mask
CLK	EQU	%00000010	; LED port clk mask
LOAD	EQU	%00000100	; LED port load mask
DRFDG0	EQU	$01
DRFDG1	EQU	$02
DRFDG2	EQU	$03
DRFRMP	EQU	$04
ANGDG0	EQU	$05
ANGDG1	EQU	$06
ANGDG2	EQU	$07
ANGRMP	EQU	$08

ALLKEY	EQU	%00111100	; Mask for keys
ANGKEY	EQU	%00001100
ANGPL	EQU	%00001000
ANGMI	EQU	%00000100
DRFKEY	EQU	%00110000
DRFPL	EQU	%00100000
DRFMI	EQU	%00010000
ENTKEY	EQU	%00001000

XCSTHI	equ	$0000
XCSTMD	equ	$0296		; 180*3600*60*0.01375*255/PI
XCSTLO	equ	$1FBE		;	= 43392959 = $0000 0296 1FBE

YCSTHI	equ	$0000
YCSTMD	equ	$0302		; 180*3600*60*0.01600*255/PI
YCSTLO	equ	$78B9		;	= 50493625 = $0000 0302 78B9

ST4ST	equ	$002f
ST4XRF	equ	$7E7C
ST4YRF	equ	$7E7D

ST4XMAX	equ	192
ST4YMAX	equ	165

	ORG	$0800		; variable area
pdmir	rmb	1		; mirror of portd keys
pamir	rmb	1		; mirror of porta keys
angl	rmb	2		; angle
angmod	rmb	1		; angle mod 90
quadr	rmb	1		; quadrant
drift	rmb	2		; dirft
flen	rmb	2		; focal length
xrmplod	rmb	2		; led ramp counter load value x
xrmpcnt	rmb	2		; led ramp counter x
xrmpld	rmb	1		; led for x ramp
yrmplod	rmb	2		; led ramp counter load value y
yrmpcnt	rmb	2		; led ramp coutner y
yrmpld	rmb	1		; led for y ramp
rgled	rmb	1		; mirror red/green LED
entflg	rmb	1		; enter key pressed
cor_mod	rmb	1		; correction active mode flag
timout	rmb	1		; time out counter
sec_cnt	rmb	1		; seconds counter for timer irq 1/122
sec_flg	rmb	1		; 1 second toggle flag (chg each 1/2 sec)
lst_sec	rmb	1		; previous value of 1 sec flag
key_cnt	rmb	1		; 1/5 sec counter for key query
angdsp0	rmb	1		; mirror angle digit 0
angdsp1	rmb	1		; mirror angle digit 1
angdsp2	rmb	1		; mirror angle digit 2
drfdsp0	rmb	1		; mirror drift digit 0
drfdsp1	rmb	1		; mirror drift digit 1
drfdsp2	rmb	1		; mirror dirft digit 2
tmp	rmb	2		; temp storage
sendbuf	rmb	16		; send buffer for irq serial io
sendrd	rmb	2		; send buffer write ptr
sendwr	rmb	2		; send buffer read ptr
recvbuf	rmb	16		; receive buffer for irq serial io
recvrd	rmb	2		; receive buffer write ptr
recvwr	rmb	2		; receive buffer read ptr
scsr_mr	rmb	1		; SIO status reg mirror
rtiv_mr	rmb	2		; orig rti vector
siov_mr	rmb	2		; orig sio vector
st4refx	rmb	1		; reference x-coord from ST4
st4refy	rmb	1		; reference y-coord from ST4
st4buf	rmb	8		; receive buffer from st4
errcnt	rmb	1		; error counter for ST4 communication
errcnt1	rmb	1		; 2nd levl errcount
countx	rmb	2		; 1sec down x-counter for next ST4 ref x-coord modify
county	rmb	2		; 1sec down y-counter for next ST4 ref y-coord modify
cntxld	rmb	2		; 1sec down x-counter load value
cntyld	rmb	2		; 1sec down y-counter load value
xvalid	rmb	1		; flag if x corrections are enabled
yvalid	rmb	1		; flag if y corrections are enabled

*******************************************
*************     Stuff for multiply divide
*******************************************

CNTR     RMB 1  ; Counter for mul/div
DIVPTR   RMB 2  ; Ptr to MS byte of divisor
DIVCNT   RMB 1  ; Number of bytes in the divisor
DVDCNT   RMB 1  ; Number of bytes in the dividend
QPTR     RMB 2  ; Ptr to current byte of quotient
QCNT     RMB 1  ; Number of bytes remaining to be found in quotient
TRIALQ   RMB 1  ; Current quotient trial value

	        ; Temp storage for multiply
A24      RMB 3  ; Argument for multiple precision mul
B24      RMB 3  ; Argument for multiple precision mul
A48      RMB 6  ; Product from multiple precision mul

	        ; Temp storage for  divide
D24      RMB 3  ; Divisor
Q24      RMB 3  ; Quotient - Must immediately precede dividend
D48      RMB 6  ; Dividend

	        ; Temp storage for  divide
TEMP     RMB 2  ; Work space for divide

************************************************************************
************************************************************************
************; P R O G R A M   S T A R T S   H E R E ********************
************************************************************************
************************************************************************
$IF DEBUG = 0
	ORG	$C000		; location in FLASH RAM
$ELSE
	ORG	$2000
$ENDIF

*************

start	sei			; disable irq to set real time irq routine
$IF DEBUG = 0
	lds	#$07FF
$ENDIF
	ldx	#REGBS		; register base address
	ldaa	DDRA,x
	anda	#%11110111	; make 'ent' key -> input
	ora	#%10000111	; make LED interface -> output
	staa	DDRA,x
	ldaa	DDRD,x
	anda	#%11000011	; make pd2-pd5 'keys' -> input
	staa	DDRD,x
	ldaa	#%00000001
	staa	PACTL,X
	bset	TMSK2,X,%01000000	; allow real time irq to cause int
	ldaa	#%00110000		; first prescaler/13 sec ps/1 => 9600bd@8MHz
	staa	BAUD,x

$IF DEBUG = 1
	ldd	RAM_RTI		; in Debug mode use buffalo jump vectors
	std	rtiv_mr
	ldd	#isr_RTI
	std	RAM_RTI		; replace with ROM irq later

	ldd	RAM_SIO
	std	siov_mr
	ldd	#isr_SCI
	std	RAM_SIO		; replace with ROM irq later
	ldaa	#%00000000
$ELSE
	ldaa	#%00010000		; bit8+7 are recv bit8 and send bit8
$ENDIF
	staa	SCCR1,x
	ldaa	#%00101100		; enable transmitter and receiver of SCI
	staa 	SCCR2,x

	ldd	#sendbuf	;  init sio queues prior to first possible irq
	std	sendrd
	std	sendwr
	ldd	#recvbuf
	std	recvrd
	std	recvwr

$IF GGMODE = 1
	ldd	#180		; maps to 1800mm Focal Length in GGs setup
	std	drift
$ENDIF

	ldd	#0
	std	angl

$IF GGMODE = 0
	std	drift
$ENDIF

	std	xrmplod
	std	xrmpcnt
	std	yrmplod
	std	yrmpcnt
	staa	entflg
	staa	xrmpld
	staa	yrmpld
	staa	rgled
	staa	cor_mod
	staa	timout
	staa	sec_cnt
	staa	sec_flg
	staa	lst_sec
	staa	key_cnt
	staa	xvalid
	staa	yvalid
$IF DEBUG = 0
	staa	st4refx
	staa	st4refy
$ELSE
	ldaa	#80
	staa	st4refx
	ldaa	#70
	staa	st4refy
$ENDIF

	ldx	#REGBS		; Eat up any trash in serial input data reg
start10	ldaa	SCSR,x
	anda	#%00100000
	beq	start20
	ldaa	SCDR,x		; count shows -> one read is enough to clr bit
	bra	start10

start20	cli

	jsr	ang2rmp
	jsr	drf2rmp
	jsr	ang2dsp
	jsr	dft2dsp
	jsr	dispang
	jsr	dispdrf
	jsr	initled		; show 'FL ' on left display
	jsr	ldgren

$IF DEBUG = 1
	ldaa	#'I'
	jsr	sendv24
	jsr	crlf

$ELSE
start21	jsr	rd_stat		; retrieve status
	ora	#$20		; set bit 5 -> call for interrupt ST4
	jsr	wr_stat

	ldaa	#10
	staa	errcnt1		; initialize error counter
start22	jsr	rd_stat
	anda	#$20		; wait for status to clear
	beq	start23		; done
	dec	errcnt1
	bne	start22
	ldd	#02		; if 10 errors encountered -> goto fatal halt
	jmp	fatal
$ENDIF

$IF DEBUG = 1
	jsr	ldorng
	ldaa	#$ff
	staa	timout
startxx	tst	timout
	bne	startxx
$ENDIF

start23	ldaa	#$09		; hi	decode mode
	ldab	#%00000111	; lo	left BIN right BCD
	jsr	disp
	ldaa	#ANGDG2
	ldab	#%01000111	; 'F'
	jsr	disp
	ldaa	#ANGDG1
	ldab	#%00001110	; 'L'
	jsr	disp
	ldaa	#ANGDG0
	ldab	#%00000000	; ' '
	jsr	disp
	jsr	ldred

start30	jsr	dft2dsp		; wait for enter focal length
	jsr	dispfl
	jsr	ang2rmp
	jsr	drf2rmp
	tst	entflg
	beq	start30

	ldd	drift
	std	flen		; store focal length
	ldd	#0
	std	drift
	jsr	initled		; overwrite 'FL' back to numeric

start40	tst	entflg
	bne	start40

$IF DEBUG = 1
	ldd	flen		; print focal length for debug
	jsr	out_dec
	jsr	crlf
$ENDIF

main	tst	cor_mod		; 0= pre correction. 1= correction mode
	bne	main_a0
	jmp	main00		;  skip if not yet in correction mode

main_a0	jsr	ldgren		; in corr mode led = green
	tst	sec_flg		; count down x y correction 1sec timers
	bne	main_a2		; go there if sec flag == 1
	jmp	main00a		; nothing to do

main_a2	tst	lst_sec		; if secflg == 1 and lastsec == 0 -> we just have pos edge
	beq	main_a4		; we have found the edge -> go there
	jmp	main00a		; otherwise goto end of main loop

main_a4	inc	lst_sec		; we just have a positive edge on secflag -> do the stuff

$IF DEBUG = 1
	ldd	countx
	jsr	out_dec
	ldaa	#' '
	jsr	sendv24
	ldd	county
	jsr	out_dec
	jsr	crlf
$ENDIF

	tst	xvalid		; test if we have excluded X
	beq	main0b		; don't consider x

	ldx	xrmpcnt		; dec ramp x counter
	dex
	stx	xrmpcnt
	bne	main_a6		; if not zero -> no ramp step needed

	ldx	xrmplod		; reset ramp x counter
	stx	xrmpcnt
	dec	xrmpld		; dec x led ramp value

main_a6	ldx	countx
	dex
	stx	countx
	bne	main0b		; countx > 0

	ldx	cntxld		; reset x-counter + apply x correction
	stx	countx
	ldx	xrmplod
	stx	xrmpcnt
	ldaa	#5
	staa	xrmpld
	jsr	ang2rmp

$IF DEBUG =  1
	ldaa	#'X'
	jsr	sendv24
	jsr	crlf
$ENDIF

	ldaa	quadr

$IF ST4_90 = 0
	;; This table for straight through guide scope
	beq	main0c0		; quadr=0 -> increment xref
	cmpa	#1
	beq	main0d0		; quadr=1 -> decrement xref
	cmpa	#2
	beq	main0d0		; quadr=2 -> decrement xref
	cmpa	#3
	beq	main0c0		; quadr=3 -> increment xref
	bra	main0b
$ELSE
	;; This table for guide scope with 90deg prism
	beq	main0d0		; quadr=0 -> decrement xref
	cmpa	#1
	beq	main0c0		; quadr=1 -> increment xref
	cmpa	#2
	beq	main0c0		; quadr=2 -> increment xref
	cmpa	#3
	beq	main0d0		; quadr=3 -> decrement xref
	bra	main0b
$ENDIF

main0c0	ldaa	st4refx		; get xref
	cmpa	#ST4XMAX
	beq	main0b		; see if we would go beyond max x -> yes, do nothing
	inca			; increment xref
	bra	main0d2

main0d0	ldaa	st4refx		; get xref
	beq	main0b		; see if we would go below 0 -> yes, do nothing
	deca			; decrement xref
main0d2	staa	st4refx
main0d3	jsr	wr_xref		; write to ST4
	jsr	rd_xref
	cmpa	st4refx
	beq	main0b
	ldaa	st4refx
	bra	main0d3

main0b	tst	yvalid
	beq	main0d		; don't consider y

	ldx	yrmpcnt		; dec ramp x counter
	dex
	stx	yrmpcnt
	bne	main0bx		; if not zero -> no ramp step needed

	ldx	yrmplod		; reset ramp x counter
	stx	yrmpcnt
	dec	yrmpld		; dec x led ramp value

main0bx	ldx	county
	dex
	stx	county
	bne	main0d		; county > 0 -> nothing to do

	ldx	cntyld		; reset y-counter + apply y correction
	stx	county
	ldx	yrmplod
	stx	yrmpcnt
	ldaa	#5
	staa	yrmpld
	jsr	drf2rmp

$IF DEBUG = 1
	ldaa	#'Y'
	jsr	sendv24
	jsr	crlf
$ENDIF

	ldaa	quadr
	beq	main0f0		; quadr=0 -> increment yref
	cmpa	#1
	beq	main0f0		; quadr=1 -> increment yref
	cmpa	#2
	beq	main0g0		; quadr=2 -> decrement yref
	cmpa	#3
	beq	main0g0		; quadr=3 -> decrement yref
	bra	main0d		; illegal quadrant value. should be 0...3

main0f0	ldaa	st4refy		; get yref
	cmpa	#ST4YMAX
	beq	main0d		; see if we would go beyond max y -> yes, do nothing
	inca			; increment yref
	bra	main0g2

main0g0	ldaa	st4refy		; get xref
	beq	main0d		; see if we would go below 0 -> yes, do nothing
	deca			; decrement yref
main0g2	staa	st4refy
main0g3	jsr	wr_yref		; write to ST4
	jsr	rd_yref
	cmpa	st4refy
	beq	main0d
	ldaa	st4refy
	bra	main0g3

main0d	bra	main01

main00	jsr	ldorng
main00a	tst	sec_flg		; see if sec flag still ==1
	bne	main01		; wait as long as secflag ==1
	clr	lst_sec		; reset lastflag so that we can find next edge
main01	jsr	ang2dsp		; do all the display stuff
	jsr	dft2dsp
	jsr	dispang
	jsr	dispdrf
	jsr	ang2rmp
	jsr	drf2rmp


main08	tst	entflg		; see if we have enter key pressed
	bne	main08a		; if enter key pressed
				; -> have to do the x y calc and reset counters
main081	jmp	main		; if no enter key go back to main loop start

main08a	ldx	drift
	beq	main081		; No drift => dont enter corrections mode + go to main
	ldaa	#1
	staa	cor_mod

	jsr	rd_xref		; retrieve current ref position on ST4
	staa	st4refx
	jsr	rd_yref
	staa	st4refy
				; calc x y down counter values

	ldd	angl		; get angle to calc modulus 90deg and quadrant
	ldx	#0		; x will become quadrant
main09	cmpd	#89		; see if we can subtract 90deg (once again)
	ble	main10		; no. =>  0<=D<=90
	subd	#90		; yes. subtract
	inx			; increment quadrant counter
	bra	main09		; loop around

main10	stab	angmod
	xgdx
	stab	quadr


xcalc	ldd	angl
	cmpd	#90		; if angle = 90 or 270 -> disable x-calc + x-corr
	beq	xcalc10
	cmpd	#270
	beq	xcalc10

	ldaa	#1
	staa	xvalid
	bra	xcalc20
xcalc10	clr	xvalid		;  disable x-calc + x-corr
	clr	xrmpld
	jsr	ang2rmp
	bra	ycalc		;  jmp to y-calc. no x-calc required.

xcalc20	ldd	flen		; do x calculation
	std	A24+1
	clr	A24		; A24= Flen / 10
	ldd	drift
	std	B24+1		; B24= Drift * 10
	clr	B24
	jsr	mul24		; A48= Flen * Drift

	ldd	A48+3
	std	D24
	ldaa	A48+5
	staa	D24+2		; D24= A48= Flen * Drift

	ldd	#XCSTHI
	std	D48
	ldd	#XCSTMD
	std	D48+2
	ldd	#XCSTLO
	std	D48+4		; D48= X_CONST

	jsr	DIV24		; Q24= X_CONST / ( Flen * Drift )
	ldaa	angmod
	jsr	cos_x
	staa	D24+2
	clr	D24+1
	clr	D24		; D24= cos(angle) * 255

	clr	D48
	clr	D48+1
	clr	D48+2
	ldd	Q24
	std	D48+3
	ldaa	Q24+2
	staa	D48+5		; D48= X_CONST / ( Flen * Drift )

	jsr	div24		; Q24= X_CONST / ( Flen * Drift * cos(angle) )

	ldd	Q24+1
	std	cntxld
	std	countx		; x counter value done
	ldx	#05
	idiv
	stx	xrmplod
	stx	xrmpcnt
	ldaa	#5
	staa	xrmpld

ycalc	ldd	angl
	beq	ycalc10		; if angle=0 or 180 disable y-calc + y-corr
	cmpd	#180
	beq	ycalc10
	ldaa	#1
	staa	yvalid
	bra	ycalc20
ycalc10	clr	yvalid
	clr	yrmpld
	jsr	drf2rmp
	bra	main25		;  skip y-calc

ycalc20	ldd	flen		; do y calculation
	std	A24+1
	clr	A24		; A24= Flen / 10
	ldd	drift
	std	B24+1		; B24= Drift * 10
	clr	B24
	jsr	mul24		; A48= Flen * Drift

	ldd	A48+3
	std	D24
	ldaa	A48+5
	staa	D24+2		; D24= A48= Flen * Drift

	ldd	#YCSTHI
	std	D48
	ldd	#YCSTMD
	std	D48+2
	ldd	#YCSTLO
	std	D48+4		; D48= X_CONST

	jsr	DIV24		; Q24= X_CONST / ( Flen * Drift )
	ldaa	angmod
	jsr	sin_x
	staa	D24+2
	clr	D24+1
	clr	D24		; D24= sin(angle) * 255

	clr	D48
	clr	D48+1
	clr	D48+2
	ldd	Q24
	std	D48+3
	ldaa	Q24+2
	staa	D48+5		; D48= X_CONST / ( Flen * Drift )

	jsr	div24		; Q24= X_CONST / ( Flen * Drift * sin(angle) )

	ldd	Q24+1
	std	cntyld
	std	county
	ldx	#05
	idiv
	stx	yrmplod
	stx	yrmpcnt
	ldaa	#5
	staa	yrmpld

main25:
$IF DEBUG = 1
	ldd	angl
	jsr	out_dec
	ldaa	#' '
	jsr	sendv24
	ldd	drift
	jsr	out_dec
	ldaa	#' '
	jsr	sendv24
	ldab	angmod
	clra
	jsr	out_dec
	ldaa	#' '
	jsr	sendv24
	ldaa	quadr
	adda	#'0'
	jsr	sendv24
	ldaa	#' '
	jsr	sendv24
	ldd	cntxld
	jsr	out_dec
	ldaa	#' '
	jsr	sendv24
	ldd	cntyld
	jsr	out_dec
	jsr	crlf
$ENDIF
main26	tst	entflg
	bne	main26
	jmp	main


initled	ldaa	#$0f		; hi	test register
	ldab	#%00000000	; lo	0 = normal ops
	jsr	disp

	ldaa	#$0c		; hi	shut down reg
	ldab	#%00000001	; lo	1 = normal ops
	jsr	disp

	ldaa	#$0b		; hi	scan limit reg
	ldab	#%00000111	; lo	%0111 display all digits
	jsr	disp

	ldaa	#$0a		; hi	intensity reg
	ldab	#%00001000	; lo	1/2 intensity
	jsr	disp

	ldaa	#$09		; hi	decode mode
	ldab	#%01110111	; lo	left BCD right BCD
	jsr	disp
	rts


ang2rmp	ldab	xrmpld
	clra
	addd	#ledtab
	xgdx
	ldab	0,x
	ldaa	#ANGRMP
	jsr	disp
	rts

drf2rmp	ldab	yrmpld
	clra
	addd	#ledtab
	xgdx
	ldab	0,x
	orab	rgled
	ldaa	#DRFRMP
	jsr	disp
	rts

dgreen	ldaa	rgled
	anda	#%11111100
	ora	#%00000001
	bra	dled
dred	ldaa	rgled
	anda	#%11111100
	ora	#%00000010
	bra	dled
dorng	ldaa	rgled
	anda	#%11111100
	ora	#%00000011
	bra	dled
dblak	ldaa	rgled
	anda	#%11111100
	bra	dled

dled	staa	rgled
	rts

ldgren	bsr	dgreen
	bsr	drf2rmp
	rts

ldred	bsr	dred
	bsr	drf2rmp
	rts

ldorng	bsr	dorng
	bsr	drf2rmp
	rts

ldblak	bsr	dblak
	bsr	drf2rmp
	rts

ang2dsp	ldd	angl
	ldx	#10
	idiv
	stab	angdsp0
	xgdx
	ldx	#10
	idiv
	stab	angdsp1
	xgdx
	stab	angdsp2
	rts

dft2dsp	ldd	drift
	ldx	#10
	idiv
	stab	drfdsp0
	xgdx
	ldx	#10
	idiv
	stab	drfdsp1
	xgdx
	stab	drfdsp2
	rts

dispang	ldaa	#ANGDG0
	ldab	angdsp0
	jsr	disp
	ldaa	#ANGDG1
	ldab	angdsp1
	jsr	disp
	ldaa	#ANGDG2
	ldab	angdsp2
	jsr	disp
	rts

dispdrf	ldaa	#DRFDG0
	ldab	drfdsp0
	jsr	disp
	ldaa	#DRFDG1
	ldab	drfdsp1
	orb	#%10000000
	jsr	disp
	ldaa	#DRFDG2
	ldab	drfdsp2
	jsr	disp
	rts

dispfl	ldaa	#DRFDG0
	ldab	drfdsp0
	jsr	disp
	ldaa	#DRFDG1
	ldab	drfdsp1
	jsr	disp
	ldaa	#DRFDG2
	ldab	drfdsp2
	jsr	disp
	rts

dispdec	clra			; accu b into angle disp as dec
	ldx	#10
	idiv
	pshb
	xgdx
	ldx	#10
	idiv
	pshb
	xgdx
	ldaa	#ANGDG2
	jsr	disp
	pulb
	ldaa	#ANGDG1
	jsr	disp
	pulb
	ldaa	#ANGDG0
	jsr	disp
	rts


disp	psha
	pshb
	pshx
	pshy
	ldx	#REGBS
	bclr	PORTA,x,CLK
	bclr	PORTA,x,LOAD
	bclr	PORTA,x,DATA
	ldy	#16
disp05	lsld
	bcc	disp10
	bset	PORTA,x,DATA
	bra	disp15
disp10	bclr	PORTA,x,DATA
disp15	bset	PORTA,x,CLK
	bclr	PORTA,x,CLK
	dey
	bne	disp05
	bset	PORTA,x,LOAD
	puly
	pulx
	pulb
	pula
	rts

fatal	std	angl
	ldd	#00
	std	drift
	jsr	ang2dsp
	jsr	dft2dsp
	jsr	dispang
	jsr	dispdrf
	jsr	ldred
fatal1	bra	fatal1

$IF DEBUG = 1
rd_stat	ldaa	#00
	rts

wr_stat rts

rd_xref	ldaa	st4refx
	rts

rd_yref	ldaa	st4refy
	rts

wr_xref	psha
	ldaa	#'X'
	jsr	sendv24
	pula
	tab
	clra
	jsr	out_dec
	jsr	crlf
	rts

wr_yref	psha
	ldaa	#'Y'
	jsr	sendv24
	pula
	tab
	clra
	jsr	out_dec
	jsr	crlf
	rts


$ELSE
rd_stat ldaa	#10
	staa	errcnt		; init error counter
rd_st02	ldaa	#$02		; read RAM cmd. and reetry point for error recovery
	jsr	sendv24
	ldaa	#$01		; number of bytes
	jsr	sendv24
	ldaa	#$01		; 0=ext, 1=int ram
	jsr	sendv24
	ldaa	#[ST4ST		; LSB of Status address
	jsr	sendv24
	ldaa	#]ST4ST		; MSB of status address
	jsr	sendv24
	ldaa	#$33		; checksum=$02+$01+$01+$2f+$00
	jsr	sendv24


	;; response :	 02 01 ST CS


	ldx	#st4buf		; pointer to read buffer
rd_st05	ldaa	#$ff		; init value for timeout
	staa	timout
	ldy	recvrd
rd_st10	cpy	recvwr		; check if we have received something
	bne	rd_st15		; we have received something
	tst	timout		; wait a while to receive data
	bne	rd_st10		; lop around to try wait more
	dec	errcnt		; time out! count down error
	bne	rd_st02		; loop back to send command
	ldd	#10		; had 10 failures -> fatal error
	jmp	fatal		; go fatal

rd_st15	ldaa	0,y
	staa	0,x		; copy v24 receive data to st4 receive buffer
	inx
	cpy	#recvbuf+15
	bne	rd_st20
	ldy	#recvbuf-1
rd_st20	iny
	sty	recvrd
	cpx	#st4buf+4
	bne	rd_st05		; go back for next char. incl re-init of timeout

	ldaa	st4buf		; all received. now check
	cmpa	#$02		; $02
	bne	rd_st95
	ldaa	st4buf+1
	cmpa	#$01		; $01
	bne	rd_st95

	clra
	ldx	#st4buf
rd_st30	adda	0,x		; add up checksum
	inx
	cpx	#st4buf+3
	bne	rd_st30
	cmpa	0,x		; compare with received checksum value
	bne	rd_st95		; goto error handler if wrong checksum
	ldaa	st4buf+2	; retrieve status byte and return it
	rts

rd_st95	dec	errcnt		; count down for error
	beq	rd_st97
	jmp	rd_stat		; if received data are bad goto re issue command

rd_st97	ldd	#01		; if 10 errors encountered -> goto fatal halt
	jmp	fatal

wr_stat	staa	tmp
	ldaa	#10
	staa	errcnt
wr_st02	ldaa	#$01		; write RAM cmd
	jsr	sendv24
	ldaa	#$04		; number of bytes
	jsr	sendv24
	ldaa	#$01		; 0=ext, 1=int ram
	jsr	sendv24
	ldaa	#[ST4ST		; LSB of Status address
	jsr	sendv24
	ldaa	#]ST4ST		; MSB of status address
	jsr	sendv24
	ldaa	tmp		; status byte
	jsr	sendv24
	ldaa	tmp
	adda	#$35		;checksum $35=$01+$04+$01+$2f+$00
	jsr	sendv24

	;; response :	 06

wr_st05	ldy	recvrd
	ldaa	#$ff
	staa	timout
wr_st10	cpy	recvwr
	bne	wr_st15		; wait till we have received something
	tst	timout
	bne	wr_st10
	dec	errcnt
	bne	wr_st02
	ldd	#11
	jmp	fatal

wr_st15	ldaa	0,y
	cpy	#recvbuf+15
	bne	wr_st20
	ldy	#recvbuf-1
wr_st20	iny
	sty	recvrd
	cmpa	#$06
	beq	wr_st25
	dec	errcnt
	bne	wr_st02
	ldd	#$03
	jmp	fatal
wr_st25	rts

	;; x ref addr $7e7c

rd_xref	ldaa	#10
	staa	errcnt
rd_xr02	ldaa	#$02		; read RAM cmd
	jsr	sendv24
	ldaa	#$01		; number of bytes
	jsr	sendv24
	ldaa	#$00		; 0=ext, 1=int ram
	jsr	sendv24
	ldaa	#[ST4XRF	; LSB of x ref coordinate address
	jsr	sendv24
	ldaa	#]ST4XRF	; MSB of x ref coordinate address
	jsr	sendv24
	ldaa	#$FD		; checksum $FD=$02+$01+$00+$7c+$7e
	jsr	sendv24

	;; response $02 $01 xref CS

	ldx	#st4buf
rd_xr05	ldaa	#$ff
	staa	timout
	ldy	recvrd
rd_xr10	cpy	recvwr
	bne	rd_xr15		; wait till we have received something
	tst	timout
	bne	rd_xr10
	dec	errcnt
	bne	rd_xr02
	ldd	#12
	jmp	fatal

rd_xr15	ldaa	0,y
	staa	0,x		; copy v24 receive data to st4 receive buffer
	inx
	cpy	#recvbuf+15
	bne	rd_xr20
	ldy	#recvbuf-1
rd_xr20	iny
	sty	recvrd
	cpx	#st4buf+4
	bne	rd_xr05

	ldaa	st4buf		; all received. now check
	cmpa	#$02		; $02
	bne	rd_xr95
	ldaa	st4buf+1
	cmpa	#$01		; $01
	bne	rd_xr95

	clra
	ldx	#st4buf
rd_xr30	adda	0,x		; add up checksum
	inx
	cpx	#st4buf+3
	bne	rd_xr30
	cmpa	0,x		; compare with received checksum value
	bne	rd_xr95		; goto error handler if wrong checksum
	ldaa	st4buf+2	; retrieve x ref byte and return it
	rts

rd_xr95	dec	errcnt		; count down for error
	beq	rd_xr97
	jmp	rd_xref

rd_xr97	ldd	#04		; if 10 errors encountered -> goto fatal halt
	jmp	fatal

	;; y ref addr $7e7d

rd_yref	ldaa	#10
	staa	errcnt
rd_yr02	ldaa	#$02		; read RAM cmd
	jsr	sendv24
	ldaa	#$01		; number of bytes
	jsr	sendv24
	ldaa	#$00		; 0=ext, 1=int ram
	jsr	sendv24
	ldaa	#[ST4YRF	; LSB of y ref coordinate address
	jsr	sendv24
	ldaa	#]ST4YRF	; MSB of y ref coordinate address
	jsr	sendv24
	ldaa	#$FE		; checksum $FE=$02+$01+$00+$7d+$7e
	jsr	sendv24

	;; response $02 $01 yref CS

	ldx	#st4buf
rd_yr05	ldaa	#$ff
	staa	timout
	ldy	recvrd
rd_yr10	cpy	recvwr
	bne	rd_yr15		; wait till we have received something
	tst	timout
	bne	rd_yr10
	dec	errcnt
	bne	rd_yr02
	ldd	#13
	jmp	fatal

rd_yr15	ldaa	0,y
	staa	0,x		; copy v24 receive data to st4 receive buffer
	inx
	cpy	#recvbuf+15
	bne	rd_yr20
	ldy	#recvbuf-1
rd_yr20	iny
	sty	recvrd
	cpx	#st4buf+4
	bne	rd_yr05

	ldaa	st4buf		; all received. now check
	cmpa	#$02		; $02
	bne	rd_yr95
	ldaa	st4buf+1
	cmpa	#$01		; $01
	bne	rd_yr95

	clra
	ldx	#st4buf
rd_yr30	adda	0,x		; add up checksum
	inx
	cpx	#st4buf+3
	bne	rd_yr30
	cmpa	0,x		; compare with received checksum value
	bne	rd_yr95		; goto error handler if wrong checksum
	ldaa	st4buf+2	; retrieve x ref byte and return it
	rts

rd_yr95	dec	errcnt		; count down for error
	beq	rd_yr97
	jmp	rd_yr02

rd_yr97	ldd	#05		; if 10 errors encountered -> goto fatal halt
	jmp	fatal


wr_xref	staa	tmp
	ldaa	#10
	staa	errcnt
wr_xr02	ldaa	#$01		; write RAM cmd
	jsr	sendv24
	ldaa	#$04		; number of bytes
	jsr	sendv24
	ldaa	#$00		; 0=ext, 1=int ram
	jsr	sendv24
	ldaa	#[ST4XRF	; LSB of x ref address $7e7c
	jsr	sendv24
	ldaa	#]ST4XRF	; MSB of x ref address
	jsr	sendv24
	ldaa	tmp		; x ref byte
	jsr	sendv24
	ldaa	tmp
	adda	#$ff		;checksum $ff=$01+$04+$00+$7c+$7e
	jsr	sendv24

	;; response :	 06

wr_xr05	ldy	recvrd
	ldaa	#$ff
	staa	timout
wr_xr10	cpy	recvwr
	bne	wr_xr15		; wait till we have received something
	tst	timout
	bne	wr_xr10
	dec	errcnt
	bne	wr_xr02
	ldd	#14
	jmp	fatal

wr_xr15	ldaa	0,y
	cpy	#recvbuf+15
	bne	wr_xr20
	ldy	#recvbuf-1
wr_xr20	iny
	sty	recvrd
	cmpa	#$06
	beq	wr_xr25
	dec	errcnt
	bne	wr_xr02
	ldd	#$06
	jmp	fatal
wr_xr25	rts

wr_yref	staa	tmp
	ldaa	#10
	staa	errcnt
wr_yr02	ldaa	#$01		; write RAM cmd
	jsr	sendv24
	ldaa	#$04		; number of bytes
	jsr	sendv24
	ldaa	#$00		; 0=ext, 1=int ram
	jsr	sendv24
	ldaa	#[ST4YRF	; LSB of y ref address $7e7d
	jsr	sendv24
	ldaa	#]ST4YRF	; MSB of y ref address
	jsr	sendv24
	ldaa	tmp		; x ref byte
	jsr	sendv24
	ldaa	tmp
	adda	#$00		;checksum $00=$01+$04+$00+$7d+$7e
	jsr	sendv24

	;; response :	 06

wr_yr05	ldy	recvrd
	ldaa	#$ff
	staa	timout
wr_yr10	cpy	recvwr
	bne	wr_yr15		; wait till we have received something
	tst	timout
	bne	wr_yr10
	dec	errcnt
	bne	wr_yr02
	ldd	#15
	jmp	fatal

wr_yr15	ldaa	0,y
	cpy	#recvbuf+15
	bne	wr_yr20
	ldy	#recvbuf-1
wr_yr20	iny
	sty	recvrd
	cmpa	#$06
	beq	wr_yr25
	dec	errcnt
	bne	wr_yr02
	ldd	#$07
	jmp	fatal
wr_yr25	rts
$ENDIF

out_dec	ldy	#5
out_d10	ldx	#10
	idiv
	pshb
	xgdx
	dey
	bne	out_d10

	ldy	#5
out_d20	pula
	adda	#'0'
	bsr	sendv24
	dey
	bne	out_d20
	rts

outbyte	psha			* output accu a 1 byte as 2 hex digits
	anda	#$F0
	lsra
	lsra
	lsra
	lsra
	jsr	outbyt1
	pula
	anda	#$0F

outbyt1	adda	#'0'		* output 4 bits as 1 hex digit
	cmpa	#'9'
	ble	sendv24
	adda	#7

sendv24 pshy
	ldy	sendwr
	staa	0,y
	cpy	#sendbuf+15
	bne	sendv50
	ldy	#sendbuf-1
sendv50	iny
sendv60	cpy	sendrd
	beq	sendv60
	sty	sendwr
	puly
	rts

crlf	psha
	ldaa	#13
	bsr	sendv24
	ldaa	#10
	bsr	sendv24
	pula
	rts

*   Calculate the 48 bit unsigned product of two 24 bit unsigned numbers.
*   Place the multiplicand in A24, the multiplier in B24, then JSR MUL24.
*   Result is returned in A48.  Put smaller value in B24 when given a
*   choice.
*             Note: A48 must immediately follow B24 in memory !!!!
*   B24 may overlap A48 to save space if desired - some changes req'd.
*   The approach used is similar to multiplication with pencil and paper,
*   where bytes are used as if they were digits. It may be extended to
*   any reasonable number of bytes quite easily.


MUL24:   PSHB           ; Preserve regs
         PSHA
         PSHX
         LDX    #B24    ; Address of args
         CLR    3,X     ; Clear A48
         CLR    4,X
         CLR    5,X
         CLR    6,X
         CLR    7,X
         CLR    8,X
         LDAA   #3      ; # bytes in multiplier
         STAA   CNTR

MULOOP:  LDAA   2,X
         BEQ    MULUP2  ; Skip if 0 multiplier
         LDAB   A24+2   ; Least signifigant byte of multiplicand
         MUL
         ADDD   7,X
         STD    7,X
         BCC    MUL1
         INC    6,X     ; Propagate the carry
         BNE    MUL1
         INC    5,X
MUL1:    LDAA   2,X
         LDAB   A24+1
         MUL
         ADDD   6,X
         STD    6,X
         BCC    MUL2
         INC    5,X     ; Propagate Carry
MUL2:    LDAA   2,X
         LDAB   A24     ; Most signifigant byte
         MUL
         ADDD   5,X
         STD    5,X     ; No carry possible on hi end
MULUP2:  DEX            ; Move left 1 byte in multiplier & result
         DEC    CNTR
         BNE    MULOOP
         PULX           ; Restore regs
         PULA
         PULB
         RTS


* Calculate the 24 bit unsigned quotient resulting from division of a
* 48 bit dividend by a 24 bit divisor. Place the divisor in D24 and the
* dividend in D48. The result will be returned in Q24.

*   The approach is similar to long division by hand, but here the
* IDIV instruction is used to form the trial quotient. Care is taken
* to ensure that the trial quotient is always <= the required quotient.

* The division routine can be modified to handle any reasonable number
* of bytes, but it is more challenging than extending the multiplication
* routine.

*   Typical operation is two passes through the loop per byte in the
* dividend.


DIV24:   PSHB
         PSHA
         PSHX
	 PSHY
         CLR    Q24     ; Clear quotient to 0
         CLR    Q24+1
         CLR    Q24+2
         LDAB   #6      ; Size of dividend, max
         LDY    #D48    ; Count bytes in the dividend
DIV0:    TST    0,Y
         BNE    DIV1
         DECB
         BEQ    DIVER1  ; Done if dividend = 0, Return 0
         INY
         BRA    DIV0

DIVER1:  JMP    DIVBY0  ; Extend branch range

*  Find size of divisor and dividend & thus the hi byte of quotient.
*  Also do gross checks to avoid dividing by 0, etc.
DIV1:    STAB   DVDCNT  ; Save # bytes in dividend
         LDAA   #$FF
         NEGB
         ADDD   #D48+5  ; + Addr of LS Byte-1 in dividend
         XGDY           ; Y points to MSB of dividend
         LDA    #3      ; Count bytes in the divisor
         LDX    #D24
DIV1CT:  TST    0,X
         BNE    DIV2
         DECA
         BEQ    DIVER1  ; Quit if divide by 0, Return 0
         INX
         BRA    DIV1CT

DIV2:    STX    DIVPTR  ; Ptr to MSB of divisor
         STA    DIVCNT  ; Number of bytes in divisor
         SUBA   DVDCNT
         BGT    DIVER1  ; Quit if divisor > dividend, Return 0
         LDAB   1,Y     ; Get hi byte of dividend
         CMPB   0,X     ; IF hi byte of dividend > hi byte of divisor
         BLO    DIV3    ;  THEN one more byte needed in the quotient
         DECA
         DEY            ;  and in Dividend
DIV3:    TAB
         NEGA           ; Make it positive
         STAA   QCNT    ; # Bytes in quotient
         CMPA   #4      ; ??? MAKE THIS PREVENT OVERFLOW ???
         BGT    DIVER1  ; Quit if quotient too big, return 0
         LDAA   #$FF    ; Make it a negative 16 bit value
         ADDD   #Q24+2  ; Ptr to hi byte of quotient
         STD    QPTR

*  Decide whether to use 1 or 2 bytes as trial divisor
DIVLUP:  LDX    DIVPTR  ; Get ptr to MSB of divisor
         LDAA   DIVCNT  ; Get # bytes in divisor
         DECA
         BEQ    DIV1BY  ; Only 1 byte in divisor
         STA    CNTR    ; Temp, # bytes in divisor -1
         LDD    1,Y     ; Get hi word of dividend in B
         CPD    0,X     ; IF hi word of dividend < hi word of divisor
         BLO    DIVBYB  ;  THEN use only MSB of divisor
         BHI    DIVW2B  ; Divisor < Dividend  & 2+ bytes in divisor
         LDA    CNTR
         DECA
         BEQ    DIVWRD  ; BR if divisor = MSB's of dividend
         LDAB   3,Y     ; Get next byte of dividend in B
         CMPB   2,X     ; IF next byte of dividend < this byte of divisor
         BLO    DIVBYB  ;  THEN use only MSB of divisor, rounded up
         LDAB   #1      ;  ELSE Trial Quotient = 1
         BRA    DIVSTO

DIVBYB:  JMP    DIVBYT  ; Extend branch range
DIV1BY:  JMP    DIV1BYT

*  Trial divisor is 2 bytes. Round up if required.
DIVW2B:  LDA    CNTR
         DECA           ; Make A = 0 if exactly 2 bytes in divisor
DIVWRD:  LDX    0,X     ; Get MSW of divisor
         TSTA
         BEQ    DIVWNR  ; BR if exactly 2 bytes in divisor
         INX            ; Round divisor up
DIVWNR:  LDD    1,Y     ; Get MSW of dividend
         IDIV
         XGDX           ; Get trial quotient into B
*  Multiply divisor by trial quotient and subtract from dividend
DIVSTO:  STAB   TRIALQ  ; Save trial quotient
         LDX    QPTR
         ADDB   0,X     ; Add Trial Q to current Q
         STAB   0,X
         LDAB   DIVCNT
         STAB   CNTR
         LDX    DIVPTR
         LDAA   0,X     ; Get hi byte of divisor
         LDAB   TRIALQ  ; Get trial quotient byte
         MUL
         STD    TEMP    ; Seems crude, but it works...
         LDD    0,Y
         SUBD   TEMP    ; No borrow possible from hi byte
         STD    0,Y
         DEC    CNTR
         BEQ    DIVLND ; BR if divisor is 1 byte
         LDX    DIVPTR
         LDAA   1,X    ; Get next byte of divisor
         LDAB   TRIALQ
         MUL
         STD    TEMP
         LDD    1,Y
         SUBD   TEMP
         STD    1,Y
         BCC    *+5
         DEC    0,Y     ; Propagate borrow
         DEC    CNTR
         BEQ    DIVLND  ; BR if divisor is 2 bytes
         LDX    DIVPTR
         LDAA   2,X
         LDAB   TRIALQ
         MUL
         STD    TEMP
         LDD    2,Y
         SUBD   TEMP
         STD    2,Y
         BCC    DIVLND
         LDX    0,Y     ; Propagate borrow
         DEX
         STX    0,Y
DIVLND:  LDD    0,Y
         BEQ    DIVDN0
         TSTA
         BEQ    DIVLN2  ; Almost always branches, 4000:1 or so
         LDX    DIVPTR  ; Ptr to MSB of divisor
         BRA    DIVBYR  ; Last divide by byte was too coarse, correct it

DIVDN0:  INY            ; Hi word of dividend=0, move 1 position right
         DEC    QCNT
         BLT    DIVDUN
         INC    QPTR+1
DIVLN2:  JMP    DIVLUP  ; Loop to do next byte

*  Trial divisor is hi byte of divisor.  Always round it up.
DIVBYT:  DEC    QCNT    ; Divisor > dividend, move 1 position right
         BLT    DIVDUN  ; BR if done
         INC    QPTR+1
         INY
DIVBYR:  LDAB   0,X     ; Get MSB of divisor
         CLRA
         XGDX
         INX            ; Round divisor up
         LDD    0,Y     ; Dividend, current hi 16 bits
         BEQ    DIVDN0  ; Shortcut on 0 dividend     ?? Does this help??
         IDIV           ; MSW of dividend / MSB of divisor = Trial Q
         XGDX           ; Get quotient in D
         TSTA
         BEQ    DIVBT2
         LDAB   #$F0    ; Overflow on this trial, force max
DIVBT2:  JMP    DIVSTO

* Handle single byte divisor specially
DIV1BYT: LDAB   0,X     ; Get divisor, single byte
         CLRA
         XGDX
DIV1SKP: CPX    0,Y
         BLS    DIV1BGO
         DEC    QCNT   ; Divisor > Dividend,  move 1 position right
         BLT    DIVDUN  ; BR if done
         INC    QPTR+1
         INY
         BRA    DIV1SKP
DIV1BGO: LDD    0,Y     ; Dividend, current hi 16 bits
         IDIV           ; MSW of dividend / divisor = Trial Q
         XGDX           ; Get quotient in D
         JMP    DIVSTO

DIVDUN:
DIVBY0:  PULY
	 PULX           ; Restore regs
         PULA
         PULB
         RTS


cos_x	ldab	quadr
	beq	cos_x10
	cmpb	#1
	beq	cos_x20
	cmpb	#2
	beq	cos_x10
	cmpb	#3
	beq	cos_x20

cos_x10	staa	tmp
	ldaa	#90
	suba	tmp

cos_x20	tab
	clra
	addd	#sintab
	xgdx
	ldaa	0,x
	rts


sin_x	ldab	quadr
	beq	sin_x10
	cmpb	#1
	beq	sin_x20
	cmpb	#2
	beq	sin_x10
	cmpb	#3
	beq	sin_x20

sin_x20	staa	tmp
	ldaa	#90
	suba	tmp

sin_x10	tab
	clra
	addd	#sintab
	xgdx
	ldaa	0,x
	rts


isr_RTI	ldx	#REGBS
	ldaa	#%01000000
	staa	TFLG2,X

	tst	timout
	beq	key005
	dec	timout

key005	inc	sec_cnt
	ldaa	sec_cnt
	cmpa	#62
	bne	key00
	clr	sec_cnt
	ldaa	sec_flg
	eora	#%00000001
	staa	sec_flg

key00	inc	key_cnt
	ldaa	key_cnt
	cmpa	#12
	beq	key02
	rti			; skip all irq until key_cnt = 12

key02	clr	key_cnt
	ldaa	PORTD,x
	staa	pdmir
	anda	#ALLKEY
	cmpa	#ALLKEY
	beq	key10
	anda	#ANGKEY
	cmpa	#ANGKEY
	beq	key20		; no ANG key pressed
	cmpa	#ANGPL
	bne	angminu

angplus	ldd	angl
	addd	#1
	cmpd	#360
	bne	angpl10
	ldd	#0
	std	angl
angpl10	std	angl
	bra	key20

angminu	ldd	angl
	subd	#1
	bpl	angmi10
	ldd	#359
angmi10	std	angl

key20	ldaa	pdmir
	anda	#DRFKEY
	cmpa	#DRFKEY
	beq	key10
	cmpa	#DRFPL
	bne	drfminu

drfplus	ldd	drift
	cmpd	#999
	beq	key10
	addd	#1
	std	drift
	bra	key10

drfminu	ldd	drift
	beq	key10
	subd	#1
	std	drift
	bra	key10

key10	ldaa	PORTA,x
	anda	#ENTKEY
	bne	key30
	ldaa	#$01
	staa	entflg
	bra	key99

key30	clr	entflg

key99	bra	sio_snd		; see if we have to initiate sending


isr_SCI	ldx	#REGBS
	ldaa	SCSR,x
	staa	scsr_mr
	anda	#%00100000
	bne	sio_rcv
	ldaa	scsr_mr
	anda	#%10000000
	bne	sio_snd
	rti			; if not rcv or snd. No other irq served.

sio_rcv	ldaa	SCDR,x		; dont care for overrun error yet
	ldy	recvwr
	staa	0,y
	cpy	#recvbuf+15
	bne	sio_r10
	ldy	#recvbuf-1
sio_r10	iny
	sty	recvwr
	ldaa	scsr_mr
	anda	#%10000000
	bne	sio_snd
	rti

sio_snd	ldy	sendrd
	cpy	sendwr
	beq	sio_s08				; nothing to send
sio_s03	brclr	SCSR,x,%10000000,sio_s03	; wait till send reg empty

	pshx
	ldx	#8
	clrb
	ldaa	0,y
	psha
sio_s0a	lsra
	bcc	sio_s04
	incb
sio_s04	dex
	bne	sio_s0a
	pula
	pulx
	andb	#$01
	bne	sio_s0c
	bclr	SCCR1,x,%01000000
	bra	sio_s0d
sio_s0c	bset	SCCR1,x,%01000000

sio_s0d	staa	SCDR,x
	cpy	#sendbuf+15
	bne	sio_s05
	ldy	#sendbuf-1
sio_s05	iny
	sty	sendrd
	cpy	sendwr
	beq	sio_s08			; nothing more to send
	bset	SCCR2,x,%10000000	; if more bytes to send set irq enable bit
	bra	sio_s10
sio_s08	bclr	SCCR2,x,%10000000	; if no more bytes to send clr irq enable
sio_s10	rti

ledtab	fcb	%00000000
	fcb	%00000100
	fcb	%00100100
	fcb	%00110100
	fcb	%00111100
	fcb	%01111100

ledtab1	fcb	%00000000
	fcb	%00000100
	fcb	%00100000
	fcb	%00010000
	fcb	%00001000
	fcb	%01000000

sintab  fcb     000		; 0
        fcb     004
        fcb     009
        fcb     013
        fcb     018
        fcb     022
        fcb     027
        fcb     031
        fcb     035
        fcb     040
        fcb     044		; 10
        fcb     049
        fcb     053
        fcb     057
        fcb     062
        fcb     066
        fcb     070
        fcb     075
        fcb     079
        fcb     083
        fcb     087		; 20
        fcb     091
        fcb     096
        fcb     100
        fcb     104
        fcb     108
        fcb     112
        fcb     116
        fcb     120
        fcb     124
        fcb     127		; 30
        fcb     131
        fcb     135
        fcb     139
        fcb     143
        fcb     146
        fcb     150
        fcb     153
        fcb     157
        fcb     160
        fcb     164		; 40
        fcb     167
        fcb     171
        fcb     174
        fcb     177
        fcb     180
        fcb     183
        fcb     186
        fcb     190
        fcb     192
        fcb     195		; 50
        fcb     198
        fcb     201
        fcb     204
        fcb     206
        fcb     209
        fcb     211
        fcb     214
        fcb     216
        fcb     219
        fcb     221		; 60
        fcb     223
        fcb     225
        fcb     227
        fcb     229
        fcb     231
        fcb     233
        fcb     235
        fcb     236
        fcb     238
        fcb     240		; 70
        fcb     241
        fcb     243
        fcb     244
        fcb     245
        fcb     246
        fcb     247
        fcb     248
        fcb     249
        fcb     250
        fcb     251		; 80
        fcb     252
        fcb     253
        fcb     253
        fcb     254
        fcb     254
        fcb     254
        fcb     255
        fcb     255
        fcb     255
	fcb	255		; 90

$IF DEBUG = 0
	ORG	$FF00			; start of EEPROM

* INITIAL THE CPU

RESET	LDS	#$01FF			; put stack in CPU RAM
	LDX	#$1000			; register base address
	LDAA	#%10010001		; adpu, irqe, dly, cop = 65mS
	STAA	OPTION,X
	ldaa	#%00000000
	staa	TMSK2,X
	LDAA	#%00000000
	STAA	BPROT,X			; make CONFIG & EEPROM writable
	LDS	#$03ff
	LDAA	#%00000101
	STAA	CSCTL,X			; enable program CS for 32K
	LDAA	#%00000000
	STAA	CSGADR,X		; RAM starts at address 0000H
	LDAA	#%00000001
	STAA	CSGSIZ,X		; RAM block size is 32K
	LDAA	#%00011111
	STAA	DDRG,X			; bank select bits = outputs
	LDAA	#%00000000
	STAA	PORTG,X			; select 1ST bank
	jmp	start


******************************************************************************
*                           Interrupt Service Routinen                       *
******************************************************************************

*	Commented vectors are serviced in the code

* isr_SCI
isr_SPI
isr_PAIE
isr_PAO
isr_TIMO
isr_TIC4
isr_TOC4
isr_TOC3
isr_TOC2
isr_TOC1
isr_TIC3
isr_TIC2
isr_TIC1
* isr_RTI
isr_IRQ
isr_XIRQ
isr_SWI
isr_ILLOP
isr_COPFAIL
isr_CLMONFAIL

        rti                   * keine Bearbeitung fuer diese IRQ's

***********************************
* VECTOR TABLE
***********************************

        ORG     $FFD6             * Start der Vectortabelle

        fdb     isr_SCI
        fdb     isr_SPI
        fdb     isr_PAIE
        fdb     isr_PAO
        fdb     isr_TIMO
        fdb     isr_TIC4
        fdb     isr_TOC4
        fdb     isr_TOC3
        fdb     isr_TOC2
        fdb     isr_TOC1
        fdb     isr_TIC3
        fdb     isr_TIC2
        fdb     isr_TIC1
        fdb     isr_RTI
        fdb     isr_IRQ
        fdb     isr_XIRQ
        fdb     isr_SWI
        fdb     isr_ILLOP
        fdb     isr_COPFAIL
        fdb     isr_CLMONFAIL
        fdb     RESET
$ENDIF

*************
* END OF FILE

*	END
