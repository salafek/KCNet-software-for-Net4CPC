;*******************************************************
;
;	Config file "KCNet" Network-Interface 
; (hardwired TCP/IP-Stack with Ethernet W3150A+ / W5100)
;	LOW LEVEL Interface for Hardware-Driver 
;
;		###     KCNET     ###
;		 WIZnet TCP/IP-Stack 
;		###  susowa 2008  ###
;
;  Name		: KCNET.INC
;  Date		: 18.08.2008
;  Modified	: 10.11.2011
;		: 08.08.2020
;		: 21.11.2022 d_kef
;  Hardware	: Net4CPC - W5100S
;
;	- configure CP/M system
;	- define Terminal codes
;
;*******************************************************
	;
;----------- SYSTEM ------------------------------------
	;set this SYMBOL to NO, if you
	;assemble for another system
	;than KC85,
	;in this case the include file
	;KCN-ZPIO.INC is used with a low
	;level PIO-driver, where the PIO
	;can direct programmed by the
	;CP/M - CPU
	;
	;### Interface I/O ADDRESS ###
	;
	;fill in the first I/O-address
	;(PIO channel A Data) for the 
	;Z80-PIO used for the KCNET-
	;Interface in your system
	;
	;
	;Net4CPC W5100S MR address
W51ADDR	EQU	0FD20H	;I/O-address W5100S on CPC
	;
;-------------------------------------------------------
	;
;----------- CP/M TERMINAL-Codes -----------------------
; modify these codes, if required for your CP/M system
	;
	; Keyboard-Codes (input)
SCUL	EQU	1	; ^A Shift+Cursor left
ETX	EQU	3	; ^C Break
CUR	EQU	4	; ^D Cursor right
CUU	EQU	5	; ^E Cursor up
SCUR	EQU	6	; ^F Shift+Cursor right
DELE	EQU	7	; ^G Delete
CUL	EQU	8	; ^H Cursor left
TAB	EQU	9	; ^I Tabulator
ENT	EQU	0DH	; ^M Enter
CUD	EQU	18H	; ^X Cursor down
CLN	EQU	19H	; ^Y Clear line
ESC	EQU	1BH	;    Escape
BSP	EQU	7FH	;    Backspace
	;
	; Console-Codes (output)
BEEP	EQU	7	;Bell *
LEFT	EQU	8	;Cursor left *
HT	EQU	9	;hor. Tabulator *
LF	EQU	0AH	;Line Feed *
CLS	EQU	0CH	;Clear Screen & Cursor Home (only CP/M 2.2)
CR	EQU	0DH	;Carriage Return *
RIGHT	EQU	09H	;Cursor right (only CP/M 2.2)
CLLN	EQU	11H	;Clear Line from start (only CP/M 2.2)
CLEL	EQU	12H	;Clear Line to end (only CP/M 2.2)
LNUP	EQU	0BH	;Line Up (only CP/M 2.2)
;-------------------------------------------------------
	;
;----------- LOW LEVEL Interface -----------------------
; don't change anything !
	;
	;### Constants ###
N_XTIME	EQU	6554	;max. value (ms) of time-service
	;
	;###  known SERVER-IP numbers ###
N_DNSIP	EQU	0	;DNS-Server
N__1_IP	EQU	1	;reserved
N__2_IP	EQU	2	;reserved
N__3_IP	EQU	3	;reserved
N__4_IP	EQU	4	;reserved
N__5_IP	EQU	5	;reserved
N__6_IP	EQU	6	;reserved
N__7_IP	EQU	7	;reserved
	;
	;### required "KCNet" firmware version ###
N_MAJOR	EQU	1
N_MINOR	EQU	3	;---------------------------- WAS 2
	;
	;### driver function numbers ###
N_FWRBF	EQU	0	;write buffer bytes
N_FRDBF	EQU	1	;read buffer bytes
N_FPTR	EQU	2	;set buffer pointer
N_FTIME	EQU	3	;read timer value
N_FWRBT	EQU	4	;write byte
N_FRDBT	EQU	5	;read byte
N_FWIPA	EQU	6	;write SERVER-IP
N_FRIPA	EQU	7	;read SERVER-IP
N_FDPRT	EQU	8	;read dynamic Port number
N_FSVER	EQU	9	;read Software-Version
N_FHVER	EQU	10	;read Hardware-Version
N_FLSTA	EQU	11	;read link state
N_FINFO	EQU	12	;read interface info
N_FCERR	EQU	13	;read number of errors
	;
	;### driver function vectors ###
N_WRBF:	JP	$-$	;filled from 'N_INIT'
N_RDBF:	JP	$-$
N_PTR:	JP	$-$
N_TIME:	JP	$-$
N_WRBT:	JP	$-$
N_RDBT:	JP	$-$
N_WIPA:	JP	$-$
N_RIPA:	JP	$-$
N_DPRT:	JP	$-$
N_SVER:	JP	$-$
N_HVER:	JP	$-$
N_LSTA:	JP	$-$
N_INFO:	JP	$-$
N_CERR:	JP	$-$
	;
N_VTABL	EQU	$-N_WRBF ;table length
;------------------------------------------------------------
;VT52 Code conversion in case of CP/M Plus - uses BDOS CONOUT
BDCOUT:	PUSH	AF		;store character
	LD	A,(SYSVER)
	CP	31H		;is it CP/M 3.1
	JP	NZ,JPCOUT
	POP	AF		;retrieve character
	PUSH	AF
	PUSH	BC
	PUSH	DE
	PUSH	HL
	CP	CLS
	JR	NZ,BDCO1
	LD	C,2
	LD	E,ESC
	CALL	BDOS
	LD	C,2
	LD	E,'E'
	CALL	BDOS
	LD	C,2
	LD	E,ESC
	CALL	BDOS
	LD	E,'H'
	JR	BDCO5
BDCO1:	CP	CLLN
	JR	NZ,BDCO0
	LD	C,2
	LD	E,ESC
	CALL	BDOS
	LD	E,'o'
	JR	BDCO5
BDCO0:	CP	CLEL
	JR	NZ,BDCO2
	LD	C,2
	LD	E,ESC
	CALL	BDOS
	LD	E,'K'
	JR	BDCO5
BDCO2:	CP	LNUP
	JR	NZ,BDCO3
	LD	C,2
	LD	E,ESC
	CALL	BDOS
	LD	E,'A'
	JR	BDCO5
BDCO3:	CP	RIGHT
	JR	NZ,BDCO4
	LD	C,2
	LD	E,ESC
	CALL	BDOS
	LD	E,'C'
	JR	BDCO5
BDCO4:	LD	E,A
BDCO5:	LD	C,2
	CALL	BDOS
	POP	HL
	POP	DE
	POP	BC
	POP	AF
	RET
JPCOUT:	POP	AF		;retrieve character
	JP	COUT##		;send it to console
;-------------------------------------------------------