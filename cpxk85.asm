IF NOT lasm
.printx * CPXK85.ASM *
ENDIF	;NOT lasm
;	KERMIT - (Celtic for "FREE")
;
;	This is the CP/M-80 implementation of the Columbia University
;	KERMIT file transfer protocol.
;
;	Version 4.0
;
;	Copyright June 1981,1982,1983,1984,1985
;	Columbia University
;
; Originally written by Bill Catchings of the Columbia University Center for
; Computing Activities, 612 W. 115th St., New York, NY 10025.
;
; Contributions by Frank da Cruz, Daphne Tzoar, Bernie Eiben,
; Bruce Tanner, Nick Bush, Greg Small, Kimmo Laaksonen, Jeff Damens, and many
; others.
;
;	This file contains the system-dependent code and data for KERMIT.
;	It will be probably be broken into independent files to generate
;	overlays for the various systems, one or more overlay possible
;	from each file.	 For now, we will leave it in one piece.
;
; revision history:
;
; edit 3,  21 Nov 2022 by Dimitris Kefalas
;	modified the driver to support the Net4CPC interface
;
; edit 2,  31 Jul 2012 by Ralf Kaestner
;	modified network driver initialization: test KOP -> DRV
;
; edit 1,  22 Nov 2011
;	Original version by Ralf Kaestner
;       The 'KC85' is designed and manufactured in the former G.D.R. by
;       "MPM Muehlhausen", the microprocessor is a Z80 at 4MHz.
;
; 		*** MAIN CODE START ***
;
; Keep module name, edit number, and last revision date in memory.

family: db	'CPXK85.ASM (1)  12-Nov-2022 $'	; now a family...

; Assembly time message announcing which version we're building

.printx * Assembling KCNet for Net4CPC Kermit-80 *

z80	EQU	TRUE		;They all use Z80s

defesc  EQU     'S'-100O        ;The default escape character for KC85: CTRL+S or STOP-Key

vtval	EQU	0		; no VT52 emulation required

spdtbl:	DW	0		; address of baud rate command table, or zero
sphtbl:	DW	0		; address of baud rate help table, or zero
prttbl:	DW	0		; address of port command table, or zero
prhtbl:	DW	0		; address of port help table, or zero

sysver:	db	' Net4CPC - W5100S$ for Amstrad CPC with CP/M Plus'
outlin:	db	esc,'E',esc,'H$'	;Clear screen, home cursor
eralin:	db	cr,esc,'K$'		;Clear line.
curldn:	db	esc,'Y$'		;cursor leadin
ttab:					;Table start location.
ta:	db	esc,'A$',0		;Cursor up.
tb:	db	esc,'B$',0		;Cursor down.
tc:	db	esc,'C$',0		;Cursor right.
td:	db	esc,'D$',0		;Cursor left
te:	db	esc,'E$',0		;Clear display
tf:	db	'$',0,0,0		;Enter Graphics Mode
tg:	db	'$',0,0,0		;Exit Graphics mode
th:	db	esc,'H$',0		;Cursor home.
ti:	db	esc,'I$',0		;Reverse linefeed.
tj:	db	esc,'J$',0		;Clear to end of screen.
tk:	db	esc,'K$',0		;Clear to end of line.

extfnt:	db	0			;extended font flag
;------------------------------------------------------------------------------
	;KCNet data
KMODE:	db	0		;KERMIT mode: 0 -Kermit / 1-Telnet Client
BSSTAM:	db	0		;screen state (KC85)
SOCKNM:	db	0FFH		;current socket handle
outdat:	db	0		;output byte
inpdat:	db	0		;input byte
PEERIP:	db	0,0,0,0		;Server IP (=0->force input)
PEERPT:	db	0,0		;Server PORT: Telnet-23 / Kermit-1649
	;connect Negotiation
CONNEG:	db	255,253,1	;Do Echo
	db	255,253,3	;Do Supress-GA
	db	255,253,0	;Do Binary
	db	255,251,0	;Will Binary
	db	255,252,39	;Wont new env.
	db	255,253,47	;Do Kermit
CONNGL	equ	$-CONNEG
	;standard option answers
WONTOP:	db	255,252,0	;Wont option
DONTOP:	db	255,254,0	;Dont option
	;Server: 'Do terminal'
	;db	255,253,24		;just info
	;Client: 'Will terminal'
CWILLT:	db	255,251,24
	;Server: 'Send your terminal'
	;db	255,250,24,1,255,240	;just info
	;Client: 'My Terminal is VT52'
CMYTIS:	db	255,250,24,0,'DEC-VT52',255,240
CMYTIL	equ	$-CMYTIS

;------------------------------------------------------------------------------

; Input/output routines. 

	; select modem for I/O
selmdm:	RET

	; output character in E to network
outmdm:	PUSH	HL
	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	HL,outdat	;address
	LD	(HL),E		;save data
	LD	BC,1		;length
	LD	A,(SOCKNM)
	CALL	SEND		;Send TCP-Data
	POP	AF
	POP	BC
	POP	DE
	POP	HL
	RET

	; read character from network. 
	; return character or 0 in A.
	; test, answer and flush Telnet commands
inpmdm:	PUSH	HL
	PUSH	DE
	PUSH	BC
	CALL	inpmdc		;read next char
	JR	C,inpmde	;no Data
	LD	A,(HL)		;get char
	INC	(HL)
	JR	Z,inpiac	;-> IAC received
;	CP	80H		; <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<< 
;	JR	C,inpmde
;	XOR	A		;suppress > 127
inpmde:	POP	BC
	POP	DE
	POP	HL
	RET
	;IAC received
inpiac:	CALL	inpmdc		;read next char
	JR	C,inpmde	;no Data
	;check options
	LD	A,(HL)
	CP	250		;Suboption -> (flush)
	JR	Z,inpsbo
	CP	251		;Will -> flush
	JR	Z,inpiaf
	CP	252		;Wont -> Dont
	JR	Z,inpsdt
	CP	253		;Do -> (Wont)
	JR	Z,inpdot
	CP	254		;Dont -> Wont
	JR	Z,inpswt
	JR	inpmnc		;ignore other commands
inpiaf:	CALL	inpmdc		;flush option char
inpmnc:	XOR	A		;set A to no data
	JR	inpmde
inpsdt:	;sent Dont
	CALL	inpmdc		;read option char
	JR	C,inpmde	;no Data
	LD	A,(HL)
	LD	(DONTOP+2),A
	LD	HL,DONTOP
inpsdi:	LD	BC,3		;length
	LD	A,(SOCKNM)
	CALL	SEND		;Send TCP-Data
	JR	inpmnc
inpswt:	;sent Wont
	CALL	inpmdc		;read option char
	JR	C,inpmde	;no Data
	LD	A,(HL)
inpswi:	LD	(WONTOP+2),A
	LD	HL,WONTOP
	JR	inpsdi
inpdot:	;check Do terminal
	CALL	inpmdc		;read suboption char
	JR	C,inpmde	;no Data
	LD	A,(HL)		;get option
	CP	24
	JR	NZ,inpswi	;not Terminal
	LD	HL,CWILLT
	JR	inpsdi		;send Will Term.
	;check suboption
inpsbo:	CALL	inpmdc		;read suboption char
	JR	C,inpmde	;no Data
	LD	A,(HL)		;get option
	CP	24
	JR	NZ,inpsbi	;not Terminal
	CALL	inpmdc		;read next char
	JR	C,inpmde	;no Data
	LD	A,(HL)		;get option
	CP	1
	JR	NZ,inpsbi	;not Send
	;send My Terminal is
	LD	HL,CMYTIS	;address
	LD	BC,CMYTIL	;length
	LD	A,(SOCKNM)
	CALL	SEND		;Send TCP-Data
inpsbw:	;flush suboption data
	CALL	inpmdc		;read next char
	JP	C,inpmde	;no Data
	LD	A,(HL)		;get char
inpsbi:	INC	A
	JR	NZ,inpsbw	;search IAC
	CALL	inpmdc		;read last char
	JR	inpmnc		;end with no data
	;read char in buffer
inpmdc:	LD	A,(SOCKNM)
	LD	HL,inpdat
	LD	BC,1		;length
	CALL	RECV		;Recv TCP-Data
	DEC	HL		;=inpdat
	LD	A,0		;no data
	RET

	; flush pending input from network
flsmdm:	LD	A,(SOCKNM)
flsmdr:	LD	HL,inpdat
	LD	BC,1		;length
	CALL	RECV		;Recv TCP-Data
	JR	NC,flsmdr	;repeat
	RET

	; select console for I/O
selcon:	RET

	; output character in E to console
outcon:	LD	C,E
;	RES	7,C		;suppress > 127  <<<<<<<<<<<<<<<<<<<<<<<<*>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
	LD	A,E
	CP	' '		;if it is printable character go print it <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	JP	NC,bcnout
	CP	0CH		;convert form feed to clear screen <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
	JR	NZ,outcn1
	JP	clrtop
outcn1:	CP	15H
	JR	NZ,outcn2
	LD	C,09H
	JP	bcnout
outcn2:	CP	18H
	JR	NZ,outcn3
	LD	C,11H
	JP	bcnout
outcn3:	CP	1AH
	JR	NZ,outcn4
	LD	C,0BH
	JP	bcnout
outcn4:	CP	7
	JP	Z,bcnout
	CP	8
	JP	Z,bcnout
	CP	9
	JP	Z,bcnout
	CP	0AH
	JP	Z,bcnout
	CP	0DH
	JP	Z,bcnout
	CP	1BH
	JP	Z,bcnout
	RET			;ignore any other control code
;
	; read char from console. return character or 0 in A
inpcon:	CALL	bconst
	OR	A
	RET	Z		;no char
	CALL	bconin
	RET

	; output character in E to printer
outlpt:	PUSH	DE
	LD	C,E
	CALL	blsout
	POP	DE
	RET

	; *NEW*  get the status for the printer. 
lptstat:
	CALL	bprtst
	INC	A		;FF->00 = ready
	RET

;------------------------------------------------------------------------------

; screen formatting routines

clrlin:	; erase current line
	ld	DE,eralin
	jp	prtstr

clrspc:	; erase current position (after backspace)
	ld	e,' '
	call	outcon
	ld	e,bs		;get a backspace
	jp	outcon

delchr:	; make delete look like backspace
	ld	e,bs		;get a backspace
	call	outcon
	call	clrspc
	ld	e,bs		;get a backspace
	call	outcon
	jp	clrspc
	
; erase the whole screen, and go home. preserves b (but not c)
clrtop:	ld	de,outlin
	jp	prtstr

;------------------------------------------------------------------------------

; other system dependent routines

;	sysxin - system dependent initialisation code, called from SYSINIT
sysxin:
;	xor	a		;allow IBM extended charset (>128)  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
;	ld	(3e0eh),a	;by removing the AND 7FH command
;	ld	(3e0fh),a
;	ld	(4e04h),a	;treat DEL as BS
;	ld	(4e05h),a
;	ld	(4e06h),a	
	;check for CP/M 2+
	LD	C,12		;get CP/M-Version
	CALL	BDOS
	AND	0F0H		;version 2+ required
	CP	20H
	JR	NC,N_TRYN
	LD	DE,CPMERR
	JP	ETXOUT
N_TRYN:	;try Network init
	LD	A,1		;test KOP -> DRV (required only for KC85)
	CALL	N_INIT		;PO: DE=ERR-string
	JP	C,ETXOUT
	;check Network
	CALL	NETTST		;configured?
	JP	C,T_ENDE
	CALL	ONTEST		;online?
	JP	C,T_ENDE
	;get TCP-socket
	LD	A,0FFH		;any
	LD	D,SK_STREAM	;Mode
	LD	E,SO_NDACK	;Flags
	CALL	SOCKET
	LD	DE,SCKERR	;socket-error
	JP	C,ETXOUT	;Err
	LD	(SOCKNM),A
	;select KERMIT-Mode
	ld	de,kmodtx	;op-mode msg
	call	prtstr
N_SMOD:	CALL	CHRIN
	JR	C,N_EXCS	;exit with close socket
	AND	0DFH		;ucase
	LD	HL,KMODE
	LD	DE,01700H	;NOrder!
	LD	(PEERPT),DE
	SET	0,(HL)		;TELNET mode
	CP	'T'
	JP	Z,N_TMOD
	LD	DE,07106H	;NOrder!
	LD	(PEERPT),DE
	RES	0,(HL)		;KERMIT mode
	CP	'C'
	JR	Z,N_KCLT
	CP	'S'
	JR	NZ,N_SMOD
N_KSRV:	;Server: wait for connection request from tcp-client
	LD	E,A
	CALL	outcon
	LD	A,(SOCKNM)
	LD	HL,PEERPT	;bind Server Port
	CALL	BIND
	JR	C,N_EXCS	;Err->close
	CALL	LISTEN
	JR	C,N_EXCS	;Err->close
	ld	de,kwtmsg	;wait msg
	call	prtstr
N_SVWT:	LD	A,(SOCKNM)
	LD	HL,PEERIP	;address for Peer-IP
	CALL	ACCEPT		;Client ?
	JR	NC,N_CLCN	;-> connected
	CALL	CHRIN
	JR	NC,N_SVWT	;wait
N_EXCS:	;stop -> exit with close socket!
	CALL	eclose		;close socket!
	JP	T_ENDE
N_CLCN:	;client connected
	ld	de,kccmsg	;client c. msg
	CALL	prtstr
	CALL	PDTOUT		;Peer Data Out
	JR	N_KCCN
N_KCLT:	;Client: connect to tcp-server
	LD	E,A
	CALL	outcon
	LD	HL,buff
	LD	DE,ovlend
	LD	BC,80H
	LDIR			;save commandline
	LD	HL,GKPTXT
	LD	(ADRTXT),HL
	CALL	GTPEER		;input NAME|IP:[PORT]
	LD	HL,ovlend
	LD	DE,buff
	LD	BC,80H
	LDIR			;restore commandline
	LD	A,(SOCKNM)
	LD	HL,PEERIP	;Peer IP:PORT
	CALL	CONNECT		;with local dyn. Port
	JR	NC,N_KCCN	;-> ok
	ld	de,kncmsg	;err msg
	CALL	prtstr
	JR	N_EXCS		;exit with close socket
N_KCCN:	;send first Negotiation
	LD	HL,CONNEG	;address
	LD	BC,CONNGL	;length
	CALL	SEND		;Send TCP-Data
	;check receiver for first Negotiation data
N_RFNG:	CALL	inpmdm
	OR	A
	JR	NZ,N_RFNG	;flush all data
	ld	de,kcnmsg	;session ok msg
	CALL	prtstr
	JR	N_KRET
N_TMOD:	;Telnet mode without connection in sysxin 
	LD	E,A
	CALL	outcon
	ld	de,ktnmsg	;telnet msg
	CALL	prtstr
N_KRET:	;return to kermit loop
	CALL	prcrlf		;always NEWLN
	RET			;Init OK
	;select mode messages
kmodtx:	db	cr,lf
	db	' Select Kermit operation mode:',CR,LF
	db	'  >T< Telnet client',CR,LF
	db	'  >C< Kermit service Client side',CR,LF
	db	'  >S< Kermit service Server side '
	db	'$'
kncmsg:	db	cr,lf
	db	' Kermit session failed !',Beep
	db	'$'
kcnmsg:	db	cr,lf
	db	' Kermit session opened.'
	db	'$'
kwtmsg:	db	cr,lf
	db	' Waiting for client ... stop with >ESC<.'
	db	'$'
kccmsg:	db	cr,lf
	db	' Client connected.',CR,LF
	db	'$'
ktnmsg:	db	cr,lf
	db	' Open Telnet session with "CONNECT".'
	db	'$'
	;
	;error message
ETXOUT:	CALL	prtstr
T_ENDE:	;Quit
	CALL	prcrlf		;always NEWLN
	;don't JUMP (gives an
	;Error with Z-System)
	LD	C,0
	CALL	bdos
	;go to CP/M

;	system-dependent termination processing
;	If we've changed anything, this is our last chance to put it back.
sysexit:
eclose:	;release socket
	LD	A,(SOCKNM)
	CALL	CLOSE
	RET

;	system-dependent processing for start of CONNECT command
syscon:	ld	de,conmsg	;about obscure key combinations
	call	prtstr
	LD	A,(KMODE)
	OR	A
	RET	Z		;KERMIT mode: nothing to do
	CALL	ONTEST		;online?
	JR	C,syscoe
	LD	HL,GTPTXT
	LD	(ADRTXT),HL
	CALL	GTPEER		;input NAME|IP:[PORT]
	LD	A,(SOCKNM)
	LD	HL,PEERIP	;Peer IP:PORT
	CALL	CONNECT		;with local dyn. Port
	ld	de,ncnmsg
	JR	C,syscoc	;err msg
	;ok -> continue with connect-negotiation
	LD	HL,CONNEG	;address
	LD	BC,CONNGL	;length
	CALL	SEND		;Send TCP-Data
	ld	de,scnmsg	;ok msg
syscoc:	CALL	prtstr
syscoe:	CALL	prcrlf		;always NEWLN
	RET
	; Messages printed when entering transparent (CONNECT) mode:
conmsg:	db	' [Use STOP key to generate a Control-S !]',cr,lf
	db	'$'
ncnmsg:	db	' Telnet session failed!',Beep
	db	'$'
scnmsg:	db	' Telnet session opened.'
	db	'$'

;	syscls - system-dependent close routine
;	called when exiting transparent session.
syscls:	LD	A,(KMODE)
	OR	A
	RET	Z		;KERMIT mode: nothing to do
	LD	A,(SOCKNM)
	CALL	SHUTDN
	LD	DE,SYSCTX
	call	prtstr
	RET
SYSCTX:	DB	cr,lf
	DB	' Telnet session closed.'
	DB	'$'

;	sysinh - help for system-dependent special functions.
;	called in response to <escape>?, after listing all the
;	system-independent escape sequences.
sysinh:	LD	DE,inhlps	; we got options...
	call	prtstr		; print them.
	RET

;	additional, system-dependent help for transparent mode
;	 (two-character escape sequences)
inhlps:
	db	cr,lf,'T  TCP connection state'
;
	db	'$'			;[hh] table terminator

;	sysint - system dependent special functions
;	called when transparent escape character has been typed;
;	the second character of the sequence is in A (and in B).
;	returns:
;	non-skip: sequence has been processed
;	skip:	sequence was not recognized
sysint:	and	137O		; convert lower case to upper, for testing...

	CP	'T'
	JR	Z,tlstat

	jp	rskp		; take skip return - command not recognized.

tlstat:	;check socket State
	LD	A,(SOCKNM)
	LD	E,SL_STAT	;State
	CALL	SELECT
	LD	HL,TLTXT1	;'online'
	LD	A,S_ESTABLISHED
	CP	E
	JR	Z,tlstae
	LD	HL,TLTXT2	;'offline'
	LD	A,S_CLOSED
	CP	E
	JR	Z,tlstae
	LD	HL,TLTXT3	;'undefined'
tlstae:	EX	DE,HL
	CALL	prtstr
	CALL	prcrlf		;always NEWLN
	RET
TLTXT1:	DB	cr,lf,' PEER online.','$'
TLTXT2:	DB	cr,lf,' PEER offline.',BEEP,'$'
TLTXT3:	DB	cr,lf,' PEER state undefined.',BEEP,'$'

;	sysflt - system-dependent filter
;	called with character in E.
;	if this character should not be printed, return with A = zero.
;	preserves bc, de, hl.
;	note: <xon>,<xoff>,<del>, and <nul> are always discarded.
sysflt:	ld	a,e		; get character for testing
	RET

; system-dependent processing for BYE command.
sysbye:	RET

; system-dependent baud rate change routine.
sysspd:	RET

; system-dependent port change routine.
sysprt:	RET

; move cursor to row B, column C
csrpos:	push	bc		; save coordinates
	ld	de,curldn	; get cursor leadin sequence
	call	prtstr		; print it
	pop	hl		; restore coordinates
	ld	a,h		; get row
	add	a,' '-1		; space is row one
	ld	e,a
	push	hl
	call	outcon		; output row
	pop	hl
	ld	a,l		; get column
	add	a,' '-1		; space is column one
	ld	e,a
	jp	outcon		; output it and return

;------------------------------------------------------------------------------

;	### "KCNet" TCP/IP driver ###

;	    ###     KCNET     ###
;	     WIZnet TCP/IP-Stack 
;	    ###  susowa 2008  ###
;
;	required Includes in CPXTYP.ASM:
;
;	N4C-W51.INC - Interface driver Net4CPC W5100S
;	W5100-12.INC - TCP/IP and Socket driver
;	DNSC-10.INC  - DNS client
;
	;### KERMIT version ***
V_MAJOR	EQU	4
V_MINO1	EQU	1
V_MINO2	EQU	1
;	;### buffer net-test ###
LCFNBF:	DS	8,0
;	### START Error-Messages ###
SCKERR:	DB	CR,LF,'Socket Error !',BEEP,'$'
CPMERR:	DB	CR,LF,'CP/M-Version 2+ required !',BEEP,'$'
	;Online-Test with ERR-Message to CON
	;PO:	CY=1 offline
ONTEST:	CALL	N_LSTA		;LINK STATE
	XOR	A
	OR	E		;OK?
	LD	DE,NONTXT
	JR	NETTSE
NONTXT:	DB	'Network-cable not connected!',BEEP,'$'
	;Network-Test with ERR-Message to CON
	;PO:	CY=1 not configured
NETTST:	LD	HL,LCFNBF	;Buffer
	PUSH	HL
	CALL	N_GLIP		;read IP
	CALL	N_GLMA		;read MASK
	XOR	A
	LD	DE,NKNTXT
	POP	HL
	CALL	CPA4BT		;IP 4*000 ?
	JR	Z,NETTSE	;not conf.
	CALL	CPA4BT		;SNM 4*000 ?
NETTSE:	CALL	Z,prtstr
	SCF			;Error
	RET	Z		;not conf./offline
	CCF			;OK
	RET
NKNTXT:	DB	'Network not configured!',BEEP,'$'
	;compare A-(HL) 4 Byte
CPA4BT:	LD	BC,4
CPABTL:	CPI
	RET	NZ
	JP	PE,CPABTL
	RET			;Z=1 same
	;(DE)-string + 0 Out
ZKOUT:	PUSH	AF
ZKOU1:	LD	A,(DE)
	INC	DE
	AND	A
	JR	Z,ZKOU2
	PUSH	DE
	LD	E,A
	CALL	outcon
	POP	DE
	JR	ZKOU1
ZKOU2:	POP	AF
	RET
	;(HL) IP-Address to CON Out
IPOUT:	PUSH	AF
	PUSH	DE
	PUSH	HL
	LD	DE,ovlend+80H	;buffer
	CALL	I_NTOA		;convert
	CALL	ZKOUT		;Out
	POP	HL
	POP	DE
	POP	AF
	RET
	;Peer Data to CON Out
PDIPXT:	DB	'   IP: ',0
PDNMXT:	DB	CR,LF,' Name: ',0
PDTOUT:	PUSH	HL		;IP pointer
	LD	DE,PDIPXT
	CALL	ZKOUT
	POP	HL
	CALL	IPOUT		;Peer-IP Out
	LD	DE,ovlend+128	;Domain-Name buffer (256 byte)
	PUSH	DE
	LD	BC,4
	LDIR			;copy
	POP	DE
	LD	HL,ovlend+128+256 ;DNS-MSG buffer (534 byte)
	CALL	GHBADR		;IP-Address inverse query
	RET	C		;no name from DNS-Server
	PUSH	DE
	LD	DE,PDNMXT
	CALL	ZKOUT
	POP	DE
	CALL	ZKOUT		;Peer DNS-Name Out
	RET
CHRIN:	CALL	inpcon
	OR	A		;CY=0!
	RET	Z		;no Input
	;check "stop"
	CP	ETX
	SCF			;BRK
	RET	Z
	CP	ESC
	SCF			;BRK
	RET	Z
	CCF			;CY=0!
	RET
	;Input PEER[:PORT] and resolve to PEERIP[:PEERPT]
DNSQSE:	DB	'DNS-Server error!',BEEP,'$'
DNSQCE:	DB	'DNS-Client error!',BEEP,'$'
ARG:	DW	0		;buffer
ADRTXT:	DW	0		;address of Get Peer text
GKPTXT:	db	cr,lf
	db	'Connect to Kermit-Server (default PORT 1649)',CR,LF
	db	' NAME|IP[:PORT] : ','$'
GTPTXT:	db	cr,lf
	db	'Connect to Telnet-Server (default PORT 23)',CR,LF
	db	' NAME|IP[:PORT] : ','$'
GTPEER:	LD	DE,(ADRTXT)
	CALL	prtstr
	LD	DE,buff		;DMA buffer
	LD	A,126
	LD	(DE),A
	LD	C,rdstr		;input string
	CALL	BDOS
	LD	HL,buff+1	;# of char's
	LD	C,(HL)
	LD	B,0
	INC	HL
	XOR	A
	OR	C
	JR	NZ,GTPEE0
	LD	HL,PEERIP
	CALL	CPA4BT		;IP 4*000 ?
	JR	Z,GTPEER	;not conf.
	JR	GTPEE7		;no Input -> use last IP:Port
GTPEE0:	PUSH	HL
	ADD	HL,BC
	LD	(HL),0		;write 0-delimiter
	POP	DE		;buff+2 = first char
	PUSH	DE
GTPEE1:	LD	A,(DE)
	CP	':'		;find PORT delimiter
	JR	Z,GTPEE2
	OR	A
	JR	Z,GTPEE3	;not found
	INC	DE
	JR	GTPEE1
GTPEE2:	EX	DE,HL
	LD	(HL),0		;':'->0 = name delimiter
	INC	HL
	CALL	ATOI		;(HL)STRING->DE=PORT#
	JR	C,GTPEE3	;PORT Input-ERR
	LD	(ARG),DE
	LD	HL,ARG
	LD	DE,PEERPT
	CALL	HTONS		;conv.+copy PORT
GTPEE3:	POP	DE		;first char input
	LD	A,(DE)
	CP	' '
	JR	Z,GTPEER	;' '-PEER from input
	CP	'0'
	JR	C,GTPEE4	;<0 no Digit
	CP	'9'+1
	JR	NC,GTPEE4	;>9 no Digit
	;IP ?
	LD	HL,LCFNBF	;buffer
	CALL	I_ADDR		;(DE)STRING->(HL)IP
	JR	NC,GTPEE6	;copy IP
GTPEE4:	;Name
	LD	HL,ovlend+80H	;DNS-MSG buffer (534 byte)
	CALL	GHBNAM		;query:(DE)Name->(DE)IP
	EX	DE,HL
	JR	NC,GTPEE6	;ok -> copy
	;ERR resolve
	CALL	prcrlf		;NL
	LD	DE,DNSQSE	;Server-
	CP	16
	JR	C,GTPEE5
	LD	DE,DNSQCE	;Client-
GTPEE5:	CALL	prtstr		;ERROR
	JP	GTPEER
GTPEE6:	LD	DE,PEERIP
	LD	BC,4
	LDIR
GTPEE7:	CALL	prcrlf		;NL
	RET

;------------------------------------------------------------------------------
;       !!! 918 byte free space after "ovlend" required !!!
; if this KERMIT version uses nearly your whole TPA, be aware,
;           that no tests for free memory are performed
;------------------------------------------------------------------------------

IF lasm and termin	; if no terminal, no need to link
LINK CPXVDU.ASM
ENDIF	; lasm and termin

IF NOT termin		; any terminal selected?
ovlend	EQU	$
ENDIF	; termin

IF lasm
	END		; If m80 then this ignored
ENDIF	; lasm
