;*******************************************************
;
;		"KCNet" Hardware-Driver
;	     (W5100S-Version for direct I/O)
;
;		###     KCNET     ###
;		 WIZnet TCP/IP-Stack 
;		###  d_kef 2020  ###
;
;  Name		: KCN-W51.INC
;  Date		: 18.08.2020
;  Modified	: 09.10.2021
;		: 25.11.2022
;  Hardware	: Net4CPC - W5100S
;
;*******************************************************

;#######################################################
;### Declarations ######################################
;#######################################################
	;
	;### 4 contiguous W5100s I/O-Addresses ###
W51MR	EQU	W51ADDR	; W5100S-address from KCNET.INC
W51HAD	EQU	W51MR + 1
W51LAD	EQU	W51MR + 2
W51DAT	EQU	W51MR + 3

W51OK	EQU	3	; W5100S init ok value

W51MRH	EQU	HIGH W51MR
W51MRL	EQU	LOW W51MR
	;
	;W5100S MAC Address
W51MAC:	DB 0DEH, 0ADH, 0BEH, 0EFH, 00H, 00H
D_ADR1: DB 00, 00, 00, 00
D_ADR2: DB 00, 00, 00, 00
D_ADR3: DB 00, 00, 00, 00
D_ADR4: DB 00, 00, 00, 00
D_ADR5: DB 00, 00, 00, 00
D_ADR6: DB 00, 00, 00, 00
D_ADR7: DB 00, 00, 00, 00
D_ADR8: DB 00, 00, 00, 00
	;
;#######################################################
;### Test and Activation ###############################
;#######################################################

	;### INTERFACE INIT ###
	;PI:	-
	;PO:      OK: CY = 0
	;        ERR: CY = 1 / DE=address ERR-string
	;
	;driver label
N_DLBL:	DB	'       System : Net4CPC - W5100S',CR,LF
	DB	'    Interface : 0'
IF (W51MRH / 16) LT 10
	DB	(W51MRH / 16) + '0'
ELSE
	DB	(W51MRH / 16) + 7 + '0'
ENDIF
IF (W51MRH AND 15) LT 10
	DB	(W51MRH AND 15) + '0'
ELSE
	DB	(W51MRH AND 15) + 7 + '0'
ENDIF
IF (W51MRL / 16) LT 10
	DB	(W51MRL / 16) + '0'
ELSE
	DB	(W51MRL / 16) + 7 + '0'
ENDIF
IF (W51MRL AND 15) LT 10
	DB	(W51MRL AND 15) + '0'
ELSE
	DB	(W51MRL AND 15) + 7 + '0'
ENDIF
	DB	'H',CR,LF,'$'
	;
N_INIT:	LD	BC,W51MR
	IN	A,(C)
	CP	W51OK
	JR	Z,INIMAC
	SCF
	LD	DE,NITERR	;no Interface
	RET	C
;
INIMAC:
IF	CKMAC
        LD      DE,MACFCB
        CALL    F$EXIST##       ;check if file exists
        JR      Z,WRINI
        CALL    FI0$OPEN##      ;open file for byte read
        JP      NZ,FLERR
        LD      HL,MACADD
        LD      B,12            ;read 12 characters
NEXTC:  CALL    F0$GET##
        JP      NZ,FLERR
        LD      (HL),A
        INC     HL
        DJNZ    NEXTC
        CALL    FI0$CLOSE##
        JP      NZ,FLERR
;
	LD	B,6
	LD	HL,MACADD
	LD	DE,W51MAC
NEXTH:	PUSH	DE
	LD	A,(HL)
	INC	HL
	CALL	ISHEX##
	JP	NZ,FLERR1
	CALL	CAPS##
	LD	D,A
	LD	A,(HL)
	INC	HL
	CALL	ISHEX##
	JP	NZ,FLERR1
	CALL	CAPS##
	LD	E,A
	LD	A,D
;
	CALL	HEX1
	ADD	A,A
	ADD	A,A
	ADD	A,A
	ADD	A,A
	LD	D,A
	LD	A,E
	CALL	HEX1
	OR	D
	JR	NEXTHC
HEX1:	SUB	'0'
	CP	10
	RET	C
	SUB	'A'-'0'-10
	RET
;
NEXTHC:
	POP	DE
	LD	(DE),A
	INC	DE
	DJNZ	NEXTH
;
        JP      INITOK          ;exit
WRINI:  CALL	RND##
	LD	(W51MAC+4),A
	LD	DE,MACADD+8
	CALL	MA2HC##
	CALL	RND##
	LD	(W51MAC+5),A
	LD	DE,MACADD+10
	CALL	MA2HC##
	LD	DE,MACFCB
	CALL    FO0$OPEN##
        JR      NZ,FLERR
        LD      HL,MACADD
        LD      B,12
NEXTM:  LD      A,(HL)
        CALL    F0$PUT##
        JR      NZ,FLERR
        INC     HL
        DJNZ    NEXTM
        LD      DE,MACFCB
        CALL    FO0$CLOSE##
        JR      INITOK
;
FLERR1:	POP	DE
FLERR:	SCF
	LD	DE,MACERR
        RET
;
MACFCB: DB      0
        DB      'MACADDR INI'
        DS      24,0
MACADD: DB      "DEADBEEF"
	DB      "0000"
MACERR:	DB	CR,LF,'Could not assign MAC Address !',BEEP,'$'
;
ENDIF
INITOK:
	;Init OK
	LD	HL,W51MAC	;write MAC Adress to W5100S
	LD	DE,N_MAC0
	LD	BC,6
	CALL	L_WRBF
	LD	HL,L_FTAB
	LD	DE,N_WRBF
	LD	BC,N_VTABL
	LDIR			;copy vectors
	;test FW-Version
	CALL	N_SVER		;read FW-Version
	LD	A,H
	LD	DE,FWTERR	;FW Error-Msg
	CP	N_MAJOR
	RET	C
	LD	A,L
	CP	N_MINOR
	RET
	;
NITERR:	DB	CR,LF,'Net4CPC Interface not found !',BEEP,'$'
FWTERR:	DB	CR,LF,'At least KCNet Firmware-Version '
	DB	N_MAJOR+'0','.',N_MINOR+'0',' required !',BEEP,'$'

;#######################################################
;### Driver functions ##################################
;#######################################################
	;
	;### FTAB ###
L_FTAB:	JP	L_WRBF
	JP	L_RDBF
	JP	L_PTR
	JP	L_TIME
	JP	L_WRBT
	JP	L_RDBT
	JP	L_WIPA
	JP	L_RIPA
	JP	L_DPRT
	JP	L_SVER
	JP	L_HVER
	JP	L_LSTA
	JP	L_INFO
	JP	L_CERR

	;### WRITE BYTE ### (#define IINCHIP_WRITE(addr, data))
	;PI:	HL - NetAddress
	;	A - byte
	;PO:	-
	;AR:	AF
L_WRBT:	PUSH	BC
	LD	BC,W51HAD
	OUT	(C),H
	INC	C
	OUT	(C),L
	INC	C
	OUT	(C),A
	POP	BC
	RET

	;### READ BYTE ### (#define IINCHIP_READ(addr))
	;PI:	HL - NetAddress
	;PO:	A - byte
	;AR:	AF
L_RDBT:	PUSH	BC
	LD	BC,W51HAD
	OUT	(C),H
	INC	C
	OUT	(C),L
	INC	C
	IN	A,(C)
	POP	BC
	RET

	;### SET BUFFER POINTER ###
	;PI:	HL - NetAddress Pointer
	;PO:	-
	;AR:	-
L_PTR:	RET

	;### WRITE BUFFER ### (#define wiz_write_buf(addr, buf, len))
	;PI:	HL - HostAddress
	;	DE - NetAddress buffer
	;	BC - length
	;PO:	HL - HostAddress+length
	;AR:	HL,BC
L_WRBF:	DI
	PUSH	AF
	PUSH	DE
	PUSH	BC
	LD	BC,W51HAD
	OUT	(C),D
	INC	C
	OUT	(C),E
	INC	C
	POP	DE	;length now in DE
L_WRB1:
	XOR	A
	OR	D
	JP	Z,L_WRB2
	REPT	256
	INC	B
	OUTI
	ENDM
	DEC	D
	JP	L_WRB1
L_WRB2:
	XOR	A
	OR	E
	JR	Z,L_WRB3
	SRL	A
	SRL	A
	SRL	A
	SRL	A
	OR	A
	JR	Z,L_WRB4
L_WRB5:
	REPT	16
	INC	B
	OUTI
	ENDM
	DEC	A
	JR	NZ,L_WRB5
L_WRB4:
	XOR	A
	OR	E
	AND	0FH
	LD	E,A
	LD	A,45
	SUB	E
	SUB	E
	SUB	E
	LD	(W15BR-1),A
	JR	W15BR
W15BR:
	REPT	15
	INC	B
	OUTI
	ENDM
L_WRB3:
	POP	DE
	POP	AF
	EI
	RET

	;### READ BUFFER ### (#define wiz_read_buf(addr, buf, len))
	;PI:	HL - HostAddress
	;	DE - NetAddress Buffer
	;	BC - length
	;PO:	HL - HostAddress+length
	;AR:	HL,BC
L_RDBF:	DI
	PUSH	AF
	PUSH	DE
	PUSH	BC
	LD	BC,W51HAD
	OUT	(C),D
	INC	C
	OUT	(C),E
	INC	C
	POP	DE	;length now in DE
L_RDB1:
	XOR	A
	OR	D
	JP	Z,L_RDB2
	REPT	256
	INI
	INC	B
	ENDM
	DEC	D
	JP	L_RDB1
L_RDB2:
	XOR	A
	OR	E
	JR	Z,L_RDB3
	SRL	A
	SRL	A
	SRL	A
	SRL	A
	OR	A
	JR	Z,L_RDB4
L_RDB5:
	REPT	16
	INI
	INC	B
	ENDM
	DEC	A
	JR	NZ,L_RDB5
L_RDB4:
	XOR	A
	OR	E
	AND	0FH
	LD	E,A
	LD	A,45
	SUB	E
	SUB	E
	SUB	E
	LD	(R15BR-1),A
	JR	R15BR
R15BR:
	REPT	15
	INI
	INC	B
	ENDM
L_RDB3:
	POP	DE
	POP	AF
	EI
	RET

	;### WRITE IP-ADDRESS ###
	;PI:	 A - number
	;	HL - HostAddress of IP-Address
	;PO:	HL - HostAddress+4
	;AR:	HL
L_WIPA:	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	DE,0050H
	LD	BC,4
	CALL	L_WRBF
	POP	AF
	POP	BC
	POP	DE
	RET
	PUSH	BC
	PUSH	AF
	PUSH	DE
	PUSH	HL
	LD	D,0
	ADD	A,A
	ADD	A,A
	LD	E,A
	LD	HL,D_ADR1
	ADD	HL,DE
	EX	DE,HL
	POP	HL
	LD	BC,4
	LDIR
	POP	DE
	POP	AF
	POP	BC
	RET

	;### READ IP-ADDRESS ###
	;PI:	A - number
	;	HL - HostAddress of IP-Address
	;PO:	HL - HostAddress+4
	;AR:	HL
L_RIPA:	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	DE,0050H
	LD	BC,4
	CALL	L_RDBF
	POP	AF
	POP	BC
	POP	DE
	RET
	PUSH	BC
	PUSH	AF
	PUSH	DE
	PUSH	HL
	LD	D,0
	ADD	A,A
	ADD	A,A
	LD	E,A
	LD	HL,D_ADR1
	ADD	HL,DE
	EX	DE,HL
	POP	HL

	EX	DE,HL
	LD	BC,4
	LDIR
	EX	DE,HL
	POP	DE
	POP	AF
	POP	BC
	RET

	;### READ INFO ###
	;PI:	HL - HostAddress character buffer for 128 Byte
	;PO:	ID-string+0+'$' in buffer
	;AR:	-
L_INFO:	PUSH	HL
	PUSH	DE
	PUSH	BC
	EX	DE,HL
	LD	HL,L_INFS
	LD	BC,L_SVER - L_INFS
	LDIR
	POP	BC
	POP	DE
	POP	HL
	RET
L_INFS:
	DB	'Net4CPC - W5100S Ethernet Interface',CR,LF, 0, '$'

	;### READ SW-VERSION ###
	;PI:	-
	;PO:	HL - software version
	;binary:(MSB-major/LSB-minor)
	;AR:	HL
L_SVER:	LD H,N_MAJOR	; dummy firmware version
	LD L,N_MINOR
	RET

	;### READ HW-VERSION ###
	;PI:	-
	;PO:	HL - hardware version
	;binary:(MSB-major/LSB-minor)
	;AR:	HL
L_HVER:	PUSH	AF
	PUSH	BC
	LD	BC,W51HAD
	LD	A,00H
	OUT	(C),A
	INC	C
	LD	A,80H
	OUT	(C),A
	INC	C
	IN	A,(C)
	LD	H,A
	AND	0FH
	LD	L,A
	LD	A,H
	AND	0F0H
	SRL	A
	SRL	A
	SRL	A
	SRL	A
	LD	H,A
	POP	BC
	POP	AF
	RET

	;### GET LINK STATE ###
	;PI:	-
	;PO:	E - network state
	;AR:	E
L_LSTA:	PUSH	AF
	PUSH	BC
	LD	BC,W51HAD
	LD	A,00H
	OUT	(C),A
	INC	C
	LD	A,3CH
	OUT	(C),A
	INC	C
	IN	A,(C)
	AND	80H	; mask cable connect bit
	LD	E,0FFH
	JR	Z,L_STA0
	LD	E,0
L_STA0:	POP	BC
	POP	AF
	RET

	;### READ TIMER ###
	;PI:	-
	;PO:	HL - timer value HOrder
	;binary:16 Bit /  0...59999 ms
	;AR:	HL
L_TIME:
	PUSH	AF
	PUSH	BC
	LD	BC,W51HAD
	LD	A,00H
	OUT	(C),A
	INC	C
	LD	A,82H
	OUT	(C),A
	INC	C
	IN	A,(C)
	LD	H,A
	IN	A,(C)
	LD	L,A
	LD	C,10	;convert W5100S timer from usec to msec
	CALL	DIVHLC
	POP	BC
	POP	AF
	RET
;L_TIME:
	push	af
	push	bc
	push	de
	ld	hl,(1)	;get address of routine to call to execute a firmware function
	ld	de,57h	;
	add	hl,de
	ld	(fw_jump+1),hl
	call	fw_jump
	dw	0bd0dh	;KL_TIME_PLEASE
	pop	de
	pop	bc
	pop	af
	ret
fw_jump:		;execute a firmware function
	jp	$-$

L_MSEC:	DW	0

	;### READ Dynamic PORT ###
	;PI:	-
	;PO:	HL - dynamic Port number NOrder!
	;binary:16 Bit /  49152...65535
	;AR:	HL
L_DPRT:	PUSH	AF
	PUSH	BC
	LD	BC,W51HAD
	LD	A,00H
	OUT	(C),A
	INC	C
	LD	A,82H
	OUT	(C),A
	INC	C
	IN	A,(C)	; read timer
	OR	0C0H	; make it >= 49152
	LD	L,A
	IN	A,(C)
	LD	H,A
	POP	BC
	POP	AF
	RET

	;### READ COMMAND-ERROR's ###
	;PI:	-
	;PO:	HL - number of errors HOrder
	;binary:16 Bit /  0...65535
	;AR:	HL
L_CERR:	LD	HL,0000H	; no command error
	RET

	;### 16bit / 8bit division ###
	;divides hl by c and places the quotient in hl and the remainder in a
	;PI:
	;PO:
DIVHLC:
	XOR	A
	LD	B, 16
DLOOP:
	ADD	HL, HL
	RLA
	JR	C, $+5
	CP	C
	JR	C, $+4
	SUB	C
	INC	L
	DJNZ	DLOOP
	RET
 ;#######################################################
