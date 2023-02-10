;*******************************************************
;
;		"KCNet" Network-Interface 
; MID LEVEL TCP/IP Driver and HIGH LEVEL Socket Driver
;
;		###     KCNET     ###
;		 WIZnet TCP/IP-Stack 
;		###  susowa 2008  ###
;
;  Name		: W5100-xx.INC
;  Date		: 03.10.2007
;  Modified	: 15.10.2011
;		: 21.11.2022 d_kef
;  Hardware	: Net4CPC - W5100S
;
;  Version 0.1  : 03.10.2007
;  Version 0.2  : 30.11.2007
;  Version 0.3  : 31.01.2008
;  Version 0.4  : 15.06.2008
;
;  Version 1.0  : 22.08.2008
;  Version 1.1  : 02.11.2008
;  Version 1.2  : 15.10.2011
;
;*******************************************************


;*******************************************************
;		Addresses and Variables
;*******************************************************
;     ### Data-storage of variables in memory ###
;	
;   the so-called NOrder=NetOrder is always BIG ENDIAN
;   (the Most Significant Byte lies FIRST in memory)
;
;    the HOrder=HostOrder of the Z80 is LITTLE ENDIAN 
;   (the Least Significant Byte lies FIRST in memory)
;
; - all variables on both sides lie ALWAYS in NOrder in
;   memory, so transfers can be done without conversion
;
; - to compute with variables in the registers of Z80 
;   the NOrder of memory have to be converted before
;   reading or after writing - otherwise it gives wrong
;   results !!!
;
; - the Socket-Interface has 4 Functions for conversion
;   in both directions:
;
;	16 Bit variables: NTOHS, HTONS
;       32 Bit variables: NTOHL, HTONL
;
;		### KCNET-Addresses ###
;
; - all Adresses in Host-Memory or KCNET-Memory are in
;   HOrder - as usual in Z80-Assembler
;
;*******************************************************


;*******************************************************
;		ADDRESSES of KCNET TCPIP-Stack
; - there is an offset of 8000H to all addresses in the 
;   WIZnet datasheets for W3150A+ or W5100 !
; - please use the following SYMBOLS for addresses and
;   constants in your network-programs !
;*******************************************************
	;### Basis address ###
N_BASE	EQU	8000H
;-------------------------------------------------------
	;### COMMON REGISTERS ###
N_MODE	EQU	N_BASE		;Mode-Register

N_GAW0	EQU	N_BASE + 00001H	;Gateway-IP Addressregister 0
N_SUB0	EQU	N_BASE + 00005H ;Subnet-Maske Addressregister 0
N_MAC0	EQU	N_BASE + 00009H	;Source-MAC Addressregister 0
N_SIP0	EQU	N_BASE + 0000FH	;Source-IP Addressregister 0

N_IR	EQU	N_BASE + 00015H	;Interrupt Register
N_IMR	EQU	N_BASE + 00016H	;Interrupt Mask Register

N_RTR0	EQU	N_BASE + 00017H	;Timeout Addressregister 0 (1=100us)
N_RCNR	EQU	N_BASE + 00019H	;Retry-Count Register

N_RMSR	EQU	N_BASE + 0001AH	;Receive-Memory-Size Register
N_TMSR	EQU	N_BASE + 0001BH	;Transmit-Memory-Size Reigster

N_PATR	EQU	N_BASE + 0001CH	;AUTH-Type Addressregister 0 PPPoE Mode
N_PTIM	EQU	N_BASE + 00028H
N_PMGC	EQU	N_BASE + 00029H

N_UIP0	EQU	N_BASE + 0002AH	;Unreachable IP-Adressreg.0 UDP Mode
N_UPT0	EQU	N_BASE + 0002EH	;Unreachable Port-Adressreg.0 UDP Mode
;-------------------------------------------------------
	;### SOCKET REGISTERS (Socket 0) ###
	;    (4 * 0100H Bytes per Socket)
;	
;  ->   the Registeraddress for any Destination-Socket is
;	calculated through addition of the Socket-Number
;	(0-3) to the MSB of the same register-address of
;	Socket 0 - Register
;
N_SRBS	EQU	N_BASE + 00400H	;Basisaddress of Socketregisters

N_SMR	EQU 	N_SRBS + 00000H	;Socket Mode Register
N_SCR	EQU 	N_SRBS + 00001H	;Socket Command register
N_SIR	EQU	N_SRBS + 00002H	;Socket Interrupt Register
N_SSR	EQU	N_SRBS + 00003H	;Socket State Register
N_SSPT	EQU	N_SRBS + 00004H	;Socket Source Port Reg. 0
N_SDMC	EQU	N_SRBS + 00006H	;Socket Destination MAC Register 0
N_SDIP	EQU	N_SRBS + 0000CH	;Socket Destination IP Register 0
N_SDPT	EQU	N_SRBS + 00010H	;Socket Destination Port Register 0
N_SMSS	EQU	N_SRBS + 00012H	;Socket Maximum Segment Size Reg. 0

N_SPRT	EQU	N_SRBS + 00014H	;Socket Protocol in IP RAW-Mode
N_STOS	EQU	N_SRBS + 00015H	;Socket TOS-Register (type of service)
N_STTL	EQU	N_SRBS + 00016H	;Socket TTL-Register (time to live)

N_FSTX	EQU	N_SRBS + 00020H	;free Datasize Socket TX Register 0
N_STRP	EQU	N_SRBS + 00022H	;TX Readpointer Socket-Memory Reg. 0
N_STWP	EQU	N_SRBS + 00024H	;TX Writepointer Socket-Memory Reg.0

N_RSRX	EQU	N_SRBS + 00026H	;received Datasize Socket RX Register 0
N_SRRP	EQU 	N_SRBS + 00028H	;RX Readpointer Socket-Memory Reg. 0
N_SRWP	EQU	N_SRBS + 0002AH	;RX Writepointer Socket-Memory Reg. 0
;-------------------------------------------------------
	;### Buffer for Send- and Receive-Data ###
;(the configurable (N_TMSR/N_RMSR) distribution of BUFFER-MEMORY
;   	it is not supported by the following driver !!!)
N_TXBF	EQU	N_BASE + 04000H	;Send buffer for all Sockets
N_RXBF	EQU	N_BASE + 06000H	;Receive buffer for all Sockets
	;Constants for TX/RX-Buffer: standard configuration 4*2kB
N_TXSZ	EQU	2048		; Max Tx buffer size by each Socket
N_RXSZ	EQU	2048		; Max Rx buffer size by each Socket
;*******************************************************


;*******************************************************
;		CONSTANTS TCPIP-Stack
;*******************************************************
		;LIMITS
MAXSCK		EQU	4	;max. Number of sockets
MAXMTU		EQU	1480	;max. Data Size IP-Sender
MAXSEG		EQU	1460	;max. Segment Size (TCP-MSS)

		;COMMON REGISTERS
; MODE register values
MR_RST		EQU	80H	; reset
MR_PB		EQU	10H	; ping block
MR_PPPOE	EQU	08H	; enable pppoe
; INTERRUPT register values
IR_CONFLICT	EQU	80H	; check ip conflict
IR_UNREACH	EQU	40H	; got the destination unreachable message in UDP sending
IR_PPPoE	EQU	20H	; got the PPPoE close message

		;SOCKET REGISTERS
; Sn_MR values (socket mode register)
SMR_CLOSE	EQU	00H	; unused SOCKET
SMR_TCP		EQU	01H	; TCP SOCKET
SMR_UDP		EQU	02H	; UDP SOCKET
SMR_IPRAW	EQU	03H	; IP LAYER RAW SOCKET
SMR_MACRAW 	EQU	04H	; MAC LAYER RAW SOCKET
SMR_PPPOE	EQU	05H	; PPPoE SOCKET
; Sn_CR values (socket command register)
SCR_OPEN	EQU	01H	; initialize or open socket
SCR_LISTEN 	EQU	02H	; wait connection request in tcp mode (Server mode)
SCR_CONNECT	EQU	04H	; send connection request in tcp mode (Client mode)
SCR_DISCON	EQU	08H	; send closing request in tcp mode
SCR_CLOSE	EQU	10H	; close socket
SCR_SEND	EQU	20H	; updata txbuf pointer, send data
SCR_SMAC	EQU	21H	; send data with MAC address (without ARP process)
SCR_SKEEP	EQU	22H	; send keep alive message
SCR_RECV	EQU	40H	; update rxbuf pointer, receive data
; Sn_SR values (socket state register)
S_CLOSED	EQU	00H	; closed
S_INIT		EQU	13H	; init State
S_LISTEN	EQU	14H	; listen State
S_SYNSENT	EQU	15H	; connection State
S_SYNRECV	EQU	16H	; connection State
S_ESTABLISHED 	EQU	17H	; success to connect
S_FINWAIT	EQU	18H	; closing State
S_CLSING	EQU	1AH	; closing State
S_TIMEWAIT	EQU	1BH	; closing State
S_CLSWAIT	EQU	1CH	; closing State
S_LASTACK	EQU	1DH	; closing State
S_UDP		EQU	22H	; udp socket
S_IPRAW		EQU	32H	; ip raw mode socket
S_MACRAW	EQU	42H	; mac raw mode socket
S_PPPOE		EQU	5FH	; pppoe socket
; Sn_IR values (socket interrupt register)
SIR_SENDOK	EQU	10H	; complete sending
SIR_TIMEOUT	EQU	08H	; assert timeout
SIR_RECV	EQU	04H	; receiving data
SIR_DISCON	EQU	02H	; closed socket
SIR_CON		EQU	01H	; established connection
;*******************************************************


;*******************************************************
;		CONSTANTS SOCKET-Interface
;*******************************************************
; SOCKET PROTOCOL
SK_CLOSED	EQU	0	; unused socket = free socket
SK_STREAM	EQU	1	; TCP
SK_DGRAM	EQU	2	; UDP
SK_IPLRAW	EQU	3	; IP LAYER RAW
SK_MACLRAW	EQU	4	; MAC LAYER RAW
SK_PPPOE	EQU	5	; PPPoE

; SOCKET FLAGS
SO_NOFLAG	EQU	0	; Dummy for No Flags
SO_NDACK	EQU	20H	; (only TCP) no delayed Ack flag
SO_MULTI	EQU	80H	; (only UDP) support multicasting

; SOCKET SELECT FUNCTION
SL_STAT		EQU	0	; socket State
SL_SEND		EQU	1	; free data size in socket Tx buffer
SL_RECV		EQU	2	; received data size in socket Rx buffer

; IP PROTOCOL
PT_IP		EQU	0	; Dummy for IP
PT_ICMP		EQU	1	; Control message protocol
PT_IGMP		EQU	2	; Internet group management protocol
PT_GGP		EQU	3	; Gateway^2 (deprecated)
PT_TCP		EQU	6	; TCP
PT_PUP		EQU	12	; PUP
PT_UDP		EQU	17	; UDP
PT_IDP		EQU	22	; XNS idp
PT_ND		EQU	77	; UNOFFICIAL net disk protocol
PT_RAW		EQU	255	; Raw IP packet

; Receiver Headerlengths
RH_UDP		EQU	8	; UDP-Receiver
RH_IPR		EQU	6	; IP-Receiver
RH_MAC		EQU	2	; MAC-Receiver
;*******************************************************


;#######################################################
;### TCP/IP Driver : MID LEVEL Interface ###############
;#######################################################

;--- Functions for 'COMMON REGISTERS' -----------------

	;### SOFTWARE RESET ### (void iinchip_init(void))
	;PI:	-
	;PO:	Z=1 - RESET successful
	;AR:	AF,E
N_SWRS:	LD	E,MR_RST
	CALL	N_STMD
	XOR	A
SWRSW:	DEC	A
	JR	Z,SWRSC
	PUSH	AF		;wait some time
	PUSH	AF
	POP	AF
	POP	AF
	JR	SWRSW
SWRSC:	LD	HL,N_MODE	;Mode-Register
	CALL	N_RDBT
	AND	MR_RST
	RET
	
	;### SYSINIT ### (void sysinit(uint8 tx_size, uint8 rx_size))
	;- this function of the original-driver is not implemented !
	;- after Power-Up and Reset the RMSR-register of the stack contains 055H
	;- this is the standard configuration with 2048 bytes of buffer memory
	;  for every Socket in sending as well as receiving
	
	;### SET MODE REGISTER ### (#define setMR(val) (*((volatile uint8*)(MR)) = val))
	;PI:	E - Mode
	;AR:	-
N_STMD:	PUSH	HL
	LD	HL,N_MODE	;Mode-Register
STMDIN:	PUSH	AF
	LD	A,E
	CALL	N_WRBT
	POP	AF
	POP	HL
	RET

	;### SET INTERRUPT State ### (#define setISR(s, val (I_STATUS[s] = val))
	;PI:	E - Interrupt-State (clear Bits by writing '1'!)
	;AR:	-
N_SIST:	PUSH	HL
	LD	HL,N_IR		;Interrupt Register
	JR	STMDIN

	;### GET INTERRUPT STATE ### (#define getISR(s) (I_STATUS[s]))
	;PI:	-
	;PO:	E - Interrupt-State
	;AR:	E
N_GIST:	PUSH	HL
	LD	HL,N_IR		;Interrupt Register
	PUSH	AF
	JP	GSSTIC

	;### SET INTERRUPT MASK ### (#define setIMR(mask) (IINCHIP_WRITE(IMR,mask)))
	;PI:	E - Interrupt-Mask
	;AR:	-
N_SIMA:	PUSH	HL
	LD	HL,N_IMR	;Interrupt Mask Register
	JR	STMDIN

	;### SET RETRY TIME ### (void setRTR(uint16 timeout))
	;PI:	HL - HAddress Duration (2 Byte NOrder)
	;PO:	HL - HAddress + 2
	;AR:	HL
N_SRTI:	PUSH	DE
	LD	DE,N_RTR0	;Timeout Addressregister 0
	PUSH	BC
	LD	BC,2
	JR	SLIPIC

	;### SET RETRY COUNT ### (#define setRCR(retry) (IINCHIP_WRITE(RCR,retry)))
	;PI:	E - Retry-Counter
	;AR:	-
N_SRCN:	PUSH	HL
	LD	HL,N_RCNR	;Retry-Count Register
	JR	STMDIN

	;### SET LOCAL IP ### (void setSIPR(uint8 * addr))
	;PI:	HL - HAddress IP (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_SLIP:	PUSH	DE
	LD	DE,N_SIP0	;Source-IP Addressregister 0
SLIPIN:	PUSH	BC
	LD	BC,4
SLIPIC:	CALL	N_WRBF
	POP	BC
	POP	DE
	RET

	;### GET LOCAL IP ### (void getSIPR(uint8 * addr))
	;PI:	HL - HAddress IP (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_GLIP:	PUSH	DE
	LD	DE,N_SIP0	;Source-IP Addressregister 0
GLIPIN:	PUSH	BC
	LD	BC,4
GLIPIC:	CALL	N_RDBF
	POP	BC
	POP	DE
	RET

	;### SET SUBNET MASK ### (void setSUBR(uint8 * addr))
	;PI:	HL - HAddress MASK (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_SLMA:	PUSH	DE
	LD	DE,N_SUB0	;Subnet-Mask Addressregister 0
	JR	SLIPIN

	;### GET SUBNET MASK ### (void getSUBR(uint8 * addr))
	;PI:	HL - HAddress MASK (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_GLMA:	PUSH	DE
	LD	DE,N_SUB0	;Subnet-Mask Addressregister 0
	JR	GLIPIN

	;### SET GATEWAY ### (void setGAR(uint8 * addr))
	;PI:	HL - HAddress GATEWAY (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_SGTW:	PUSH	DE
	LD	DE,N_GAW0	;Gateway-IP Addressregister 0
	JR	SLIPIN

	;### GET GATEWAY ### (void getGAR(uint8 * addr))
	;PI:	HL - HAddress GATEWAY (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_GGTW:	PUSH	DE
	LD	DE,N_GAW0	;Gateway-IP Addressregister 0
	JR	GLIPIN

	;### GET LOCAL MAC ### (void getSHAR(uint8 * addr))
	;PI:	HL - HAddress MAC (6 Byte NOrder)
	;PO:	HL - HAddress + 6
	;AR:	HL
N_GLMC:	PUSH	DE
	LD	DE,N_MAC0	;Source-MAC Addressregister 0
	PUSH	BC
	LD	BC,6
	JR	GLIPIC

	;### GET UNREACHABLE IP ###
	;PI:	HL - HAddress IP (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_GUIP:	PUSH	DE
	LD	DE,N_UIP0	;Unreachable IP-Addressreg.0 UDP Mode
	JR	GLIPIN

	;### GET UNREACHABLE PORT ###
	;PI:	HL - HAddress Port (2 Byte NOrder)
	;PO:	HL - HAddress + 2
	;AR:	HL
N_GUPT:	PUSH	DE
	LD	DE,N_UPT0	;Unreachable Port-Addressreg.0 UDP Mode
	PUSH	BC
	LD	BC,2
	JR	GLIPIC

;--- Functions for 'SOCKET REGISTERS' -----------------
;( only for STANDARD CONFIGURATION with 4*2048 Byte ! )

	;### SET SOCKET MODE ###
	;PI:	A - Socket
	;	E - Socket-Mode
	;AR:	-
N_SSMD:	PUSH	HL
	LD	HL,N_SMR		;Socket Mode Register Socket 0
	JR	SIPPIN

	;### GET SOCKET MODE ###
	;PI:	A - Socket
	;PO:	E - Socket-Mode
	;AR:	E
N_GSMD:	PUSH	HL
	LD	HL,N_SMR		;Socket Mode Register Socket 0
	JR	GSSTIN

	;### SET SOCKET COMMAND ###
	;PI:	A - Socket
	;	E - Socket-Command
	;AR:	-
N_SSCD:	PUSH	HL
	LD	HL,N_SCR		;Socket Command Register Socket 0
	JR	SIPPIN

	;### GET SOCKET STATE ### (getSn_SR(s) (IINCHIP_READ(Sn_SR(s))))
	;PI:	A - Socket
	;PO:	E - Socket-State
	;AR:	E
N_GSST:	PUSH	HL
	LD	HL,N_SSR		;State Register Socket 0
GSSTIN:	PUSH	AF
	ADD	A,H
	LD	H,A
GSSTIC:	CALL	N_RDBT
	LD	E,A		;State
	POP	AF
	POP	HL
	RET

	;### SET SOCKET INTERRUPT STATE ### (#define setSn_IR(s, val) (IINCHIP_WRITE(Sn_IR(s),val)))
	;PI:	A - Socket
	;	E - Socket-Interrupt-State (clear Bits by writing '1'!)
	;AR:	-
N_SSIS:	PUSH	HL
	LD	HL,N_SIR		;Socket Interrupt Register Socket 0
	JR	SIPPIN

	;### GET SOCKET INTERRUPT STATE ### (#define getSn_IR(s) (IINCHIP_READ(Sn_IR(s))))
	;PI:	A - Socket
	;PO:	E - Socket-Interrupt-State
	;AR:	E
N_GSIS:	PUSH	HL
	LD	HL,N_SIR		;Socket Interrupt Register Socket 0
	JR	GSSTIN

	;### SET IP-PROTOCOL IN IP-RAW-Mode ### (setSn_PROTO(s, proto) IINCHIP_WRITE(Sn_PROTO(s),proto))
	;PI:	A - Socket
	;	E - Protocol-Number
	;AR:	-
N_SIPP:	PUSH	HL
	LD	HL,N_SPRT	;IP Protocol Register Socket 0
SIPPIN:PUSH	AF
	ADD	A,H
	LD	H,A
	LD	A,E		;Mode/Command/IStat/Protocol/TOS/TTL
	CALL	N_WRBT
	POP	AF
	POP	HL
	RET

	;### SET TOS IN IP-RAW-Mode ###
	;PI:	A - Socket
	;	E - TypeOfService
	;AR:	-
N_SSTOS:PUSH	HL
	LD	HL,N_STOS		;Socket TOS-Register (type of service) Socket 0
	JR	SIPPIN

	;### SET TTL IN IP-RAW-Mode ### (#define setSn_TTL(s, ttl) IINCHIP_WRITE(Sn_TTL(s), ttl))
	;PI:	A - Socket
	;	E - TimeToLive
	;AR:	-
N_SSTTL:PUSH	HL
	LD	HL,N_STTL		;Socket TTL-Register (time to live) Socket 0
	JR	SIPPIN

	;### SET SOCKET SOURCE PORT ###
	;PI:	A - Socket
	;	HL - HAddress Source-Port (2 Byte NOrder)
	;PO:	HL - HAddress + 2
	;AR:	HL
N_SSSPT:PUSH	DE
	LD	DE,N_SSPT	;Socket0 Source Port Register 0
	JR	SSDPTI

	;### GET SOCKET SOURCE PORT ###
	;PI:	A - Socket
	;	HL - HAddress Source-Port (2 Byte NOrder)
	;PO:	HL - HAddress + 2
	;AR:	HL
N_GSSPT:PUSH	DE
	LD	DE,N_SSPT	;Socket0 Source Port Register 0
	JR	GSDPTI

	;### SET SOCKET DESTINATION PORT ### (void setSn_DPORT(SOCKET s, uint8 * addr))
	;PI:	A - Socket
	;	HL - HAddress Destination-Port (2 Byte NOrder)
	;PO:	HL - HAddress + 2
	;AR:	HL
N_SSDPT:PUSH	DE
	LD	DE,N_SDPT	;Destination Port Register 0 Socket 0
SSDPTI:	PUSH	BC
	LD	BC,2
	JR	SSDMCI	
	
	;### GET SOCKET DESTINATION PORT ### (void getSn_DPORT(SOCKET s, uint8 * addr))
	;PI:	A - Socket
	;	HL - HAddress Destination-Port (2 Byte NOrder)
	;PO:	HL - HAddress + 2
	;AR:	HL
N_GSDPT:PUSH	DE
	LD	DE,N_SDPT	;Destination Port Register 0 Socket 0
GSDPTI:	PUSH	BC
	LD	BC,2
	JR	GSDMCI

	;### SET SOCKET MAX SEGMENT SIZE ### (void setSn_MSS(SOCKET s, uint16 Sn_MSSR0))
	;PI:	A - Socket
	;	HL - HAddress max. Segment-Size (2 Byte NOrder)
	;PO:	HL - HAddress + 2
	;AR:	HL
N_SSMSS:PUSH	DE
	LD	DE,N_SMSS	;Socket Maximum Segment Size Reg. 0 Socket 0
	JR	SSDPTI

	;### SET SOCKET DESTINATION IP ### (void setSn_DIPR(SOCKET s, uint8 * addr))
	;PI:	A - Socket
	;	HL - HAddress IP (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_SSDIP:PUSH	DE
	PUSH	BC
	LD	DE,N_SDIP	;Destination IP Register 0 Socket 0
	LD	BC,4
	JR	SSDMCI

	;### GET SOCKET DESTINATION IP ### (void getSn_DIPR(SOCKET s, uint8 * addr))
	;PI:	A - Socket
	;	HL - HAddress IP (4 Byte NOrder)
	;PO:	HL - HAddress + 4
	;AR:	HL
N_GSDIP:PUSH	DE
	PUSH	BC
	LD	DE,N_SDIP	;Destination IP Register 0 Socket 0
	LD	BC,4
	JR	GSDMCI

	;### SET SOCKET DESTINATION MAC ### (void setSn_DHAR(SOCKET s, uint8 * addr))
	;PI:	A - Socket
	;	HL - HAddress MAC (6 Byte NOrder)
	;PO:	HL - HAddress + 6
	;AR:	HL
N_SSDMC:PUSH	DE
	PUSH	BC
	LD	DE,N_SDMC	;Destination MAC Register 0 Socket 0
	LD	BC,6
SSDMCI:	PUSH	AF
	ADD	A,D		;+ Socket-Offset
	LD	D,A		;= Register Socket
	CALL	N_WRBF
	POP	AF
	POP	BC
	POP	DE
	RET

	;### GET SOCKET DESTINATION MAC ### (void getSn_DHAR(SOCKET s, uint8 * addr))
	;PI:	A - Socket
	;	HL - HAddress MAC (6 Byte NOrder)
	;PO:	HL - HAddress + 6
	;AR:	HL
N_GSDMC:PUSH	DE
	PUSH	BC
	LD	DE,N_SDMC	;Destination MAC Register 0 Socket 0
	LD	BC,6
GSDMCI:	PUSH	AF
	ADD	A,D		;+ Socket-Offset
	LD	D,A		;= Register Socket
	CALL	N_RDBF
	POP	AF
	POP	BC
	POP	DE
	RET

	;### GET TXMAX SIZE ### (#define getIINCHIP_TxMAX(s) (SSIZE[s]))
	;PI:	A - Socket
	;PO:	HL - TXMAX SIZE
	;AR:	HL
N_GTXX:	LD	HL,N_TXSZ
	RET
	;### GET RXMAX SIZE ### (#define getIINCHIP_RxMAX(s) (RSIZE[s]))
	;PI:	A - Socket
	;PO:	HL - RXMAX SIZE
	;AR:	HL
N_GRXX:	LD	HL,N_RXSZ
	RET
	;### GET TXMASK ### (#define getIINCHIP_TxMASK(s) SMASK[s]))
	;PI:	A - Socket
	;PO:	DE - TXMASK
	;AR:	DE
N_GTXM:	LD	DE,N_TXSZ-1
	RET
	;### GET RXMASK ### (#define getIINCHIP_RxMASK(s) RMASK[s]))
	;PI:	A - Socket
	;PO:	DE - RXMASK
	;AR:	DE
N_GRXM:	LD	DE,N_RXSZ-1
	RET
	;### GET TXBASE ### (#define getIINCHIP_TxBASE(s) SBUFBASEADDRESS[s]))
	;PI:	A - Socket
	;PO:	HL - TXBASE
	;AR:	HL
N_GTXB:	LD	HL,N_TXBF
	JR	GRXBIN
	;### GET RXBASE ### (#define getIINCHIP_RxBASE(s) RBUFBASEADDRESS[s]))
	;PI:	A - Socket
	;PO:	HL - RXBASE
	;AR:	HL
N_GRXB:	LD	HL,N_RXBF
GRXBIN:	PUSH	AF
	PUSH	DE
	ADD	A,A
	ADD	A,A
	ADD	A,A		;*8
	LD	D,A
	LD	E,0		;*256 = 2048 Byte
	ADD	HL,DE		;+ Base = Socket-BASE
	POP	DE
	POP	AF
	RET

	;### WRITE DATA ### (void write_data(SOCKET s, vuint8 * src, vuint8 * dst, uint16 len))
	;PI:	A - Socket
	;	HL - HAddress
	;	DE - NAddress
	;	BC - Length
	;PO:	A - Socket
	;	HL - HAddress+Length
	;AR:	F,HL,DE,BC
N_WRDT:	EXX
	PUSH	HL
	EXX
	PUSH	HL
	EX	DE,HL		;HL-vSoADR
	CALL	N_GTXM		;DE-SoMASK
	PUSH	AF
	LD	A,H
	AND	D
	LD	D,A
	LD	A,L
	AND	E
	LD	E,A		;DE-SoOffset
	POP	AF
	CALL	N_GTXB		;HL-SoBASEADR
	ADD	HL,DE		;HL-SoADR
	PUSH	HL
	LD	H,D
	LD	L,E
	ADD	HL,BC		;SoOffset+Length
	PUSH	BC
	LD	B,H
	LD	C,L
	CALL	N_GTXX		;HL-TXMAX SIZE
	OR	A
	SBC	HL,BC		;TXMAX - SoOffset+Length
	POP	BC		;Length
	POP	HL		;HL-SoADR
	JR	NC,WRDTOK	;OK: SoOffset+Length < TXMAX
	EXX
	POP	HL		;HAddress
	EXX
	PUSH	BC
	PUSH	HL
	CALL	N_GTXX
	OR	A
	SBC	HL,DE
	LD	B,H
	LD	C,L		;Length 1 = TXMAX SIZE - SoOffset
	POP	DE		;SoADR
	EXX
	PUSH	HL
	EXX
	POP	HL		;HAddress
	PUSH	BC
	CALL	N_WRBF		;WRITE BUFFER Length 1
	EX	DE,HL
	POP	BC		;Length 1
	POP	HL		;Length
	OR	A
	SBC	HL,BC
	LD	B,H
	LD	C,L		;Length 2 = Length - Length 1
	CALL	N_GTXB		;HL-SoBASEADR
	EX	DE,HL
	JR	WRDTCN		;WRITE BUFFER Length 2
WRDTOK:	EX	DE,HL		;DE-SoADR
	POP	HL		;HAddress
WRDTCN:	CALL	N_WRBF		;WRITE BUFFER
	EXX
	POP	HL
	EXX
	RET

	;### READ DATA ### (void read_data(SOCKET s, vuint8 * src, vuint8 * dst, uint16 len))
	;PI:	A - Socket
	;	HL - HAddress
	;	DE - NAddress
	;	BC - Length
	;PO:	A - Socket
	;	HL - HAddress+Length
	;AR:	F,HL,DE,BC
N_RDDT:	EXX
	PUSH	HL
	EXX
	PUSH	HL
	EX	DE,HL		;HL-vSoADR
	CALL	N_GRXM		;DE-SoMASK
	PUSH	AF
	LD	A,H
	AND	D
	LD	D,A
	LD	A,L
	AND	E
	LD	E,A		;DE-SoOffset
	POP	AF
	CALL	N_GRXB		;HL-SoBASEADR
	ADD	HL,DE		;HL-SoADR
	PUSH	HL
	LD	H,D
	LD	L,E
	ADD	HL,BC		;SoOffset+Length
	PUSH	BC
	LD	B,H
	LD	C,L
	CALL	N_GRXX		;HL-RXMAX SIZE
	OR	A
	SBC	HL,BC		;RXMAX - SoOffset+Length
	POP	BC		;Length
	POP	HL		;HL-SoADR
	JR	NC,RDDTOK	;OK: SoOffset+Length < RXMAX
	EXX
	POP	HL		;HAddress
	EXX
	PUSH	BC
	PUSH	HL
	CALL	N_GRXX
	OR	A
	SBC	HL,DE
	LD	B,H
	LD	C,L		;Length 1 = RXMAX SIZE - SoOffset
	POP	DE		;SoADR
	EXX
	PUSH	HL
	EXX
	POP	HL		;HAddress
	PUSH	BC
	CALL	N_RDBF		;READ BUFFER Length 1
	EX	DE,HL
	POP	BC		;Length 1
	POP	HL		;Length
	OR	A
	SBC	HL,BC
	LD	B,H
	LD	C,L		;Length 2 = Length - Length 1
	CALL	N_GRXB		;HL-SoBASEADR
	EX	DE,HL
	JR	RDDTCN		;READ BUFFER Length 2
RDDTOK:	EX	DE,HL		;DE-SoADR
	POP	HL		;HAddress
RDDTCN:	CALL	N_RDBF		;READ BUFFER
	EXX
	POP	HL
	EXX
	RET

	;### SEND DATA ### (void send_data_processing(SOCKET s, uint8 *data, uint16 len))
	;PI:	A - Socket
	;	HL - HAddress
	;	BC - Length
	;PO:	HL - HAddress+Length
	;AR:	HL
N_SDDT:	PUSH	DE
	PUSH	AF
	PUSH	HL
	LD	HL,N_STWP	;TX Write Pointer Socket 0
	ADD	A,H		;+ Socket-Offset
	LD	H,A		;= TX Write Pointer Socket
	CALL	N_RDBT		;MSB
	LD	D,A
	INC	HL
	CALL	N_RDBT		;LSB
	LD	E,A		;DE = virtual Write Address N_TXBF
	POP	HL
	POP	AF	
	PUSH	DE
	PUSH	BC
	CALL	N_WRDT		;Hostdata -> N_TXBF(Socket)
	EX	DE,HL
	POP	BC
	POP	HL
	ADD	HL,BC
	EX	DE,HL		;DE: N_TXBF = NAddress+Length
	PUSH	AF
	PUSH	HL
	LD	HL,N_STWP
	ADD	A,H
	LD	H,A
	LD	A,D		;MSB
	CALL	N_WRBT
	INC	HL
	LD	A,E		;LSB
	CALL	N_WRBT		;update Write Address N_TXBF 
	POP	HL
	POP	AF
	POP	DE
	RET

	;### RECEIVE DATA ### (void recv_data_processing(SOCKET s, uint8 *data, uint16 len))
	;PI:	A - Socket
	;	HL - HAddress
	;	BC - Length
	;PO:	HL - HAddress+Length
	;AR:	HL
N_RCDT:	PUSH	DE
	PUSH	AF
	PUSH	HL
	LD	HL,N_SRRP	;RX Read Pointer Socket 0
	ADD	A,H		;+ Socket-Offset
	LD	H,A		;= RX Read Pointer Socket
	CALL	N_RDBT		;MSB
	LD	D,A
	INC	HL
	CALL	N_RDBT		;LSB
	LD	E,A		;DE = virtual Read Address N_RXBF
	POP	HL
	POP	AF	
	PUSH	DE
	PUSH	BC
	CALL	N_RDDT		;N_RXBF(Socket) -> Hostdata
	EX	DE,HL
	POP	BC
	POP	HL
	ADD	HL,BC
	EX	DE,HL		;DE: N_RXBF = NAddress+Length
	PUSH	AF
	PUSH	HL
	LD	HL,N_SRRP
	ADD	A,H
	LD	H,A
	LD	A,D		;MSB
	CALL	N_WRBT
	INC	HL
	LD	A,E		;LSB
	CALL	N_WRBT		;update Read Address N_RXBF
	POP	HL
	POP	AF
	POP	DE
	RET

	;### GET FREE SEND DATA SIZE ### (uint16 getSn_TX_FSR(SOCKET s))
	;PI:	A - Socket
	;PO:	BC - Length HOrder
	;AR:	BC
N_GSDSZ:PUSH	DE
	LD	DE,N_FSTX	;FREEMEM TX Register 0 Socket 0
GSDSZI:	PUSH	HL
	PUSH	AF
	ADD	A,D		;+ Socket-Offset
	LD	D,A		;= Register Socket
	POP	AF
GSDSZW:	CALL	GSDSZU		;1. Value
	LD	HL,0
	OR	A
	SBC	HL,BC
	JR	Z,GSDSZE	;=0 -> END
	PUSH	BC
	CALL	GSDSZU		;2. Value
	POP	HL
	OR	A
	SBC	HL,BC
	JR	NZ,GSDSZW	;1.<>2. -> Repeat
GSDSZE:	POP	HL
	POP	DE
	RET
GSDSZU:	PUSH	AF
	EX	DE,HL		;get address
	CALL	N_RDBT		;MSB
	LD	B,A
	INC	HL
	CALL	N_RDBT		;LSB
	LD	C,A		;BC = Length HOrder
	DEC	HL
	EX	DE,HL		;save address
	POP	AF
	RET

	;### GET RECEIVED DATA SIZE ### (uint16 getSn_RX_RSR(SOCKET s))
	;PI:	A - Socket
	;PO:	BC - Length HOrder
	;AR:	BC
N_GRCSZ:PUSH	DE
	LD	DE,N_RSRX	;RECEIVED SIZE RX Reg. 0 Socket 0
	JR	GSDSZI

;#######################################################


;#######################################################
;### Socket Driver : HIGH LEVEL Interface ##############
;#######################################################

;--- Functions for INIT and STATE --------------------

	;### SOCKET (TCP, UDP, IP, MAC) ### (uint8 socket(SOCKET s, uint8 protocol, uint8 flag))
	; - reserves Socket for a calling program
	; - defines Socket-Protocol
	; - Socket is released after CLOSE only!
	;
	;PI:      A - wished Socket-Number
	;		  = FFH any
	;		  = 0...(MAXSCK-1) explicit this Socket-Number
	;	  D - Socket-Protocol (CONSTANTS SOCKET-Interface: SOCKET PROTOCOL)
	;
	;TCP/UDP: E - Socket-FLAGS (CONSTANTS SOCKET-Interface: SOCKET FLAGS)
	;     IP: E - IP-Protocol (CONSTANTS SOCKET-Interface: IP PROTOCOL)
	;    MAC: E - not used
	;
	;PO:	  A - reserved Socket-Number until CLOSE - only if CY=0 !!!
	;      CY=1 - Error (no Socket free, Socket-Mode invalid, MAC Socket-Number not 0)
	;
	;AR:	AF,BC,DE
SOCKET:	LD	C,E		;Flags/IP-Protocol
	INC	A
	JR	Z,SOCKE1	;any Number 
	DEC	A
	CP	MAXSCK
	LD	B,1		;only 1 Number 
	JR	C,SOCKE3	;OK->test
	SCF			;Number invalid
	RET
SOCKE1:	LD	B,MAXSCK
	LD	A,B
SOCKE2:	DEC	A
SOCKE3:	CALL	N_GSMD
	INC	E
	DEC	E
	JR	Z,SOCKE5	;free
	DJNZ	SOCKE2
SOCKE4:	SCF			;Socket(s) not free!
	RET
SOCKE5:	LD	B,A		;Socket Number
	LD	A,D
	CP	SK_STREAM
	JR	Z,SOCKE6
	CP	SK_DGRAM
	JR	NZ,SOCKE7
SOCKE6:	LD	A,C		;TCP/UDP Socket-Flags
	AND	SO_NDACK OR SO_MULTI
	OR	D
	LD	D,A		;Mode+Flags
	JR	SOCKE9
SOCKE7:	CP	SK_IPLRAW
	JR	NZ,SOCKE8
	LD	A,B
	LD	E,C
	CALL	N_SIPP		;set IP-Protocol
	JR	SOCKE9
SOCKE8:	CP	SK_MACLRAW
	JR	NZ,SOCKE4	;Mode invalid
	XOR	A
	OR	B
	JR	NZ,SOCKE4	;only Socket 0 allowed !
SOCKE9:	LD	E,D		;Mode(+Flags)
	LD	A,B		;Socket Number
	CALL	N_SSMD		;reserve Socket
	OR	A		;OK
	RET

	;### BIND (TCP, UDP) ### (uint8 socket(SOCKET s, uint8 * addr, uint16 port))
	; - binds Socket to a given local Port
	; - it must be called before CONNECT!
	;
	;PI:	 A - Socket
	;	HL - HAddress local Port (2 Byte Port NOrder)
	;
	;PO:  CY=1 - Error (in Socket or Port)
	;
	;AR:	F,HL
BIND:	PUSH	DE
	CALL	N_GSMD
	LD	D,A
	LD	A,E
	AND	7
	CP	SMR_TCP
	JR	Z,BIND1
	CP	SMR_UDP
	LD	A,D
	JP	NZ,SCMDER	;invalid Protocol
BIND1:	LD	A,D
	CALL	N_GSST
	INC	E
	DEC	E
	JP	NZ,SCMDER	;State old invalid
	LD	A,(HL)
	INC	HL
	OR	(HL)
	DEC	HL		;restore
	LD	A,D
	JP	Z,SCMDER	;ERR Port=0!
	CALL	N_SSSPT
	JP	SCMDOK

	;### SELECT (TCP, UDP, IP, MAC) ### (uint16 select(SOCKET s, uint8 func))
	; - delivers socket-informationen
	;
	;PI:	 A - Socket
	;	 E - Info-Constant (CONSTANTS SOCKET-Interface: SOCKET SELECT FUNCTION)
	;
	;PO:	 E - State (CONSTANTS TCPIP-Stack: Sn_SR values (socket state register))
	;	DE - DataSize HOrder
	;     CY=1 - DataSize=0
	;
	;AR:	F,DE
SELECT:	LD	D,A
	LD	A,E
	CP	SL_STAT
	JR	Z,SELEC1
	CP	SL_SEND
	JR	Z,SELEC2
	CP	SL_RECV
	JR	Z,SELEC3
	LD	A,D
	SCF
	CCF			;CY=0!
	RET	
SELEC1:	LD	A,D
	CALL	N_GSST
	RET
SELEC2:	LD	A,D
	PUSH	BC
	CALL	N_GSDSZ
	JR	SELEC4
SELEC3:	LD	A,D
	PUSH	BC
	CALL	N_GRCSZ
SELEC4:	LD	A,B
	OR	C
	SCF
	JR	Z,SELEC5
	CCF			;DataSize<>0
SELEC5:	LD	A,D
	LD	D,B
	LD	E,C
	POP	BC
	RET	
	
;--- FUNCTIONS FOR CONNECT ----------

	;### CONNECT (TCP activ, UDP, IP, MAC) ### (uint8 connect(SOCKET s, uint8 * addr, uint16 port))
	;- if the Socket is bound on a local Port (for example from BIND), it goes with this
	;  Port online (UDP and TCP mode)
	;- if the Socket is not bound on a local Port (Port=0),it goes with a itself-chosen 
	;  dynamic local Port online (UDP and TCP mode)
	;
	;	  TCP: - local TCP-Client connects to the given Peer-Server
	;
	;UDP, IP, MAC: - Socket can send/receive Data after CONNECT immediately
	;		 (Peer-Data is given in SEND TO / returned from RECEIVE FROM)
	;
	;PI:	        A - Socket
	;	  TCP: HL - HAddress of Peer-Data (4 Byte Peer-IP NOrder, 2 Byte Peer-Port NOrder)
	;UDP, IP, MAC: HL - not used
	;
	;PO:	 A - Socket
	;     CY=1 - Error (Error Socket mode or states, TCP: Error Peer-Data or Peer unreachable)
	;
	;AR:	F,HL
CONNECT:PUSH	DE
	CALL	N_GSMD
	LD	D,A
	LD	A,E
	AND	7
	LD	E,A		;current mode
	CP	SMR_TCP
	JR	Z,CONNE1
	CP	SMR_UDP
	JR	Z,CONNE1
	CP	SMR_IPRAW
	JR	Z,CONNE1
	CP	SMR_MACRAW
	LD	A,D
	JP	NZ,SCMDER	;invalid mode
CONNE1:	LD	A,D
	LD	D,E		;store mode
	CALL	N_GSST
	INC	E
	DEC	E
	JP	NZ,SCMDER	;current State not closed!
	PUSH	DE		;save mode
	LD	E,A	
	LD	A,D
	CP	SMR_IPRAW
	LD	A,E
	JR	Z,CONNE3	;skip Porttest
	LD	A,D
	CP	SMR_MACRAW
	LD	A,E
	JR	Z,CONNE3	;skip Porttest
	CALL	CONNE7
	INC	E
	DEC	E
	JR	NZ,CONNE3	;local Port<>0
	;Socket not bound (Port=0)
	PUSH	HL
	PUSH	AF
	CALL	N_DPRT		;dynamic Port Norder!
	EX	DE,HL
	LD	HL,N_SSPT	;Socket0 Source Port Reg. 0
	ADD	A,H
	LD	H,A
	LD	A,E		;MSB NOrder
	CALL	N_WRBT	
	INC	HL	
	LD	A,D		;LSB NOrder
	CALL	N_WRBT	
	POP	AF
	POP	HL
CONNE3:	;OPEN ...
	LD	E,SCR_OPEN
	CALL	N_SSCD		;Command
	POP	DE
	LD	E,A
	LD	A,D
	CP	SMR_TCP
	JR	Z,CONNE4
	; ... UDP/IP/MAC
	LD	L,S_UDP		;state after Open UDP
	CP	SMR_UDP
	JR	Z,CONNE9
	LD	L,S_IPRAW	;state after Open IP
	CP	SMR_IPRAW
	JR	Z,CONNE9
	LD	L,S_MACRAW	;state after Open MAC
CONNE9:	LD	A,E
	CALL	N_GSST
	LD	D,A
	LD	A,L
	CP	E
	LD	A,D
	JP	NZ,SCMDER	;state new invalid
	JP	SCMDOK		;UDP/IP/MAC online
CONNE4:	; ... TCP
	LD	A,E
	CALL	N_GSST
	LD	D,A
	LD	A,S_INIT	;state after Open TCP
	CP	E
	LD	A,D
	JP	NZ,SCMDER	;state new invalid
	PUSH	AF
	PUSH	HL		;Peer Data
	PUSH	HL
	LD	A,0FFH
	LD	D,4
	CALL	ATEST
	POP	HL		;Peer Data
	JR	Z,CONNE6	;ERR Peer-IP=4*255
	XOR	A
	LD	D,4
	CALL	ATEST
	JR	Z,CONNE6	;ERR Peer-IP=4*000
	LD	D,2
	CALL	ATEST
	JR	Z,CONNE6	;ERR Peer-Port=0
	POP	HL
	POP	AF
	CALL	N_SSDIP		;SET Peer-IP
	CALL	N_SSDPT		;SET Peer-Port
	LD	E,SCR_CONNECT
	CALL	N_SSCD		;now connect to Peer
CONNE5:	CALL	N_GSST
	LD	D,A
	LD	A,S_ESTABLISHED	;state after TCP connect SUCCESS
	CP	E
	LD	A,D
	JR	Z,SCMDOK	;connected
	LD	A,S_CLOSED	;state after TCP connect FAIL
	CP	E
	LD	A,D
	JR	NZ,CONNE5	;wait
	JR	SCMDER		;timeout Peer
CONNE6:	POP	HL
	POP	AF
	JR	SCMDER		;Error Peer-Data
CONNE7:	PUSH	HL
	PUSH	AF
	LD	HL,N_SSPT	;Socket0 Source Port Reg. 0
	ADD	A,H
	LD	H,A
	CALL	N_RDBT
	OR	A
	LD	E,A		;MSB
	JR	NZ,CONNE8	;<>0
	INC	HL	
	CALL	N_RDBT
	LD	E,A		;LSB
CONNE8:	POP	AF
	POP	HL
	RET

	;### LISTEN (TCP passive) ### (uint8 listen(SOCKET s))
	; - local TCP-Server waits for connection-request from a Peer-Client
	; - before calling LISTEN the local Serverport is to be set with BIND !
	; 
	;PI:	 A - Socket
	;
	;PO:	 A - Socket
	;     CY=1 - Error
	;
	;AR:	F
LISTEN:	PUSH	DE
	CALL	N_GSMD
	LD	D,A
	LD	A,E
	AND	7
	CP	SMR_TCP
	LD	A,D
	JR	NZ,SCMDER	;invalid Protocol
	CALL	N_GSST
	INC	E
	DEC	E
	JR	NZ,SCMDER	;State old invalid
	CALL	CONNE7
	INC	E
	DEC	E
	JR	Z,SCMDER	;local Port=0
	LD	E,SCR_OPEN
	CALL	N_SSCD		;Command
	CALL	N_GSST
	LD	A,S_INIT
	CP	E
	LD	A,D
	JR	NZ,SCMDER	;State new invalid
	LD	E,SCR_LISTEN
	CALL	N_SSCD		;Command
	CALL	N_GSST
	LD	A,S_LISTEN
	CP	E
	LD	A,D
	JR	NZ,SCMDER	;State new invalid
SCMDOK:	POP	DE
	OR	A		;OK
	RET
SCMDER:	POP	DE
	SCF			;ERR
	RET

	;### ACCEPT (TCP passive) ###
	; - local TCP-Server accepts a wished connection request from a Peer-Client
	; - function returns the IP-Address of the Peer-Client
	;
	;PI:	 A - Socket, which waits for Peer-Client
	;	HL - HAddress for storage of the returned Peer-IP (4 Byte Peer-IP NOrder)
	;
	;PO:	 A - Socket, which waits for Peer-Client
	;	HL - Peer-IP after successful (CY=0) ACCEPT (4 Byte Peer-IP NOrder)
	;     CY=1 - no Peer-Client connection request or Error
	;
	;AR:	F
ACCEPT:	PUSH	DE
	CALL	N_GSST
	LD	D,A
	LD	A,S_ESTABLISHED
	CP	E
	LD	A,D
	JR	NZ,SCMDER	;State invalid
	PUSH	HL
	CALL	N_GSDIP		;get Peer-IP (Port is not available!)
	POP	HL
	JR	SCMDOK

;--- Functions for FINISH and CLOSE -------------

	;### SHUTDOWN (TCP, UDP, IP, MAC) ###
	;
	;TCP/UDP-Port: Socket-PORT is always reset to 0! - this makes REBIND possible
	;
	;	  TCP: active disconnecting of a connection through the Host
	;UDP, IP, MAC: Socket becomes inactive - remains reserved, however!
	;
	;PI:	 A - Socket
	;
	;PO:	 A - Socket
	;     CY=1 - Error
	;
	;AR:	F
SHUTDN:	PUSH	DE
	CALL	N_GSST
	LD	D,A
	LD	A,S_ESTABLISHED
	CP	E
	LD	E,SCR_DISCON	;if TCP aktiv
	JR	Z,SHUTD1
	LD	E,SCR_CLOSE	;all other
SHUTD1:	LD	A,D
	CALL	N_SSCD		;Command
	CALL	N_GSST
	INC	E
	DEC	E
	JR	NZ,SCMDER	;state new invalid
	CALL	CLPORT
	JR	SCMDOK
	;
CLPORT:	PUSH	AF		;clear local Port
	PUSH	HL
	LD	HL,N_SSPT	;Socket0 Source Port Reg. 0
	ADD	A,H
	LD	H,A
	XOR	A
	CALL	N_WRBT		;MSB NOrder
	INC	HL	
	XOR	A
	CALL	N_WRBT		;LSB NOrder
	POP	HL
	POP	AF
	RET
	
	;### CLOSE (TCP, UDP, IP, MAC) ### (void close(SOCKET s))
	; - a possibly existing connection is actively closed
	; - the reserved Socket-Number, got from function SOCKET, declines
	; - Socket is closed and released for other programs again
	;
	;PI:	A - Socket
	;
	;AR:	F
CLOSE:	PUSH	DE
	LD	E,SCR_CLOSE
	CALL	N_SSCD		;Command
	LD	E,SMR_CLOSE
	CALL	N_SSMD		;release Socket
	CALL	CLPORT
	JR	SCMDOK

;--- Functions for DATA-TRANSFER ------------------
	
	;### SEND (TCP) ### (uint16 send(SOCKET s, const uint8 * buf, uint16 len))
	;
	;PI:	 A - Socket
	;	HL - HAddress Data
	;	BC - to sending Data length in HOrder
	;
	;PO:	 A - Socket
	;	HL - HAddress + sent Data length
	;	BC - sent Data length in HOrder
	;     CY=1 - Error
	;
	;AR:	F,HL,DE,BC
SEND:	LD	D,A
	LD	A,B
	OR	C
	LD	A,D
	SCF
	RET	Z		;Length=0
	PUSH	HL
	CALL	N_GTXX		;get max. Size
	PUSH	HL
	OR	A
	SBC	HL,BC
	POP	HL
	JR	NC,SEND1	;SendSize <= MaxSize
	LD	B,H
	LD	C,L		;new SendSize = MaxSize
SEND1:	LD	D,B
	LD	E,C		;DE: SendSize
SEND2:	CALL	N_GSDSZ		;get FreeSize
	LD	H,B
	LD	L,C
	OR	A
	SBC	HL,DE
	JR	NC,SEND5
	PUSH	DE
	LD	D,A
	CALL	N_GSST
	LD	A,S_ESTABLISHED
	CP	E
	JR	Z,SEND3
	LD	A,S_CLSWAIT
	CP	E
SEND3:	LD	A,D
	POP	DE
	JR	Z,SEND2		;wait for Freesize >= SendSize
	POP	HL		;offline
SEND4:	LD	BC,0
	SCF			;Error
	RET
SEND5:	LD	B,D
	LD	C,E		;SendSize
	POP	HL		;HAddress
	CALL	N_SDDT		;Write Data (HL+(BC))
	LD	E,SCR_SEND
	CALL	N_SSCD		;Send Data
SEND6:	CALL	N_GSIS		;SoINT-State
	LD	D,A
	LD	A,SIR_SENDOK
	AND	E
	JR	NZ,SEND9
	LD	A,D
	CALL	N_GSST		;SoSTAT
	LD	A,S_CLOSED
	CP	E
	LD	A,D
	JR	NZ,SEND6
	;Socket closed 
SEND7:	CALL	N_GSIS
SEND8:	LD	A,SIR_SENDOK OR SIR_TIMEOUT OR SIR_RECV OR SIR_DISCON OR SIR_CON
	CALL	SENDU
	JR	SEND4
SEND9:	;Sent OK
	LD	A,SIR_SENDOK
SENDU:	OR	E		;clear with '1'
	LD	E,A
	LD	A,D
	CALL	N_SSIS
	OR	A		;OK
	RET

	;### RECEIVE (TCP) ### (uint16 recv(SOCKET s, uint8 * buf, uint16 len))
	;
	;PI:	 A - Socket
	;	HL - HAddress Data
	;	BC - max. Data length for RECEIVE in HOrder
	;
	;PO:	 A - Socket
	;	HL - HAddress + received Data length
	;	BC - received Data length in HOrder
	;     CY=1 - no Data received
	;
	;AR:	F,HL,DE,BC
RECV:	LD	D,B
	LD	E,C
	PUSH	DE
	CALL	N_GRCSZ		;RecvSize (BC)
	LD	D,A
	LD	A,B
	OR	C		;BC=0 ...
	LD	A,D
	POP	DE		;MaxSize
	SCF
	RET	Z		;-> no Data
	PUSH	HL
	LD	H,D
	LD	L,E
	OR	A
	SBC	HL,BC		;MaxSize - RecSize
	POP	HL
	JR	NC,RECV1	;OK
	LD	B,D
	LD	C,E		;new RecSize = MaxSize
RECV1:	CALL	N_RCDT		;Read Data (HL+(BC))
	OR	A		;OK
RECV2:	LD	E,SCR_RECV
	CALL	N_SSCD		;Receive Data
	RET

	;### SEND TO (UDP, IP, MAC) ###
	;(uint16 sendto(SOCKET s, const uint8 * buf, uint16 len, uint8 * addr, uint16 port))
	;
	;PI:	 A - Socket
	;	HL - HAddress Data
	;	DE - HAddress Peer-Data
	;		UDP: 4 Byte Peer-IP NOrder, 2 Byte Peer-Port NOrder
	;		 IP: 4 Byte Peer-IP NOrder
	;		MAC: -
	;	BC - to sending Data length in HOrder (max. Length = MAXMTU = 1480)
	;	     (Remark: the Data length of the receiver is smaller, since there are
	;	      still header Data in front - UDP-1472 Byte/IP-1474 Byte/MAC-1478 Byte)
	;
	;PO:	 A - Socket
	;	HL - HAddress + sent Data length
	;	BC - sent Data length in HOrder
	;     CY=1 - Error
	;
	;AR:	F,HL,DE,BC
SENDTO:	PUSH	HL
	CALL	N_GTXX		;get MaxSize
	PUSH	HL
	OR	A
	SBC	HL,BC
	POP	HL
	JR	NC,SENDT1	;SendSize <= MaxSize
	LD	B,H
	LD	C,L		;new SendSize = MaxSize
SENDT1:	PUSH	DE		;Peer-Data
	PUSH	AF
	LD	A,B
	OR	C
	JR	Z,SENDT2	;Length=0
	EX	DE,HL		;Peer-Data->HL
	POP	AF
	PUSH	AF
	CALL	N_GSMD
	LD	A,E
	AND	7
	LD	D,4		;for IP-test
	CP	SMR_UDP
	JR	Z,SENDTU
	CP	SMR_IPRAW
	JR	Z,SENDTI
	CP	SMR_MACRAW
	JR	NZ,SENDT2	;unknown Mode
	POP	AF
	POP	DE
	JR	SENDT4
SENDTI:	XOR	A
	CALL	ATEST		;D=0
	JR	Z,SENDT2	;Peer-IP=0
	JR	SENDT3
SENDTU:	XOR	A
	CALL	ATEST
	JR	Z,SENDT2	;Peer-IP=0
	XOR	A
	LD	D,2
	CALL	ATEST
	LD	D,1		;for N_SSDPT
	JR	NZ,SENDT3	;Peer-Port<>0
SENDT2:	POP	AF
	POP	DE
	POP	HL
	LD	BC,0
	SCF			;Error
	RET
SENDT3:	POP	AF
	POP	HL		;Peer Data
	CALL	N_SSDIP		;set Peer-IP
	DEC	D
	CALL	Z,N_SSDPT	;set UDP Peer-Port
SENDT4:	POP	HL		;HAddress
	CALL	N_SDDT		;Write Data (HL+(BC))
	LD	E,SCR_SEND
	CALL	N_SSCD		;send Data
SENDT5:	CALL	N_GSIS		;Socket INT-State
	LD	D,A
	LD	A,SIR_SENDOK
	AND	E
	JP	NZ,SEND9	;OK
	LD	A,SIR_TIMEOUT
	AND	E
	JP	NZ,SEND8	;Timeout
	LD	A,D
	CALL	N_GSST		;Socket State
	LD	A,S_CLOSED
	CP	E
	LD	A,D
	JR	NZ,SENDT5
	JP	SEND7		;closed
	;check A with D*(HL)	
ATEST:	LD	E,D
ATEST1:	CP	(HL)
	JR	NZ,ATEST2
	DEC	E		
ATEST2:	INC	HL
	DEC	D
	JR	NZ,ATEST1
	XOR	A
	OR	E		;Z=1: same
	RET
	
	;### RECEIVE FROM (UDP, IP, MAC) ###
	;(uint16 recvfrom(SOCKET s, uint8 * buf, uint16 len, uint8 * addr, uint16 *port))
	;
	;PI:	 A - Socket
	;	HL - HAddress for storage of Data
	;	DE - HAddress for storage of Header Data:
	;		UDP - 8 Byte (4:Peer-IP, 2:Peer-Port, 2:Data Size - all in NOrder!)
	;		IP  - 6 Byte (4:Peer-IP, 2:Data Size - all in NOrder!)
	;		MAC - 2 Byte (2:Data Size in NOrder!)
	;	BC - max. Data length for RECEIVE FROM in HOrder without Header Data 
	;	     (the highest possible lengths are UDP-1472/IP-1474/MAC-1478 Byte,
	;	      this corresponds to the max. Data length of one received packet)
	;
	;PO:	 A - Socket
	;	HL - HAddress + received Data length
	;	BC - received Data length in HOrder
	;	     (UDP: if the Data length of an UDP packet is bigger than the
	;		   max. Data length, BC returns with 0 and CY is set, because 
	;		   fragmented Packets cannot be received by the TCPIP-Stack!)
	;     CY=1 - no Data received
	;
	;AR:	F,HL,DE,BC
RECVFR:	EXX			;'
	PUSH	BC		;'
	PUSH	AF
	CALL	N_GRCSZ
	LD	A,B
	OR	C		;BC=0 ...
	JR	NZ,RCVF2
	POP	AF
RCVF1:	POP	BC
	EXX			;/'
	LD	BC,0
	SCF			;-> no Data
	RET
RCVF2:	POP	AF
	PUSH	DE		;'
	CALL	N_GSMD
	LD	D,A		;Socket Number
	LD	A,E
	AND	7
	LD	BC,RH_MAC
	CP	SMR_MACRAW
	JR	Z,RCVF4
	LD	BC,RH_IPR
	CP	SMR_IPRAW
	JR	Z,RCVF4
	LD	BC,RH_UDP
	CP	SMR_UDP
	JR	Z,RCVF4
	LD	A,D
RCVF3:	CALL	RECV2		;Type ???: quit Receive only!
	POP	DE
	JR	RCVF1
RCVF4:	PUSH	HL		;'
	PUSH	DE		;for AF!
	PUSH	AF		;Socket Type
	LD	A,D
	PUSH	AF		;Socket Number
	LD	HL,N_SRRP	;RX Read Pointer Socket 0
	ADD	A,H		;+ Socket-Offset
	LD	H,A		;= RX Read Pointer Socket
	CALL	N_RDBT		;MSB
	LD	D,A
	INC	HL
	CALL	N_RDBT		;LSB
	LD	E,A		;DE: NAddress
	EXX			;/'
	PUSH	DE
	EXX			;'
	POP	HL		;HL: HAddress Header
	POP	AF		; A: Socket Number
	PUSH	DE		;DE: NAddress
	PUSH	BC		;BC: Headerlength
	CALL	N_RDDT		;read Header to HAddress
	EX	DE,HL		;DE: HAddress + Headerlength
	POP	BC
	POP	HL
	ADD	HL,BC		;NAddress = NAddress + Headerlength
	EX	DE,HL		;HL: HAddress + Headerlength / DE: NAddress
	DEC	HL
	LD	C,(HL)
	DEC	HL
	LD	B,(HL)		;BC: DataSize from Header
	POP	AF		;Socket Type
	CP	SMR_MACRAW
	JR	NZ,RCVF5
	DEC	BC
	DEC	BC		;MAC: DataSize = DataSize from Header - 2
RCVF5:	CP	SMR_UDP
	JR	NZ,RCVF6
	;check UDP-DataSize from Header
	LD	HL,MAXMTU-RH_UDP ;max. Data-Length UDP
	OR	A
	SBC	HL,BC		;max. Data-Length - DataSize from Header
	JR	NC,RCVF6
	;ERROR: PACKET > max. Data-Length UDP
	POP	AF		;Socket Number (from DE!)
	POP	HL		;'
	CALL	N_GRCSZ		;get Socket Receive-Size (BC)
	EX	DE,HL
	ADD	HL,BC		;NAddress = NAddress + Receive-Size
	EX	DE,HL	
	PUSH	DE
	EXX			;/'
	POP	DE
	LD	BC,0
	SCF			;for exit
	JR	RCVF9		;skip Read, set SRRPR, quit Receive
RCVF6:	POP	AF		;Socket Number (from DE!)
	POP	HL		;'
RCVF7:	PUSH	BC
	EXX			;/'
	POP	DE		;DataSize from Header
	PUSH	HL
	LD	H,D
	LD	L,E
	OR	A
	SBC	HL,BC		;DataSize from Header - max. Data length (ReadSize)
	JR	NC,RCVF8	;OK: DataSize from Header >= max. Data length
	LD	B,D
	LD	C,E		;-> otherwise ReadSize = DataSize from Header
RCVF8:	POP	HL
	EXX			;'
	PUSH	DE
	EXX			;/'
	POP	DE		;NAddress
	PUSH	BC		;ReadSize
	CALL	N_RDDT		;read Data
	POP	BC		;save for PO
	EXX			;'
	EX	DE,HL
	ADD	HL,BC		;NAddress = NAddress + DataSize from Header(always for Pointer!)
	EX	DE,HL
	PUSH	DE
	EXX			;/'
	POP	DE
	OR	A		;OK for exit
RCVF9:	PUSH	HL
	PUSH	AF
	LD	HL,N_SRRP
	ADD	A,H
	LD	H,A
	LD	A,D		;MSB
	CALL	N_WRBT
	INC	HL
	LD	A,E		;LSB
	CALL	N_WRBT		;update RX Read Pointer Socket (NAddress)
	POP	AF
	POP	HL
	EXX			;'
	POP	DE		;'	
	POP	BC		;'
	EXX
	JP	RECV2		;quit Receive

;--- Functions DATABASE ----------------------

	;### GET PEER DATA ###
	;
	;PI:	 A - Socket
	;	HL - HAddress for storage of Peer-Data (4:Peer-IP, 2:Peer-Port - all in NOrder !!!)
	;
	;PO:	HL - HAddress + 6
	;
	;AR:	HL
GPDATA:	PUSH	DE
	CALL	N_GSDIP
	CALL	N_GSDPT
	POP	DE
	RET

	;### GET LOCAL DATA ###
	;
	;PI:	 A - Socket
	;	HL - HAddress for storage of local Data (4:local IP, 2:local Port - all in NOrder !!!)
	;
	;PO:	HL - HAddress + 6
	;
	;AR:	HL
GLDATA:	PUSH	DE
	CALL	N_GLIP
	CALL	N_GSSPT
	POP	DE
	RET

;--- Functions for CONVERSION ----------------------

;REMARK: for all conversion functions don't overlap addresses !

	;### INTEGER TO ASCII (SHORT, 16 Bit) ###
	;
	;PI:	HL - address decimal ASCII-string (max. 5 digits)	
	;	DE - integer value
	;
	;PO:	HL - address last digit + 1
	;
	;AR:	AF,HL,DE
ITOA:	PUSH	IY
	PUSH	BC
	PUSH	HL
	POP	IY
	LD	HL,ITOATB
	LD	C,0		;blank on
ITOA1:	LD	A,(HL)
	OR	A		;last DIGIT ?
	JR	Z,ITOA4		;-> yes
	LD	B,0
ITOA2:	INC	B
	LD	A,E
	SUB	(HL)
	LD	E,A
	INC	HL
	LD	A,D
	SBC	A,(HL)
	LD	D,A
	DEC	HL
	JR	NC,ITOA2	;cont.
	LD	A,E		;correct SBC
	ADD	A,(HL)
	LD	E,A
	INC	HL
	LD	A,D
	ADC	A,(HL)
	INC	HL
	LD	D,A
	DEC	B
	JR	Z,ITOA3		;0 ?
	LD	C,1		;blank off
ITOA3:	INC	C
	DEC	C		;check blank
	JR	Z,ITOA1		;-> next DIGIT
	LD	A,B
	ADD	A,'0'
	LD	(IY),A		;write DIGIT
	INC	IY
	JR	ITOA1		;next
ITOATB:	DW	10000,1000,100,10,0
	;last DIGIT - not blank!
ITOA4:	LD	A,E
	ADD	A,'0'
	LD	(IY),A		;write DIGIT
	INC	IY
	PUSH	IY
	POP	HL
	POP	BC
	POP	IY
	RET
	
	;### ASCII TO INTEGER (SHORT, 16 Bit) ###
	;
	;PI:	HL - address decimal ASCII-string (max. 5 digits)
	;
	;PO:	HL - address last digit + 1
	;	DE - integer value
	;	 A - E value
	;     CY=1 - Error
	;
	;AR:	AF,HL,DE
ATOI:	PUSH	BC
	LD	BC,00500H	;max./count
ATOI1:	LD	A,(HL)
	CP	'0'
	JR	C,ATOI2		;<0
	CP	'9'+1
	JR	NC,ATOI2	;>9
	INC     HL  
	INC	C
	DJNZ	ATOI1
ATOI2:	XOR	A
	LD	D,A		;clear!
	LD	E,A		;=0
	CP	C
	SCF			;ERR no digit
	JR	Z,ATOI5
	PUSH	HL		;save PTR
	DEC	HL		;D0
	LD	B,C		;digits
ATOI3:	LD	A,(HL)
	SUB	'0'
	LD	E,A
	PUSH	DE		;save Dx invers
	DEC	HL		;next Dx
	DJNZ	ATOI3
	LD	B,C		;digits
	LD	H,D
	LD	L,D		;HL=0
ATOI4:	LD	E,L
	LD	D,H
	ADD	HL,HL		;*2
	ADD	HL,HL		;*4
	ADD	HL,DE		;*5
	ADD	HL,HL		;*10
	POP	DE		;next Dx
	ADD	HL,DE		;+
	DJNZ	ATOI4
	EX	DE,HL		;DE: value
	POP	HL		;restore PTR
	OR	A		;OK
ATOI5:	LD	A,E		;return LSB
	POP	BC		;CY=ERR
	RET

	;### NUMERIC IP TO IP-ADDRESS-STRING ###
	;
	;PI:	HL - HAddress of numeric IP-Address 32bit (4 Byte NOrder)
	;	DE - HAddress of IP-address-string+0 in dotted decimal format (max. 16 Byte)
	;
	;PO:	IP-ADDRESS from HL (numeric) to DE (string)
	;
	;AR:	-
I_NTOA:	PUSH	HL
	PUSH	DE
	PUSH	BC
	PUSH	AF
	LD	B,4		;4*dec.
	EX	DE,HL
	JR	I_NTO2
I_NTO1:	LD	(HL),'.'
	INC	HL
I_NTO2:	LD	A,(DE)
	INC	DE
	PUSH	DE
	LD	D,0	
	LD	E,A
	CALL	ITOA		;conv. NUM
	POP	DE
	DJNZ	I_NTO1
	LD	(HL),0		;delimiter
	POP	AF
	POP	BC
	POP	DE
	POP	HL
	RET
	
	;### IP-ADDRESS-STRING TO NUMERIC IP ###
	;
	;PI:	HL - HAddress of numeric IP-Address 32bit (4 Byte NOrder)
	;	DE - HAddress of IP-address-string+0 in dotted decimal format
	;
	;PO:	IP-ADDRESS from DE (string) to HL (numeric)
	;     CY=1 - Error (only 0-9 and Dot allowed / NUM's 0-255)
	;
	;AR:	AF
I_ADDR:	PUSH	HL
	PUSH	DE
	PUSH	BC
	LD	B,4		;4 NUM's
	EX	DE,HL
	JR	I_ADD2
I_ADD1:	LD	A,(HL)
	INC	HL
	CP	'.'
	SCF
	JR	NZ,I_ADD3	;String-ERR	
I_ADD2:	PUSH	DE
	CALL	ATOI		;conv. 1 NUM
	LD	A,E
	INC	D
	DEC	D
	POP	DE
	JR	C,I_ADD3	;ATOI-ERR
	SCF			;set ERR
	JR	NZ,I_ADD3	;>8Bit
	LD	(DE),A
	INC	DE
	DJNZ	I_ADD1
	CCF			;OK
I_ADD3:	POP	BC
	POP	DE
	POP	HL
	RET

	;### HOST TO NET SHORT (16 Bit) ### (unsigned short htons( unsigned short hostshort))
	;
	;PI:	HL - HAddress SHORT HOrder (2 Byte)
	;	DE - HAddress SHORT NOrder (2 Byte)
	;
	;PO:	SHORT from HL (HOrder) to DE (NOrder)
	;
	;AR:	-
HTONS:	PUSH	BC
	LD	BC,2
HTONS1:	PUSH	HL
	PUSH	DE
HTONS2:	PUSH	AF
	EX	DE,HL
	ADD	HL,BC
	EX	DE,HL
	DEC	DE
HTONS3:	LDI
	DEC	DE
	DEC	DE
	JP	PE,HTONS3
	POP	AF
	POP	DE
	POP	HL
	POP	BC
	RET

	;### NET TO HOST SHORT (16 Bit) ### (unsigned long ntohs(unsigned short netshort))
	;
	;PI:	HL - HAddress SHORT HOrder (2 Byte)
	;	DE - HAddress SHORT NOrder (2 Byte)
	;
	;PO:	SHORT from DE (NOrder) to HL (HOrder)
	;
	;AR:	-
NTOHS:	PUSH	BC
	LD	BC,2
NTOHS1:	PUSH	HL
	PUSH	DE
	EX	DE,HL
	JR	HTONS2

	;### HOST TO NET LONG (32 Bit) ### (unsigned long htonl(unsigned long hostlong))
	;
	;PI:	HL - HAddress LONG HOrder (4 Byte)
	;	DE - HAddress LONG NOrder (4 Byte)
	;
	;PO:	LONG from HL (HOrder) to DE (NOrder)
	;
	;AR:	-
HTONL:	PUSH	BC
	LD	BC,4
	JR	HTONS1

	;### NET TO HOST LONG (32 Bit) ### (unsigned long ntohl(unsigned long netlong))
	;
	;PI:	HL - HAddress LONG HOrder (4 Byte)
	;	DE - HAddress LONG NOrder (4 Byte)
	;
	;PO:	LONG from DE (NOrder) to HL (HOrder)
	;
	;AR:	-
NTOHL:	PUSH	BC
	LD	BC,4
	JR	NTOHS1

;#######################################################
