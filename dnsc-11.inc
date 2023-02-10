;******************************************************************************
;
;		Simple DNS Protocol Client (RFC 1034)
;
;			###     KCNET     ###
;			 WIZnet TCP/IP-Stack 
;			###  susowa 2008  ###
;
;  Name		: DNSC-xx.INC
;  Date		: 06.10.2008
;  Modified	: 26.11.2012
;		: 21.11.2022 d_kef
;  Hardware	: Net4CPC - W5100S
;
;  Version 1.1  : 02.11.2008
;
;******************************************************************************
;
; If the resolution by name or address can't be successfully performed, the
; main functions "GET HOST BY NAME" and "GET HOST BY ADRESS" return with CY=1
; and one of the following error codes in register A.
;
;	DNS-Server:
;
;	1	Format error:	The name server was unable to interpret the
;				query.
;	2	Server failure:	The name server was unable to process this
;				query due to a problem with the name server.
;	3	Name Error:	Meaningful only for responses from an 
;				authoritative name server, this code signifies
;				that the domain name referenced in the query
;				does not exist.
;	4	Not Implemented:The name server does not support the requested
;				kind of query.
;	5	Refused:	The name server refuses to perform the 
;				specified operation for policy reasons, for
;				example, a name server may not wish to provide
;				the information to the particular requester, 
;				or a name server may not wish to perform a 
;				particular operation (e.g., zone).
;	6...15			reserved for future use	by name server
;
;
;	DNS-Client:
;
;	16	SOCKET		The SOCKET-Function returned with Error. 
;	17	CONNECT		The CONNECT-Function returned with Error. 
;	18	NAME ERROR	The domain name referenced in the query
;				contains empty labels, too long labels or the
;				name exceeds the maximum length.
;	19	SENDTO		The SENDTO-Function returned with Error. 
;	20	TIMEOUT		There is no response from DNS-Server.
;	21	QR=0		The QR-Bit in FLAG0 of the response is cleared.
;
;
;  RA=0 / RA=1 -> nonrecursive / recursive response from name server:
;
;    22	/ 150	PARSE QUESTION	Parsing QUESTION SECTION of response message 
;				failed. 
;    23 / 151	NO ANSWER	ANSWER SECTION of response message contains 0
;				ressource records. 
;    24 / 152	PARSE ANSWER	Parsing ANSWER SECTION of response message 
;				failed. 
;
;******************************************************************************
;
;	### DNS MESSAGES ###
;
;	<Message Format>
;	+---------------------+
;	|        Header       |
;	+---------------------+
;	|       Question      | the question for the name server
;	+---------------------+
;	|        Answer       | Resource Records answering the question            
;	+---------------------+                                        
;	|      Authority      | Resource Records pointing toward an authority      
;	+---------------------+                                        
;	|      Additional     | Resource Records holding additional information
;	+---------------------+
;
;		As follow, example of DNS Standard Query
;		+---------------------------------------------------+
;Header		| OPCODE=SQUERY                                     |
;		+---------------------------------------------------+
;Question	| QNAME=SRI-NIC.ARPA., QCLASS=IN, QTYPE=A           |
;		+---------------------------------------------------+
;Answer		| <empty>                                           |
;		+---------------------------------------------------+
;Authority	| <empty>                                           |
;		+---------------------------------------------------+
;Additional	| <empty>                                           |
;		+---------------------------------------------------+
;
;		As follow, example of DNS response
;		+---------------------------------------------------+
;Header		| OPCODE=SQUERY, RESPONSE, AA                       |
;		+---------------------------------------------------+
;Question	| QNAME=SRI-NIC.ARPA., QCLASS=IN, QTYPE=A           |
;		+---------------------------------------------------+
;Answer		| SRI-NIC.ARPA. 86400 IN A 26.0.0.73                |
;		|               86400 IN A 10.0.0.51                |
;		+---------------------------------------------------+
;Authority	| <empty>                                           |
;		+---------------------------------------------------+
;Additional	| <empty>                                           |
;		+---------------------------------------------------+
;
;------------------------------------------------------------------------------
;
;	### DNS PROTOCOL DEFINITIONS ###
;
;		QCLASS values
;
;	CLASS fields appear in resource records.
;	The following CLASS mnemonics and values are defined: 
;
; * CLASS *
CLASS_IN	EQU	1	;the Internet
CLASS_CS	EQU	2 	;the CSNET class (Obsolete - used
				;only for examples in some obsolete RFCs)
CLASS_CH	EQU	3 	;the CHAOS class
CLASS_HS	EQU	4 	;Hesiod [Dyer 87]
; * QCLASS *
QCLASS_ANY	EQU	255 	;any class
;
;		QTYPE values
;
;	TYPE fields are used in resource records.
;	Note that these types are a subset of QTYPEs
;
; * TYPE *
TYPE_A		EQU	1	;the ARPA Internet
TYPE_NS		EQU	2 	;an authoritative name server
TYPE_MD		EQU	3 	;a mail destination (Obsolete - use MX)
TYPE_MF		EQU	4 	;a mail forwarder (Obsolete - use MX)
TYPE_CNAME	EQU	5 	;the canonical name for an alias
TYPE_SOA	EQU	6 	;marks the start of a zone of authority
TYPE_MB		EQU	7	;a mailbox domain name (EXPERIMENTAL)
TYPE_MG		EQU	8 	;a mail group member (EXPERIMENTAL)
TYPE_MR		EQU	9 	;a mail rename domain name (EXPERIMENTAL)
TYPE_NULL	EQU	10 	;a null RR (EXPERIMENTAL)
TYPE_WKS	EQU	11 	;a well known service description
TYPE_PTR	EQU	12 	;a domain name pointer
TYPE_HINFO	EQU	13 	;host information
TYPE_MINFO	EQU	14 	;mailbox or mail list information
TYPE_MX		EQU	15 	;mail exchange
TYPE_TXT	EQU	16 	;text strings
; * QTYPE *
QTYPE_AXFR	EQU	252	;a request for a transfer of an entire zone
QTYPE_MAILB	EQU	253 	;a request for mailbox-related records (MB, MG or MR)
QTYPE_MAILA	EQU	254 	;a request for mail agent RRs (Obsolete - see MX)
QTYPE_TYPE_ALL	EQU	255 	;a request for all records
;
INITRTT		EQU	2000	;Initial smoothed response time
MAXCNAME	EQU	10	;Maximum amount of cname recursion
;
; * Round trip timing parameters *
AGAIN		EQU	8	;Average RTT gain = 1/8
LAGAIN		EQU	3	;Log2(AGAIN)
DGAIN		EQU	4	;Mean deviation gain = 1/4
LDGAIN		EQU	2	;log2(DGAIN)
;
;	HEADER FOR ALL DOMAIN MESSAGES
;                                    1  1  1  1  1  1
;      0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;    |                      ID                       |
;    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;    |QR|   OPcode  |AA|TC|RD|RA|   Z    |   RCcode  |
;    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;    |                    QDCOUNT                    |
;    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;    |                    ANCOUNT                    |
;    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;    |                    NSCOUNT                    |
;    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;    |                    ARCOUNT                    |
;    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;
; * FLAG0 *
;	QR    : Query / Response
;	OPcode: OP_QUERY / OP_IQUERY / OP_STATUS
;	AA    : answer authoritative 
;	TC    : answer truncated
;	RD    : Recursion desired
;
;  A one bit field that specifies whether this message is a:
QR_QUERY	EQU	0 SHL 7	;query
QR_RESPONSE	EQU	1 SHL 7	;response
;
;  A four bit field that specifies kind of query in this
;  message.  This value is set by the originator of a query
;  and copied into the response.  The values are:
OP_QUERY	EQU	0 SHL 3	;a standard query (QUERY)
OP_IQUERY	EQU	1 SHL 3	;an inverse query (IQUERY)
OP_STATUS	EQU	2 SHL 3	;a server status request (STATUS)
;
AA_FLAG0	EQU	1 SHL 2	;AND-mask for AA-Bit
;
TC_FLAG0	EQU	1 SHL 1	;AND-mask for TC-Bit
;
RD_FLAG0	EQU	1	;OR/AND-mask for RD-Bit

; * FLAG1 *
;	RA    : Recursion available - this is be set or cleared in a response
;		and denotes whether recursive query support is available in
;		the name server
;	Z     : Reserved for future use, must be zero in all queries
;		and responses
;	RCcode: Response code
;
RA_FLAG1	EQU	1 SHL 7	;AND-mask for RA-Bit
;
Z_FLAG1		EQU	0 SHL 4	;3 reserved bits set to zero
;
;  Response code - this 4 bit field is set as part of responses,
;                  the values have the following interpretation:
RC_NO_ERROR	EQU	0	;No error condition
RC_FORMAT_ERROR	EQU	1	;Format error - The name server was unable
				;to interpret the query
RC_SERVER_FAIL	EQU	2	;Server failure - The name server was unable
				;to process this query due to a problem with
				;the name server
RC_NAME_ERROR	EQU	3	;Name Error - Meaningful only for responses
				;from an authoritative name server, this code
				;signifies that the domain name referenced in
				;the query does not exist
RC_NOT_IMPL	EQU	4	;Not Implemented - The name server does not
				;support the requested kind of query
RC_REFUSED	EQU	5	;Refused - The name server refuses to perform
				;the specified operation for policy reasons,
				;for example, a name server may not wish to
				;provide the information to the particular
				;requester, or a name server may not wish to
				;perform a particular operation (e.g., zone)
;			6...15	;reserved for future use		
;
;	QUESTION FORMAT
;
;    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                                               |
;   /                     QNAME                     /
;   /                                               /
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                     QTYPE                     |
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                     QCLASS                    |
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;
;	where:
;
;QNAME          a domain name represented as a sequence of labels, where
;		each label consists of a length octet followed by that
;               number of octets.  The domain name terminates with the
;               zero length octet for the null label of the root.  Note
;               that this field may be an odd number of octets; no
;               padding is used.
;
;QTYPE          a two octet code which specifies the type of the query.
;               The values for this field include all codes valid for a
;               TYPE field, together with some more general codes which
;               can match more than one type of RR.
;
;QCLASS         a two octet code that specifies the class of the query.
;               For example, the QCLASS field is IN for the Internet.
;

;	RESOURCE RECORD - ANSWER, AUTHORITY, AND ADDITIONAL FORMAT
;
;  The answer, authority, and additional sections all share the same
;  format: a variable number of resource records, where the number of
;  records is specified in the corresponding count field in the header.
;  Each resource record has the following format:
;
;                                  1  1  1  1  1  1
;    0  1  2  3  4  5  6  7  8  9  0  1  2  3  4  5
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                                               |
;   /                                               /        
;   /                      NAME                     /
;   |                                               |
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                      TYPE                     |
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                     CLASS                     |
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                      TTL                      |
;   |                                               |
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;   |                   RDLENGTH                    |
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--|
;   /                     RDATA                     /
;   /                                               /
;   +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;
;	where:
;
;NAME           a domain name to which this resource record pertains.
;		In order to reduce the size of messages, the domain system
;		utilizes a compression scheme which eliminates the repetition
;		of domain names in a message. In this scheme, an entire domain
;		name or a list of labels at the end of a domain name is
;		replaced with a pointer to a prior occurance of the same name.
;		The pointer takes the form of a two octet sequence:
;		    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;		    | 1  1|                OFFSET                   |
;		    +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
;		The first two bits are ones.  This allows a pointer to be
;		distinguished from a label, since the label must begin with
;		two zero bits because labels are restricted to 63 octets or
;		less (the 10 and 01 combinations are reserved for future use).
;		The OFFSET field specifies an offset from the start of the
;		message (i.e., the first octet of the ID field in the domain
;		header). 
;		A zero offset specifies the first byte of the ID field, etc.
;		The compression scheme allows a domain name in a message to be
;		represented as either:
;		   - a sequence of labels ending in a zero octet
;		   - a pointer
;		   - a sequence of labels ending with a pointer
;
;TYPE           two octets containing one of the RR type codes.  This
;               field specifies the meaning of the data in the RDATA
;               field.
;
;CLASS          two octets which specify the class of the data in the
;               RDATA field.
;
;TTL            a 32 bit unsigned integer that specifies the time
;               interval (in seconds) that the resource record may be
;               cached before it should be discarded.  Zero values are
;               interpreted to mean that the RR can only be used for the
;               transaction in progress, and should not be cached.
;
;RDLENGTH       an unsigned 16 bit integer that specifies the length in
;               octets of the RDATA field.
;
;RDATA          a variable length string of octets that describes the
;               resource.  The format of this information varies
;               according to the TYPE and CLASS of the resource record.
;               For example,  if the TYPE is A and the CLASS is IN,
;               the RDATA field is a 4 octet ARPA-Internet address.
;
;------------------------------------------------------------------------------
;
;	### DNS CONSTANTS & STRUCTURES ###
;
MXDMSG_SIZE	EQU 	512		;maximum size of a DNS message (UDP)
MXDDMN_SIZE	EQU	255		;maximum size of a domain name
MXDLBL_SIZE	EQU	63		;maximum size of a domain label
COMP_SCHEME	EQU	0C0H		;compression scheme

PORT_DOMAIN	EQU	53		;DNS-Server Port (UDP & TCP)
INQ_STRING:	DB	".in-addr.arpa",0 ;inverse query string
INQ_STRLG	EQU	$-INQ_STRING	;string length

	;DNS-Message header structure
DNSH_ID		EQU	0		;Identification
DNSH_FLAG0	EQU	DNSH_ID+2	;Flag 0 
DNSH_FLAG1	EQU	DNSH_FLAG0+1	;Flag 1
QDCNT		EQU	DNSH_FLAG1+1	;Question count
ANCNT		EQU	QDCNT+2		;Answer count
NSCNT		EQU	ANCNT+2		;Authority (name server) count
ARCNT		EQU	NSCNT+2		;Additional record count
DHDR_SIZE	EQU	ARCNT+2-DNSH_ID	;Headerlength

	;Question section structure
;QNAME					;variable length domain name
QTYPE		EQU	0		;type of query
QCLASS		EQU	QTYPE+2		;class of query

	;Resource record structure
;RR_NAME				;variable length domain name (compr.)
RR_TYPE		EQU	0		;type of RR
RR_CLASS	EQU	RR_TYPE+2	;class of RR
RR_TTL		EQU	RR_CLASS+2	;cache time of RR in s
RR_RDLEN	EQU	RR_TTL+4	;data length of RR_RDATA
;RR_RDATA	EQU	RR_RDLEN+2	;variable length data

;------------------------------------------------------------------------------
	
	;### GET HOST BY NAME ###
	;retrieves host IP Address corresponding to a Host Name from DNS Server
	;
	;PI:	HL - HAddress of MSG-Buffer ( min. 534 Byte ! )
	;	DE - HAddress of Domain-Name-Buffer (min. 256 Byte!) wich 
	;	     contains the Host-Name-String+0
	;
	;PO:	HL - MSG-Buffer contains the complete response from DNS-Server
	;	     at address+22 if any
	;	DE - Domain-Name-Buffer contains the 1. resolved IP Address
	;	     from answer section of response (32 Bit, NOrder) if any
	;     CY=1 - resolve ERR with ERR-Code in A (1-15 Server/16-25 Client)
	;
	;AR:	AF,BC
GHBNAM:	RES	0,C
	JR	GHBAD1

;------------------------------------------------------------------------------

	;### GET HOST BY ADRESS ###
	;retrieves the Host Domain Name corresponding to a host IP Address from
	;DNS Server
	;
	;PI:	HL - HAddress of MSG-Buffer ( min. 534 Byte ! )
	;	DE - HAddress of Domain-Name-Buffer (min. 256 Byte!) wich 
	;	     contains the IP Address (32 Bit, NOrder)
	;
	;PO:	HL - MSG-Buffer contains the complete response from DNS-Server
	;	     at address+22 if any
	;	DE - Domain-Name-Buffer contains the 1. resolved Domain-Name-
	;	     String+0 from answer section of response if any
	;     CY=1 - resolve ERR with ERR-Code in A (1-15 Server/16-25 Client)
	;
	;AR:	AF,BC
GHBADR:	PUSH	HL
	PUSH	DE
	LD	HL,32
	ADD	HL,DE
	EX	DE,HL
	LD	BC,4
	LDIR			;shift IP to DE+32 for retries (IQUERY-DN!)
	POP	DE
	POP	HL
	SET	0,C
GHBAD1:	PUSH	HL
	LD	(HL),E
	INC	HL
	LD	(HL),D		;save address DNB
	INC	HL
	LD	(HL),LOW(750)
	INC	HL
	LD	(HL),HIGH(750)	;load first Timeout/2 in ms
	POP	HL
	LD	B,3		;number of retries
GHBAD2:	PUSH	BC
	BIT	0,C
	JR	Z,GHBAD3
	PUSH	HL
	PUSH	DE
	LD	HL,32
	ADD	HL,DE
	LD	BC,4
	LDIR			;restore IP from DE+32
	POP	DE
	POP	HL
GHBAD3:	POP	BC
	PUSH	BC
	CALL	DQUERY
	POP	BC
	RET	NC		;OK
	CP	20
	SCF			;set ERR
	RET	NZ		;no TIMEOUT-ERR
	DJNZ	GHBAD2		;retry
	RET			;ready with ERR

;------------------------------------------------------------------------------
	
	;### DNS QUERY ###
	;- make and send query message to DNS-Server
	;- wait for, receive and parse response message from DNS-Server
	;
	;PI:	HL - HAddress MSG-Buffer
	;	DE - HAddress Domain-Name-Buffer
	;	 C - Bit 0=0 resolve by NAME
	;		 0=1 resolve by IP (inverse query)
	;
	;PO:	HL - HAddress MSG-Buffer with complete response from DNS-Server
	;	DE - HAddress Domain-Name-Buffer with resolved IP or Name
	;     CY=1 - resolve Error with ERR-Code in A
	;
	;AR:	AF,BC
DQUERY:	PUSH	IY
	PUSH	DE
	PUSH	HL
	POP	IY
	LD	A,0FFH		;any
	LD	D,SK_DGRAM	;Mode
	LD	E,0		;Flags
	PUSH	BC
	CALL	SOCKET		;get free Socket
	POP	BC
	LD	(IY+4),16	;16
	JP	C,DQUEND	;Error
	CALL	CONNECT		;open Socket
	INC	(IY+4)		;17
	JP	C,DQCERR	;Error+close Socket
	;make query
	LD	B,A		;save Socket
	CALL	MKQUER
	LD	A,B		;restore Socket
	INC	(IY+4)		;18
	JP	C,DQCERR	;Error+close Socket
	PUSH	BC
	LD	B,H
	LD	C,L		;Datalength
	PUSH	IY
	POP	HL
	LD	DE,6
	ADD	HL,DE
	EX	DE,HL		;Peer Data
	ADD	HL,DE		;+HL=6->Data
	;send query
	CALL	SENDTO		;send query
	POP	BC		;B:Socket,C(0): NAME/IP
	INC	(IY+4)		;19
	JP	C,DQCERR	;Error+close Socket
	CALL	N_TIME
	LD	(IY+4),L
	LD	(IY+5),H	;save SEND TIME
	SLA	(IY+2)
	RL	(IY+3)		;Timeout*2
DQUER1:	;wait for response
	CALL	N_TIME		;get TIME NOW
	LD	E,(IY+4)
	LD	D,(IY+5)
	OR	A
	SBC	HL,DE		;NOW-SEND
	JR	NC,DQUER2	;OK
	LD	DE,N_XTIME
	ADC	HL,DE		;correct Overflow
DQUER2:	LD	E,(IY+2)
	LD	D,(IY+3)	;load current Timeout
	SBC	HL,DE
	JR	NC,DQUER5	;Timediff.>=Timeout
	LD	E,SL_RECV
	CALL	SELECT
	JR	C,DQUER1	;no Data
	;receive and test data 
	PUSH	BC
	PUSH	IY
	POP	HL
	LD	DE,2+2+2+4
	ADD	HL,DE
	PUSH	HL		;query PeerData Port
	LD	DE,2+2
	ADD	HL,DE
	PUSH	HL
	EX	DE,HL		;DE: Header Data RECVFR
	ADD	HL,HL		;HL:=8
	ADD	HL,DE		;HL: Data RECVFR
	LD	BC,MXDMSG_SIZE
	CALL	RECVFR
	POP	HL		;Header Data RECVFR
	INC	HL
	INC	HL
	INC	HL
	INC	HL		;-> Header Data Port
	POP	DE		;query PeerData Port
	PUSH	DE		;save
	LD	B,2
DQUER3:	LD	A,(DE)
	CP	(HL)		;test Port only
	JR	NZ,DQUER4
	INC	HL
	INC	DE
	DJNZ	DQUER3
	INC	HL
	INC	HL		;skip Data Size Header Data
	LD	A,(DE)
	CP	(HL)		;test ID(1)
	JR	NZ,DQUER4
	INC	HL
	INC	DE
	LD	A,(DE)
	CP	(HL)		;test ID(0)
DQUER4:	POP	DE		;query PeerData Port
	DEC	DE
	DEC	DE
	DEC	DE
	DEC	DE		;back to query PeerData
	POP	BC
	LD	A,B		;restore Socket!
	JR	NZ,DQUER1	;not expected data
	INC	HL
	JR	DQUER6		;Data OK -> continue
DQUER5:	LD	(IY+4),20	;20 (Timeout)
	SCF
	JR	DQCERR		;Error+close Socket
DQUER6:	;parse response message
	LD	A,QR_RESPONSE
	AND	(HL)		;Flag0
	SCF
	LD	(IY+4),21	;21
	JR	Z,DQCERA	;Error+close Socket
	INC	HL
	LD	A,00FH
	AND	(HL)		;Flag1
	JR	Z,DQUER7	;Server ERR-Code = 0
	LD	(IY+4),A	;save Server ERR-Code
	SCF
	JR	DQCERA		;Error+close Socket
DQUER7:	LD	A,RA_FLAG1
	AND	(HL)		;Flag1
	LD	(IY+5),A	;save RA-bit
	;copy+conv. QD/AN/NS/ARCOUNT 
	INC	HL		;QDCOUNT
	EX	DE,HL
	PUSH	HL		;save PTR Counter HOrder
	LD	A,4
DQUER8:	CALL	NTOHS
	INC	HL
	INC	HL
	INC	DE
	INC	DE
	DEC	A
	JR	NZ,DQUER8
	POP	HL		;QDCOUNT HOrder
	;parse question section
DQUER9:	LD	A,(HL)
	INC	HL
	OR	(HL)
	DEC	HL
	JR	Z,DQUERA	;QDCOUNT=0	
	CALL	PRSQUE		;current PTR in DE
	PUSH	AF
	LD	A,22		;22
	OR	(IY+5)		;+ RA-bit
	LD	(IY+4),A
	POP	AF
	JR	C,DQCERA	;Error+close Socket
	LD	A,(HL)
	SUB	1
	LD	(HL),A
	INC	HL
	LD	A,(HL)
	SBC	A,0
	LD	(HL),A
	DEC	HL
	JR	DQUER9
DQUERA:	INC	HL
	INC	HL		;ANCOUNT HOrder
	;parse answer section
	LD	A,23		;23
	OR	(IY+5)		;+ RA-bit
	LD	(IY+4),A	;!set ERR!
DQUERB:	LD	A,(HL)
	INC	HL
	OR	(HL)
	DEC	HL
	JR	Z,DQUERC	;QDCOUNT=0	
	LD	A,24		;24
	OR	(IY+5)		;+ RA-bit
	LD	(IY+4),A	;!set ERR!
	CALL	PRSANS		;current PTR in DE, if answer OK reset ERR-Code
	JR	C,DQCERA	;Error+close Socket
	LD	A,(HL)
	SUB	1
	LD	(HL),A
	INC	HL
	LD	A,(HL)
	SBC	A,0
	LD	(HL),A
	DEC	HL
	JR	DQUERB
DQUERC:	XOR	A
	CP	(IY+4)
	JR	Z,DQCERA	;got ANSWER
	SCF			;ERR
DQCERA:	LD	A,B		;restore Socket!			
DQCERR:	PUSH	AF		;save CY
	CALL	CLOSE		;close Socket
	POP	AF		;restore CY
DQUEND:	LD	A,(IY+4)	;get ERR-Code
	PUSH	IY
	POP	HL
	POP	DE
	POP	IY
	RET

;------------------------------------------------------------------------------

	;### MAKE QUERY ###
	;			 <- MSG-Buffer (534 Byte) ->
	;		query message		       response message
	;
	;	- 2 Byte address DN-Buffer	- 2 Byte address DN-Buffer
	;	- 2 Byte TIMEOUT in ms		- 2 Byte TIMEOUT in ms
	;	- 2 Byte query SEND-TIME or	- 2 Byte query SEND-TIME or
	;	    ERROR-Code			    ERROR-Code
	;	- 6 Byte query PeerData		- 6 Byte query PeerData
	;	- 12 Byte DNS-Message Header	- 2 Byte QUERY-ID
	;	- max. 256 Byte domain name+0	- 8 Byte 'RECEIVE FROM' UDP
	;	- 2 Byte type of query		    Header Data
	;	- 2 Byte class of query		---- MSG-Buffer + 22 ----
	;					- max. 512 Byte response
	;					  message from DNS-Server
	;
	;PI:	IY - HAddress MSG-Buffer
	;	 C - Bit 0=0 resolve by NAME
	;		 0=1 resolve by IP (inverse query)
	;
	;PO:    HL - MSG length
	;     CY=1 - Error
	;
	;AR:	AF,HL,DE
MKQUER:	PUSH	BC
	PUSH	IY
	POP	HL
	LD	DE,6
	ADD	HL,DE
	PUSH	HL		;MSG-Buffer+6
	BIT	0,C
	JR	Z,MKQUE2
	;make inverse query domain name
	LD	E,(IY+0)
	LD	D,(IY+1)
	CALL	NTOHL		;convert+move IP (DE->HL)
	CALL	I_NTOA		;make ip-string
MKQUE1:	LD	A,(DE)
	INC	DE
	OR	A
	JR	NZ,MKQUE1	;find zero
	DEC	DE		;exclude
	LD	A,C
	LD	HL,INQ_STRING
	LD	BC,INQ_STRLG
	LDIR			;append iquery-string
	LD	C,A
MKQUE2:	;make Peer-Data
	POP	HL		;MSG-Buffer+6
	LD	A,N_DNSIP
	CALL	N_RIPA		;load DNS-IP
	LD	(HL),0
	INC	HL
	LD	(HL),PORT_DOMAIN
	INC	HL
	;make Header
	EX	DE,HL
	CALL	N_DPRT
	EX	DE,HL
	LD	(HL),D
	INC	HL
	LD	(HL),E		;dyn. Port->MSG-ID
	INC	HL
	LD	(HL),OP_QUERY OR QR_QUERY OR RD_FLAG0 ;Flag 0 (rec. desired!)
	INC	HL
	LD	(HL),0		;Flag 1
	INC	HL
	LD	(HL),0
	INC	HL
	LD	(HL),1		;QDCOUNT=1
	INC	HL
	LD	B,6
MKQUE3:	LD	(HL),0		;AN/NS/ARCOUNT=0
	INC	HL
	DJNZ	MKQUE3
	EX	DE,HL
	;make QNAME
	LD	L,(IY+0)
	LD	H,(IY+1)	;address domain name+0
MKQUE4:	PUSH	HL
	LD	B,MXDLBL_SIZE
MKQUE5:	LD	A,(HL)
	OR	A
	JR	Z,MKQUE7
	CP	'.'
	JR	Z,MKQUE7
	INC	HL		;next character
	DJNZ	MKQUE5
MKQUE6:	POP	HL
	SCF			;ERR:domain label>MXDLBL_SIZE
	JR	MKQEND
MKQUE7:	LD	A,MXDLBL_SIZE
	SUB	B
	LD	B,A		;label length
	JR	Z,MKQUE6	;=0->ERR: label empty!
	LD	(DE),A		;write length
	INC	DE
	POP	HL
MKQUE8:	LD	A,(HL)
	LD	(DE),A		;copy label
	INC	HL
	INC	DE
	DJNZ	MKQUE8
	;check domain name length
	PUSH	HL
	PUSH	DE
	LD	E,(IY+0)
	LD	D,(IY+1)	;address domain name
	XOR	A
	SBC	HL,DE		;current - address
	CP	H
	POP	DE
	POP	HL
	SCF
	JR	NZ,MKQEND	;ERR: length > 255
	CP	(HL)
	INC	HL		;skip '.'
	JR	NZ,MKQUE4	;<>0->next label
	EX	DE,HL
	LD	(HL),A		;write 0 terminator	
	INC	HL
	;make qtype
	LD	(HL),0
	INC	HL
	BIT	0,C
	LD	A,TYPE_A
	JR	Z,MKQUE9
	LD	A,TYPE_PTR
MKQUE9:	LD	(HL),A
	INC	HL
	;make qclass
	LD	(HL),0
	INC	HL
	LD	(HL),CLASS_IN
	INC	HL
	;compute MSG-Length 
	PUSH	IY
	POP	DE
	XOR	A
	SBC	HL,DE		;MSG-End-MSG-Buffer
	LD	DE,12		;- (DNB+TIMEOUT+SEND-TIME+PeerData)
	SBC	HL,DE		;= MSG-Length
MKQEND:	POP	BC
	RET

;------------------------------------------------------------------------------

	;### PARSE QUESTION SECTION ###
	;- analyze a question section in response message
	;
	;PI:	DE - HAddress QNAME question section
	;	IY - HAddress MSG-Buffer
	;	 C - Bit 0=0 resolve by NAME
	;		 0=1 resolve by IP (inverse query)
	;
	;PO:	DE - HAddress question section end + 1
	;     CY=1 - Error
	;
	;AR:	AF,DE
PRSQUE:	PUSH	HL
	PUSH	BC
	XOR	A		;do nothing
	CALL	PRSNAM		;variable length of QNAME
	INC	DE
	INC	DE		;skip QTYPE
	INC	DE
	INC	DE		;skip QCLASS
	POP	BC
	POP	HL
	RET

;------------------------------------------------------------------------------

	;### PARSE ANSWER SECTION ###
	;- analyze a answer part in resource record
	;- store resolved IP address OR domain name into domain name buffer
	;- if answer OK reset ERR-Code in (IY+4)
	;
	;PI:	DE - HAddress NAME answer section
	;	IY - HAddress MSG-Buffer
	;	 C - Bit 0=0 resolve by NAME
	;		 0=1 resolve by IP (inverse query)
	;
	;PO:	DE - HAddress answer section end + 1
	;     CY=1 - Error
	;
	;AR:	AF,DE
PRSANS:	PUSH	HL
	PUSH	BC
	XOR	A		;do nothing
	CALL	PRSNAM		;variable length of NAME
	JR	C,PRSAN3	;ERR
	INC	DE		;skip MSB TYPE field
	LD	A,(DE)		;get content of TYPE field
	LD	HL,6+1
	ADD	HL,DE		;skip LSB TYPE field/CLASS/TTL
	CP	TYPE_A
	JR	Z,PRSAN1
	CP	TYPE_PTR
	JR	NZ,PRSAN2
PRSAN1:	INC	HL
	INC	HL		;skip RDLENGTH
	EX	DE,HL
	CALL	PRSNAM		;parse and copy TYPE_A or TYPE_PTR answers
	JR	PRSAN3
PRSAN2:	LD	D,(HL)
	INC	HL
	LD	E,(HL)		;RDLENGTH in NOrder!
	INC	HL
	ADD	HL,DE		;skip RDDATA
	EX	DE,HL
	OR	A		;CY=0!
PRSAN3:	POP	BC
	POP	HL
	RET

;------------------------------------------------------------------------------

	;### PARSE NAME ###
	;- analyze a (compressed) domain name in QNAME-, NAME- or RDDATA-field
	;- copy TYPE_A or TYPE_PTR answers to Domain-Name-Buffer
	;- resets ERR-Code if answer OK
	;
	;  domain names can be represented:
	;	- a sequence of labels ending in a zero octet
	;	- a sequence of labels ending with a pointer
	;	- a pointer
	;
	;PI:	DE - HAddress (compressed) domain name
	;	 A - content of TYPE field or 0 (do nothing)
	;	IY - HAddress MSG-Buffer
	;	 C - Bit 0=0 resolve by NAME
	;		 0=1 resolve by IP (inverse query)
	;
	;PO:	DE - HAddress (compressed) domain name end + 1
	;     CY=1 - Error
	;
	;AR:	AF,DE,HL
PRSNAM:	PUSH	BC
	EX	DE,HL
	CP	TYPE_A
	JR	NZ,PRSNA3
	BIT	0,C
	JR	NZ,PRSNER	;ERR no QUERY
	LD	E,(IY+0)
	LD	D,(IY+1)	;address Domain-Name-Buffer
	LD	BC,4
	LDIR			;copy "A" to Domain-Name-Buffer
PRSNOK:	;exit without ERR
	EX	DE,HL
	POP	BC
	LD	(IY+4),0	;reset ERR-Code	
	OR	A
	RET
PRSNER:	;exit with ERR
	EX	DE,HL
	POP	BC
	SCF
	RET
PRSNA3:	;ANALYZE NAME
	SET	7,C		;  SET Bit: DO NOTHING
	OR	A
	JR	Z,PRSNA4
	BIT	0,C
	JR	Z,PRSNER	;ERR no IQUERY
	RES	7,C		;RESET Bit: TYPE_PTR
PRSNA4:	PUSH	HL		;save address of (a)nalyzed name
	LD	E,(IY+0)
	LD	D,(IY+1)	;address Domain-Name-Buffer
	PUSH	DE		;save current
	RES	6,C		;label found flag OFF
	RES	5,C		;indirect pointer flag OFF
	LD	B,0		;length of (a)nalyzed name
PRSNA5:	LD	A,(HL)		;get 1. byte
	INC	HL		;2. byte!
	BIT	5,C
	JR	NZ,PRSNA6	;indirect ON
	INC	B		;length aname +1 (1. byte)
PRSNA6:	LD	E,A
	AND	COMP_SCHEME
	CP	COMP_SCHEME
	LD	A,E
	JR	NZ,PRSNA8	;not compressed
	;follow indirection
	AND	NOT COMP_SCHEME	;MSB PTR (1. byte)
	LD	E,(HL)		;LSB PTR (2. byte)
	PUSH	IY
	POP	HL
	LD	D,A
	ADD	HL,DE		;MSG-Buffer+PTR
	LD	DE,22
	ADD	HL,DE		;+WORK-RAM = indirect ADDRESS in DNS-MSG
	BIT	5,C
	JR	NZ,PRSNA7	;indirect ON
	INC	B		;length aname +1 (2. byte)
PRSNA7:	SET	5,C		;set indirect ON
	LD	A,(HL)		;get length from indirected ADDRESS
	INC	HL
PRSNA8:	OR	A		;contains label length
	JR	Z,PRSNAE	;zero length -> all done
	BIT	5,C
	JR	NZ,PRSNA9	;indirect ON
	LD	E,A
	ADD	A,B		;length aname + label length
	LD	B,A
	LD	A,E
PRSNA9:	;check length of fname < 256
	POP	DE		;current address DNB
	PUSH	HL
	PUSH	DE
	LD	L,E
	LD	H,D
	LD	E,(IY+0)
	LD	D,(IY+1)	;start address DNB
	OR	A
	SBC	HL,DE		;current - start
	LD	E,A
	LD	D,0
	ADD	HL,DE		;+label length
	INC	H
	DEC	H		;< 256 ?
	POP	DE
	POP	HL
	JR	Z,PRSNAA	;OK
	POP	HL		;address of (a)nalyzed name
	JP	PRSNER		;-> ERR
PRSNAA:	;copy label to DNB
	PUSH	BC
	LD	B,A
PRSNAB:	LD	A,(HL)
	CALL	PRSNAL
	INC	HL
	INC	DE
	DJNZ	PRSNAB
	POP	BC
	;copy '.' to DNB
	LD	A,'.'
	CALL	PRSNAL
	INC	DE
	PUSH	DE		;current address of DNB
	SET	6,C		;label found ON
	JR	PRSNA5		;next label or pointer
PRSNAE:	POP	DE		;current address of DNB
	BIT	6,C
	JR	Z,PRSNAF	;no label found
	DEC	DE		;overwrite '.'
	JR	PRSNAG
PRSNAF:	LD	A,'.'
	CALL	PRSNAL		;"." for root
	INC	DE
PRSNAG:	XOR	A
	CALL	PRSNAL		;end delimiter of TYPE_PTR
	POP	HL		;address of (a)nalyzed name
	LD	D,0
	LD	E,B
	ADD	HL,DE		;address + (a)nalyzed name length
	JP	PRSNOK		;-> OK
PRSNAL:	BIT	7,C
	RET	NZ		;do nothing!
	LD	(DE),A
	RET
;------------------------------------------------------------------------------
