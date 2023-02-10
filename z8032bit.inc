;*******************************************************
;
;	Copyright (C)	Werner Cirsovius
;
;		Hohe Weide 44
;		D-20253 Hamburg
;		Tel.: +49-40-4223247
;
;		Version 1.0 October 1989
;
;*******************************************************
;
SIGNB	equ	10000000b		;Sign bit
sign	equ	7
;
;***********************************************
;	arithmetic
;***********************************************
	;
;	Add 32 bit integers
; ENTRY	Reg pair BC points to first number
;	Reg pair DE points to second number
;	Reg pair HL points to 32 bit sum
; EXIT	Result filled with sum
add32:
	push	hl
	ld	hl,$add32	; Load routine
	jp	$op32		; Go add
	;
;	Subtract 32 bit integers
; ENTRY	Reg pair BC points to first number
;	Reg pair DE points to second number
;	Reg pair HL points to 32 bit difference
; EXIT	Result filled with difference
sub32:
	push	hl
	ld	hl,$sub32	; Load routine
	jp	$op32		; Go subtract
	;
;	Multiply 32 bit integers
; ENTRY	Reg pair BC points to first number
;	Reg pair DE points to second number
;	Reg pair HL points to 32 bit product
; EXIT	Result filled with product
;	Carry set on overflow
mul32:
	push	hl
	ld	hl,$mul32	; Load routine
	jp	$op32		; Go multiply
	;
;	Divide 32 bit integers
; ENTRY	Reg pair BC points to dividend
;	Reg pair DE points to divisor
;	Reg pair HL points to 32 bit quotient
; EXIT	Result filled with quotient
;	Carry set on overflow
div32:
	push	hl
	ld	hl,$div32	; Load routine
	jp	$op32		; Go divide
	;
;	Get modulo of 32 bit integers
; ENTRY	Reg pair BC points to dividend
;	Reg pair DE points to divisor
;	Reg pair HL points to 32 bit remainder
; EXIT	Result filled with quotient
;	Carry set on overflow or negative divisor
mod32:
	push	hl
	ld	hl,$mod32	; Load routine
	jp	$op32		; Go modulo
	;
;	Negate 32 bit integer
; ENTRY	Reg pair HL points to number
; EXIT	Number filled with negated value
neg32:
	push	hl
	call	$lod32		; Load 32 bit
	ex	de,hl
	call	$neg32		; Go negate
	pop	hl
	call	$sto32		; Save result
	ret
	;
;	Shift left 32 bit integer
; ENTRY	Reg pair HL points to number
; EXIT	Number shifted left
shl32:
	push	ix
	push	hl
	pop	ix		; Copy pointer
	call	$shl32		; Go shift
	pop	ix
	ret
	;
;	Shift right 32 bit integer
; ENTRY	Reg pair HL points to number
; EXIT	Number shifted right
shr32:
	push	ix
	push	hl
	pop	ix		; Copy pointer
	call	$shr32		; Go shift
	pop	ix
	ret
	;
;***********************************************
;	arithmetic sub's
;***********************************************
;	Add 32 bit integers
; ENTRY	Regs HL,BC hold 2nd operand and result
;	Stack hold 1st number
; EXIT	Result filled with sum
$add32:	
	pop	iy		; Get PC
	ex	(sp),hl		; Get lo
	add	hl,bc		; .. add
	ld	b,h
	ld	c,l
	pop	de		; Get hi
	pop	hl
	adc	hl,de		; .. add, too
	jp	(iy)
	;
;	Subtract 32 bit integers
; ENTRY	Regs HL,BC hold 2nd operand and result
;	Stack hold 1st number
; EXIT	Result filled with difference
$sub32:	
	pop	iy		; Get PC
	ex	(sp),hl
	or	a
	sbc	hl,bc		; Subtract lo
	ld	b,h
	ld	c,l
	pop	de
	pop	hl
	sbc	hl,de		; .. and hi
	jp	(iy)
	;
;	Multiply 32 bit integers
; ENTRY	Regs HL,BC hold 2nd operand and result
;	Stack hold 1st number
; EXIT	Result filled with product
;	Carry set on overflow
$mul32:
	sub	a
	bit	sign,h		; Check sign of 2nd
	jr	z,mulpos1
	call	$neg32		; .. fix it
	ld	a,10000000b
mulpos1:	
	pop	iy
	pop	de
	ex	(sp),hl
	ex	(sp),iy
	push	bc
	ld	b,d
	ld	c,e
	bit	sign,h		; .. same for 2nd
	jr	z,mulpos2
	add	a,11000000b
	call	$neg32
mulpos2:	
	pop	de
	push	af
	push	bc
	push	de
	push	bc
	push	iy
	ld	a,h
	or	l
	jr	z,mul1
	pop	bc
	ld	a,b
	or	c
	jr	nz,mulerr1
	pop	bc
	ld	b,h
	ld	c,l
	call	shft
	jr	mul3
mul1:	
	pop	bc
	pop	de
	ld	a,b
	or	c
	jr	z,mul4
	call	shft
mul3:	
	ld	a,b
	and	SIGNB
	or	h
	or	l
	jr	nz,mulerr2
	ld	h,b
	ld	l,c
mul4:	
	pop	bc
	pop	de
	push	hl
	call	shft
	pop	de
	add	hl,de
	pop	af
	or	a
	call	m,$neg32	; Check result
	or	a
	ret
mulerr1:	
	pop	de
mulerr2:	
	pop	de
	pop	de
	pop	de
	scf			; Set error
	ret
;
; Shift number
;
shft:	
	ld	hl,0
	ld	a,b		; Check part
	or	a
	jr	z,shl1
	ld	b,16		; .. 16 bit
shl3:	
	or	a
	bit	0,c
	jr	z,shl2
	add	hl,de
shl2:	
	rr	h
	rr	l
	rra
	rr	c
	djnz	shl3
	ld	b,a
	ret
shl1:	
	ld	b,8		; .. 8 bit
shl5:	
	or	a
	bit	0,c
	jr	z,shl4
	add	hl,de
shl4:	
	rr	h
	rr	l
	rr	c
	djnz	shl5
	ld	b,l
	ld	l,h
	ld	h,0
	ret
	;
;	Divide 32 bit integers
; ENTRY	Regs HL,BC hold 2nd operand and result
;	Stack hold 1st number
; EXIT	Result filled with product
;	Carry set on zero divisor or overflow
$div32:
	sub	a
	bit	sign,h		; Test sign
	jr	z,divpos1
	call	$neg32
	ld	a,10000000b
divpos1:	
	pop	iy
	pop	de
	ex	(sp),hl
	ex	(sp),iy
	push	bc
	ld	b,d
	ld	c,e
	bit	sign,h		; .. test 2nd
	jr	z,divpos2
	add	a,11000000b
	call	$neg32
divpos2:	
	pop	de
	push	ix
	push	af
	push	bc
	ex	(sp),iy
	pop	bc
	ld	a,b
	or	c
	or	d
	jr	nz,div1
	or	e
	jr	z,div2
	cp	2
	jr	nz,div1
	push	iy
	pop	bc
	pop	af
	sra	h
	rr	l
	rr	b
	rr	c
	jr	div3
div1:	
	call	$fix32
	pop	af
	push	ix
	pop	hl
	push	iy
	pop	bc
div3:	
	rla
	call	c,$neg32
	pop	ix
	or	a
	ret
div2:	
	pop	de
	pop	de
	scf
	ret
	;
;	Get remainder of division of 32 bit integers
; ENTRY	Regs HL,BC hold 2nd operand and result
;	Stack hold 1st number
; EXIT	Result filled with product
;	Carry set on zero divisor or overflow
$mod32:
	ld	a,h
	pop	iy
	pop	de
	ex	(sp),hl
	ex	(sp),iy
	or	a
	jp	m,moderr2
	sub	a
	push	bc
	ld	b,d
	ld	c,e
	bit	sign,h		; Test sign
	jr	z,modpos
	call	$neg32
	or	10000000b
modpos:	
	pop	de
	push	ix
	push	af
	push	bc
	ex	(sp),iy
	pop	bc
	ld	a,b
	or	c
	or	d
	jr	nz,mod1
	or	e
	jr	z,moderr1
	cp	2
	jr	nz,mod1
	push	iy
	pop	hl
	rr	l
	ld	hl,0
	rl	l
	pop	af
	push	hl
	ld	hl,0
	jr	mod2
mod1:	
	call	$fix32
	push	af
	pop	iy
	pop	af
	push	iy
mod2:	
	rla
	jr	nc,mod3
	rla
	jr	nc,mod4
	ld	bc,0
	ld	d,b
	ld	e,b
mod4:	
	ld	a,h
	or	l
	ex	(sp),hl
	or	h
	or	l
	ex	(sp),hl
	jr	z,mod3
	ex	de,hl
	push	bc
	pop	iy
	pop	bc
	or	a
	sbc	hl,bc
	push	iy
	ex	(sp),hl
	sbc	hl,de
mod3:	
	pop	bc
	pop	ix
	ret
moderr1:	
	pop	de
	pop	de
moderr2:	
	scf
	ret
	;
;	Shift left 32 bit integer
; ENTRY	Reg IX points to number
; EXIT	Number set to shifted value
$shl32:	
	sla	(ix+3)		; Shift four bytes
	rl	(ix+2)
	rl	(ix+1)
	rl	(ix+0)
	ret
	;
;	Shift rigth 32 bit integer
; ENTRY	Reg IX points to number
; EXIT	Number set to shifted value
$shr32:	
	srl	(ix+0)		; Shift four bytes
	rr	(ix+1)
	rr	(ix+2)
	rr	(ix+3)
	ret
	;
;	Negate 32 bit integer
; ENTRY	Regs HL,BC hold number
; EXIT	Number holds negated value
$neg32:
	ex	de,hl
	ld	hl,0
	or	a
	sbc	hl,bc		; Negate it
	ld	b,h
	ld	c,l
	ld	hl,0		; .. hi part, too
	sbc	hl,de
	ret
	;
; Prepare registers for 32 bit operation
; ENTRY	See routines
; EXIT	dto
;
; Load regs for requested operation
;
; ENTRY	Reg BC points to 1st number
;	Reg DE points to 2nd number
;	Reg HL points to requested routine
;	On stack address of result
;
; EXIT	Requested operation executed
;
$op32:
	push	ix		; Save a bit
	push	iy
	push	hl
	pop	iy		; Get routine
	push	de
	ld	l,c		; Copy 1st
	ld	h,b
	call	$lod32
	pop	hl		; Get 2nd
	push	de
	push	bc		; Put 1st onto stack
	call	$lod32		; Load 2nd
	ex	de,hl
	ld	de,$ret		; Set return
	push	de
	jp	(iy)		; Start
$ret:
	ex	de,hl
	pop	iy
	pop	ix
	pop	hl		; Get result
	; 
; Save 32 bit number
;
; ENTRY	Regs DE,BC hold number
;	Reg HL points to result
;
; EXIT	Result filled with number
;
$sto32:
	ld	(hl),d		; .. simple store
	inc	hl
	ld	(hl),e
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(hl),c
	ret
	; 
; Load 32 bit number
;
; ENTRY	Reg HL points to number
;
; EXIT	Regs DE,BC holds number
;
$lod32:
	ld	d,(hl)		; .. simple load
	inc	hl
	ld	e,(hl)
	inc	hl
	ld	b,(hl)
	inc	hl
	ld	c,(hl)
	ret
	;
; Subroutine for divide and modulo of 32 bit integers
; ENTRY	Regs <HL,IY> and <BC,DE> hold numbers
; EXIT	A little bit of division performed
$fix32:
	push	hl		; Save entry
	ld	a,h		; Test part
	or	l
	jr	z,fix1		; .. 16 bit
	ld	a,32		; Set 32 bit
	ld	hl,0
	jr	fix2
fix1:	
	ld	a,16
	ex	(sp),iy
fix2:	
	pop	ix
	push	hl
fix6:	
	add	iy,iy		; Now go thru the loop
	rla
	add	ix,ix
	ex	(sp),hl
	adc	hl,hl
	ex	(sp),hl
	adc	hl,hl
	rra
	jr	nc,fix3
	inc	ix
fix3:	
	ex	(sp),hl
	or	a
	sbc	hl,de
	ex	(sp),hl
	sbc	hl,bc
	jp	p,fix4
	ex	(sp),hl
	add	hl,de
	ex	(sp),hl
	adc	hl,bc
	jr	fix5
fix4:	
	inc	iy
fix5:	
	dec	a		; Check done
	jr	nz,fix6
	pop	af
	ret
	;
;***********************************************
;	convert
;***********************************************
	;
;	Convert ASCII decimal string to 32 bit integer
; ENTRY	Reg pair DE points to string
;	Reg pair HL points to 32 bit result
;	Reg B holds end marker of string
;	(usually zero or $)
;	If B holds 255 (hex 0FFH), routine ends at first
;	non numeric character found
; EXIT	Result filled with value
;	Reg pair DE points to delimiter or first
;	non numeric character
;	Carry set on overflow or illegal digit
din32:
	push	bc
	push	hl
	push	hl
	push	de
	push	bc
	ld	hl,..res	; Set temporary value
	ld	(hl),0
	ld	de,..res+1
	ld	bc,res.len-1
	ldir			; Clear it
	pop	bc
	ld	c,0		; Clear counter
	pop	de
	ld	hl,..res
	call	skpblk		; Skip blanks here
dig???:
	ld	a,(de)
	cp	b		; Check end
	jr	z,convert
	call	tstdig		; Should be number here
	jr	nc,nxt.dig
	inc	b		; 0FFH is valid here
	jr	z,convert	; Start
	scf
	jr	end.bin		; .. error
nxt.dig:
	ld	(hl),a		; Save digit
	inc	hl
	inc	c		; Count digits
	inc	de
	jr	dig???
convert:
	ld	a,c
	or	a
	scf			; Check no digit
	jr	z,end.bin
	ld	de,..res
	ld	a,(de)
	ld	($$num+2),a	; Init digit
	inc	de
cnv.loop:
	dec	c
	jr	z,end.dig	; Test end
	call	@copy32		; Copy old value
	call	@shl32		; * 2
	call	nc,@shl32	; * 4
	call	nc,@add32	; * 5
	call	nc,@shl32	; *10
	jr	c,end.bin	; Check overflow
	call	add.dig		; Add new digit
	jr	nc,cnv.loop
	jr	end.bin
end.dig:	
	or	a		; Reset carry
end.bin:
	pop	hl		; Get result pointer
	ld	de,($$num)
	ld	(hl),d		; Save result
	inc	hl
	ld	(hl),e
	inc	hl
	ld	de,($$num+2)
	ld	(hl),d
	inc	hl
	ld	(hl),e
	pop	hl
	pop	bc
	ret
;
; Shift 32-bit number times 2
;
@shl32:
	push	hl
	ld	hl,($$num+2)
	add	hl,hl		; Shift times 2
	ld	($$num+2),hl
	ld	hl,($$num)
	adc	hl,hl
	ld	($$num),hl
	pop	hl
	ret
;
; Copy 32-bit number
;
@copy32:
	push	hl
	ld	hl,($$num)
	ld	($num),hl	; Simple copy
	ld	hl,($$num+2)
	ld	($num+2),hl
	pop	hl
	ret
;
; Add 32-bit numbers
;
@add32:
	push	hl
	push	de
	ld	hl,($$num+2)
	ld	de,($num+2)
	add	hl,de		; Add
	ld	($$num+2),hl
	ld	hl,($$num)
	ld	de,($num)
	adc	hl,de
	ld	($$num),hl
	pop	de
	pop	hl
	ret
;
; Add digit to 32-bit number
;
add.dig:
	ld	a,(de)		; Get new digit
	inc	de
	push	hl
	push	de
	ld	e,a
	ld	d,0
	ld	hl,($$num+2)
	add	hl,de		; Add
	ld	($$num+2),hl
	ld	hl,($$num)
	ld	e,d
	adc	hl,de
	ld	($$num),hl
	pop	de
	pop	hl
	ret
	;
;	Convert ASCII decimal string to 32 bit signed integer
; ENTRY	Reg pair DE points to string
;	Reg pair HL points to 32 bit result
;	Reg B holds end marker of string
;	(usually zero or $)
;	If B holds 255 (hex 0FFH), routine ends at first
;	non numeric character found
; EXIT	Result filled with signed value
;	Reg pair DE points to delimiter or first
;	non numeric character
;	Carry set on overflow or illegal digit
sin32:
	push	hl
	call	skpblk		; No blanks here
	ld	a,(de)
	inc	de
	cp	'-'		; Check sign
	jr	z,din
	cp	'+'
	jr	z,din
	dec	de		; .. fix pointer
	ld	a,'+'
din:
	ld	(dinsin),a	; .. save sign
	call	din32		; Get nummber
	pop	hl
	ret	c		; .. error
	ld	a,(dinsin)
	cp	'+'		; Test <0
	ret	z		; .. no
	ld	a,(hl)
	rla			; Test overflow
	call	nc,neg32	; Make >0
	ret
	;
;	Convert 32 bit integer to ASCII decimal string
; ENTRY	Reg pair HL points to 32 bit number
;	Reg pair DE points to string
;	Reg B holds end marker of string
;	(usually zero or $)
; EXIT	Decimal string closed by character in B
;	Reg pair HL points to close location
dout32:
	push	ix
	push	iy
	push	bc		; Save entry
	push	de
	ld	de,$$num
	ld	bc,4
	ldir			; Get value
	ld	hl,..res
	ld	(hl),0
	ld	e,l
	ld	d,h
	inc	de
	ld	bc,res.len-1
	ldir			; Clear result
	ld	iy,$$dec	; Point to table
	ld	ix,$$num
	ld	b,res.len
	ld	hl,..res
out.loop:
	call	divfix		; Divide by 10**N
	ld	de,-4
	add	iy,de		; next table entry
	djnz	out.loop
	ld	de,..res
	ld	b,res.len-1
	pop	hl		; Get string
loop.1:
	ld	a,(de)		; Check zero
	or	a
	jr	nz,loop.ex
	inc	de
	djnz	loop.1
loop.ex:
	inc	b
loop.2:
	ld	a,(de)		; Unpack
	add	a,'0'
	ld	(hl),a		; .. as ASCII
	inc	de
	inc	hl
	djnz	loop.2
	pop	bc
	ld	(hl),b		; Close string
	pop	iy
	pop	ix
	ret
divfix:
	call	@csub32		; Subtract
	jr	c,div.done	; Check <0
	inc	(hl)		; Count digits
	jr	divfix
div.done:
	call	@cadd32		; Make >0
	inc	hl
	ret
@cadd32:
	ld	a,(ix+3)	; Add four bytes
	add	a,(iy+3)
	ld	(ix+3),a
	ld	a,(ix+2)
	adc	a,(iy+2)
	ld	(ix+2),a
	ld	a,(ix+1)
	adc	a,(iy+1)
	ld	(ix+1),a
	ld	a,(ix+0)
	adc	a,(iy+0)
	ld	(ix+0),a
	ret
@csub32:
	ld	a,(ix+3)	; Subtract four bytes
	sub	(iy+3)
	ld	(ix+3),a
	ld	a,(ix+2)
	sbc	a,(iy+2)
	ld	(ix+2),a
	ld	a,(ix+1)
	sbc	a,(iy+1)
	ld	(ix+1),a
	ld	a,(ix+0)
	sbc	a,(iy+0)
	ld	(ix+0),a
	ret
	;
	db	000h,000h,000h,001h	; 1
	db	000h,000h,000h,00ah	; 10
	db	000h,000h,000h,064h	; 100
	db	000h,000h,003h,0e8h	; 1000
	db	000h,000h,027h,010h	; 10000
	db	000h,001h,086h,0a0h	; 100000
	db	000h,00fh,042h,040h	; 1000000
	db	000h,098h,096h,080h	; 10000000
	db	005h,0f5h,0e1h,000h	; 100000000
$$dec:	db	03bh,09ah,0cah,000h	; 1000000000
	;
;	Convert signed 32 bit integer to ASCII decimal string
; ENTRY	Reg pair HL points to 32 bit number
;	Reg pair DE points to string
;	Reg B holds end marker of string
;	(usually zero or $)
; EXIT	Decimal string closed by character in B
;	Reg pair HL points to close location
sout32:
	bit	sign,(hl)	; Check sign
	jp	z,dout32	; Normal
	ld	a,'-'
	ld	(de),a		; Set sign
	inc	de
	push	hl
	call	neg32		; Make >0
	call	dout32		; Convert
	ex	(sp),hl
	call	neg32		; .. old sign
	pop	hl
	ret
	;
;	Convert 16 bit signed number to 32 bits
; ENTRY	Reg HL holds signed number
;	Reg DE points to location holding 32 bits
; EXIT	Result filled with 32 bits
c1632:
	push	ix
	push	de
	pop	ix
	ld	(ix+2),h	; Set 16 bit part
	ld	(ix+3),l
	xor	a
	bit	sign,(ix+2)	; Check sign
	jr	z,no.sign
	dec	a
no.sign:
	ld	(ix+0),a	; Set hi bits
	ld	(ix+1),a
	pop	ix
	ret
	;
;	Convert 32 bit signed number to 16 bits
; ENTRY	Reg DE points to location holding 32 bits
; EXIT	Reg HL holds signed number
c3216:
	ex	de,hl
	inc	hl
	inc	hl
	ld	d,(hl)		; Get the part
	inc	hl
	ld	e,(hl)
	ex	de,hl
	ret
	;
;	Skip blanks in buffer pointed to by <DE>
; ENTRY	Reg pair DE points to string buffer
; EXIT	Reg pair DE points to first non blank character
skp???:
	inc	de
skpblk:
	ld	a,(de)
	cp	' '		; Test blank
	jr	z,skp???	; Skip it
	cp	tab
	jr	z,skp???	; .. or tab
	ret
	;
;	Test if character in range 0 .. 9 and get binary
; ENTRY	Accu holds character
; EXIT	Carry reset if character in range
;	Carry set if not in range
;	Accu holds binary value on normal return
;	else accu not changed
tstdig:
	cp	'9'+1		; Check digits
	ccf
	ret	c
	cp	'0'
	ret	c
	sub	'0'		; Strip off offset
	ret
	;
;*******************************************************
