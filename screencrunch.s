screen=$0400
screenend=$0400+(40*25)

r0=$f7
r1=$f8
r2=$f9
r3=$fa
r4=$fb
ptr0=$fc
ptr0h=$fd

	;chars*=$0334 ; tape buffer ends at $03fb
	*=$1000
	jmp crunch
	jmp decrunch

crunch
.block
	;; --- setup for testing
	; target addr for crunched data
	lda #<$3000
	sta r0
	lda #>$3000
	sta r1

	lda #<screen
	sta ptr0
	lda #>screen
	sta ptr0+1

	; --- actual routine starts here

	lda #$ff
	sta runlim+1

	; count repeated chars from current pos
loop	ldy #0
	lda (ptr0),y

countrun
	iny
	cmp (ptr0),y
	bne diff
runlim	cpy #$ff ; selfmod: limit run length to end of screen (reset to ff at start)
	bne countrun
	beq writerun ; always taken

diff
	cpy #3
	bcc writeone

writerun
	; >3 repeats: write $ff <count> <value>
	pha ; value
	tya
	pha ; count
	lda #$ff
	jsr putch ; RLE code
	pla
	jsr putch ; count
	pla
	jsr putch ; value
	jmp next	
	
writeone
	; not enough repeats: write current char
	ldy #1

	; if it's a command byte ($ff or $fe)
	cmp #$fe
	bcs writeesc

	; regular char: just write it
	jsr putch
	jmp next

	; metachar: write it as $ff 1 <value>
writeesc
	ldy #1
	jmp writerun	

next	; ptr0 += y
	tya
	clc
	adc ptr0
	sta ptr0
	lda #0
	adc ptr0+1
	sta ptr0+1

	; if ptr0 > screenend, we are done
	sec
	lda #<screenend
	sbc ptr0
	tax ; remember lo byte
	lda #>screenend
	sbc ptr0+1
	bne loop     ; >255 to go

	txa ; recall lo byte
	sta runlim+1 ; <=255 to go
	bne loop     ; >0 to go

	rts

putch	; write a to output
	; preserves y
	sty r2
	ldy #0
	sta (r0),y
	inc r0
	bne nc
	inc r1
nc	ldy r2
	rts
.bend

; ----------- decrunch
decrunch
	.block
	lda #<screen
	sta r0
	lda #>screen
	sta r1

	lda #<$3000
	sta ptr0
	lda #>$3000
	sta ptr0+1

loop	ldy #0
	lda (ptr0),y
	cmp #$ff
	beq dorun

dochar	sta (r0),y
	ldx #1
	ldy #1
	bne next

dorun	iny
	lda (ptr0),y
	tax ; loop count
	iny
	lda (ptr0),y ; char
	pha
	txa
	tay
	pla
runlp	dey
	sta (r0),y
	bne runlp

	; restore original loop count for next
	txa
	tay
	ldx #3 ; skip next 3 input bytes

next	; r0 += y
	clc
	tya
	adc r0
	sta r0
	lda #0
	adc r1
	sta r1

	; stop if we pass screenend
	lda r1
        cmp #>screenend
        bcc more 
        lda r0
        cmp #<screenend
        bcc more
        rts

more
	; ptr0 += x
	txa
	clc
	adc ptr0
	sta ptr0
	lda #0
	adc ptr0+1
	sta ptr0+1
	jmp loop
	.bend
