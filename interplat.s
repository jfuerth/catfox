*=$c000

.include "catfox_spritenums.s"

screen=$0400
spriteimg=screen+1024-8

; temporary vars and pointers
r0=$f7
r1=$f8
ptr0=$f9
ptr0h=$fa


init
	; sprite 0
	lda #100
	sta $d000 ; spr0 x
	lda #catfox_stand_0
	sta spriteimg+0

install
	sei
	lda #<handleint
	sta $0314
	lda #>handleint
	sta $0315
	cli
	rts

handleint
	.block
	inc $d020

	lda #1
	sta $d015 ; enable spr0

	lda $d001 ; y coord
	sec
	sbc #29
	lsr a
	lsr a
	lsr a
	tay
	
	lda $d000
	sec
	sbc #24
	lsr a
	lsr a
	lsr a
	tax

	jsr getsc

	cmp #$80
	bcc fall

	ldx standcount
	beq walk

stand
	dec standcount
        lda #catfox_stand_0
        sta spriteimg+0

	jmp done

fall
	inc $d001
        lda #catfox_fall_0
        sta spriteimg+0

	lda #15
	sta standcount

	jmp done

walk
	lda walkframe
	tax
	lsr a
	lsr a
	tay
	lda walkframes,y
	sta spriteimg+0
	inx
	cpx #(4*4) ; num of walkframes
	bne walkframesave
	ldx #0
walkframesave
	stx walkframe

	inc $d000 ; TODO high bit

	jmp done

done
	dec $d020
	jmp $ea31

standcount
	.byte 5
walkframe
	.byte 2
walkframes
	.byte catfox_run_0
	.byte catfox_run_1
	.byte catfox_run_2
	.byte catfox_run_3

	.bend

getsc
; gets screencode at xr,yr -> acc
	.block
	; find line addr in table
	tya    ; y coord
	asl a
        tay

	; store scr line addr to ptr0
	lda scrlines,y
	sta ptr0
	iny
	lda scrlines,y
	sta ptr0h
	txa
	tay  ; x coord
	lda (ptr0),y
	rts
	.bend

scrlines
; table of screen line addresses
        .block
line    .var screen
lineloop .lbl 
        .word line
line    .var line+40
        .ifne 1000+screen-line
        .goto lineloop
        .endif
        .bend

; ------------- Movable Objects -----

; mob struct
mobxl=0
mobxh=1
mobdxl=2
mobdxh=3
mobyl=4
mobyh=5
mobdyl=6
mobdyh=7
mobcolr=8
mobstructsz=9

mobupdate
; shift a mob into the vic registers
; in: ptr0 - mob struct pointer
	.block
	; apply dx
	clc
	ldy #mobdxl
	lda (ptr0),y
	ldy #mobxl
	adc (ptr0),y
	sta (ptr0),y
	ldy #mobdxh
	lda (ptr0),y
	ldy #mobxh
	adc (ptr0),y
	sta (ptr0),y

	; apply dy

	; transfer regs to vic

	; store x in sprite reg
	; 16-bit shift 3 places left
	ldy #mobxl
	lda (ptr0),y
	sta r0
	ldy #mobxh
	lda (ptr0),y
	sta r1

	clc
	rol r0
	rol r1
	rol r0
	rol r1
	rol r0
	rol r1

	; overflow to msb
	rol $d010

	; pixel position to x&y regs
	ldx r1
	lda spritenum
	asl a
	tay
	stx $d000,y

	rts
	.bend
