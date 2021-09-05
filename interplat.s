
.include "catfox_spritenums.s"

screen=$0400
spriteimg=screen+1024-8

; temporary vars and pointers
r0=$f7
r1=$f8
r2=$f9
r3=$fa
r4=$fb
ptr0=$fc
ptr0h=$fd

add16	.macro
	clc
	lda \1
	adc #\2
	sta \1
	lda \1+1
	adc #0
	sta \1+1
	.endm

; --------- start of code ---------
*=$c000
	jmp init

handleint
	.block
	inc $d020

	; ack vic interrupt
	lda #$ff
	sta $d019

	lda #<catmob1
	sta ptr0
	lda #>catmob1
	sta ptr0+1

	ldy #mobxh
	lda (ptr0),y
	tax

	ldy #mobyh
	lda (ptr0),y
	tay


	jsr getsc

	cmp #$80
	bcc fall

	ldx standcount
	beq walk


; --------- actions ------------
stand
	dec standcount
        lda #catfox_stand_0
        sta catmob1+mobimg

	lda #0
	sta catmob1+mobdxl
	sta catmob1+mobdxh
	sta catmob1+mobdyl
	sta catmob1+mobdyh

	jmp done

fall
	inc $d001
        lda #catfox_fall_0 + numsprites
        sta catmob1+mobimg

	lda #15
	sta standcount

	lda catmob1+mobdyl
	clc
	adc #2
	sta catmob1+mobdyl
	lda catmob1+mobdyh
	adc #0
	sta catmob1+mobdyh

	jmp done

walk	; TODO handle anim in mobupdate
	lda catmob1+mobanimframe
	tax
	lsr a
	lsr a
	tay
	lda walkframes,y
	sta catmob1+mobimg
	inx
	cpx #(4*4) ; num of walkframes
	bne walkframesave
	ldx #0
walkframesave
	stx catmob1+mobanimframe

	; set walk speed
	lda #$20
	sta catmob1+mobdxl
	lda #0
	sta catmob1+mobdxh

	jmp done

done
	jsr mobupdate

	dec $d020
	jmp $ea31

standcount
	.byte 5
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

; ------- one time setup --------
init
	; catfox mob setup
	lda #20
	sta catmob1+mobxh
	sta catmob1+mobyh
	lda #catfox_stand_0
	sta catmob1+mobimg

	; testmob setup
	lda #10
	sta testmob+mobxh
	sta testmob+mobyh
	lda #firstsprite
	sta testmob+mobimg
	lda #1
	sta testmob+mobcolr
	
	lda #numsprites
	ldx #<(firstsprite*64)
vicram=>(screen & $C000) ; top 2 bits
	ldy #>((firstsprite*64) . vicram)
	sei

	; has to be done with intrpt
	; disabled because it uses zp
	jsr mirrorsprites

install
	lda #<handleint
	sta $0314
	lda #>handleint
	sta $0315

	lda #$7f
	sta $dc0d ; set to clear timer
	lda $dc0d ; ack pending intrpt

	lda #1
	sta $d01a ; enable raster irq
	
	lda #140
	sta $d012 ; set raster line num

	lda #$1b
	sta $d011 ; clear 9th bit

	cli
	rts

; --------- mob structs ---------

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
mobimg=9
mobanimframe=10
mobstructsz=11

; --- mob structs
catmob1	.repeat mobstructsz,$00
testmob	.repeat mobstructsz,$00

; --- mob pointers
mobtab
	.word catmob1
	.word testmob
	.word 0

; --- mob routine state ---
spritenum
	.byte 0

; -------- mob routines --------

mobupdate
; shift a mob into the vic registers
	.block
	lda #0
	sta $d010 ; sprite hi x bits
	sta $d015 ; sprite enable

	lda #1
	sta spritenum

	lda #<catmob1
	sta ptr0
	lda #>catmob1
	sta ptr0+1
	jsr updateone
	
	;TODO use mobtab/mobtabsz
	dec spritenum
	lda #<testmob
	sta ptr0
	lda #>testmob
	sta ptr0+1
	jsr updateone

	rts

updateone
; in: ptr0 - mob struct pointer
;     spritenum - vic sprite number
;     (call in desc order)

	; apply dx
	clc
	ldy #mobdxl
	lda (ptr0),y
	ldy #mobxl
	adc (ptr0),y
	sta (ptr0),y
	sta r0
	ldy #mobdxh
	lda (ptr0),y
	ldy #mobxh
	adc (ptr0),y

	cmp #40 ; check for wrap
	bmi savex
	lda #(0-3)
savex
	sta (ptr0),y
	sta r1
	
	; apply dy
	clc
	ldy #mobdyl
	lda (ptr0),y
	ldy #mobyl
	adc (ptr0),y
	sta (ptr0),y
	sta r2
	ldy #mobdyh
	lda (ptr0),y
	ldy #mobyh
	adc (ptr0),y

	cmp #26 ; check for wrap
	bmi savey
	lda #(0-3)
savey
	sta (ptr0),y
	sta r3

	; calc pixel positions

	; calc x pixel pos -> r1
	; 16-bit shift 3 places left

	; merge in x-offset of 24
	; into high byte (24>>3 = 3)
	; can ignore lo byte because
	; xoffs is exactly 3 chars wide
	clc
	lda r1
	adc #3

	; expand fixedpt pos to px in r1
	clc
	rol r0
	rol a
	rol r0
	rol a
	rol r0
	rol a
	sta r1

	; x overflow to msb
	rol $d010

	; calc y pixel pos -> r3
	; don't need fancy prescaled
	; offset because we don't save
	; the carry for y
	lda r3
	clc
	rol r2
	rol a
	rol r2
	rol a
	rol r2
	rol a

	adc #29 ; sprite y offset
	sta r3

	; enable this sprite
	sec
	rol $d015

	; set colour
	ldy #mobcolr
	lda (ptr0),y
	ldy spritenum
	sta $d027,y

	; set animation frame
	; TODO animate
	ldy #mobimg
	lda (ptr0),y
	ldy spritenum
	sta spriteimg,y

	; pixel position to x&y regs
	lda spritenum
	asl a ;for interleaved xy coords
	tay

	lda r1 ; x coord
	sta $d000,y

	lda r3 ; y coord
	sta $d001,y

	rts

	.bend

mirrorsprites
; mirrors single colour sprite images
; x & y - lo and hi bytes of 1st sprite
; a - number of sprites to flip
;     (also offset of flipped sprites)
	.block

	stx ptr0 ; source pointer
	sty ptr0h

	sta r3 ; overall loop count

	; set first dest line addr
	; = ptr0 + 64*a
	; using >>2 rather than <<6
	ldx #0
	stx r0
	lsr a
	ror r0
	lsr a
	ror r0 ; lo = 2 LSB of a in MSB
	sta r1 ; hi = >(a << 6)

	clc
	lda ptr0
	adc r0
	sta dstaddr+1
	lda ptr0h
	adc r1
	sta dstaddr+2

flipsprite
	ldx #21
	stx r4  ; lines per sprite

flipline
	ldy #2  ; bytes per line
	lda (ptr0),y
	ldx #7
shiftr0	asl a
	ror r0
	dex
	bpl shiftr0

	dey
	lda (ptr0),y
	ldx #7
shiftr1	asl a
	ror r1
	dex
	bpl shiftr1

	dey
	lda (ptr0),y
	ldx #7
shiftr2	asl a
	ror r2
	dex
	bpl shiftr2

	ldx #2
writedst
	lda r0,x
dstaddr	sta $ffff,x ; addr set by code
	dex
	bpl writedst	

	; inc ptrs by 3 (1 line)
	#add16 ptr0,3
	#add16 dstaddr+1,3

	dec r4
	bne flipline
	
	; inc ptrs past unused byte
	#add16 ptr0,1
	#add16 dstaddr+1,1

	dec r3
	bne flipsprite

	rts
	.bend


