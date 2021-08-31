*=$c000

.include "catfox_spritenums.s"

screen=$0400
spriteimg=screen+1024-8

; temporary vars and pointers
r0=$f7
r1=$f8
r2=$f9
r3=$fa
ptr0=$fb
ptr0h=$fc

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
        lda #catfox_fall_0
        sta catmob1+mobimg

	lda #15
	sta standcount

	lda #$30
	sta catmob1+mobdyl
	lda #0
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

install
	sei
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

; --- catmob1
catmob1
	.repeat mobstructsz,$00

; --- mob routine state ---
spritenum
	.byte 0

; -------- mob routines --------

mobupdate
; shift a mob into the vic registers
	.block
	lda #0
	sta spritenum
	sta $d010 ; sprite hi x bits
	sta $d015 ; sprite enable
	lda #<catmob1
	sta ptr0
	lda #>catmob1
	sta ptr0+1
	jsr updateone
	
	rts

updateone
; in: ptr0 - mob struct pointer
;     spritenum - vic sprite number
;     (must be in order staring with 0)

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
	sta (ptr0),y
	sta r3

	; TODO wrap x at 40, y at 25

	; calc pixel positions

	; calc x pixel pos -> r1
	; 16-bit shift 3 places left

	; merge in x-offset of 24
	; into high byte (24<<3 = 3)
	clc
	lda r1
	adc #3
	sta r1

	clc
	rol r0
	rol r1
	rol r0
	rol r1
	rol r0
	rol r1

	; x overflow to msb
	rol $d010

	; calc y pixel pos -> r3
	lda r2
	clc
	adc #$a0 ;hi bits y offset 29>>3
	sta r2
	lda r3
	adc #$03 ; y offset 29>>3
	sta r3

	clc
	rol r2
	rol r3
	rol r2
	rol r3
	rol r2
	rol r3

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
