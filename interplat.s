
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

; mob struct
mobxl=0
mobxh=1
mobdxl=2
mobdxh=3
mobyl=4
mobyh=5
mobdyl=6
mobdyh=7
mobcolr=8 ; bit 7 set: disabled
          ; bit 6 set: x-mirrored
mobimg=9
mobalist=10 ; +11
mobaframe=12
mobattl=13 ; countdown current frame
mobact=14 ; +15
mobstructsz=16

ifmobdis .segment
	ldy #mobcolr
	lda (ptr0),y
	and #%10000000
	bne @1
	.endm

; test if current mob is x-mirrored
; jump to label if mob not x-mirrored:
;   #ifmobxm "eq","label"
; jump to label if mob is x-mirrored:
;   #ifmobxm "ne","label"
ifmobxm	.segment
	ldy #mobcolr
	lda (ptr0),y
	and #%01000000
	b@1 @2
	.endm

setmobxm .macro
	ldy #mobcolr
	lda (ptr0),y
	.ifne \1
	ora #%01000000
	.endif
	.ifeq \1
	and #%10111111
	.endif
	sta (ptr0),y
	.endm

; test if current mob's alist is same
; as given alist
; jump to given label if it is
ifalist	.segment
	ldy #mobalist
	lda (ptr0),y
	cmp #<@1
	bne *+7 ; perf: skip hi byte?
	iny
	lda (ptr0),y
	cmp #>@1
	beq @2
	.endm

setalist .segment
	ldy #mobalist
	lda #<@1
	sta (ptr0),y
	iny
	lda #>@1
	sta (ptr0),y
	ldy #mobaframe
	lda #0
	sta (ptr0),y
	.endm

add16	.macro
	clc
	lda \1
	adc #\2
	sta \1
	lda \1+1
	adc #0
	sta \1+1
	.endm

; ----- settings ------
wlkspd=30
jmpspd=50
sitdelay=60
gravity=2

; --------- start of code ---------
*=$c000
	jmp init

handleint
	.block
	inc $d020

	; ack vic interrupt
	lda #$ff
	sta $d019

	lda mobcount
	jsr vicupdate

	inc $d020
	jsr mobupdate
	sta mobcount
	dec $d020

	dec $d020
	jmp $ea31
	
mobcount .byte 0

	.bend	

playeract
.block
	; jsr readjoy
	lda $dc00
	and #%00011111
	cmp #%00011111
	bne notidle

	lda #0
	sta catmob+mobdxl
	sta catmob+mobdxh
	jmp donejoy ; rts

notidle
	.block
ckleft	lda #%00000100
	bit $dc00
	bne done

	lda #<($ffff-wlkspd)
	sta catmob+mobdxl
	lda #>($ffff-wlkspd)
	sta catmob+mobdxh
	#setmobxm 1
done
	.bend

	.block
ckright	lda #%00001000
	bit $dc00
	bne done

	lda #<wlkspd
	sta catmob+mobdxl
	lda #>wlkspd
	sta catmob+mobdxh
	#setmobxm 0
done
	.bend

	.block
ckfire	lda #%00010000
	bit $dc00
	bne done

	lda #<($ffff-jmpspd)
	sta catmob+mobdyl
	lda #>($ffff-jmpspd)
	sta catmob+mobdyh
done
	.bend
donejoy

	; check if mob is on a platform
	ldy #mobxh
	lda (ptr0),y
	tax
	inx ; centre hitbox on sprite

	ldy #mobyh
	lda (ptr0),y
	tay

	jsr getsc ; trashes r3,r4

	cmp #$80
	bcc fall

	; check if not moving
	lda #<catmob
	sta ptr0
	lda #>catmob
	sta ptr0h

	ldy #mobdxh
	lda (ptr0),y
	ldy #mobdxl
	ora (ptr0),y
	bne walk

; --------- player actions ------------
stand
	lda #0
	sta catmob+mobdxl
	sta catmob+mobdxh
	sta catmob+mobdyl
	sta catmob+mobdyh
	sta catmob+mobyl

	jmp done

fall
	inc $d001

	lda catmob+mobdyl
	clc
	adc #gravity
	sta catmob+mobdyl
	lda catmob+mobdyh
	adc #0
	sta catmob+mobdyh

	jmp done

walk	
	jmp done

done
	rts
	.bend

; ---- mob action: stay on platform
platstayact
	.block
	ldy #mobxh
	lda (ptr0),y
	sta r0
	tax

	ldy #mobyh
	lda (ptr0),y
	sta r1
	tay

	inx ; look below middle
	jsr getsc

	; if nothing under: fall
	cmp #$80
	bcs nofall
	ldy #mobdyl
	lda (ptr0),y
	clc
	adc #gravity
	sta (ptr0),y
	iny
	lda (ptr0),y
	adc #0
	sta (ptr0),y
	#ifalist "cffallanim","done"
	#setalist "cffallanim"
	jmp done
nofall
	; stop dy
	ldy #mobdyl
	lda #0
	sta (ptr0),y
	iny
	sta (ptr0),y

	#ifalist "cfwalkanim","contwalk"
	#setalist "cfwalkanim"
contwalk

	; if nothing ahead: reverse dx
	ldx r0 ; xh
	ldy #mobdxh
	lda (ptr0),y
	bmi checkfall
	inx ; look below right side
	inx
checkfall ldy r1
	jsr getsc
	cmp #$80
	bcs done
; no platform ahead: negate mobdx
	lda #0
	sec
	ldy #mobdxl
	sbc (ptr0),y
	sta (ptr0),y
	iny
	lda #0
	sbc (ptr0),y
	sta (ptr0),y

	bmi faceleft
	#setmobxm 0
	jmp done
faceleft
	#setmobxm 1
done	rts
	.bend

getsc
; gets screencode at xr,yr -> acc
; trashes r3&r4
	.block
	; find line addr in table
	tya    ; y coord
	asl a
        tay

	; store scr line addr to ptr0
	lda scrlines,y
	sta r3
	iny
	lda scrlines,y
	sta r4
	txa
	tay  ; x coord
	lda (r3),y
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
	lda #1
	sta catmob+mobxh
	sta catmob+mobyh
	lda #0
	sta catmob+mobdxl
	sta catmob+mobdyh
	sta catmob+mobdyl
	sta catmob+mobdyh

	lda #<cfidleanim
	sta catmob+mobalist
	lda #>cfidleanim
	sta catmob+mobalist+1
	lda #0
	sta catmob+mobaframe
	lda #1
	sta catmob+mobattl

	lda #1
	sta catmob+mobcolr
	
	lda #<playeract
	sta catmob+mobact
	lda #>playeract
	sta catmob+mobact+1

	; testmob setup
	lda #10
	sta testmob+mobxh
	sta testmob+mobyh
	lda #<cfwalkanim
	sta testmob+mobalist
	lda #>cfwalkanim
	sta testmob+mobalist+1
	lda #0
	sta testmob+mobaframe
	lda #1
	sta testmob+mobattl

	lda #2
	sta testmob+mobcolr

	lda #wlkspd
	sta testmob+mobdxl
	lda #0
	sta testmob+mobdxh

	lda #<platstayact
	sta testmob+mobact
	lda #>platstayact
	sta testmob+mobact+1
	
	; tm2 setup
	lda #15
	sta tm2+mobxh
	sta tm2+mobyh
	lda #<cfjumpanim
	sta tm2+mobalist
	lda #>cfjumpanim
	sta tm2+mobalist+1
	lda #0
	sta tm2+mobaframe
	lda #1
	sta tm2+mobattl

	lda #3
	sta tm2+mobcolr
	
	; mirror sprite images
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
	
	lda #251
	sta $d012 ; set raster line num

	lda #$1b
	sta $d011 ; clear 9th bit

	cli
	rts

; --------- animation ---------
cfidleanim
	.byte catfox_stand_0,60
	.byte catfox_sitting_0,4
	.byte catfox_sitting_1,4
	.byte catfox_sitting_2,4
	.byte catfox_sitting_3,250
	.byte 0,4 ; stay on last

cfjumpanim
	.byte catfox_jump_0,2
	.byte catfox_jump_1,0
	.byte 0,1

cfwalkanim
	.byte catfox_run_0,6
	.byte catfox_run_1,8
	.byte catfox_run_2,10
	.byte catfox_run_3,6
	.byte 0,0 ; goto frame 0

cffallanim
	.byte catfox_fall_0,2
	.byte catfox_fall_1,2
	.byte 0,0 ; goto frame 0

; --------- mobs ---------

; --- mob structs
catmob	.repeat mobstructsz,$00
testmob	.repeat mobstructsz,$00
tm2	.repeat mobstructsz,$00

; --- mob pointers
mobtab
	.word catmob
	.word testmob
	.word tm2

mobtabsz .byte 3

; --- mob routine state ---
spritenum
	.byte 0

; --------- mob update ---------
mobupdate
; process actions, physics, and
; animation for all active mobs.
; can be called during screen draw.
; changes are made visible by calling
; vicupdate.
; returns:
;  a - # of active mobs (<=8,<=mobtabsz)
;      which is # vic sprites needed
	.block
	ldx #0
	stx mobtabpos
	stx activemobs

	; this loop counts up from 0
updateloop
	lda mobtabpos
	cmp mobtabsz
	beq done
	asl a ; *2 for word offset
	tax
	lda mobtab,x
	sta ptr0
	inx
	lda mobtab,x
	sta ptr0+1
	#ifmobdis "skip"
	inc activemobs
	jsr mobupdate1
skip	inc mobtabpos
	bne updateloop ; always taken

done	ldx activemobs
	txa
	rts
mobtabpos .byte 0
activemobs .byte 0

mobupdate1
	; --- custom action

	; "jsr mobact" trampoline:
	; load "jmp mobact" into r0,r3
	; then jsr to that
	ldy #mobact
	lda (ptr0),y
	sta r1
	iny
	lda (ptr0),y
	sta r2
	ora r1
	beq skipact ; hi and low are 0

	lda #$4c ; direct jmp opcode
	sta r0
	jsr r0
skipact

	; --- physics
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

	cmp #40 ; check for wrap
	bmi savex
	lda #(0-3)
savex
	sta (ptr0),y
	
	; apply dy
	clc
	ldy #mobdyl
	lda (ptr0),y
	ldy #mobyl
	adc (ptr0),y
	sta (ptr0),y
	ldy #mobdyh
	lda (ptr0),y
	ldy #mobyh
	adc (ptr0),y

	cmp #26 ; check for wrap
	bmi savey
	lda #(0-3)
savey
	sta (ptr0),y

	; --- animation
	ldy #mobattl
	lda (ptr0),y
	sec
	sbc #1
	beq nextaframe
	sta (ptr0),y
	jmp donealist

nextaframe
	; set animation frame
	; put anim list ptr in r3/r4
	ldy #mobalist
	lda (ptr0),y
	sta r3
	iny
	lda (ptr0),y
	sta r4

	; fetch current anim list instr
	; into r0/r1
fetchanimcode
	ldy #mobaframe
	lda (ptr0),y
	sta r2 ; current list offset
	tay
	lda (r3),y
	sta r0 ; command/imgnum
	iny
	lda (r3),y
	sta r1 ; cmd arg/frame count
	
	ldx r0
	bne newimg

	; process command
	; goto frame in r1 (still in a)
	asl ; frame offset is frame*2
	ldy #mobaframe
	sta (ptr0),y

	; try again with new aframe offs
	jmp fetchanimcode
	
newimg
	; update list position
	clc
	lda r2
	adc #2
	ldy #mobaframe
	sta (ptr0),y
	
	; set frame ttl counter
	lda r1
	ldy #mobattl
	sta (ptr0),y

	; update mobimg
	lda r0 ; new image num
	ldy #mobimg
	sta (ptr0),y

donealist ; end of alist processing
	rts
	.bend

; --------- vic update ---------
vicupdate
; shift all active mobs into the vic
; registers.
; no side effects on mob data.
; call during VBL.
; inputs:
; a - count of live mobs
	.block

	; store # live mobs in spritenum
	; start at count-1 because it's
	; a 0-based index
	tax
	dex
	stx spritenum ; count-1

	ldx mobtabsz
	txa
	asl a
	sec
	sbc #1
	sta mobtabpos ; (count*2)-1

	lda #0
	sta $d010 ; sprite hi x bits
	sta $d015 ; sprite enable

	; loop counts down from mobtabsz
updateloop
	ldx mobtabpos
	cpx #$ff
	beq done
	lda mobtab,x
	sta ptr0+1
	dex
	lda mobtab,x
	sta ptr0
	dex
	stx mobtabpos
	#ifmobdis "updateloop"
	jsr vicupdate1
	dec spritenum
	jmp updateloop
	
done	rts
mobtabpos .byte 0

vicupdate1
; in: ptr0 - mob struct pointer
;     spritenum - vic sprite number
;     (call in desc order)


	; calc pixel positions

	; calc x pixel pos -> r1
	; 16-bit shift 3 places left

	; merge in x-offset of 24
	; into high byte (24>>3 = 3)
	; can ignore lo byte because
	; xoffs is exactly 3 chars wide
	; and hi byte counts chars
	ldy #mobxl
	lda (ptr0),y
	sta r0
	ldy #mobxh
	lda (ptr0),y
	clc
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
	ldy #mobyl
	lda (ptr0),y
	sta r2
	ldy #mobyh
	lda (ptr0),y
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

	; pixel position to hw regs
	lda spritenum
	asl a ;for interleaved xy coords
	tay

	lda r1 ; x coord
	sta $d000,y

	lda r3 ; y coord
	sta $d001,y

	; set colour
	ldy #mobcolr
	lda (ptr0),y
	ldy spritenum
	sta $d027,y

	; set hw sprite to mobimg
	ldy #mobimg
	lda (ptr0),y
	tax
	#ifmobxm "eq","noflip"
	clc
	txa ; macro wiped a
	adc #numsprites
	tax
noflip	txa
	ldy spritenum
	sta spriteimg,y

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


