
.include "globals.s"
.include "target/sprites_nums.s"
.include "mobsupport.s"
.include "math16.s"

catmob=$800
cpmob=$810

; VIC sprite image pointers
spriteimg=screen+1024-8

; vic bank base addr
vb=screen & $c000

; buffer area for cleaned color ram
; when saving a screen
cleancolorbuf=$0400

spriteyoffs=29 ; y pos is bottom of spr

; ----- settings ------
wlkspd=85
jumpimpulse=20 ; first frame only
jumpboost=10   ; subsequent frames
jumpframes=8   ; boost frame limit
gravity=3      ; standard gravity
fallgrav=40    ; gravity when falling
coyoteframes=5 ; jump after walkoff
friction=20    ; left/right slowdown
sitdelay=120

; --------- start of code ---------
*=entrypoint
	jmp init

handleint
	.block
	inc $d020

	; ack vic interrupt
	lda #$ff
	sta $d019

	; game engine stopped?
	lda gamestop
	bne doneint

	lda mobcount
	jsr vicupdate

	inc $d020
	jsr mobupdate
	sta mobcount
	dec $d020

doneint	dec $d020
	jmp $ea31
	.bend

; ------ global vars ------
mobcount .byte 0
gamestop .byte 0
wantscr	.word $0000
havescr .word $ffff
; PERF: move to ZP if not returning
;       to BASIC

playeract
.block
	; check enemy collision
	; and colorcycle while hit
	lda $d01e
	and #%00000001
	beq nocoll
	ldx catmob+mobcolr
	txa
	and #$f0
	sta r0 ; preserve flags
	inx
	txa
	and #$0f
	ora r0 ; mix in flags
	sta catmob+mobcolr
	jmp donecoll
nocoll	lda catmob+mobcolr
	and #$f0
	ora #1
	sta catmob+mobcolr

donecoll
	; check for edge of screen
	ldx catmob+mobxh
	cpx #0
	beq scrleft
	cpx #38
	beq scrright
	ldy catmob+mobyh
	cpy #$ff ; above top
	beq scrup
	cpy #24
	beq scrdown

	jmp dostates

scrleft
	; dec x unless at left of world
	ldx wantscr
	beq donescr
	dex
	stx wantscr

	; move player to rhs
	lda #$ff
	ldx #37
	#mobstax "xl"
	bne donescr ; always
	
scrright
	; inc x unless at right of world
	ldx wantscr
	inx
	beq donescr
	stx wantscr

	; move player to lhs
	lda #0
	ldx #1
	#mobstax "xl"
	bne donescr ; always

scrup
	; inc y unless at top of world
	ldy wantscr+1
	iny
	beq donescr
	sty wantscr+1

	; move player to bottom
	lda #0
	ldx #23
	#mobstax "yl"
	bne donescr ; always

scrdown
	; dec y unless at top of world
	ldy wantscr+1
	beq donescr
	dey
	sty wantscr+1

	; move player to top
	lda #0
	ldx #1
	#mobstax "yl"
	bne donescr ; always

donescr


; process inputs and bg interactions
; based on current state

dostates
	#ifalist "cfidleanim","isidle0"
	#ifalist "cfwalkanim","iswalk0"
	#ifalist "cfjumpanim","isjump"
	#ifalist "cffallanim","isfall"
	brk ; TODO break vector

isidle0	jmp isidle
iswalk0	jmp iswalk

isjump	; ------------
	.block
	jsr applygravity
	; if +ve dy, we are falling
	bmi rising

	; extra fall gravity
	lda #<fallgrav
	sta catmob+mobdyl
	lda #>fallgrav
	sta catmob+mobdyh

	#setalist "cffallanim"
	bmi done
rising
	dec jumpttl
	bmi done
	lda #jfire
	bit $dc00
	bne done
	; TODO consider #mobadd8/mobadd16
	#mobldax "dyl"
	sec
	sbc #jumpboost
	sta (mobptr),y
	txa
	iny
	sbc #0
	sta (mobptr),y
done
	jmp donestates
	.bend

isfall	; ------------
	.block
	jsr applygravity

	jsr chkplatform ; trashes r3,r4
	bpl falling

landed
	; stop falling
	lda #0
	ldx #0
	#mobstax "dyl"
	#mobsta "yl" ; stay on top

	#setalist "cfidleanim"
	bne done

falling
	ldx coyotettl
	dex
	bmi done
	stx coyotettl
ckfire
	lda #jfire
	bit $dc00
	bne done
	jsr dojump
	jmp done
done
	jmp donestates
	.bend

isidle	; ------------
	; check if started moving
	#moblda "dxh"
	sta r1
	#moblda "dxl"
	ora r1
	beq idlewalk
	#setalist "cfwalkanim"
	jmp idlewalk

iswalk	; --------------
	; check if still moving
	#moblda "dxh"
	sta r1
	#moblda "dxl"
	ora r1
	bne idlewalk
	#setalist "cfidleanim"

; idle and walk are the same from here
idlewalk

	.block
ckfire
	lda #jfire
	bit $dc00
	bne done
	jsr dojump
done
	.bend

	.block
	; check if mob is on a platform
	jsr chkplatform
	bmi done ; still on a platform
	lda #coyoteframes
	sta coyotettl
	#setalist "cffallanim"
done
	.bend
	jmp donestates

; check if catfox mob is on platform.
; consider screencodes below it.
; in: nothing (uses catfox mob struct)
; out: a <- OR of screencodes considered
; out: N flag is set if on plat
;   jsr chkplatform
;   bpl noplatform
;   ; handle "on platform"
chkplatform
	.block
	; get screencodes starting at
	; mobx+1 through mobx+2
	; and current y addr

	lda catmob+mobyh
	asl a
	tay
	lda scrlines,y
	sta r3
	iny
	lda scrlines,y
	sta r4

	ldy catmob+mobxh
	iny ; offset from left of sprite

	; if any have high bit set,
	; we are on a platform
	; (need to OR them)
	lda (r3),y
	iny
	ora (r3),y
	rts
	.bend

dojump
	.block
	lda #<($ffff-jumpimpulse)
	sta catmob+mobdyl
	lda #>($ffff-jumpimpulse)
	sta catmob+mobdyh
	
	lda #jumpframes
	sta jumpttl
	lda #0
	sta coyotettl

	#setalist "cfjumpanim"
	rts
	.bend

donestates
	
; all states:
; apply joystick l/r and x-mirroring
	.block
ckleft	lda #jleft
	bit $dc00
	bne done

	lda #<($ffff-wlkspd)
	ldx #>($ffff-wlkspd)
	#mobstax "dxl"
	#setmobxm 1
done
	.bend

	.block
ckright	lda #jright
	bit $dc00
	bne done

	lda #<wlkspd
	ldx #>wlkspd
	#mobstax "dxl"
	#setmobxm 0
done
	.bend

	; friction: pull dx toward 0
applyfriction
	.block
	#moblda "dxh"
	bmi goingleft
goingright
	#moblda "dxl"
	beq donefriction ; not moving
	sec
	sbc #friction
	sta (mobptr),y
	#moblda "dxh"
	sbc #0
	sta (mobptr),y
	; if 0 crossed, goingleft will
	; clamp to 0 next frame
	jmp donefriction

goingleft
	#moblda "dxl"
	clc
	adc #friction
	sta (mobptr),y
	#moblda "dxh"
	adc #0
	sta (mobptr),y
	; if 0 crossed, clamp to 0
	bcc donefriction
	lda #0
	sta (mobptr),y
	dey
	sta (mobptr),y
	jmp donefriction

donefriction
	.bend

	rts
	.bend

; apply pull of gravity to current mob
; mobptr -> pointer to current mob
; a <- high byte of mobdy
; N, V, Z, C flags <- reflects mobdy
; trashes y
applygravity
	.block
	#moblda "dyl"
	clc
	adc #gravity
	sta (mobptr),y
	#moblda "dyh"
	adc #0
	sta (mobptr),y
	rts
	.bend

; ---- mob action: stay on platform
platstayact
	.block

	ldy #mobyh
	lda (mobptr),y
	sta r1 ; screen y coord

	jsr pointsc ; screen line addr r3

	ldy #mobxh
	lda (mobptr),y
	sta r0 ; screen x coord
	tay
	iny ; middle of sprite

	lda (r3),y ; screen code below mob

	; if nothing under: fall
	cmp #$80
	bcs nofall
	jsr applygravity
	#ifalist "cffallanim","falling"
	#setalist "cffallanim"
falling
	jmp done
nofall
	; stop dy
	ldy #mobdyl
	lda #0
	sta (mobptr),y
	iny
	sta (mobptr),y
	ldy #mobyl
	sta (mobptr),y

	#ifalist "cfwalkanim","contwalk"
	#setalist "cfwalkanim"
contwalk

	; if at edge of screen: reverse dx
	ldx r0 ; xh
	cpx #0
	bcc turnback
	cpx #38
	bcs turnback

	; if nothing ahead: reverse dx
	#moblda "dxh"
	bmi checkcliff
	inx ; look below right side
	inx
checkcliff
	txa
	tay
	lda (r3),y ; stepping toward this
	cmp #$80
	bcs done

turnback
	lda #0
	sec
	ldy #mobdxl
	sbc (mobptr),y
	sta (mobptr),y
	iny
	lda #0
	sbc (mobptr),y
	sta (mobptr),y

	bmi faceleft
	#setmobxm 0
	jmp done
faceleft
	#setmobxm 1
done	rts
	.bend

; ---- mob action: fly back and forth
lrflyact
	.block
	; TODO fill this in
	.bend

; ---- p2 copy/paste controls ----
cpstate	.byte 0
cpsoff	= 0
cpssel	= 1
cpsplc	= 2
	
cpact
; mob action: copy/paste controls
	.block
r	lda #jright
	bit $dc01
	bne l
	inc cpmob+mobxh

l	lda #jleft
	bit $dc01
	bne u
	dec cpmob+mobxh

u	lda #jup
	bit $dc01
	bne d
	dec cpmob+mobyh

d	lda #jdown
	bit $dc01
	bne f
	inc cpmob+mobyh

f	lda #jfire
	bit $dc01
	bne done
	; lay down a character here
	#moblda "yh"
	jsr pointsc
	#moblda "xh"
	tay
	lda #$bf ; reverse "?"
	sta (r3),y
	; affect colour
	clc
	lda r4
	adc #($d8-$48)
	sta r4
	lda #7
	sta (r3),y

done
	rts
; need 4 mobs: tl, tr, bl, br
; modes:
;  no fire: moving with same size
;  fire+move: resize (move bl and br)
;  C= X: cut
;  C= C: copy
;  C= V: paste
;  C= N: save as new stamp
	.bend

pointsc
; point r3&r4 at at screen line y
; example:
;  ldy #5  ; row 5 from top
;  jsr pointsc
;  lda #somecharcode
;  ldy #20 ; column 20
;  sta (r3),y
; a -> vertical char pos 0..24
; r3&r4 <- pointer to start of line
; trashes y
	.block
	; find line addr in table
	asl a
    tay

	; store scr line addr to r3/r4
	lda scrlines,y
	sta r3
	iny
	lda scrlines,y
	sta r4
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
	; set VIC bank
	lda $dd00
	and #%11111100

	; VIC banks numbered in reverse
	; $0000..$3fff : %xxxxxx11
	; $4000..$7fff : %xxxxxx10
	; $8000..$bfff : %xxxxxx01
	; $c000..$ffff : %xxxxxx00
	ora #(3-(vb/$4000))
	sta $dd00

	charbank=(charset-vb)/$800
	scrbank=(screen-vb)/$400
	lda #(scrbank*16) . (charbank*2)
	sta $d018

	; blue background
	lda #14
	sta $d021

	; multi-color mode
	lda $d016
	ora #$f0
	sta $d016
	lda #7
	sta $d022
	lda #9
	sta $d023

	; reset globals
	; (in case of restart)
	lda #0
	sta mobcount
	sta wantscr
	sta wantscr+1

	lda #$ff ; diff from wantscr
	sta havescr
	sta havescr+1

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

	; copy/paste mob setup
	ldx #mobstructsz-1
	lda #0
loop
	sta cpmob,x
	dex
	bpl loop

	lda #10
	sta cpmob+mobxh
	sta cpmob+mobyh
	
	lda #7
	sta cpmob+mobcolr
	
	lda #cursor1x1_1
	sta cpmob+mobimg

	lda #<cpact
	sta cpmob+mobact
	lda #>cpact
	sta cpmob+mobact+1

	; terminate mobtab with xh=80
	lda #$80
	sta cpmob+mobstructsz
	sta cpmob+mobstructsz+1

	; mirror sprite images
	; has to be done with intrpt
	; disabled because it uses zp
	sei
	lda #(sprites_last - sprites_first)
	ldx #<((sprites_first*64) . vb)
	ldy #>((sprites_first*64) . vb)
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

; ------- done init
; the CPU spends its non-interrupt
; time in this loop, checking if we
; need to load a new screen
; and loading it if so.
ioloop
	sei
	jsr chkneedload
	bcs loadnow
	cli

	; user wants to save?
	jsr GETIN
	cmp #"\"
	beq savenow

	jmp ioloop
	
loadnow
	inc gamestop
	jsr loadscr

	; prepare for next vicupdate
	; (mobtab has been modified)
	jsr mobupdate
	sta mobcount

	sei
	ldx wantscr
	ldy wantscr+1
	stx havescr
	sty havescr+1
	cli
	dec gamestop
	jmp ioloop

; === START only needed for edit mode
savenow
	inc gamestop

	ldx havescr
	ldy havescr+1
	jsr savescr

	dec gamestop
	jmp ioloop
; === END only needed for edit mode

chkneedload
; checks if havescr != wantscr
; sets carry if they are different
; X <- lo byte of wantscr
; Y <- hi byte of wantscr
; C <- 1:have!=want; 0:have==want
	.block
	ldx wantscr
	ldy wantscr+1
	cpx havescr
	bne differ
	cpy havescr+1
	bne differ
	clc
	rts

differ	sec
	rts
	.bend
	

; --------- animation ---------
cfidleanim
	.byte catfox_stand_1,60
	.byte catfox_sitting_1,4
	.byte catfox_sitting_2,4
	.byte catfox_sitting_3,4
	.byte catfox_sitting_4,250
	.byte 0,4 ; stay on last

cfjumpanim
	.byte catfox_jump_1,10
	.byte catfox_jump_2,10
	.byte 0,1

cfwalkanim
	.byte catfox_run_1,6
	.byte catfox_run_2,8
	.byte catfox_run_3,10
	.byte catfox_run_4,6
	.byte 0,0 ; goto frame 0

cffallanim
	.byte catfox_fall_1,4
	.byte catfox_fall_2,4
	.byte 0,0 ; goto frame 0

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
;  a - # of active mobs (<=8)
;      which is # vic sprites needed
	.block
	ldx #0
	stx activemobs

	; start pointing at mob 0
	lda #<mobtab
	sta mobptr
	lda #>mobtab
	sta mobptr+1

	; move mobptr through mobtab until
	; we see xh=80
updateloop
	#moblda "xh"
	cmp #$80
	beq done
	#ifmobdis "nextmob"
	inc activemobs
	; TODO mark temporary out of bounds (maybe xh=$fe)
	jsr mobupdate1
nextmob
	#add16ai mobptr,mobstructsz
	bne updateloop ; always taken

done
	ldx activemobs
	txa ; return mobcount
	rts
activemobs .byte 0

mobupdate1
	; --- custom action

	; "jsr mobact" trampoline:
	; jsr to mob's custom routine
	; unless routine is $0000
	ldy #mobact
	lda (mobptr),y
	sta actjsr+1
	iny
	lda (mobptr),y
	sta actjsr+2
	ora actjsr+1
	beq skipact ; hi and low are 0

actjsr	jsr $ffff ; selfmod
skipact

	; --- physics
; apply dx
	clc
	ldy #mobdxl
	lda (mobptr),y
	ldy #mobxl
	adc (mobptr),y
	sta (mobptr),y
	ldy #mobdxh
	lda (mobptr),y
	ldy #mobxh
	adc (mobptr),y
	sta (mobptr),y
	
; apply dy
	clc
	ldy #mobdyl
	lda (mobptr),y
	ldy #mobyl
	adc (mobptr),y
	sta (mobptr),y
	ldy #mobdyh
	lda (mobptr),y
	ldy #mobyh
	adc (mobptr),y
	sta (mobptr),y

	; --- animation
	ldy #mobattl
	lda (mobptr),y
	sec
	sbc #1
	beq nextaframe
	sta (mobptr),y
	jmp donealist

nextaframe
	; set animation frame
	; put anim list ptr in r3/r4
	ldy #mobalist
	lda (mobptr),y
	sta r3
	iny
	lda (mobptr),y
	sta r4

	; if alist pointer is null,
	; no alist processing
	ora r3
	beq donealist

	; fetch current anim list instr
	; into r0/r1
fetchanimcode
	ldy #mobaframe
	lda (mobptr),y
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
	sta (mobptr),y

	; try again with new aframe offs
	jmp fetchanimcode
	
newimg
	; update list position
	clc
	lda r2
	adc #2
	ldy #mobaframe
	sta (mobptr),y
	
	; set frame ttl counter
	lda r1
	ldy #mobattl
	sta (mobptr),y

	; update mobimg
	lda r0 ; new image num
	ldy #mobimg
	sta (mobptr),y

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

	sta livemobcnt

	; store # live mobs in spritenum
	; start at count-1 because it's
	; a 0-based index
	tax
	beq done ; 0 active mobs
	dex
	stx spritenum ; count-1

	lda #0
	sta $d010 ; sprite hi x bits
	sta $d015 ; sprite enable

	; start by pointing at mob 0
	lda #<mobtab
	sta mobptr
	lda #>mobtab
	sta mobptr+1

	; loop counts down from livemobcnt
	; mobtab array is never empty
updateloop
	#ifmobdis "nextmob"
	jsr vicupdate1
	dec spritenum

nextmob
	#add16ai mobptr,mobstructsz
	#moblda "xh"
	cmp #$80 ; end of mobtab
	beq done
	bne updateloop
	
done
	rts
livemobcnt .byte 0 ; maybe use r1 instead?

vicupdate1
; in: mobptr - mob struct pointer
;     spritenum - vic sprite number
;     (call in desc order)
; caller should ensure x&y coords are
; not so large that they wrap around

	; calc pixel positions

	; calc x pixel pos -> r1
	; 16-bit shift 3 places left

	; merge in x-offset of 24
	; into high byte (24>>3 = 3)
	; can ignore lo byte because
	; xoffs is exactly 3 chars wide
	; and hi byte counts chars
	ldy #mobxl
	lda (mobptr),y
	sta r0
	ldy #mobxh
	lda (mobptr),y
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
	sta r1 ;lo 8 bits x pos on screen

	; 9th bit of x into sprite msb reg
	rol $d010

	; calc y pixel pos -> r3
	; don't need fancy prescaled
	; offset because we don't save
	; the carry for y
	ldy #mobyl
	lda (mobptr),y
	sta r2
	ldy #mobyh
	lda (mobptr),y
	clc
	rol r2
	rol a
	rol r2
	rol a
	rol r2
	rol a

	adc #spriteyoffs
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
	lda (mobptr),y
	ldy spritenum
	sta $d027,y

	; set hw sprite to mobimg
	ldy #mobimg
	lda (mobptr),y
	tax
	#ifmobxm "eq","noflip"
	clc
	txa ; macro wiped a
	adc #(sprites_last - sprites_first)
	tax
noflip	txa
	ldy spritenum
	sta spriteimg,y

	rts

	.bend

; --- stamps: placeable rectangles
;     of background chars
mkstamp
; creates a stamp from the given
; screen coords
; x, y -> top-left col, row
; r1, r2 -> width, height
; a -> mem slot (address aa00)
; a, x, y, r3, r4 <- junk
	.block

	; set hi byte of dest addrs
	sta stchar+2
	sta stcolr+2
	sta stlen+2
	sta stwid+2

	; calc char src start addr
	tya
	asl a
	tay
	clc
	txa            ;  col number
	adc scrlines,y ; + line addr
	sta ldchar+1 ; char src lobyte
	sta ldcolr+1 ; color src lobyte
	iny
	lda scrlines,y
	adc #0
	sta ldchar+2 ; char src hi byte
	
	; calc color src start addr
	; = char addr + ($d800-screen)
	; (lo byte is set above)
	clc
	lda #>($d800-screen)
	adc ldchar+2
	sta ldcolr+2

	; calc num char bytes
	; r3 <- width*height
	lda #0
	ldy r2
	clc
csloop	adc r1
	dey
	bne csloop
	sta r3
stlen	sta $ff00 ; selfmod
	lda r1
stwid	sta $ff01 ; selfmod

	; set char&color dest addrs
	lda stchar+1
	adc r3
	sta stcolr+1

	; r4 <- 40-width
	; for incrementing src addrs
	lda #40
	sec
	sbc r1
	sta r4

	; do the copy
	ldy #0 ; dest offset
rowloop	ldx r1 ; src offset
colloop
ldchar	lda screen,x; selfmod
stchar	sta $ff02,y ; selfmod
ldcolr	lda $d800,x ; selfmod
stcolr	sta $ff00,y ; selfmod
	iny
	dex
	bpl colloop

	cpy r3  ; all bytes copied?
	beq done

	; next row: src += (40-width)
	clc
	lda ldchar+1
	adc r4
	sta ldchar+1
	lda ldchar+2
	adc #0
	sta ldchar+2

	clc
	lda ldcolr+1
	adc r4
	sta ldcolr+1
	lda ldcolr+2
	adc #0
	sta ldcolr+2
	jmp rowloop

done	; zero remainder of page
	; to improve RLE on disk save
	lda stchar+2
	sta zloop+2
	lda #0
zloop	sta $ff00,y
	iny
	bne zloop
	rts
	.bend

usestamp
; a -> slot number (mem page)
; x, y: top left row, col
; a, x, y, r3, r4 <- junk
	.block
	sta getlen+2
	sta getwid+2
	sta ldchar+2
	sta ldcolr+2

	; calc char dest start addr
	tya
	asl a
	tay
	clc
	txa            ;  col number
	adc scrlines,y ; + line addr
	sta stchar+1 ; char dest lobyte
	sta stcolr+1 ; color dest lobyte
	iny
	lda scrlines,y
	adc #0
	sta stchar+2 ; char src hi byte
	
	; calc color dest start addr
	; = char addr + ($d800-screen)
	; (lo byte is set above)
	clc
	lda #>($d800-screen)
	adc stchar+2
	sta stcolr+2

getlen	ldy $ff00 ; byte 0: total chars
	sty r3
getwid	ldx $ff01 ; byte 1: row width
	stx r1

	; calc color src start
	clc
	lda ldchar+1
	adc r3
	sta ldcolr+1

	; r4 <- 40-width
	; for incrementing dest addrs
	lda #40
	sec
	sbc r1
	sta r4

	; do the copy
	ldy #0 ; dest offset
rowloop	ldx r1 ; src offset
colloop
ldchar	lda $ff02,y ; selfmod
stchar	sta screen,x; selfmod
ldcolr	lda $ff00,y ; selfmod
stcolr	sta $d800,x ; selfmod
	iny
	dex
	bpl colloop

	cpy r3  ; all bytes copied?
	beq done

	; next row: src += (40-width)
	clc
	lda stchar+1
	adc r4
	sta stchar+1
	lda stchar+2
	adc #0
	sta stchar+2

	clc
	lda stcolr+1
	adc r4
	sta stcolr+1
	lda stcolr+2
	adc #0
	sta stcolr+2
	jmp rowloop

done	rts
	.bend

mvstamp
	rts

rmstamp
	rts
;xxx

; --- sprite mirroring (setup)

mirrorsprites
; mirrors single colour sprite images
; x & y - lo and hi bytes of 1st sprite
; a - number of sprites to flip
;     (also offset of flipped sprites)
; trashes ptr0
	.block

	stx ptr0 ; source pointer
	sty ptr0+1

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
	lda ptr0+1
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
	#add16ai ptr0,3
	#add16ai dstaddr+1,3

	dec r4
	bne flipline
	
	; inc ptrs past unused byte
	#add16ai ptr0,1
	#add16ai dstaddr+1,1

	dec r3
	bne flipsprite

	rts
	.bend

; ---------- Screen Loader ---------

SETNAM=$ffbd
SETLFS=$ffba
OPEN=$ffc0
CHKIN=$ffc6
CHKOUT=$ffc9
CHRIN=$ffcf ; blocking
CHROUT=$ffd2
CLOSE=$ffc3
CLRCHN=$ffcc
READST=$ffb7
GETIN=$ffe4 ; nonblocking

openscrfile
; call kernal SETNAM,SETLFS,OPEN
; with filename of
; screen at world coord x,y
; x -> world x-coord of screen
; y -> world y-coord of screen
; a -> logical file number
; c -> 0=read 1=write
; r0 <- world x-coord of screen
; r1 <- world y-coord of screen
; c  <- set if error
	.block
	stx r0
	sty r1
	pha ; remember filenum
	php ; remember rd/wr

	; x high nybble
	lda r0
	lsr a
	lsr a
	lsr a
	lsr a
	tax
	lda hexchr,x
	sta scfnam+5

	; x low nybble
	lda r0
	and #$0f
	tax
	lda hexchr,x
	sta scfnam+6

	; y high nybble
	lda r1
	lsr a
	lsr a
	lsr a
	lsr a
	tax
	lda hexchr,x
	sta scfnam+7

	; y low nybble
	lda r1
	and #$0f
	tax
	lda hexchr,x
	sta scfnam+8
	
	; scfnam has correct coords
	; now send to SETNAM based on
	; read or write
	plp
	bcs write
	lda #6 ; length without @0:,u,w
	ldx #<(scfnam+3)
	ldy #>(scfnam+3)
	jmp sngo

write	lda #13 ; full length
	ldx #<scfnam
	ldy #>scfnam
sngo	jsr SETNAM

	; SETLFS
	pla      ; file num
	ldx $ba  ; current device
	bne devok
	ldx #8   ; default drive 8
	stx $ba
devok	tay      ; secondary addr
	jsr SETLFS ; a,(ba),a

	jsr OPEN

	rts

scfnam
	.text "@"
	.text "0:"
	.text "scxxyy,u,w" ; xxyy gets modified
hexchr	.text "0123456789abcdef"
	.bend

loadscr
; loads screen at world coord x,y
	.block
	lda #0
	sta $d015 ; disable all sprites

	; x and y were set by caller
	lda #2 ; logical file num
	clc    ; open for read
	jsr openscrfile
	bcs operr

	; skip load address
	ldx #2
	jsr CHKIN

nextchunk
	jsr decrunch

	cmp #0 ; no errors, not eof
	beq nextchunk

	and #$40 ; eof
	beq rderr

close	lda #2
	jsr CLOSE
	jsr CLRCHN
	rts

; handle error in open (code in a)
operr	jsr printst
	ldx #2
	stx $d021
	jsr close
	jmp * ; TODO recover

; handle error in read
rderr	jsr printst
	ldx #3
	stx $d021
	jsr close
	jmp * ; TODO recover

decrunch
; read bytes from open file and decode
; to target addr given by first 2 bytes
; trashes ptr0
	jsr CHRIN
	sta ptr0
	jsr CHRIN
	sta ptr0+1

loop	jsr READST
	bne eof
	jsr CHRIN
	cmp #$ff
	beq dorun
	cmp #$fe
	beq dosim

dochar	ldy #0
	sta (ptr0),y
	ldx #1 ; 1 screen char printed
	bne next ; always

dorun	jsr CHRIN
	tay ; loop count
	beq endchunk ; found ff00
	tax ; byte count for next
	jsr CHRIN
	; char in a
runlp	dey
	sta (ptr0),y
	bne runlp
	beq next ; always

dosim	jsr CHRIN
	sta lenchk + 1 ; sim length
	tax ; byte count for next
	jsr CHRIN
	sta r4 ; ptr0 - r4 = start addr
	sec
	lda ptr0
	sbc r4
	sta ptr1
	lda ptr0+1
	sbc #0
	sta ptr1+1
	ldy #$ff
simlp	iny
	lda (ptr1),y
	sta (ptr0),y
lenchk	cpy #0 ; selfmod, set above
	bne simlp
	
next	; ptr0 += x
	clc
	txa
	adc ptr0
	sta ptr0
	lda #0
	adc ptr0+1
	sta ptr0+1

	jmp loop
	
endchunk
	lda #0
eof	rts

	.bend

printst
; fetches error info from current drive
; and prints to top left of screen
	.block
	lda #0
	tax
	tay
	jsr SETNAM
	
	lda #15
	ldx $ba
	ldy #15
	jsr SETLFS ; 15,(ba),15

	jsr OPEN

	ldx #15
	jsr CHKIN

	ldx #0
	ldy #0
loop	jsr $ffb7  ; READST
	bne eof
	jsr $ffcf  ; CHRIN
	jsr ascii2sc
	sta screen,x
	lda #0
	sta $d800,x
	inx
	bne loop

eof	lda #15
	jsr CLOSE
	jsr CLRCHN ;TODO restore file 2?
	rts
	.bend

; convert ASCII/PETSCII to screencode
; a <- ASCII code
; a -> screen code
ascii2sc
	.block
	cmp #32     ; ' '
	beq noconv
	cmp #33     ; '!'
	beq noconv
	cmp #42     ; '*'
	beq noconv
	cmp #48     ;numbers 0-9
	bcs numconv
conv
	sec
	sbc #$40
noconv
	rts
numconv
	cmp #58
	bcc noconv
	jmp conv
	.bend

; === START only needed for level edit

savescr
; saves screen at world coord x,y
; trashes ptr0, r0, r1
	.block
	lda #0
	sta $d015 ; disable all sprites

	; x and y were set by caller
	lda #2 ; logical file num
	sec    ; open for write
	jsr openscrfile
	bcs operr

	ldx #2
	jsr CHKOUT

	; char matrix
	lda #<screen
	sta ptr0
	sta r0
	lda #>screen
	sta ptr0+1
	sta r1
	ldx #<1000
	ldy #>1000
	jsr crunch

	; colours
	jsr cleancopycolor
	lda #<cleancolorbuf
	sta ptr0
	lda #>cleancolorbuf
	sta ptr0+1
	lda #<$d800
	sta r0
	lda #>$d800
	sta r1
	ldx #<1000
	ldy #>1000
	jsr crunch

	; write end of file
	lda #0
	jsr CHROUT

close	lda #2
	jsr CLOSE
	jsr CLRCHN
	rts

; handle error in open (code in a)
operr	jsr printst
	ldx #2
	stx $d021
	jsr close
	jmp * ; TODO recover

cleancopycolor

	; reset self-mod ptrs
	lda #>screen
	sta cccloop+2
	lda #>$d800
	sta rdcolr+2
	lda #>cleancolorbuf
	sta wrcolr+2

	lda #0
cccloop	ldx screen,y ; selfmod
	cpx #$20
	beq wrcolr
rdcolr	lda $d800,y  ; selfmod
	and #$0f
wrcolr	sta cleancolorbuf,y ; selfmod
	iny
	bne cccloop

	; y wrapped around: inc ptrs
	inc cccloop+2
	inc rdcolr+2
	inc wrcolr+2

	; done?
	ldx rdcolr+2
	cpx #($d8+4)
	bne cccloop

	rts

	.bend

crunch
; compress mem block into current file
; (writes data via kernal CHROUT)
; ptr0 -> source block start addr
; r0/r1 -> start addr to decrunch to
; x -> length (lo byte)
; y -> length (hi byte)
; note: you can crunch multiple regions
; into the same file and they will
; decrunch back to the original addrs
; x, y, a, ptr0, r2, r3, r4 <- junk
; r0, r1 <- start addr of source block
; r2, r3 <- end addr of source block

	.block

	; write load addr for this chunk
	lda r0
	jsr CHROUT
	lda r1
	jsr CHROUT

	; set r2/r3 to end addr
	clc
	txa
	adc ptr0
	sta r2
	tya
	adc ptr0+1
	sta r3

	; set initial run limit
	tya ; length > 255?
	beq short
	lda #$ff
	bne *+2 ; always taken
short	txa
setlim	sta runlim+1 ; selfmod

	; count repeated chars from current pos
loop	ldy #0
	lda (ptr0),y

countrun
	iny
	cmp (ptr0),y
	bne runend
runlim	cpy #$ff ; selfmod: limit run length to end of screen (resets to ff at start)
	bne countrun
	; hit runlim - fall through

runend	sty runlen

	jsr countsim
	cmp runlen
	beq tryrun ; run==sim; use run
	bcs trysim ; run<sim; use sim
tryrun
	; need run >= 4 for RLE
	ldy runlen
	cpy #4
	bcs writerun

writechar
	; escape mechanism:
	; fe&ff are always runs
	ldy #0
	lda (ptr0),y
	cmp #$fe
	bcc writeone
	; data is $fe or $ff so
	; fall through to writerun

writerun
	; run: $ff <count> <value>
	lda #$ff
	jsr CHROUT ; RLE code
	lda runlen
	jsr CHROUT ; count
	ldy #0
	lda (ptr0),y
	jsr CHROUT ; value
	ldy runlen
	jmp next	
	
trysim
	; need len >= 4 for sim
	ldy bestsimlen
	cpy #4
	bcc writechar
writesim
	; next <count> chars are
	; similar to some previous data
	; $fe <count> <bytes back>
	lda #$fe
	jsr CHROUT
	lda bestsimlen
	jsr CHROUT

	sec
	lda ptr0
	sbc ptr1
	jsr CHROUT

	ldy bestsimlen
	jmp next
	
writeone
	; just write current char
	ldy #1

	jsr CHROUT
	jmp next

next	; ptr0 += y
	tya
	clc
	adc ptr0
	sta ptr0
	lda #0
	adc ptr0+1
	sta ptr0+1

	; reached end of src memory?
	; word at r2/r3 - ptr0
	sec
	lda r2
	sbc ptr0
	tax ; remember lo byte
	lda r3
	sbc ptr0+1
	bne loop_    ; >255 to go

	txa ; recall lo byte
	sta runlim+1 ; <=255 to go
	bne loop_    ; >0 to go

	; write end-of-chunk
	; TODO don't need this?
	lda #$ff
	jsr CHROUT
	lda #$00
	jsr CHROUT

	rts

loop_	jmp loop

runlen .byte 0
bestsimlen .byte 0
bestsimstart .word 0

countsim
; -> r0/r1 - start addr of chunk
; -> ptr0 - addr of next char to save
;    (value preserved on return)
; <- bestsimlen - longest similarity
; <- a - same as bestsimlen
; <- ptr1 - start of longest similarity
	; reset return vals
	lda #0
	sta bestsimlen
	sta bestsimstart
	sta bestsimstart+1

	; put ptr0-255 in x/y
	sec
	lda ptr0
	sbc #$ff
	tax ; lo byte of ptr0-255
	lda ptr0+1
	sbc #0
	tay ; hi byte of ptr0-255

	; start scanning for similar
	; patterns at
	; max(r0, ptr0-255)
	cpy r0+1
	bcc startr0  ; ptr0h-1 < r0h
	bne startptr0; ptr0h-1 > r0h
	cpx r0
	bcc startr0; ptr0l < r0l

; start scan at ptr0-255
startptr0
	sty ptr1+1
	stx ptr1
	jmp csscan

; start scan at beginning of
; data chunk we are saving (r0/r1 ptr)
startr0	ldx r0+1
	stx ptr1+1
	ldx r0
	stx ptr1

csscan
; figure out how far we can scan
;   r4 <- ptr0-ptr1 (never > 255)
	sec
	lda ptr0
	sbc ptr1
	sta r4
	cmp #4 ; ptr1 too close to ptr0
	bcc csdone

	ldy #0
csloop
	lda (ptr0),y
	cmp (ptr1),y
	bne cscheck
	iny
	cpy r4
	bcc csloop

cscheck
	cpy bestsimlen
	bcs csbetter
	bcc csnext  ; always taken

csbetter
	sty bestsimlen
	lda ptr1
	sta bestsimstart
	lda ptr1+1
	sta bestsimstart+1

csnext
	#add16ai ptr1,1
	jmp csscan

csdone
	lda bestsimstart
	sta ptr1
	lda bestsimstart+1
	sta ptr1+1
	lda bestsimlen
	rts
	.bend
