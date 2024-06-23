; mob action routines
; main program calls these once per frame for each enabled sprite

.include "globals.s"
.include "mobsupport.s"
.include "target/sprites_nums.s"
.include "target/alists.symbols.s"

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

*=mobinit

; just a namespace for the labels.
; this is always called as "mobinit"
mobstartup
	.block

	ldx #(mobstructsz*2+2)
loop
	lda mobdefaults,X
	sta mobtab,X
	dex
	bpl loop

	rts

mobdefaults
catfox
	;     x---- dx--- y---- dy---
	.word $0300,$0000,$1400,$0000
	.byte 1                ; color
	.byte catfox_sitting_2 ; image
	.word cfidleanim       ; alist
	.byte 0                ; aframe
	.byte 1                ; attl
	.word playeract

player2
	;     x---- dx--- y---- dy---
	.word $0a00,$0000,$0a00,$0000
	.byte 7                ; color
	.byte cursor1x1_1      ; image
	.word 0                ; alist
	.byte 0                ; aframe
	.byte 1                ; attl
	.word cpact

	.word $8080 ; mark end of mobtab
	.bend


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


; ------------------------------------
; ---- mob action: stay on platform
; ------------------------------------
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
	iny ; right block can also support
	lda (r3),y
	cmp #$80
	bcs nofall

	jsr applygravity
	#ifalist "cffallanim","falling"
	#setalist "cffallanim"
falling
	bne done
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
	; middle block if moving left
	; right block if moving right
	#moblda "dxh"
	bmi checkcliff
	inx
checkcliff
	inx
	txa
	tay
	lda (r3),y ; stepping toward this
	cmp #$80
	bcs done

turnback
	lda #0
	ldy #mobdxl
	sec
	sbc (mobptr),y
	sta (mobptr),y
	iny
	lda #0
	sbc (mobptr),y
	sta (mobptr),y

	bmi faceleft
	#setmobxm 0
	bne done
faceleft
	#setmobxm 1
done
	rts
	.bend


; ------------------------------------
; ---- mob action: dive at player
; ------------------------------------
divebombact
	.block

	#moblda "alist"
	cmp #<dbdiveanim
	beq diving

	cmp #<dbflyupanim
	beq resetting

sitting ; wait for player
	#moblda "xh"
	cmp catmob+mobxh
	bne done
	; otherwise, fall through

dive
	lda #1
	#mobsta "dyh"
	#setalist "dbdiveanim"
	bne done

diving
	#moblda "attl"
	cmp #1
	bne done
	; done diving - fall through

reset
	lda #$ff
	#mobsta "dyh"
	#setalist "dbflyupanim"
	bne done

resetting
	#moblda "attl"
	cmp #1
	bne done
	lda #0
	#mobsta "dyh"
	#setalist "dbsitanim"
	; fall through

done
	rts
	.bend


; ------------------------------------
; ---- mob action: fly back and forth
; ------------------------------------
lrflyact
	.block
	; TODO fill this in
	.bend


; ------------------------------------
; ---- p2 copy/paste controls ----
; ------------------------------------
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

done
	rts
	.bend

mvstamp
	; TODO
	rts

rmstamp
	; TODO
	rts
