
.include "globals.s"
.include "mobsupport.s"
.include "math16.s"

; only to know first and last sprites
.include "target/sprites_nums.s"

; VIC sprite image pointers
spriteimg=screen+1024-8

; vic bank base addr
vb=screen & $c000

; buffer area for cleaned color ram
; when saving a screen
cleancolorbuf=$0400

spriteyoffs=29 ; y pos is bottom of spr

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
havescr .word $ffff
; PERF: move to ZP if not returning
;       to BASIC

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
	sta gamestop
	sta wantscr
	sta wantscr+1

	lda #$ff ; diff from wantscr
	sta havescr
	sta havescr+1

	jsr mobinit

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

differ
	sec
	rts
	.bend
	
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
