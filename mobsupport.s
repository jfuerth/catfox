; mob struct indices
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

; mobXXX functions: read/write mob
; struct pointed to by mobptr

; load 8-bit mob prop into acc
; #moblda "colr"
; a <- prop value
; y <- prop offset
moblda	.macro
	ldy #mob@1
	lda (mobptr),y
	.endm

; store 8-bit mob prop from acc
; #mobsta "colr"
; a -> prop value
; y <- prop offset
mobsta	.macro
	ldy #mob@1
	sta (mobptr),y
	.endm

; load 16-bit mob prop into a and x
; a <- low byte
; x <- hi byte
; y <- prop offset
mobldax	.macro
	ldy #(mob@1+1)
	lda (mobptr),y
	tax
	dey
	lda (mobptr),y
	.endm

; store 16-bit mob prop from a and x
; a -> low byte
; x -> hi byte
; y <- prop offset
mobstax	.macro
	ldy #(mob@1)
	sta (mobptr),y
	iny
	txa
	sta (mobptr),y
	.endm

; test if current mob is disabled
; jump to given label if disabled
;   #ifmobdis "isdisabled"
ifmobdis .segment
	ldy #mobcolr
	lda (mobptr),y
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
	lda (mobptr),y
	and #%01000000
	b@1 @2
	.endm

; set mob x-mirror to given arg
;   #setmobxm 1  - facing left
;   #setmobxm 0  - facing right
setmobxm .macro
	ldy #mobcolr
	lda (mobptr),y
	.ifne \1
	ora #%01000000
	.endif
	.ifeq \1
	and #%10111111
	.endif
	sta (mobptr),y
	.endm

; copy mob attribute from current mob
;   #mobcpa "othermobname" "mobxh"
mobcpa .segment
	pha
	tya
	pha
	ldy #@2
	lda (mobptr),y
	sta @1+@2
	pla
	tay
	pla
	.endm

; test if current mob's alist is same
; as given alist
; jump to given label if it is
ifalist	.segment
	ldy #mobalist
	lda (mobptr),y
	cmp #<@1
	bne *+7 ; perf: skip hi byte?
	iny
	lda (mobptr),y
	cmp #>@1
	beq @2
	.endm

; set current mob's anim list
; and reset to frame 0
; a <- 1
; Z <- false
setalist .segment
	lda #<@1
	ldx #>@1
	#mobstax "alist"
	lda #0
	#mobsta "aframe"
	lda #1
	#mobsta "attl"
	.endm
