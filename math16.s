; add absolute addressed + immediate
; 1 - absolute arg (low byte)
; 2 - immediate value to add
; result overwrites 2 bytes at arg 1
add16ai	.macro
	clc
	lda \1
	adc #<\2
	sta \1
	lda \1+1
	adc #>\2
	sta \1+1
	.endm

; subtract absolute words
; 1 - absolute arg (low byte)
; 2 - absolute arg (low byte)
; result in x (low) and y (high)
sub16	.macro
	sec
	lda \1
	sbc \2
	tax
	lda \1+1
	sbc \2+1
	tay
	.endm
