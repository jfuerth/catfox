; Global settings for the game

;; MEMORY MAP

; JMP here to start the game
entrypoint=$c000

; animation lists (referenced by mobtab
; entries and mob action code)
alists=$2000

; mob initialization subroutine
; mob actions follow (same source file)
mobinit=$3000

; mob table base address
mobtab=$800
catmob=$800  ; player 1
cpmob=$810   ; player 2
scrmobs=$820 ; NPCs per screen
; entry of $8080 after last mob
; terminates table

; VIC-II addresses must be within a
; 16k aligned block
charset=$4000   ; chargen RAM
screen=$4800    ; screen matrix
sprites=$4c00   ; addr of first sprite

; 00000   Z e r o  P a g e   00000
; temporary vars
r0=$02
r1=$03
r2=$04
r3=$05
r4=$06

; game state
gamestop=$07
mobptr=$08 ; and $09
wantscr=$0a ; and $0b
jumpttl=$0c ; ttl for jump boost
coyotettl=$0d

; temporary pointers
ptr0=$fb ; $fc
ptr1=$fd ; $fe
; 00000    End Zero Page     00000

; constants
jup=   %00000001
jdown= %00000010
jleft= %00000100
jright=%00001000
jfire= %00010000
