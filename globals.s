mobtab=$800
charset=$4000
screen=$4800
sprites=$4c00
entrypoint=$c000


; 00000   Z e r o  P a g e   00000
; temporary vars
r0=$02
r1=$03
r2=$04
r3=$05
r4=$06

; game state
mobptr=$07 ; and $08
jumpttl=$09 ; ttl for jump boost
coyotettl=$0a

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
