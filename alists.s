.include "globals.s"
.include "target/sprites_nums.s"
.include "mobsupport.s"

*=alists

; ---- catfox anims

cfidleanim
	.block
	.byte catfox_stand_1,60
	.byte catfox_sitting_1,4
	.byte catfox_sitting_2,4
	.byte catfox_sitting_3,4
sat
	.byte catfox_sitting_4,250
	.byte algoto
	.byte <(sat-cfidleanim)
	.bend

cfjumpanim
	.byte catfox_jump_1,10
	.byte catfox_jump_2,10
	.byte algoto,2 ; frame 1

cfwalkanim
	.byte catfox_run_1,6
	.byte catfox_run_2,8
	.byte catfox_run_3,10
	.byte catfox_run_4,6
	.byte algoto,0 ; frame 0

cffallanim
	.byte catfox_fall_1,4
	.byte catfox_fall_2,4
	.byte algoto,0 ; frame 0


; ---- divebat

dbsitanim
	.byte divebird_1,250
	.byte algoto,0

dbdiveanim
	.byte alincdy,200
	.byte divebird_2,10
	.byte aljmpalist
	.word dblandedanim

dblandedanim
	.byte alsetdy,0,0
	.byte divebird_1,30
	.byte aljmpalist
	.word dbflyupanim

dbflyupanim
	.byte aldecdy,200 ; match diveanim
	.byte divebird_1,10 ; match
	.byte alsetdy,0,0
	.byte aljmpalist
	.word dbsitanim
