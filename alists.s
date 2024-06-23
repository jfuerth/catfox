.include "globals.s"
.include "target/sprites_nums.s"

*=alists

; ---- catfox anims

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


; ---- divebird

dbsitanim
	.byte divebird_1,250
	.byte 0,0

dbdiveanim
	.byte divebird_2,10
	.byte 0,0

dbflyupanim
	.byte divebird_1,10
	.byte 0,0
