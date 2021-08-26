default: interplat.prg catsprites.prg

interplat.prg: interplat.s catfox_spritenums.s

%.prg: %.s
	tmpx -i $< -o $@ -l $@.list

catfox_spritenums.s: catsprites.s
	cat catsprites.s | awk 'BEGIN { x=(9*256/64) } /catfox/ { print $$1 "=" x++ }' > catfox_spritenums.s

catsprites.s: catsprites.txt
	(echo '*=$$0900'; cat catsprites.txt) | sed 's/://' | sed 's/..A .*//' | sed 's/\/\//;/' > catsprites.s
