CFLAGS=-g

default: interplat.prg catsprites.prg

interplat.prg: interplat.s catfox_spritenums.s

%.prg: %.s tmplab2vice
	tmpx -i $< -o $@ -l $@.list
	cat $@.list | ./tmplab2vice > $@.labels

catfox_spritenums.s: catsprites.s
	# start at sprite 48 ($xC00)
	cat catsprites.s | awk 'BEGIN { x=48; nspr=0; print "firstsprite=" x; } /catfox/ { print $$1 "=" x++; nspr++ } END { print "numsprites=" nspr }' > catfox_spritenums.s

# process sprite data saved from spritemate in KickAss hex format
%.s: %.txt %_addr.txt
	cat $*_addr.txt $*.txt | sed 's/://' | sed 's/..A .*//' | sed 's/\/\//;/' > $@
