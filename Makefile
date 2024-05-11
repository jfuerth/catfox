CFLAGS=-g

default: interplat.prg catsprites.prg

# To 'make run' you need Vice x64sc running with the remote monitor on port 6510:
#     x64sc -remotemonitor -remotemonitoraddress localhost:6510
# Required settings:
#  * drive 8 mapped to "host filesystem IEC device" in this directory
#    (use path="." if you start x64sc in this directory)
#  * Joystick in port 2
run: interplat.prg
	cat setup.mon | nc localhost 6510

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

# screens
# TODO set up targets for this
# make screen_convert && ./screen_convert < ~/Downloads/sc0300.prg > sc0300 && hexdump -C sc0300 && ./decrunch < sc0300 

# TODO make the BASIC loader an autostart
# TODO dynamically add screens to the disk image
catfox.d64: *.prg
	c1541 -format catfox,cf d64 catfox.d64 -attach catfox.d64 -write catfox.prg -write interplat.prg interplat -write catsprites.prg catsprites -write cornersprites.prg cornersprites -write bgchars.prg bgchars -write sc0000 sc0000 -write sc0100 sc0100 -write sc0200 sc0200 -write sc0300 sc0300
