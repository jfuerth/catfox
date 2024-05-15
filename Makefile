CFLAGS=-g

default: catfox.d64

.PHONY: tools clean run

# To 'make run' you need Vice x64sc running with the remote monitor on port 6510:
#     x64sc -remotemonitor -remotemonitoraddress localhost:6510
# Required settings: (currently broken because of assets splitup)
#  * drive 8 mapped to "host filesystem IEC device" in this directory
#    (use path="." if you start x64sc in this directory)
#  * Joystick in port 2
run: interplat.prg assets
	cat setup.mon | nc localhost 6510

interplat.prg: interplat.s catfox_spritenums.s

assets: catfox.pe
	mkdir -p assets
	touch assets
	cd assets && ../tools/pexplode.java < ../catfox.pe

%.prg: %.s tools
	tmpx -i $< -o $@ -l $@.list
	cat $@.list | tools/tmplab2vice > $@.labels

catfox_spritenums.s: catsprites.s
	# start at sprite 48 ($xC00)
	cat catsprites.s | awk 'BEGIN { x=48; nspr=0; print "firstsprite=" x; } /catfox/ { print $$1 "=" x++; nspr++ } END { print "numsprites=" nspr }' > catfox_spritenums.s

# process sprite data saved from spritemate in KickAss hex format
%.s: %.txt %_addr.txt
	cat $*_addr.txt $*.txt | sed 's/://' | sed 's/..A .*//' | sed 's/\/\//;/' > $@

# TODO make the BASIC loader an autostart
# TODO dynamically add screens to the disk image
catfox.d64: interplat.prg catsprites.prg cornersprites.prg assets
	c1541 -format catfox,cf d64 catfox.d64 \
	 -attach catfox.d64 \
	 -write catfox-loader.prg catfox \
	 -write interplat.prg interplat \
	 -write catsprites.prg catsprites \
	 -write cornersprites.prg cornersprites \
	 -write assets/bgchars-mc.prg bgchars
	for screen in assets/sc????; do c1541 -attach catfox.d64 -write $$screen $$(basename $$screen); done

tools:
	cd tools && $(MAKE) all

clean:
	rm -f *.prg *.prg.list *.prg.labels catfox_spritenums.s
	cd tools && $(MAKE) clean
