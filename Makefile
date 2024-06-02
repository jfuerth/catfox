CFLAGS=-g

default: catfox.d64

.PHONY: tools clean run assets

# To 'make run' you need Vice x64sc running with the remote monitor on port 6510:
#     x64sc -remotemonitor -remotemonitoraddress localhost:6510
# Required settings: (currently broken because of assets splitup)
#  * drive 8 mapped to "host filesystem IEC device" in this directory
#    (use path="target" if you start x64sc in this directory)
#  * Joystick in port 2
run.dev: interplat.prg assets
	cat setup.mon | nc localhost 6510

run.d64: catfox.d64
	x64sc -autostart catfox.d64

assets: catfox.pe
	mkdir -p target
	cd target && ../tools/pexplode.java ../catfox.pe \
	  --charsets='bgchars-mc*' \
	  --load-addr='sc????=4800'

target/interplat.prg: interplat.s tools assets
	tmpx -i $< -o $@ -l $@.list
	cat $@.list | tools/tmplab2vice > $@.labels

%.prg: %.s tools
	tmpx -i $< -o target/$@ -l target/$@.list
	cat target/$@.list | tools/tmplab2vice > target/$@.labels

# TODO make the BASIC loader an autostart
# TODO dynamically add screens to the disk image
catfox.d64: assets target/interplat.prg
	tools/basic.java make-loader -s 49152 -o target/catfox-loader.prg \
	  INTERPLAT SPRITES BGCHARS
	c1541 -format catfox,cf d64 catfox.d64 \
	  -attach catfox.d64 \
	  -write target/catfox-loader.prg catfox \
	  -write target/interplat.prg interplat \
	  -write target/sprites.prg sprites \
	  -write 'target/bgchars-mc $$4000.prg' bgchars
	for screen in target/sc????; do c1541 -attach catfox.d64 -write $$screen $$(basename $$screen); done

tools:
	cd tools && $(MAKE) all

clean:
	rm -rf target
	cd tools && $(MAKE) clean
