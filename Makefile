default: catfox.d64

.PHONY: tools clean run.dev run.d64

tools:
	cd tools && $(MAKE) all

clean:
	rm -rf target catfox.d64
	cd tools && $(MAKE) clean

## sprites
#   - output sprites.prg
#   - output sprites_nums.s
target/sprites.prg: catfox.pe globals.s tools
	mkdir -p target
	cd target && ../tools/pexplode.java ../catfox.pe \
	  --process=SPRITES \
	  --symbols-file=../globals.s \
	  --first-sprite-addr=sprites

target/sprites_nums.s: target/sprites.prg

## alist tables (uses sprite_nums.s and globals)
#   - output alists.prg
#   - output symbols to reference from:
#     - mob action routines (including start address, which can follow last alist)
#     - screen file mobtabs
target/alists.prg: alists.s target/sprites_nums.s globals.s
	tmpx -i alists.s -o target/alists.prg -l target/alists.prg.list
	cat target/alists.prg.list | tools/tmplab2vice > target/alists.prg.labels

target/alists.prg.labels: target/alists.prg

target/alists.symbols.s: target/alists.prg.labels
	cat $< \
		| sed -E 's/add_label (....) \.(.*)/\2=$$\1/' \
		| grep -v '\.' > $@

## mob action routines (uses alist symbols)
#   - output symbol table to reference in screen file mobtabs
target/mobactions.prg: mobactions.s target/alists.symbols.s mobsupport.s globals.s
	tmpx -i mobactions.s -o target/mobactions.prg -l target/mobactions.prg.list
	cat target/mobactions.prg.list | tools/tmplab2vice > target/mobactions.prg.labels

target/mobactions.prg.labels: target/mobactions.prg

target/mobactions.symbols.s: target/mobactions.prg.labels
	cat $< \
		| sed -E 's/add_label (....) \.(.*)/\2=$$\1/' \
		| grep -v '\.' > $@

## remaining assets from catfox.pe (uses global symbols)
#   - charset
#   - screens
#     - char matrix, colour matrix, mob table
#     - mobtab refers to mob action symbols
target/screens: catfox.pe globals.s target/alists.symbols.s target/mobactions.symbols.s
	cd target && ../tools/pexplode.java ../catfox.pe \
	  --process=CHARSETS,SCREENS \
	  --symbols-file=../globals.s \
	  --symbols-file=alists.symbols.s \
	  --symbols-file=mobactions.symbols.s \
	  --select-charsets='bgchars-mc*' \
	  --load-addr='sc????=screen' \
	  --first-sprite-addr=sprites \
	  --mobtab-addr=scrmobs \
	  --mob-behaviours-file=../mob-behaviours.json

	touch target/screens

target/charset.prg: catfox.pe target/screens

## game engine code
# ideally shouldn't depend on target/sprites_nums.s but it needs
# to know the first and last sprite nums so it can mirror the images
# during init.
target/interplat.prg: interplat.s globals.s mobsupport.s math16.s target/sprites_nums.s
	tmpx -i interplat.s -o target/interplat.prg -l target/interplat.prg.list
	cat target/interplat.prg.list | tools/tmplab2vice > target/interplat.prg.labels

## c64 loader program - should be first file on disk image
target/catfox.prg: tools
	mkdir -p target
	tools/basic.java make-loader -s 49152 -o target/catfox.prg \
	  INTERPLAT SPRITES ALISTS MOBACTIONS BGCHARS

## common targets for building d64 image or running in dev mode
ALL_PRGS=target/catfox.prg target/interplat.prg target/sprites.prg target/alists.prg target/mobactions.prg target/screens

## disk image
catfox.d64: $(ALL_PRGS)
	c1541 -format catfox,cf d64 catfox.d64 \
	  -attach catfox.d64 \
	  -write target/catfox.prg catfox \
	  -write target/interplat.prg interplat \
	  -write target/sprites.prg sprites \
	  -write target/alists.prg alists \
	  -write target/mobactions.prg mobactions \
	  -write 'target/bgchars-mc $$4000.prg' bgchars
	for screen in target/sc????; do c1541 -attach catfox.d64 -write $$screen $$(basename $$screen); done

## run game in dev mode
# To 'make run.dev' you need Vice x64sc running with the remote monitor on port 6510:
#     x64sc -remotemonitor -remotemonitoraddress localhost:6510
# Required settings: (currently broken because of assets splitup)
#  * drive 8 mapped to "host filesystem IEC device" in the 'target' directory
#    (use path="target" if you start x64sc in this directory)
#  * Joystick in port 2
run.dev: $(ALL_PRGS)
	rm -f target/all.labels
	cat target/*.labels >target/all.labels
	cat setup.mon | nc localhost 6510

## build and run from the .d64 disk image
run.d64: catfox.d64
	x64sc -autostart catfox.d64
