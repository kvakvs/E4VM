DEBUG_EMU=cmake-build-debug/e4emu-debug

.PHONY: gdb
gdb: compile
	gdb ${DEBUG_EMU}

.PHONY: compile
compile:
	cd cmake-build-debug && $(MAKE) -j && cd ..

.PHONY: lic-add
lic-add:
	rm -rf TMP && \
	copyright-header --license-file license.snippet \
		--add-path src:include \
		--output-dir TMP
.PHONY: lic-rm
lic-rm:
	rm -rf TMP && \
	copyright-header --license-file license.snippet \
		--remove-path src:include \
		--output-dir TMP

# Convert and print PVS-studio results if it wasn't done automatically
.PHONY: plog
plog:
	plog-converter -t tasklist -a "GA:1,2;64:1;CS" \
		cmake-build-debug/PVS-Studio/src/main.cpp.plog

.PHONY: valgrind
valgrind: compile
	valgrind --log-file=valgrind.log \
		cmake-build-debug/gleam_d

# Take emulator compiled for i386 rtems and run it under QEMU
.PHONY: rtems-run
rtems-run: qemu-disk/disk.img
	qemu-system-i386 \
	    -machine pc \
	    -kernel ${DEBUG_EMU} \
	    -sdl -no-reboot \
	    -hda qemu-disk/disk.img

#	    -drive file=qemu-disk/disk.img,format=raw,index=0,media=disk
#	    -hda fat:qemu-disk
#	    -fsdev local,id=hda,security_model=none,path=/dev/hda

# Automatic target is called when disk image is required for the first time
# TODO: make it depend on the files we want on it
qemu-disk/disk.img:
	cd qemu-disk && make-image.sh && cd ..

# Doesn't really work needs extra commands to be typed
.PHONY: rtems-gdb
rtems-gdb: compile
	i386-rtems4.12-gdb ${DEBUG_EMU}
