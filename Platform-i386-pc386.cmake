set(RTEMS_ARCH "i386")
set(RTEMS_BSP  "pc386")
set(RTEMS_DIR  "$ENV{HOME}/EMB/rtems/4.12")
#set(RTEMS_TOOLS_BSP_DIR ${RTEMS_DIR}/${ARCH}-rtems4.12/lib)
#set(RTEMS_KERNEL_BSP_LIB_DIR ${RTEMS_DIR}/${RTEMS_ARCH}-rtems4.12/${RTEMS_BSP}/lib)
#set(RTEMS_GCC_LIB_DIR        "${RTEMS_ROOT_PATH}/4.12/lib/gcc/${ARCH}-rtems4.12/6.3.0")

set(CMAKE_SYSTEM_NAME       Linux-GNU-CXX)
#set(CMAKE_SYSTEM_PROCESSOR  ${RTEMS_ARCH})
SET(CMAKE_SYSTEM_VERSION    1)     # not very important
set(E4_BITS_PER_WORD        32)

include(CMakeForceCompiler)
set(CMAKE_C_COMPILER "${RTEMS_DIR}/bin/${RTEMS_ARCH}-rtems4.12-gcc")
set(CMAKE_CXX_COMPILER "${RTEMS_DIR}/bin/${RTEMS_ARCH}-rtems4.12-g++")
cmake_force_c_compiler(${CMAKE_C_COMPILER}     GNU)
cmake_force_cxx_compiler(${CMAKE_CXX_COMPILER} GNU)
set(E4_PLATF_NAME "${RTEMS_ARCH}-${RTEMS_BSP}")

# where is the target environment
#SET(CMAKE_FIND_ROOT_PATH  /opt/eldk-2007-01-19/ppc_74xx /home/alex/eldk-ppc74xx-inst)

# search for programs in the build host directories
SET(CMAKE_FIND_ROOT_PATH_MODE_PROGRAM NEVER)
# for libraries and headers in the target directories
SET(CMAKE_FIND_ROOT_PATH_MODE_LIBRARY ONLY)
SET(CMAKE_FIND_ROOT_PATH_MODE_INCLUDE ONLY)

# These get passed via G++ anyway, so use -Wl,...
set(E4_LINK_OPTS "-Wl,--gc-sections -Wl,-Ttext,0x00100000")

#i386-rtems4.12-gcc --pipe -B/home/kv/EMB/rtems/4.12/i386-rtems4.12/pc386/lib/
#       -specs bsp_specs -qrtems   -Wall  -O2 -g -ffunction-sections -fdata-sections
#       -mtune=i386       -c   -o o-optimize/test.o test.c
#i386-rtems4.12-gcc --pipe -B/home/kv/EMB/rtems/4.12/i386-rtems4.12/pc386/lib/
#       -specs bsp_specs -qrtems   -Wall  -O2 -g -ffunction-sections -fdata-sections
#       -mtune=i386      -Wl,--gc-sections -Wl,-Ttext,0x00100000   -mtune=i386   -o o-optimize/hello.exe  o-optimize/test.o
#i386-rtems4.12-nm -g -n o-optimize/hello.exe > o-optimize/hello.num
#i386-rtems4.12-size o-optimize/hello.exe
#   text	   data	    bss	    dec	    hex	filename
# 178173	   9948	  23068	 211189	  338f5	o-optimize/hello.exe
#i386-rtems4.12-objcopy -O elf32-i386 --remove-section=.comment --remove-section=.note --strip-unneeded o-optimize/hello.exe o-optimize/hello.nxe
#i386-rtems4.12-objcopy -O binary o-optimize/hello.nxe o-optimize/hello.bin
#/home/kv/EMB/rtems/4.12/i386-rtems4.12/pc386/build-tools/bin2boot -v o-optimize/hello.ralf 0x00097E00 /home/kv/EMB/rtems/4.12/i386-rtems4.12/pc386/lib/start16.bin 0x00097C00 0 o-optimize/hello.bin 0x00100000 0
#header address       0x00097e00, its memory size 0x00000200
#first  image address 0x00097c00, its memory size 0x00000200
#second image address 0x00100000, its memory size 0x0002f000
#rm -f o-optimize/hello.nxe

add_definitions(
        --pipe -B${RTEMS_DIR}/${RTEMS_ARCH}-rtems4.12/${RTEMS_BSP}/lib/
        -specs bsp_specs
        -qrtems
        -ffunction-sections -fdata-sections
        -mtune=${RTEMS_ARCH}
        )
#
#set(RTEMS_EXTRA_SOURCES
#        ${RTEMS_KERNEL_BSP_LIB_DIR}/start.o
#        ${RTEMS_XXX_LIB_DIR}/crtbegin.o
#        ${RTEMS_XXX_LIB_DIR}/crtend.o
#        ${RTEMS_XXX_LIB_DIR}/crti.o
#        )