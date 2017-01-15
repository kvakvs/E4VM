#!/bin/bash
#
# Create DOS FAT image for use under RTEMS
# This creates a 10Mb disk image, formats it and compies some files
#

IMG=disk.img

# 10 MB
dd if=/dev/zero of=${IMG} bs=512 count=20000

echo 'drive a: file="disk.img"' > mtoolsrc
MTOOLS="env MTOOLSRC=./mtoolsrc"

${MTOOLS} mformat -t 2000 -s 10 -h 4 a:

# Copy compiled modules
${MTOOLS} mcopy ../e4compile/priv/*.e4b a:/
