package main

import "strconv"
import "io"

var rom [8192]uint16
var pc uint16 = 0

var lines map[string]uint16 = make(map[string]uint16)
var linesUnresolved map[uint16]string = make(map[uint16]string)

func emit(op uint16) {
	rom[pc] = op
	pc++
}

func emitLit(lit string) {
	n, _:= strconv.Atoi(lit)
	emit(0x8000 | uint16(n))
}

func addLine(line string) {
	lines[line] = pc
	for k, v := range linesUnresolved {
		if v == line {
			rom[k] = 0x0000 | pc
		}
	}
}

func emitGoto(line string) {
	if addr, ok := lines[line]; ok {
		emit(0x0000 | addr)
	} else {
		println("unknown line:"+line)
		linesUnresolved[pc] = line
		emit(0)
	}
}

func varAddr(v string) uint16 {
	return uint16(v[0] - 'a') * 2
}

func emitAlu(op uint16, flags uint16) {
	emit(0x6000 | (op << 8) | flags)
}

func emitFetch(addr uint16) {
	emit(0x8000 | addr)
	emitAlu(12, 0)
}

func emitStore(addr uint16) {
	emit(0x8000 | addr)
	emitAlu(0, 0x0022)
	emitAlu(1, 0x0002)
}

func emitPatch(addr uint16, val uint16) {
	rom[addr] = val
}

// ret: 0x7008

func writeRom(w io.Writer) {
	for i := uint16(0); i < pc; i++ {
		var word []byte = make([]byte, 2)
		word[0] = byte(rom[i] & 0xff)
		word[1] = byte(rom[i] >> 8)
		w.Write(word)
	}
}

func emitCurrentOffset() uint16 {
	return pc
}
