package main

import (
	"io"
	"bufio"
	"flag"
	"os"
	"strconv"
	"path"
	"unicode"
)

var includePath *string = flag.String("inc", "", "Include path")

func usage() {
	println("USAGE:")
	println("\tj1c [-inc incpath] <file.fs> <rom.bin>")
}

func readRune(r *bufio.Reader) rune {
	c, _, err := r.ReadRune()
	if err != nil && err != io.EOF {
		panic(err)
	}
	return c
}

func readWord(r *bufio.Reader) (word string) {
	var c rune
	for {
		c = readRune(r)
		if c == '\\' {
			for {
				c = readRune(r)
				if c == '\n' {
					break
				}
			}
		} else if c == '(' {
			for c != ')' {
				c = readRune(r)
			}
		} else if !unicode.IsSpace(c) {
			break
		}
	}

	if c == 0 { // EOF
		return
	}

	word = string(c)
	for {
		c = readRune(r)
		if unicode.IsSpace(c) {
			return
		}
		word = word + string(c)
	}
	return
}

var rom [8192]uint16
var pc uint16 = 1
var memOffset uint16 = 0

var dict map[string]uint16 = make(map[string]uint16, 0)
var consts map[string]uint16 = make(map[string]uint16, 0)
var vars map[string]uint16 = make(map[string]uint16, 0)
var modules map[string]bool = make(map[string]bool, 0)

var base = 10

func emitAlu(op uint16, TN bool, RPC bool, TR bool, NTI bool, ds int, rs int) {
	if ds == -1 {
		ds = 2
	}
	if rs == -1 {
		rs = 2
	}
	op = (op << 8) | (3 << 13) | uint16(rs<<2) | uint16(ds)
	if RPC {
		op |= (1 << 12)
	}
	if TN {
		op |= (1 << 7)
	}
	if TR {
		op |= (1 << 6)
	}
	if NTI {
		op |= (1 << 5)
	}
	rom[pc] = op
	pc++
}

func getLiteral(w string) (n uint16, ok bool) {
	if w == "here" {
		return pc, true
	} else if addr, ok := vars[w]; ok {
		return addr, true
	} else if value, ok := consts[w]; ok {
		return value, true
	} else {
		if number, err := strconv.ParseInt(w, base, 16); err != nil {
			return 0, false
		} else {
			return uint16(number), true
		}
	}
	return 0, false
}

func emit(words... string) bool {
	for _, w := range words {
		if w == "noop" {
			emitAlu(0, false, false, false, false, 0, 0)
		} else if w == "+" {
			emitAlu(2, false, false, false, false, -1, 0)
		} else if w == "xor" {
			emitAlu(5, false, false, false, false, -1, 0)
		} else if w == "and" {
			emitAlu(3, false, false, false, false, -1, 0)
		} else if w == "or" {
			emitAlu(4, false, false, false, false, -1, 0)
		} else if w == "invert" {
			emitAlu(6, false, false, false, false, 0, 0)
		} else if w == "=" {
			emitAlu(7, false, false, false, false, -1, 0)
		} else if w == "<" {
			emitAlu(8, false, false, false, false, -1, 0)
		} else if w == "u<" {
			emitAlu(15, false, false, false, false, -1, 0)
		} else if w == "swap" {
			emitAlu(1, true, false, false, false, 0, 0)
		} else if w == "dup" {
			emitAlu(0, true, false, false, false, 1, 0)
		} else if w == "drop" {
			emitAlu(1, false, false, false, false, -1, 0)
		} else if w == "over" {
			emitAlu(1, true, false, false, false, 1, 0)
		} else if w == "nip" {
			emitAlu(0, false, false, false, false, -1, 0)
		} else if w == ">r" {
			emitAlu(1, false, false, true, false, -1, 1)
		} else if w == "r>" {
			emitAlu(11, true, false, false, false, 1, -1)
		} else if w == "r@" {
			emitAlu(11, true, false, false, false, 1, 0)
		} else if w == "@" {
			emitAlu(12, false, false, false, false, 0, 0)
		} else if w == "!" {
			emitAlu(0, false, false, false, true, -1, 0)
			emitAlu(1, false, false, false, false, -1, 0)
		} else if w == "dsp" {
			emitAlu(14, true, false, false, false, 1, 0)
		} else if w == "lshift" {
			emitAlu(13, false, false, false, false, -1, 0)
		} else if w == "rshift" {
			emitAlu(9, false, false, false, false, -1, 0)
		} else if w == "1-" {
			emitAlu(10, false, false, false, false, 0, 0)
		} else if w == "2r>" {
			emitAlu(11, true, false, false, false, 1, -1)
			emitAlu(11, true, false, false, false, 1, -1)
			emitAlu(1, true, false, false, false, 0, 0)
		} else if w == "2>r" {
			emitAlu(1, true, false, false, false, 0, 0)
			emitAlu(1, false, false, true, false, -1, 1)
			emitAlu(1, false, false, true, false, -1, 1)
		} else if w == "2r@" {
			emitAlu(11, true, false, false, false, 1, -1)
			emitAlu(11, true, false, false, false, 1, -1)
			emitAlu(1, true, false, false, false, 1, 0)
			emitAlu(1, true, false, false, false, 1, 0)
			emitAlu(1, false, false, true, false, -1, 1)
			emitAlu(1, false, false, true, false, -1, 1)
			emitAlu(1, true, false, false, false, 0, 0)
		} else if w == "dup@" {
			emitAlu(12, true, false, false, false, 1, 0)
		} else if w == "dup>r" {
			emitAlu(0, false, false, true, false, 0, 1)
		} else if w == "2dupxor" {
			emitAlu(5, true, false, false, false, 1, 0)
		} else if w == "2dup=" {
			emitAlu(7, true, false, false, false, 1, 0)
		} else if w == "!nip" {
			emitAlu(0, false, false, false, true, -1, 0)
		} else if w == "2dup!" {
			emitAlu(0, false, false, false, true, 0, 0)
		} else if w == "up1" {
			emitAlu(0, false, false, false, false, 1, 0)
		} else if w == "down1" {
			emitAlu(0, false, false, false, false, -1, 0)
		} else if w == "copy" {
			emitAlu(1, false, false, false, false, 0, 0)
		} else {
			return false
		}
	}
	return true
}

func compile(file io.Reader) {
	var w string

	var condStack [100]uint16
	var loopStack [100]uint16
	var h, l = 0, 0

	r := bufio.NewReader(file)
	for {
		w = readWord(r)

		// check EOF
		if len(w) == 0 {
			break
		}

		if w == ":" {
			w = readWord(r)
			println("added word:", w)
			dict[w] = pc
			continue
		}

		// try: dictionary words
		if addr, ok := dict[w]; ok {
			rom[pc] = (2 << 13) | addr
			pc++
			continue
		}

		// try: base words
		if emit(w) {
			continue
		}

		if w == ";" {
			rom[pc] = (3 << 13) | (1 << 12) | (8) // set return flag
			pc++
		} else if w == "include" {
			m := readWord(r)
			if _, ok := modules[m]; !ok {
				modules[m] = true
				f, err := os.Open(path.Join(*includePath, m))
				if err != nil {
					panic(err)
				}
				compile(f)
			} else {
				println("Module alreadin included, ignoring this time:", m)
			}
		} else if w == "d#" {
			base = 10
		} else if w == "h#" {
			base = 16
		} else if w == "if" {
			condStack[h] = pc
			h++
			rom[pc] = 0x2000 // jz, patch if or else
			pc++
		} else if w == "then" {
			h--
			addr := condStack[h]
			rom[addr] |= pc
		} else if w == "else" {
			h--
			addr := condStack[h]
			rom[addr] |= pc + 1 // next to "else" - patch if
			condStack[h] = pc
			rom[pc] = 0x0000
			h++
			pc++
		} else if w == "begin" {
			loopStack[l] = pc
			l++
		} else if w == "again" {
			l--
			rom[pc] = loopStack[l] // jmp
			pc++
		} else if w == "until" {
			l--
			rom[pc] = 0x2000 | loopStack[l] // jz
			pc++
		} else if w == "var" {
			w = readWord(r)
			vars[w] = memOffset
			memOffset += 2
		} else if w == "arr" {
			varName := readWord(r)
			varValue := readWord(r)
			if n, ok := getLiteral(varValue); ok {
				vars[varName] = memOffset
				memOffset += n
			} else {
				panic("Unknown word "+ varValue) // ?
			}
		} else if w == "equ" {
			varName := readWord(r)
			varValue := readWord(r)
			if n, ok := getLiteral(varValue); ok {
				println("added constant", varName, "=", varValue);
				consts[varName] = n
			} else {
				panic("Unknown word"+varValue) // ?
			}
		} else {
			// try: literals, constants, variables
			if n, ok := getLiteral(w); ok {
				rom[pc] = 0x8000 | n
				pc++
			} else {
				println("Unknown word: " + w)
			}
		}
	}
}

func main() {

	flag.Usage = usage;

	flag.Parse();

	if len(flag.Args()) != 2 {
		usage()
		os.Exit(0)
	}

	from, err := os.Open(flag.Arg(0))
	if err != nil {
		panic(err)
	}
	to, err := os.OpenFile(flag.Arg(1), os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0644)
	if err != nil {
		panic(err)
	}

	compile(from)

	if addr, ok := dict["main"]; ok {
		rom[0] = addr
	} else {
		panic("main word not found!")
	}
	for i := uint16(0); i < pc; i++ {
		var word []byte = make([]byte, 2)
		word[0] = byte(rom[i] & 0xff)
		word[1] = byte(rom[i] >> 8)
		to.Write(word)
	}
	from.Close()
	to.Close()
}
