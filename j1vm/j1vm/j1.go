package main

import (
	"fmt"
)

type J1Stack [33]uint16

type J1Cpu struct {
	pc  uint16
	r   J1Stack
	s   J1Stack
	rp  int
	sp  int
	mem []uint16
}

func NewJ1Cpu(memSize int) (j1 *J1Cpu) {
	j1 = new(J1Cpu)
	j1.mem = make([]uint16, memSize)
	j1.sp, j1.rp = -1, -1
	return j1
}

const (
	J1_OP  = (7 << 13)
	J1_ARG = 0x1fff

	J1_JMP      = (0 << 13)
	J1_JZ       = (1 << 13)
	J1_CALL     = (2 << 13)
	J1_ALU      = (3 << 13)
	J1_LIT      = (4 << 13)
	J1_LIT_MASK = 0x7fff
)

func (j1 *J1Cpu) readMem(addr uint16) uint16 {
	var val uint16
	switch addr {
	case 0x7fff: j1.dump();
	default:
		val = j1.mem[addr];
	}
	return val
}

func (j1 *J1Cpu) writeMem(addr uint16, val uint16) {
	switch addr {
	case 0x7fff:
		if val == 0 {
			println("PASS")
		} else {
			println("FAIL: ", val)
		}
	default:
		j1.mem[addr] = val;
	}
}

func (j1 *J1Cpu) Exec(op uint16) {
	if op&J1_LIT != 0 {
		j1.sp++
		j1.s[j1.sp] = op & 0x7fff
		j1.pc++
		return
	}
	arg := op & J1_ARG
	switch op & J1_OP {
	case J1_JMP:
		j1.pc = arg
	case J1_JZ:
		if j1.s[j1.sp] == 0 {
			j1.pc = arg
		} else {
			j1.pc++
		}
		j1.sp--
	case J1_CALL:
		j1.rp++
		j1.r[j1.rp] = j1.pc + 1
		j1.pc = arg
	case J1_ALU:
		j1.execAlu(arg)
	}
}

func (j1 *J1Cpu) execAlu(op uint16) {
	var res, t, n, r uint16

	if j1.sp >= 0 {
		t = j1.s[j1.sp]
	}

	if j1.sp > 0 {
		n = j1.s[j1.sp-1]
	}

	if j1.rp >= 0 {
		r = j1.r[j1.rp]
	}

	j1.pc++

	code := (op & (0xf << 8)) >> 8
	switch code {
	case 0:
		res = t
	case 1:
		res = n
	case 2:
		res = t + n
	case 3:
		res = t & n
	case 4:
		res = t | n
	case 5:
		res = t ^ n
	case 6:
		res = ^t
	case 7:
		if n == t {
			res = 1
		}
	case 8:
		if int16(n) < int16(t) {
			res = 1
		}
	case 9:
		res = n >> t
	case 10:
		res = t - 1
	case 11:
		res = r
	case 12:
		res = j1.readMem(t); //mem[t]
	case 13:
		res = n << t
	case 14:
		res = uint16(j1.sp + 1)
	case 15:
		if n < t {
			res = 1
		}
	}

	ds := op & 3
	rs := (op & (3 << 2)) >> 2

	if ds == 1 {
		j1.sp++
	}
	if ds == 2 {
		j1.sp--
	}
	if rs == 1 {
		j1.rp++
	}
	if rs == 2 {
		j1.rp--
	}

	if op&(1<<5) != 0 {
		//println("N->[T]")
		j1.writeMem(t, n);
		//j1.mem[t] = n
	}
	if op&(1<<6) != 0 {
		//println("T->R")
		if j1.rp < 0 {
			panic("Return stack underrun")
		}
		j1.r[j1.rp] = t
	}
	if op&(1<<7) != 0 {
		//println("T->N")
		if j1.sp > 0 {
			j1.s[j1.sp-1] = t
		}
	}
	if op&(1<<12) != 0 {
		//println("R->PC [ r =", r, "rp =", j1.rp, "]")
		//j1.pc = j1.r[r]
		j1.pc = r
		if j1.pc == 0 {
			j1.pc = 0xffff
		}
	}

	if j1.sp >= 0 {
		j1.s[j1.sp] = res
	}
}

func (j1 *J1Cpu) lit(x uint16) {
	j1.sp++
	j1.s[j1.sp] = x & 0x7fff
}

func (j1 *J1Cpu) t() uint16 {
	return j1.s[j1.sp]
}

func (j1 *J1Cpu) n() uint16 {
	return j1.s[j1.sp-1]
}

func (j1 *J1Cpu) rt() uint16 {
	return j1.r[j1.rp]
}

func (j1 *J1Cpu) depth() int {
	return j1.sp + 1
}

func (j1 *J1Cpu) dump() {
	fmt.Println("PC: ", j1.pc)
	fmt.Print("S: ")
	for i := 0; i <= j1.sp; i++ {
		fmt.Printf("%02x ", j1.s[i])
	}
	fmt.Println()
	fmt.Print("R: ")
	for i := 0; i <= j1.rp; i++ {
		fmt.Printf("%02x ", j1.r[i])
	}
	fmt.Println()
	fmt.Print("M: ")
	for i := 0; i < len(j1.mem); i++ {
		fmt.Printf("%02x ", j1.mem[i])
	}
	fmt.Println()
	fmt.Println()
}
