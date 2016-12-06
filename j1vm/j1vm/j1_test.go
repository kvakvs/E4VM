package main

import "testing"

func TestJ1Lit(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.Exec(0x8123)
	if j1.depth() != 1 || j1.t() != 0x123 {
		t.Fail()
	}
}

func Op(op uint16, TN bool, RPC bool, TR bool, ds int, rs int, NTI bool) uint16 {
	if ds == -1 {
		ds = 2
	}
	if rs == -1 {
		rs = 2
	}
	op = op << 8
	op |= uint16(ds)
	op |= uint16((rs << 2))
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
	op |= (1 << 14) | (1 << 13)
	return op
}

func TestJ1Dup(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1)
	j1.Exec(Op(0, true, false, false, +1, 0, false))
	if j1.depth() != 2 || j1.t() != 1 {
		t.Fail()
	}
}

func TestJ1Over(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1)
	j1.lit(2)
	j1.Exec(Op(1, true, false, false, +1, 0, false))
	if j1.depth() != 3 || j1.t() != 1 {
		t.Fail()
	}
}

func TestJ1Invert(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1)
	j1.Exec(Op(6, false, false, false, 0, 0, false))
	if j1.depth() != 1 || j1.t() != 0xfffe {
		t.Fail()
	}
}

func TestJ1Add(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1)
	j1.lit(2)
	j1.Exec(Op(2, false, false, false, -1, 0, false))
	if j1.depth() != 1 || j1.t() != 3 {
		t.Fail()
	}
}

func TestJ1Swap(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1)
	j1.lit(2)
	j1.Exec(Op(1, true, false, false, 0, 0, false))
	if j1.depth() != 2 || j1.t() != 1 || j1.n() != 2 {
		t.Fail()
	}
}

func TestJ1Nip(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1)
	j1.lit(2)
	j1.Exec(Op(0, false, false, false, -1, 0, false))
	if j1.depth() != 1 || j1.t() != 2 {
		t.Fail()
	}
}

func TestJ1Drop(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1)
	j1.lit(2)
	j1.Exec(Op(1, false, false, false, -1, 0, false))
	if j1.depth() != 1 || j1.t() != 1 {
		t.Fail()
	}
}

func TestJ1Ret(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.rp = 0
	j1.r[0] = 1234
	j1.dump()
	j1.Exec(Op(0, false, true, false, 0, -1, false))
	j1.dump()
	if j1.depth() != 0 || j1.pc != 1234 {
		t.Fail()
	}
}

func TestJ1PushR(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.lit(1234)
	j1.Exec(Op(1, false, false, true, -1, +1, false))
	if j1.depth() != 0 || j1.rt() != 1234 {
		t.Fail()
	}
}

func TestJ1PopR(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.rp = 0
	j1.r[0] = 1234
	j1.Exec(Op(11, true, false, false, +1, -1, false))
	if j1.depth() != 1 || j1.t() != 1234 || j1.rp != -1 {
		t.Fail()
	}
}

func TestJ1PeekR(t *testing.T) {
	j1 := NewJ1Cpu(128)
	j1.rp = 0
	j1.r[0] = 1234
	j1.Exec(Op(11, true, false, false, +1, 0, false))
	if j1.depth() != 1 || j1.t() != 1234 || j1.rp != 0 || j1.rt() != 1234 {
		t.Fail()
	}
}

func TestJ1PeekMem(t *testing.T) {

}

func TestJ1StoreMem(t *testing.T) {

}
