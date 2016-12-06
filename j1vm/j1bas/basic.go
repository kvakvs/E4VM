package main

import (
	"flag"
	"os"
	"strconv"
)

var t *Tokenizer

func term() {
	// as we have no MUL/DIV - parse terms tokens here
	switch t.Token() {
	case TOK_VARIABLE:
		emitFetch(varAddr(t.Value()))
		t.Next() // skip variable name
	case TOK_AT:
		t.Next() // skip '@'
		if t.Token() != TOK_NUMBER {
			panic("Invalid syntax for memory address")
		}
		n, _ := strconv.Atoi(t.Value())
		emitFetch(uint16(n))
		t.Next()
	case TOK_NUMBER:
		emitLit(t.Value())
		t.Next() // skip number 
	case TOK_LPAR:
		t.Next()
		expr()
		if t.Token() != TOK_RPAR {
			panic("')' expected");
		}
		t.Next() // expect RPAR
	default:
		panic("Expected either variable, or number, or parenthesis")
	}
}

func expr() {
	term()
	for {
		tok := t.Token()
		if tok != TOK_PLUS && tok != TOK_MINUS && tok != TOK_AND && tok != TOK_OR {
			break
		}
		t.Next()
		term()
		switch tok {
		case TOK_PLUS: // T+N
			emitAlu(2, 0x0002)
		case TOK_MINUS: // TODO: explain code
			emitAlu(6, 0)
			emitLit("1")
			emitAlu(2, 0x0002)
			emitAlu(2, 0x0002)
		case TOK_AND: // TODO
		case TOK_OR: // TODO
		}
	}
}

func let() {
	if t.Token() == TOK_LET {
		t.Next() // skip keyword LET
	}
	var addr uint16
	if t.Token() == TOK_AT {
		t.Next()
		if t.Token() != TOK_NUMBER {
			panic("Memory address expected")
		}
		n, _ := strconv.Atoi(t.Value())
		addr = uint16(n)
	} else if t.Token() == TOK_VARIABLE {
		addr = varAddr(t.Value())
	} else {
		panic("LET: variable or address expected")
	}

	println(addr)
	t.Next() // skip variable name

	if t.Token() != TOK_EQ {
		panic("LET: '=' expected")
	}

	t.Next() // skip "="
	expr()

	emitStore(addr)
}

func cond() {
	expr()
	tok := t.Token()
	t.Next()
	expr()
	switch tok {
	case TOK_EQ:
		emitAlu(7, 0x0002)
	case TOK_LT:
		emitAlu(8, 0x0002)
	case TOK_GT:
		emitAlu(8, 0x0002)
		emitLit("0")
		emitAlu(7, 0x0002)
	default:
		panic("Unexpected condition")
	}
}

func statement() {
	tok := t.Token()
	switch tok {
	case TOK_EOL: // skip empty line, do nothing
	case TOK_REM:
		t.SkipEol() // skip whole line
	case TOK_LET: fallthrough
	case TOK_VARIABLE:
		let()
	case TOK_IF:
		t.Next() // skip keyword IF
		cond()
		if t.Token() != TOK_THEN {
			panic("'THEN' expected")
		}
		t.Next() // skip keyword THEN
		condAddr := emitCurrentOffset()
		emit(0) // emit dummy word
		statement()
		// patch dummy word with conditional jump
		emitPatch(condAddr, 0x2000|emitCurrentOffset())
	case TOK_GOTO:
		t.Next() // skip goto
		if t.Token() != TOK_NUMBER {
			panic("Number expected in GOTO")
		}
		line := t.Value()
		emitGoto(line)
		t.Next() // skip line number
	case TOK_GOSUB:
		t.Next() // skip gosub
		if t.Token() != TOK_NUMBER {
			panic("Number expected in GOTO")
		}
		line := t.Value()
		// TODO: emit CALL (not jump)
		emitGoto(line)
		t.Next() // skip line number
	case TOK_RETURN:
		emitAlu(0, 0x1008) // return (";")
	default:
		panic("Unexpected statement token: " + t.Value())
	}
	if t.Token() != TOK_EOL && t.Token() != TOK_EOF {
		panic("EOL expected")
	}
	t.Next()
}

func lineStatement() {
	// line may start with a number
	if t.Token() == TOK_NUMBER {
		addLine(t.Value())
		t.Next()
	}

	statement()
}

func usage() {
	println("USAGE:")
	println("\tj1bas <file.bas> <rom.bin>")
}

func main() {
	defer func() {
		if e := recover(); e != nil {
			println("FATAL:")
			println(e)
		}
	}()

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

	t = NewTokenizer(from)

	for t.Token() != TOK_EOF {
		lineStatement()
	}

	writeRom(to)
	to.Close()
}
