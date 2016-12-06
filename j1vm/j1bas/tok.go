package main

import (
	"bufio"
	"io"
	"strings"
	"unicode"
)

const (
	TOK_EOF = iota

	TOK_NUMBER
	TOK_VARIABLE

	// comment
	TOK_REM

	// keywords
	TOK_LET
	TOK_PRINT
	TOK_IF
	TOK_THEN
	TOK_GOTO
	TOK_GOSUB
	TOK_RETURN

	TOK_PLUS
	TOK_MINUS
	TOK_AND
	TOK_OR
	TOK_XOR
	TOK_LPAR
	TOK_RPAR
	TOK_LT
	TOK_GT
	TOK_EQ
	TOK_AT

	// end of line
	TOK_EOL = -1
	// unknown token
	TOK_OTHER = -2
	// other errors
	TOK_ERROR = -3
)

var keywords map[string]int = map[string]int{"rem": TOK_REM,
	"let":  TOK_LET,
	"if": TOK_IF,
	"then": TOK_THEN,
	"goto": TOK_GOTO,
	"gosub": TOK_GOSUB,
	"return": TOK_RETURN,
}

type Tokenizer struct {
	r         *bufio.Reader
	token     int
	tokenSize int
}

func NewTokenizer(r io.Reader) *Tokenizer {
	t := new(Tokenizer)
	t.r = bufio.NewReader(r)
	t.token = TOK_ERROR
	t.tokenSize = 0
	return t
}

func (t *Tokenizer) char() int {
	var c int
	if s, err := t.r.Peek(1); err == nil && len(s) == 1 {
		c = int(s[0])
	} else {
		return TOK_ERROR
	}

	switch c {
	case '\n':
		return TOK_EOL
	case '=':
		return TOK_EQ
	case '+':
		return TOK_PLUS
	case '-':
		return TOK_MINUS
	case '(':
		return TOK_LPAR
	case ')':
		return TOK_RPAR
	case '&':
		return TOK_AND
	case '|':
		return TOK_OR
	case '^':
		return TOK_XOR
	case '<':
		return TOK_LT
	case '>':
		return TOK_GT
	case '@':
		return TOK_AT
	}

	return TOK_OTHER
}

func (t *Tokenizer) Token() int {
	const MAX_NUMLEN = 5
	var c rune

	t.tokenSize = 0

	// check EOF
	if s, err := t.r.Peek(1); err == io.EOF {
		return TOK_EOF
	} else {
		if err != nil {
			return TOK_ERROR
		} else {
			c = rune(s[0])
		}
	}

	// check numeric literal
	if unicode.IsNumber(c) {
		s, _ := t.r.Peek(MAX_NUMLEN)
		for i := 0; i < len(s); i++ {
			if !unicode.IsNumber(rune(s[i])) {
				if i > 0 {
					t.tokenSize = i
					return TOK_NUMBER
				} else {
					return TOK_ERROR /* too short */
				}
			}
		}
		return TOK_ERROR /* too long */
	}

	// check keywords
	for k, v := range keywords {
		s, _ := t.r.Peek(len(k))
		if strings.ToLower(string(s)) == k {
			t.tokenSize = len(k)
			return v
		}
	}

	// check variables
	if c >= 'a' && c <= 'z' {
		t.tokenSize = 1
		return TOK_VARIABLE
	}

	// check mathematical operators
	tok := t.char()
	if tok == TOK_ERROR {
		return TOK_ERROR
	}
	t.tokenSize = 1
	return tok
}

func (t *Tokenizer) Value() string {
	if t.tokenSize > 0 {
		buf, _ := t.r.Peek(t.tokenSize)
		return string(buf)
	}
	return ""
}

func (t *Tokenizer) Next() {
	if t.Token() == TOK_EOF {
		return
	}
	buf := make([]byte, t.tokenSize)
	t.r.Read(buf)

	for {
		s, err := t.r.Peek(1)
		if err != nil || s[0] != ' ' {
			break
		}
		t.r.Read(s)
	}
}

func (t *Tokenizer) SkipEol() {
	for {
		t.Next()
		tok := t.Token()
		if tok == TOK_ERROR || tok == TOK_EOL {
			break
		}
	}
}
