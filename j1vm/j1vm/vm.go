package main

import (
	"flag"
	"fmt"
	"os"
	"bufio"
	"io/ioutil"
)

func usage() {
	fmt.Println("USAGE:")
	fmt.Println("\tj1vm <rom>")
	fmt.Println()
}

func main() {
	var verbose *bool = flag.Bool("v", false, "Verbose execution")
	flag.Usage = usage
	flag.Parse()

	if len(flag.Args()) != 1 {
		flag.Usage()
	}

	var rom []uint8

	fileName := flag.Arg(0)

	romFile, err := os.Open(fileName)
	reader := bufio.NewReader(romFile)
	if rom, err = ioutil.ReadAll(reader); err != nil {
		fmt.Println(err)
		os.Exit(1)
	}

	j1 := NewJ1Cpu(128)
	for int(j1.pc) < len(rom)/2 && j1.pc >= 0 {
		op := uint16(rom[j1.pc*2]) + uint16(rom[j1.pc*2+1])<<8
		j1.Exec(op)
		if *verbose {
			j1.dump()
		}
	}
}
