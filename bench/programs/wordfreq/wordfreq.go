package main

import (
	"fmt"
	"os"
	"sort"
	"strconv"
	"strings"
)

type lcg struct{ state int64 }

func (r *lcg) next() int64 {
	r.state = (r.state*1103515245 + 12345) % 2147483648
	return r.state >> 16
}

func main() {
	n, _ := strconv.ParseInt(os.Args[1], 10, 64)
	rng := &lcg{42}
	vocab := make([]string, 0, 1000)
	for v := 0; v < 1000; v++ {
		length := 3 + rng.next()%8
		b := make([]byte, length)
		for i := range b {
			b[i] = byte(97 + rng.next()%26)
		}
		vocab = append(vocab, string(b))
	}
	picked := make([]string, 0)
	for i := int64(0); i < n; i++ {
		picked = append(picked, vocab[rng.next()%1000])
	}
	text := strings.Join(picked, " ")

	counts := map[string]int64{}
	for _, w := range strings.Split(text, " ") {
		counts[w]++
	}
	type entry struct {
		w string
		c int64
	}
	entries := make([]entry, 0, len(counts))
	for w, c := range counts {
		entries = append(entries, entry{w, c})
	}
	sort.Slice(entries, func(i, j int) bool {
		if entries[i].c != entries[j].c {
			return entries[i].c > entries[j].c
		}
		return entries[i].w < entries[j].w
	})
	fmt.Println(len(counts))
	for _, e := range entries[:10] {
		fmt.Println(e.w, e.c)
	}
}
