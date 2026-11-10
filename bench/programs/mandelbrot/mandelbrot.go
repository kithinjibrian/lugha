package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {
	n, _ := strconv.ParseInt(os.Args[1], 10, 64)
	size := float64(n)
	var inside int64
	for y := int64(0); y < n; y++ {
		ci := 2.0*float64(y)/size - 1.0
		for x := int64(0); x < n; x++ {
			cr := 2.0*float64(x)/size - 1.5
			var zr, zi, tr, ti float64
			for i := 0; i < 50 && tr+ti <= 4.0; i++ {
				// Explicit float64 conversions stop Go fusing into FMA,
				// which would change the results on some CPUs.
				zi = float64(2.0*zr*zi) + ci
				zr = tr - ti + cr
				tr = zr * zr
				ti = zi * zi
			}
			if tr+ti <= 4.0 {
				inside++
			}
		}
	}
	fmt.Println(inside)
}
