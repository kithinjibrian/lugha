package main

import (
	"fmt"
	"os"
	"strconv"
)

func main() {
	n64, _ := strconv.ParseInt(os.Args[1], 10, 64)
	n := int(n64)
	perm1, perm, count := make([]int, n), make([]int, n), make([]int, n)
	for i := range perm1 {
		perm1[i] = i
	}
	r, permCount, maxFlips, checksum := n, 0, 0, 0
	for {
		for r != 1 {
			count[r-1] = r
			r--
		}
		copy(perm, perm1)
		flips := 0
		for k := perm[0]; k != 0; k = perm[0] {
			for i, j := 0, k; i < j; i, j = i+1, j-1 {
				perm[i], perm[j] = perm[j], perm[i]
			}
			flips++
		}
		if flips > maxFlips {
			maxFlips = flips
		}
		if permCount%2 == 0 {
			checksum += flips
		} else {
			checksum -= flips
		}
		for {
			if r == n {
				fmt.Printf("%d %d\n", checksum, maxFlips)
				return
			}
			first := perm1[0]
			for i := 0; i < r; i++ {
				perm1[i] = perm1[i+1]
			}
			perm1[r] = first
			count[r]--
			if count[r] > 0 {
				break
			}
			r++
		}
		permCount++
	}
}
