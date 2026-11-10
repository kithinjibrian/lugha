package main

import (
	"fmt"
	"os"
	"strconv"
)

// Every node is its own heap object; nil is a leaf.
type Node struct{ l, r *Node }

func build(depth int64) *Node {
	if depth == 0 {
		return &Node{}
	}
	return &Node{build(depth - 1), build(depth - 1)}
}

func check(n *Node) int64 {
	if n == nil {
		return 0
	}
	return 1 + check(n.l) + check(n.r)
}

func main() {
	maxDepth, _ := strconv.ParseInt(os.Args[1], 10, 64)
	fmt.Printf("stretch %d %d\n", maxDepth+1, check(build(maxDepth+1)))
	longLived := build(maxDepth)
	for depth := int64(4); depth <= maxDepth; depth += 2 {
		iterations := int64(1) << (maxDepth - depth + 4)
		var sum int64
		for i := int64(0); i < iterations; i++ {
			sum += check(build(depth))
		}
		fmt.Printf("%d %d %d\n", iterations, depth, sum)
	}
	fmt.Printf("long %d %d\n", maxDepth, check(longLived))
}
