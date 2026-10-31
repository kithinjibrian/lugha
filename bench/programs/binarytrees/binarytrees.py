import sys

sys.setrecursionlimit(10000)


# Every node is its own object; None is a leaf.
class Node:
    __slots__ = ("l", "r")

    def __init__(self, l, r):
        self.l = l
        self.r = r


def make(depth):
    if depth == 0:
        return Node(None, None)
    return Node(make(depth - 1), make(depth - 1))


def check(n):
    return 0 if n is None else 1 + check(n.l) + check(n.r)


def main(max_depth):
    print(f"stretch {max_depth + 1} {check(make(max_depth + 1))}")
    long_lived = make(max_depth)
    for depth in range(4, max_depth + 1, 2):
        iterations = 1 << (max_depth - depth + 4)
        total = 0
        for _ in range(iterations):
            total += check(make(depth))
        print(f"{iterations} {depth} {total}")
    print(f"long {max_depth} {check(long_lived)}")


main(int(sys.argv[1]))
