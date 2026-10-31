import sys


class Lcg:
    def __init__(self, state):
        self.state = state

    def next(self):
        self.state = (self.state * 1103515245 + 12345) % 2147483648
        return self.state >> 16


def main(n):
    rng = Lcg(42)
    vocab = []
    for _ in range(1000):
        length = 3 + rng.next() % 8
        vocab.append("".join(chr(97 + rng.next() % 26) for _ in range(length)))
    picked = [vocab[rng.next() % 1000] for _ in range(n)]
    text = " ".join(picked)

    counts = {}
    for w in text.split(" "):
        counts[w] = counts.get(w, 0) + 1
    entries = sorted(counts.items(), key=lambda e: (-e[1], e[0]))
    print(len(counts))
    for w, c in entries[:10]:
        print(f"{w} {c}")


main(int(sys.argv[1]))
