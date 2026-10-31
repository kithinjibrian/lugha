import sys


def main(n):
    perm1 = list(range(n))
    perm = [0] * n
    count = [0] * n
    r, perm_count, max_flips, checksum = n, 0, 0, 0
    while True:
        while r != 1:
            count[r - 1] = r
            r -= 1
        perm[:] = perm1
        flips = 0
        k = perm[0]
        while k != 0:
            i, j = 0, k
            while i < j:
                perm[i], perm[j] = perm[j], perm[i]
                i += 1
                j -= 1
            flips += 1
            k = perm[0]
        max_flips = max(max_flips, flips)
        checksum += flips if perm_count % 2 == 0 else -flips
        while True:
            if r == n:
                print(f"{checksum} {max_flips}")
                return
            first = perm1[0]
            for i in range(r):
                perm1[i] = perm1[i + 1]
            perm1[r] = first
            count[r] -= 1
            if count[r] > 0:
                break
            r += 1
        perm_count += 1


main(int(sys.argv[1]))
