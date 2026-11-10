import sys


def main(n):
    size = float(n)
    inside = 0
    for y in range(n):
        ci = 2.0 * y / size - 1.0
        for x in range(n):
            cr = 2.0 * x / size - 1.5
            zr = zi = tr = ti = 0.0
            i = 0
            while i < 50 and tr + ti <= 4.0:
                zi = 2.0 * zr * zi + ci
                zr = tr - ti + cr
                tr = zr * zr
                ti = zi * zi
                i += 1
            if tr + ti <= 4.0:
                inside += 1
    print(inside)


main(int(sys.argv[1]))
