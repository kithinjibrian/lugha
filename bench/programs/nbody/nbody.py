import math
import sys

PI = 3.141592653589793
SOLAR_MASS = 4.0 * PI * PI
DAYS = 365.24


class Body:
    __slots__ = ("x", "y", "z", "vx", "vy", "vz", "mass")

    def __init__(self, x, y, z, vx, vy, vz, mass):
        self.x, self.y, self.z = x, y, z
        self.vx, self.vy, self.vz = vx, vy, vz
        self.mass = mass


def bodies():
    return [
        Body(0.0, 0.0, 0.0, 0.0, 0.0, 0.0, SOLAR_MASS),
        Body(4.84143144246472090e+00, -1.16032004402742839e+00, -1.03622044471123109e-01,
             1.66007664274403694e-03 * DAYS, 7.69901118419740425e-03 * DAYS, -6.90460016972063023e-05 * DAYS,
             9.54791938424326609e-04 * SOLAR_MASS),
        Body(8.34336671824457987e+00, 4.12479856412430479e+00, -4.03523417114321381e-01,
             -2.76742510726862411e-03 * DAYS, 4.99852801234917238e-03 * DAYS, 2.30417297573763929e-05 * DAYS,
             2.85885980666130812e-04 * SOLAR_MASS),
        Body(1.28943695621391310e+01, -1.51111514016986312e+01, -2.23307578892655734e-01,
             2.96460137564761618e-03 * DAYS, 2.37847173959480950e-03 * DAYS, -2.96589568540237556e-05 * DAYS,
             4.36624404335156298e-05 * SOLAR_MASS),
        Body(1.53796971148509165e+01, -2.59193146099879641e+01, 1.79258772950371181e-01,
             2.68067772490389322e-03 * DAYS, 1.62824170038242295e-03 * DAYS, -9.51592254519715870e-05 * DAYS,
             5.15138902046611451e-05 * SOLAR_MASS),
    ]


def offset_momentum(bs):
    px = py = pz = 0.0
    for b in bs:
        px += b.vx * b.mass
        py += b.vy * b.mass
        pz += b.vz * b.mass
    bs[0].vx = -px / SOLAR_MASS
    bs[0].vy = -py / SOLAR_MASS
    bs[0].vz = -pz / SOLAR_MASS


def energy(bs):
    e = 0.0
    for i, b in enumerate(bs):
        e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz)
        for c in bs[i + 1:]:
            dx, dy, dz = b.x - c.x, b.y - c.y, b.z - c.z
            e -= b.mass * c.mass / math.sqrt(dx * dx + dy * dy + dz * dz)
    return e


def advance(bs, dt):
    n = len(bs)
    for i in range(n):
        a = bs[i]
        for j in range(i + 1, n):
            b = bs[j]
            dx, dy, dz = a.x - b.x, a.y - b.y, a.z - b.z
            d2 = dx * dx + dy * dy + dz * dz
            mag = dt / (d2 * math.sqrt(d2))
            mi, mj = a.mass * mag, b.mass * mag
            a.vx -= dx * mj
            a.vy -= dy * mj
            a.vz -= dz * mj
            b.vx += dx * mi
            b.vy += dy * mi
            b.vz += dz * mi
    for b in bs:
        b.x += dt * b.vx
        b.y += dt * b.vy
        b.z += dt * b.vz


def main(steps):
    bs = bodies()
    offset_momentum(bs)
    print(f"{energy(bs):.9f}")
    for _ in range(steps):
        advance(bs, 0.01)
    print(f"{energy(bs):.9f}")


main(int(sys.argv[1]))
