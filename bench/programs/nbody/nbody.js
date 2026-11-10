const PI = 3.141592653589793;
const SOLAR_MASS = 4.0 * PI * PI;
const DAYS = 365.24;

const body = (x, y, z, vx, vy, vz, mass) => ({ x, y, z, vx, vy, vz, mass });

const bodies = [
  body(0, 0, 0, 0, 0, 0, SOLAR_MASS),
  body(4.8414314424647209, -1.16032004402742839, -1.03622044471123109e-1,
    1.66007664274403694e-3 * DAYS, 7.69901118419740425e-3 * DAYS, -6.90460016972063023e-5 * DAYS,
    9.54791938424326609e-4 * SOLAR_MASS),
  body(8.34336671824457987, 4.12479856412430479, -4.03523417114321381e-1,
    -2.76742510726862411e-3 * DAYS, 4.99852801234917238e-3 * DAYS, 2.30417297573763929e-5 * DAYS,
    2.85885980666130812e-4 * SOLAR_MASS),
  body(1.2894369562139131e1, -1.51111514016986312e1, -2.23307578892655734e-1,
    2.96460137564761618e-3 * DAYS, 2.3784717395948095e-3 * DAYS, -2.96589568540237556e-5 * DAYS,
    4.36624404335156298e-5 * SOLAR_MASS),
  body(1.53796971148509165e1, -2.59193146099879641e1, 1.79258772950371181e-1,
    2.68067772490389322e-3 * DAYS, 1.62824170038242295e-3 * DAYS, -9.5159225451971587e-5 * DAYS,
    5.15138902046611451e-5 * SOLAR_MASS),
];

function offsetMomentum(bs) {
  let px = 0, py = 0, pz = 0;
  for (const b of bs) {
    px += b.vx * b.mass;
    py += b.vy * b.mass;
    pz += b.vz * b.mass;
  }
  bs[0].vx = -px / SOLAR_MASS;
  bs[0].vy = -py / SOLAR_MASS;
  bs[0].vz = -pz / SOLAR_MASS;
}

function energy(bs) {
  let e = 0;
  for (let i = 0; i < bs.length; i++) {
    const b = bs[i];
    e += 0.5 * b.mass * (b.vx * b.vx + b.vy * b.vy + b.vz * b.vz);
    for (let j = i + 1; j < bs.length; j++) {
      const c = bs[j];
      const dx = b.x - c.x, dy = b.y - c.y, dz = b.z - c.z;
      e -= (b.mass * c.mass) / Math.sqrt(dx * dx + dy * dy + dz * dz);
    }
  }
  return e;
}

function advance(bs, dt) {
  const n = bs.length;
  for (let i = 0; i < n; i++) {
    const a = bs[i];
    for (let j = i + 1; j < n; j++) {
      const b = bs[j];
      const dx = a.x - b.x, dy = a.y - b.y, dz = a.z - b.z;
      const d2 = dx * dx + dy * dy + dz * dz;
      const mag = dt / (d2 * Math.sqrt(d2));
      const mi = a.mass * mag, mj = b.mass * mag;
      a.vx -= dx * mj; a.vy -= dy * mj; a.vz -= dz * mj;
      b.vx += dx * mi; b.vy += dy * mi; b.vz += dz * mi;
    }
  }
  for (const b of bs) {
    b.x += dt * b.vx;
    b.y += dt * b.vy;
    b.z += dt * b.vz;
  }
}

const steps = Number(process.argv[2]);
offsetMomentum(bodies);
console.log(energy(bodies).toFixed(9));
for (let i = 0; i < steps; i++) advance(bodies, 0.01);
console.log(energy(bodies).toFixed(9));
