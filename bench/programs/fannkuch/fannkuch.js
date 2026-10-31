const n = Number(process.argv[2]);
const perm1 = new Int32Array(n), perm = new Int32Array(n), count = new Int32Array(n);
for (let i = 0; i < n; i++) perm1[i] = i;
let r = n, permCount = 0, maxFlips = 0, checksum = 0;
outer: for (;;) {
  while (r !== 1) { count[r - 1] = r; r--; }
  perm.set(perm1);
  let flips = 0;
  for (let k = perm[0]; k !== 0; k = perm[0]) {
    for (let i = 0, j = k; i < j; i++, j--) {
      const t = perm[i]; perm[i] = perm[j]; perm[j] = t;
    }
    flips++;
  }
  if (flips > maxFlips) maxFlips = flips;
  checksum += permCount % 2 === 0 ? flips : -flips;
  for (;;) {
    if (r === n) break outer;
    const first = perm1[0];
    for (let i = 0; i < r; i++) perm1[i] = perm1[i + 1];
    perm1[r] = first;
    if (--count[r] > 0) break;
    r++;
  }
  permCount++;
}
console.log(`${checksum} ${maxFlips}`);
