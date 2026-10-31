const n = Number(process.argv[2]);
let inside = 0;
for (let y = 0; y < n; y++) {
  const ci = (2.0 * y) / n - 1.0;
  for (let x = 0; x < n; x++) {
    const cr = (2.0 * x) / n - 1.5;
    let zr = 0.0, zi = 0.0, tr = 0.0, ti = 0.0;
    for (let i = 0; i < 50 && tr + ti <= 4.0; i++) {
      zi = 2.0 * zr * zi + ci;
      zr = tr - ti + cr;
      tr = zr * zr;
      ti = zi * zi;
    }
    if (tr + ti <= 4.0) inside++;
  }
}
console.log(String(inside));
