// The LCG's products stay below 2^61, past exact doubles, so it uses BigInt.
let state = 42n;
function next() {
  state = (state * 1103515245n + 12345n) % 2147483648n;
  return Number(state >> 16n);
}

const n = Number(process.argv[2]);
const vocab = [];
for (let v = 0; v < 1000; v++) {
  const len = 3 + (next() % 8);
  let w = "";
  for (let i = 0; i < len; i++) w += String.fromCharCode(97 + (next() % 26));
  vocab.push(w);
}
const picked = [];
for (let i = 0; i < n; i++) picked.push(vocab[next() % 1000]);
const text = picked.join(" ");

const counts = new Map();
for (const w of text.split(" ")) counts.set(w, (counts.get(w) || 0) + 1);
const entries = [...counts.entries()];
entries.sort((a, b) => (b[1] - a[1]) || (a[0] < b[0] ? -1 : a[0] > b[0] ? 1 : 0));
const out = [String(counts.size)];
for (const [w, c] of entries.slice(0, 10)) out.push(`${w} ${c}`);
console.log(out.join("\n"));
