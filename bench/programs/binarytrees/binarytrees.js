// Every node is its own object; null is a leaf.
class Node {
  constructor(l, r) {
    this.l = l;
    this.r = r;
  }
}

function make(depth) {
  return depth === 0 ? new Node(null, null) : new Node(make(depth - 1), make(depth - 1));
}

function check(n) {
  return n === null ? 0 : 1 + check(n.l) + check(n.r);
}

const maxDepth = Number(process.argv[2]);
const out = [];
out.push(`stretch ${maxDepth + 1} ${check(make(maxDepth + 1))}`);
const longLived = make(maxDepth);
for (let depth = 4; depth <= maxDepth; depth += 2) {
  const iterations = 2 ** (maxDepth - depth + 4);
  let sum = 0;
  for (let i = 0; i < iterations; i++) sum += check(make(depth));
  out.push(`${iterations} ${depth} ${sum}`);
}
out.push(`long ${maxDepth} ${check(longLived)}`);
console.log(out.join("\n"));
