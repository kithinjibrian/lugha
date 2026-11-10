use std::collections::HashMap;

struct Lcg { state: i64 }

impl Lcg {
    fn next(&mut self) -> i64 {
        self.state = (self.state * 1103515245 + 12345) % 2147483648;
        self.state >> 16
    }
}

fn main() {
    let n: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    let mut rng = Lcg { state: 42 };
    let mut vocab: Vec<String> = Vec::new();
    for _ in 0..1000 {
        let len = 3 + rng.next() % 8;
        let word: String = (0..len).map(|_| (97 + rng.next() % 26) as u8 as char).collect();
        vocab.push(word);
    }
    let mut picked: Vec<&str> = Vec::new();
    for _ in 0..n {
        picked.push(&vocab[(rng.next() % 1000) as usize]);
    }
    let text = picked.join(" ");

    let mut counts: HashMap<&str, i64> = HashMap::new();
    for w in text.split(' ') {
        *counts.entry(w).or_insert(0) += 1;
    }
    let mut entries: Vec<(&str, i64)> = counts.iter().map(|(w, c)| (*w, *c)).collect();
    entries.sort_by(|a, b| b.1.cmp(&a.1).then(a.0.cmp(b.0)));
    println!("{}", counts.len());
    for (w, c) in &entries[..10] {
        println!("{} {}", w, c);
    }
}
