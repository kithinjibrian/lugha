// Every node is its own Box, dropped after each check.
enum Tree {
    Leaf,
    Node(Box<Tree>, Box<Tree>),
}

fn make(depth: i64) -> Tree {
    if depth == 0 {
        Tree::Node(Box::new(Tree::Leaf), Box::new(Tree::Leaf))
    } else {
        Tree::Node(Box::new(make(depth - 1)), Box::new(make(depth - 1)))
    }
}

fn check(t: &Tree) -> i64 {
    match t {
        Tree::Leaf => 0,
        Tree::Node(l, r) => 1 + check(l) + check(r),
    }
}

fn main() {
    let max_depth: i64 = std::env::args().nth(1).unwrap().parse().unwrap();
    println!("stretch {} {}", max_depth + 1, check(&make(max_depth + 1)));
    let long_lived = make(max_depth);
    let mut depth = 4;
    while depth <= max_depth {
        let iterations = 1i64 << (max_depth - depth + 4);
        let mut sum = 0;
        for _ in 0..iterations {
            sum += check(&make(depth));
        }
        println!("{} {} {}", iterations, depth, sum);
        depth += 2;
    }
    println!("long {} {}", max_depth, check(&long_lived));
}
