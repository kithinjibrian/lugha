//! The style rules one by one (decision 61), comments (62), verification
//! (63) and idempotence (64).

use super::{FormatError, format};

/// `src` formatted; panics on any error.
fn fmt(src: &str) -> String {
    format(src).unwrap_or_else(|e| panic!("{src:?}: {e}"))
}

/// Asserts `src` formats to `want`, and `want` to itself.
fn check(src: &str, want: &str) {
    assert_eq!(fmt(src), want);
    assert_eq!(fmt(want), want, "not idempotent");
}

#[test]
fn spaces_around_operators_after_commas_and_colons() {
    check(
        "fun  add(a:i64,b :i64):i64{a+b*2}",
        "fun add(a: i64, b: i64): i64 {\n    a + b * 2\n}\n",
    );
    check(
        "fun f(){let mut x:i64=1;x+=2;}",
        "fun f() {\n    let mut x: i64 = 1;\n    x += 2;\n}\n",
    );
}

#[test]
fn an_expression_body_stays_on_its_line() {
    check("fun two():i64=2;", "fun two(): i64 = 2;\n");
}

#[test]
fn blocks_indent_four_spaces_and_nest() {
    check(
        "fun f(){while true{if x{break;}}}",
        "fun f() {\n    while true {\n        if x {\n            break;\n        }\n    }\n}\n",
    );
    check("fun f(){}", "fun f() {}\n");
}

#[test]
fn if_else_chains_keep_else_on_the_closing_line() {
    check(
        "fun f(x:i64):i64{if x<0{1}else if x==0{2}else{3}}",
        "fun f(x: i64): i64 {\n    if x < 0 {\n        1\n    } else if x == 0 {\n        2\n    } else {\n        3\n    }\n}\n",
    );
}

#[test]
fn a_list_too_wide_breaks_one_element_per_line_with_a_trailing_comma() {
    let src = "fun f(){compute(first_value_here,second_value_here,third_value_here,fourth_value_here,fifth_value_here);}";
    check(
        src,
        "fun f() {\n    compute(\n        first_value_here,\n        second_value_here,\n        third_value_here,\n        fourth_value_here,\n        fifth_value_here,\n    );\n}\n",
    );
    // A short list loses its trailing comma.
    check("fun f(){g(1,2,);}", "fun f() {\n    g(1, 2);\n}\n");
    check(
        "fun f(){let p=Point{x:1,y:2};let xs=[1,2];let t=(1,2);}",
        "fun f() {\n    let p = Point { x: 1, y: 2 };\n    let xs = [1, 2];\n    let t = (1, 2);\n}\n",
    );
}

#[test]
fn parentheses_only_where_precedence_needs_them() {
    check(
        "fun f(){let a=(b+c)*d;let e=(b*c)+d;let g=b-(c-d);let h=(b-c)-d;let i=-(b+c);let j=(a as i64)+1;}",
        "fun f() {\n    let a = (b + c) * d;\n    let e = b * c + d;\n    let g = b - (c - d);\n    let h = b - c - d;\n    let i = -(b + c);\n    let j = a as i64 + 1;\n}\n",
    );
    check(
        "fun f(){let a=((x));let b=(a<b)==c;let c=(a+b) as i64;}",
        "fun f() {\n    let a = x;\n    let b = a < b == c;\n    let c = (a + b) as i64;\n}\n",
    );
}

#[test]
fn literals_and_names_are_kept_as_written() {
    check(
        "fun f(){let a=0xFF;let b=1_000;let c=\"a\\n{b}\";let d='\\u{1F600}';let e=1.5e3;}",
        "fun f() {\n    let a = 0xFF;\n    let b = 1_000;\n    let c = \"a\\n{b}\";\n    let d = '\\u{1F600}';\n    let e = 1.5e3;\n}\n",
    );
    check(
        "fun f(){print(format(\"{:>4}\",x));}",
        "fun f() {\n    print(format(\"{:>4}\", x));\n}\n",
    );
}

#[test]
fn blank_lines_collapse_to_one_and_none_open_or_close_a_block() {
    check(
        "fun f(){\n\n  let a=1;\n\n\n\n  let b=2;\n  let c=3;\n\n}\n\n\n\nfun g(){}",
        "fun f() {\n    let a = 1;\n\n    let b = 2;\n    let c = 3;\n}\n\nfun g() {}\n",
    );
    // Items written without a blank line between them stay that way.
    check("fun f(){}\nfun g(){}", "fun f() {}\nfun g() {}\n");
}

#[test]
fn match_arms_one_per_line_commas_except_after_blocks() {
    check(
        "fun f(s:Shape):f64{match s{Circle(r)=>3.14*r*r,Dot if x=>{0.0}_=>1.0}}",
        "fun f(s: Shape): f64 {\n    match s {\n        Circle(r) => 3.14 * r * r,\n        Dot if x => {\n            0.0\n        }\n        _ => 1.0,\n    }\n}\n",
    );
    check(
        "fun f(){match p{(a,mut b)|(b,a)=>1,x:Circle=>2,-1=>3,\"s\"=>4,V()=>5}}",
        "fun f() {\n    match p {\n        (a, mut b) | (b, a) => 1,\n        x: Circle => 2,\n        -1 => 3,\n        \"s\" => 4,\n        V() => 5,\n    }\n}\n",
    );
}

#[test]
fn closures_keep_their_body_form() {
    check(
        "fun f(){let g=fun(x:i64):i64=x+1;let h=fun(y){y};}",
        "fun f() {\n    let g = fun(x: i64): i64 = x + 1;\n    let h = fun(y) {\n        y\n    };\n}\n",
    );
}

#[test]
fn declarations_struct_enum_impl_interface_extern_import() {
    check(
        "import a.b as c;public import m{X,y as z};\nstruct P<T>{private x:T,y:i64}\nenum E{A,B(i64,string)}",
        "import a.b as c;\npublic import m { X, y as z };\nstruct P<T> {\n    private x: T,\n    y: i64,\n}\nenum E {\n    A,\n    B(i64, string),\n}\n",
    );
    check(
        "impl Stack<T>{fun push(mut self,x:T){}\nfun len(self):i64=0;}",
        "impl Stack<T> {\n    fun push(mut self, x: T) {}\n    fun len(self): i64 = 0;\n}\n",
    );
    check(
        "interface Shape<T:Eq+Ord>{fun area(self):f64;fun name(self):string{\"s\"}}\nextern fun abs(x:i32):i32;",
        "interface Shape<T: Eq + Ord> {\n    fun area(self): f64;\n    fun name(self): string {\n        \"s\"\n    }\n}\nextern fun abs(x: i32): i32;\n",
    );
}

#[test]
fn self_and_types_print_as_written() {
    check(
        "impl Point{fun o():Self=Self{x:0};fun f(a:Option<i64>[],b:m.T<i64>,c:(i64,bool),d:fun(i64):bool,e:(fun():i64[])[]){}}",
        "impl Point {\n    fun o(): Self = Self { x: 0 };\n    fun f(a: Option<i64>[], b: m.T<i64>, c: (i64, bool), d: fun(i64): bool, e: (fun(): i64[])[]) {}\n}\n",
    );
}

#[test]
fn statements_loops_and_lets() {
    check(
        "fun f(){for i in 0..n{}for x of xs{}for y of mut ys{}for (i,w) of zs{}let (a,b)=t;let Some(v)=o else{return;};while let Some(x)=it.next(){}if let Ok(v)=r{}}",
        "fun f() {\n    for i in 0..n {}\n    for x of xs {}\n    for y of mut ys {}\n    for (i, w) of zs {}\n    let (a, b) = t;\n    let Some(v) = o else {\n        return;\n    };\n    while let Some(x) = it.next() {}\n    if let Ok(v) = r {}\n}\n",
    );
    check(
        "fun f():i64{x?.y[0];return 1;}",
        "fun f(): i64 {\n    x?.y[0];\n    return 1;\n}\n",
    );
}

#[test]
fn leading_trailing_and_doc_comments_stay_attached() {
    check(
        "// top\nfun f(){\n// before a\nlet a=1; // after a\n  /* dangling */\n}\n/** Docs.\n * @returns two\n */\nfun g():i64=2; // end",
        "// top\nfun f() {\n    // before a\n    let a = 1; // after a\n    /* dangling */\n}\n/** Docs.\n * @returns two\n */\nfun g(): i64 = 2; // end\n",
    );
    check(
        "struct S{\n// the x\nx:i64, // x!\ny:i64\n}\nenum E{\nA, // a\n// b\nB\n}",
        "struct S {\n    // the x\n    x: i64, // x!\n    y: i64,\n}\nenum E {\n    A, // a\n    // b\n    B,\n}\n",
    );
}

#[test]
fn a_comment_inside_an_expression_keeps_its_statement_verbatim() {
    let src = "fun f() {\n  let a = g(1, // one\n            2);\n  let  b=2;\n}\n";
    check(
        src,
        "fun f() {\n    let a = g(1, // one\n              2);\n    let b = 2;\n}\n",
    );
}

#[test]
fn a_test_attribute_goes_on_its_own_line() {
    check(
        "@test   fun adds(){assert(1+1==2);}",
        "@test\nfun adds() {\n    assert(1 + 1 == 2);\n}\n",
    );
    check(
        "// t\n@test private fun f() {}",
        "// t\n@test\nprivate fun f() {}\n",
    );
}

#[test]
fn syntax_errors_are_reported_not_formatted() {
    assert!(matches!(format("fun f( {"), Err(FormatError::Syntax(_))));
}

#[test]
fn verification_catches_a_printer_fault_and_lost_comments() {
    let original = "fun f(){a-(b-c);} // c";
    // A faulty printer that drops the parentheses.
    let wrong = "fun f() {\n    a - b - c;\n} // c\n";
    assert!(matches!(
        super::verify::verify(original, wrong, false),
        Err(FormatError::Unsafe(_))
    ));
    let lost = "fun f() {\n    a - (b - c);\n}\n";
    assert!(matches!(
        super::verify::verify(original, lost, false),
        Err(FormatError::Unsafe(_))
    ));
    assert!(
        super::verify::verify(original, "fun f() {\n    a - (b - c);\n} // c\n", false).is_ok()
    );
}

#[test]
fn extern_types_renamed_externs_and_callbacks_v16() {
    check(
        "public   extern type  Win ;\nextern \"g_connect\"  fun on(w:Win,f:fun(Win,data),u:data,d:release):u64;",
        "public extern type Win;\nextern \"g_connect\" fun on(w: Win, f: fun(Win, data), u: data, d: release): u64;\n",
    );
}

#[test]
fn extern_structs_v16() {
    check(
        "public  extern   struct Color{r:u8,g:u8,b:u8,a:u8}",
        "public extern struct Color {\n    r: u8,\n    g: u8,\n    b: u8,\n    a: u8,\n}\n",
    );
}

#[test]
fn spawn_prints_its_call_v17() {
    check(
        "fun f(){let t=spawn  work(1,2);spawn fun(){g();}();}",
        "fun f() {\n    let t = spawn work(1, 2);\n    spawn fun() {\n        g();\n    }();\n}\n",
    );
    check(
        "fun f(){let n=(spawn g()).join();}",
        "fun f() {\n    let n = (spawn g()).join();\n}\n",
    );
}

#[test]
fn select_arms_print_one_per_line_v17() {
    check(
        "fun f(){select{m=inbox.recv()=>m,out.send(42)=>{done();}after(500)=>1,default=>2}}",
        "fun f() {\n    select {\n        m = inbox.recv() => m,\n        out.send(42) => {\n            done();\n        }\n        after(500) => 1,\n        default => 2,\n    }\n}\n",
    );
}
