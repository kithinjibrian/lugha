//! The `extern struct` shapes `tests/extern_structs.rs` (v16, milestone 90)
//! and `tests/arm.rs` (v17, milestone 134) both run: a C side computing
//! every result, the Lugha declarations of it, and programs with what they
//! print. A wrong calling convention shows as a wrong number.

#![allow(dead_code)] // Each test file uses part of it.

pub const SIDE: &str = "typedef struct { unsigned char r, g, b, a; } Color;\n\
    typedef struct { float x, y; } V2;\n\
    typedef struct { float x, y, w, h; } Rect;\n\
    typedef struct { long i; double d; } Mixed;\n\
    typedef struct { float f; int n; } FI;\n\
    typedef struct { V2 offset, target; float rotation, zoom; } Cam;\n\
    typedef struct { unsigned char tag; double v; } TagD;\n\
    typedef struct { float x, y, z; } V3;\n\
    typedef struct { long a, b; } L2;\n\
    Color color_dim(Color c) { Color r = { c.r / 2, c.g / 2, c.b / 2, c.a }; return r; }\n\
    long color_sum(Color c) { return c.r + c.g + c.b + c.a; }\n\
    V2 v2_add(V2 a, V2 b) { V2 r = { a.x + b.x, a.y + b.y }; return r; }\n\
    Rect rect_grow(Rect r, float by) { Rect o = { r.x - by, r.y - by, r.w + 2 * by, r.h + 2 * by }; return o; }\n\
    Mixed mixed_twice(Mixed m) { Mixed o = { m.i * 2, m.d * 2 }; return o; }\n\
    FI fi_swap(FI v) { FI o = { (float)v.n, (int)v.f }; return o; }\n\
    Cam cam_zoom(Cam c, float z) { c.zoom *= z; c.target.x += 1; return c; }\n\
    double cam_sum(Cam c) { return c.offset.x + c.offset.y + c.target.x + c.target.y + c.rotation + c.zoom; }\n\
    TagD tagd_bump(TagD t) { t.tag++; t.v += 0.5; return t; }\n\
    V3 v3_scale(V3 v, float k) { V3 o = { v.x * k, v.y * k, v.z * k }; return o; }\n\
    long many(long a, long b, long c, long d, long e, L2 s, long f) { return a + b + c + d + e + s.a * 100 + s.b * 1000 + f * 10000; }\n\
    double floats(double a, double b, double c, double d, double e, double f, double g, double h, V2 v, double i) { return a + b + c + d + e + f + g + h + v.x * 10 + v.y * 100 + i * 1000; }\n\
    V2 map_point(V2 (*f)(V2, void *), void *d) { V2 p = { 1.5f, 2.5f }; return f(p, d); }\n\
    double map_cam(Cam (*f)(Cam, void *), void *d) { Cam c = { { 1, 2 }, { 3, 4 }, 5, 6 }; return cam_sum(f(c, d)); }\n\
    long pick(Color (*f)(long), long i) { return color_sum(f(i)); }\n";

pub const DECLS: &str = "extern struct Color { r: u8, g: u8, b: u8, a: u8 }\n\
    extern struct V2 { x: f32, y: f32 }\n\
    extern struct Rect { x: f32, y: f32, w: f32, h: f32 }\n\
    extern struct Mixed { i: i64, d: f64 }\n\
    extern struct FI { f: f32, n: i32 }\n\
    extern struct Cam { offset: V2, target: V2, rotation: f32, zoom: f32 }\n\
    extern struct TagD { tag: u8, v: f64 }\n\
    extern struct V3 { x: f32, y: f32, z: f32 }\n\
    extern struct L2 { a: i64, b: i64 }\n\
    extern fun color_dim(c: Color): Color;\n\
    extern fun color_sum(c: Color): i64;\n\
    extern fun v2_add(a: V2, b: V2): V2;\n\
    extern fun rect_grow(r: Rect, by: f32): Rect;\n\
    extern fun mixed_twice(m: Mixed): Mixed;\n\
    extern fun fi_swap(v: FI): FI;\n\
    extern fun cam_zoom(c: Cam, z: f32): Cam;\n\
    extern fun cam_sum(c: Cam): f64;\n\
    extern fun tagd_bump(t: TagD): TagD;\n\
    extern fun v3_scale(v: V3, k: f32): V3;\n\
    extern fun many(a: i64, b: i64, c: i64, d: i64, e: i64, s: L2, f: i64): i64;\n\
    extern fun floats(a: f64, b: f64, c: f64, d: f64, e: f64, f: f64, g: f64, h: f64, v: V2, i: f64): f64;\n\
    extern fun map_point(f: fun(V2, data): V2, u: data): V2;\n\
    extern fun map_cam(f: fun(Cam, data): Cam, u: data): f64;\n\
    extern fun pick(f: fun(i64): Color, i: i64): i64;\n";

/// Each program (after `DECLS`) by name, with what it prints.
pub const PROGRAMS: [(&str, &str, &str); 3] = [
    (
        "every_shape_goes_to_c_and_back",
        "fun main() {\n\
                 let red = Color { r: 230, g: 41, b: 55, a: 255 };\n\
                 println(color_dim(red));\n\
                 println(color_sum(red));\n\
                 println(v2_add(V2 { x: 1.5, y: 2.0 }, V2 { x: 0.25, y: 3.0 }));\n\
                 println(rect_grow(Rect { x: 10.0, y: 20.0, w: 30.0, h: 40.0 }, 1.5));\n\
                 println(mixed_twice(Mixed { i: 21, d: 1.25 }));\n\
                 println(fi_swap(FI { f: 7.9, n: 3 }));\n\
                 println(tagd_bump(TagD { tag: 7, v: 1.0 }));\n\
                 println(v3_scale(V3 { x: 1.0, y: 2.0, z: 3.0 }, 0.5));\n\
             }\n",
        "Color { r: 115, g: 20, b: 27, a: 255 }\n581\nV2 { x: 1.75, y: 5.0 }\n\
         Rect { x: 8.5, y: 18.5, w: 33.0, h: 43.0 }\nMixed { i: 42, d: 2.5 }\nFI { f: 3.0, n: 7 }\n\
         TagD { tag: 8, v: 1.5 }\nV3 { x: 0.5, y: 1.0, z: 1.5 }\n",
    ),
    (
        "large_structs_and_full_registers_go_in_memory",
        "fun main() {\n\
                 let cam = Cam { offset: V2 { x: 1.0, y: 2.0 }, target: V2 { x: 3.0, y: 4.0 }, rotation: 5.0, zoom: 6.0 };\n\
                 println(cam_zoom(cam, 2.0));\n\
                 println(cam_sum(cam));\n\
                 println(many(1, 2, 3, 4, 5, L2 { a: 6, b: 7 }, 8));\n\
                 println(floats(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, V2 { x: 2.0, y: 3.0 }, 4.0));\n\
             }\n",
        "Cam { offset: V2 { x: 1.0, y: 2.0 }, target: V2 { x: 4.0, y: 4.0 }, rotation: 5.0, zoom: 12.0 }\n\
         21.0\n87615\n4328.0\n",
    ),
    (
        "callbacks_take_and_return_structs",
        "fun shade(i: i64): Color = Color { r: i as u8, g: 1, b: 2, a: 3 };\n\
             fun main() {\n\
                 let k: f32 = 2.0;\n\
                 println(map_point(fun(p: V2): V2 = V2 { x: p.x * k, y: p.y + k }));\n\
                 println(map_cam(fun(c: Cam): Cam = Cam { offset: c.offset, target: c.target, rotation: c.rotation, zoom: c.zoom * k }));\n\
                 println(pick(shade, 10));\n\
             }\n",
        "V2 { x: 3.0, y: 4.5 }\n27.0\n16\n",
    ),
];
