use util::unsafe_arena::Arena;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[derive(Default, Copy, Clone)]
struct Value {
    inner: usize,
    junk: [usize; 8],
}

fn unsafe_arena() {
    let arena = Arena::<Value>::with_capacity(256);
    let mut refs = Vec::new();
    for _ in 0..1025 {
        refs.push(arena.alloc(Value::default()));
    }

    for (idx, r) in refs.into_iter().enumerate() {
        r.inner = idx;
        r.junk = [idx; 8];
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("unsafe arena", |b| b.iter(|| unsafe_arena()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
