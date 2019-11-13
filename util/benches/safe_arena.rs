use util::arena::Arena;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

#[derive(Default, Copy, Clone)]
struct Value {
    inner: usize,
    junk: [usize; 8],
}

fn safe_arena() {
    let mut arena = Arena::<Value>::with_capacity(256);
    let mut refs = Vec::new();
    for _ in 0..1025 {
        refs.push(arena.insert(Value::default()));
    }

    for (idx, r) in refs.into_iter().enumerate() {
        let ptr = arena.get_mut(r).unwrap();
        ptr.inner = idx;
        ptr.junk = [idx; 8];
    }
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("safe arena", |b| b.iter(|| safe_arena()));
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
