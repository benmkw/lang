use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};
use lang_lib::*;

// TODO remove too much printing which is currently happening and convert it to configurable logging
fn basic_benchmark(c: &mut Criterion) {
    let input = "293474*4^5-50";
    c.bench_function("cdr", |b| b.iter(|| run(black_box(input))));
}

fn throughput(c: &mut Criterion) {
    let mut group = c.benchmark_group("throughput");

    let base_input = "29*4^5-50";
    let strlen = base_input.chars().count() + 1;

    for size in [50, 1_000].iter() {
        let input: String = std::iter::repeat(format!("{}+", base_input))
            .take(*size)
            .collect();
        let input = format!("{}{}", input, base_input);

        group.throughput(Throughput::Bytes((*size * strlen - 1) as u64));

        group.bench_with_input(BenchmarkId::new("arithmetic", *size), &input, |b, i| {
            b.iter(|| run(black_box(&i)))
        });
    }

    group.finish();
}

criterion_group!(benches, basic_benchmark, throughput);
// criterion_group!(benches, throughput);
criterion_main!(benches);
