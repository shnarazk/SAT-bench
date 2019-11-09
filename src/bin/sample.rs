use {
    futures::{executor::block_on, join, pin_mut, stream::self, Stream},
    std::{io, pin::Pin},
};

fn main() {
    block_on(async { join!(run1(), run2()) });
    let v: Vec<Result<usize, io::Error>> = vec![Ok(1), Ok(3), Ok(4)];
    let s = stream::iter(v);
    pin_mut!(s);
    block_on(jump_around(s)).unwrap();
}

 async fn run1() {
    println!("Hello, world from 1.");
}

async fn run2() {
    println!("Hello, world from 2.");
}

async fn jump_around(
    stream: Pin<&mut dyn Stream<Item = Result<usize, io::Error>>>,
) -> Result<(), io::Error> {
    use futures::stream::TryStreamExt; // for `try_for_each_concurrent`
    const MAX_CONCURRENT_JUMPERS: usize = 100;
    stream.try_for_each_concurrent(MAX_CONCURRENT_JUMPERS, |num| async move {
        // jump_n_times(num).await?;
        report_n_jumps(num).await;
        Ok(())
    }).await?;
    Ok(())
}

async fn report_n_jumps(n: usize) {
    println!("ok {}", n);
}
