use sat_bench::{bench19::SCB, utils::make_verifier};

fn main() {
    make_verifier(&SCB, "~/Library/SAT/SR19main").expect("fail to create verify.sh");
}
