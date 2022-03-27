// pub mod bench17;
// pub mod bench18;
pub mod bench19;
pub mod bench20;
pub mod bench21;

#[cfg(feature = "matrix")]
pub mod matrix;

pub mod utils;
pub const ANS_PREFIX: &str = "ans_";

#[macro_export]
macro_rules! regex {
    ($re:literal $(,)?) => {{
        static RE: once_cell::sync::OnceCell<regex::Regex> = once_cell::sync::OnceCell::new();
        RE.get_or_init(|| regex::Regex::new($re).unwrap())
    }};
}
