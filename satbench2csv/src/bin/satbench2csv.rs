use regex::Regex;
use satbench2csv::bench18::SCB;
use std::collections::HashMap;
use std::fs;
use std::fs::File;
use std::io::*;
use std::path::PathBuf;
use structopt::StructOpt;

/// Configuration built from command line options
#[derive(Debug, StructOpt)]
#[structopt(
    name = "satbench2csv",
    about = "Convert SAT Competition Benchmark results to a CSV file"
)]
pub struct Config {
    /// directory to scan
    #[structopt(long = "from", default_value = ".")]
    pub from: String,
    /// solver name (use 'from' if this is empty).
    #[structopt(long = "solver", default_value = "")]
    pub solver: String,
    /// value for instances timed out
    #[structopt(long = "timeout", default_value = "3000")]
    pub timeout: usize,
    /// Name for the target set, ending with a delimitor
    #[structopt(long = "target", default_value = "SC18main/")]
    pub target: String,
}

fn main() -> std::io::Result<()> {
    let config = Config::from_args();
    let mut hash: HashMap<&str, f64> = HashMap::new();
    let tag: &str = if config.solver.is_empty() {
        if config.from.ends_with('/') {
            &config.from[..config.from.len() - 1]
        } else {
            &config.from
        }
    } else {
        &config.solver
    };
    let timeout = config.timeout as f64;
    for e in fs::read_dir(&config.from)? {
        let f = e?;
        if !f.file_type()?.is_file() {
            continue;
        }
        let fname = f.file_name().to_string_lossy().to_string();
        if fname.starts_with(".ans_") {
            let cnf = &fname[5..];
            for key in SCB.iter() {
                if *key == cnf {
                    if None != hash.get(key) {
                        panic!("duplicated {}", cnf);
                    }
                    if let Some(t) = read_time(f.path()) {
                        hash.insert(key, timeout.min(t));
                        break;
                    }
                }
            }
        }
    }
    println!("solver, num, target, time");
    for (i, key) in SCB.iter().enumerate() {
        if let Some(v) = hash.get(key) {
            println!(
                "\"{}\",{},\"{}{}\",{:>8.2}",
                tag,
                i + 1,
                config.target,
                key,
                *v
            );
        } else {
            println!(
                "\"{}\",{},\"{}{}\",{:>5}",
                tag,
                i + 1,
                config.target,
                key,
                config.timeout
            );
        }
    }
    Ok(())
}

fn read_time(fname: PathBuf) -> Option<f64> {
    let f;
    match File::open(fname) {
        Ok(fin) => f = fin,
        Err(_) => return None,
    }
    let mut input = BufReader::new(f);
    let splr = Regex::new(r"time: +([.0-9]+)").expect("wrong regex");
    let glucose = Regex::new(r"^c CPU time +: ([.0-9]+)").expect("wrong regex");
    let mut buf = String::new();
    while let Ok(k) = input.read_line(&mut buf) {
        if k == 0 {
            break;
        }
        if let Some(c) = splr.captures(&buf) {
            if let Ok(v) = c[1].parse() {
                return Some(v);
            }
        } else if let Some(c) = glucose.captures(&buf) {
            if let Ok(v) = c[1].parse() {
                return Some(v);
            }
        }
        buf.clear();
    }
    None
}