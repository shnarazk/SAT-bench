use sat_bench::bench18::SCB;
use sat_bench::utils::parse_result;
use std::collections::HashMap;
use std::fs;
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
    let mut hash: HashMap<&str, (f64, bool, String)> = HashMap::new();
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
    let mut nsat = 0;
    let mut nunsat = 0;
    for e in fs::read_dir(&config.from)? {
        let f = e?;
        if !f.file_type()?.is_file() {
            continue;
        }
        let fname = f.file_name().to_string_lossy().to_string();
        if fname.starts_with(".ans_") {
            let cnf = &fname[5..];
            for (_n, key) in SCB.iter() {
                if *key == cnf {
                    if None != hash.get(key) {
                        panic!("duplicated {}", cnf);
                    }
                    if let Some((t, s, m)) = parse_result(f.path()) {
                        if s {
                            nsat += 1;
                        } else {
                            nunsat += 1;
                        }
                        hash.insert(key, (timeout.min(t), s, m));
                        break;
                    }
                }
            }
        }
    }
    println!(
        "# SAT: {}, UNSAT: {}, total: {} so far",
        nsat,
        nunsat,
        nsat + nunsat
    );
    println!("solver, num, target, nsolved, time, strategy, satisfiability");
    let mut nsolved = 0;
    for (i, key) in SCB.iter() {
        if let Some(v) = hash.get(key) {
            nsolved += 1;
            println!(
                "\"{}\",{},\"{}{}\",{:>3},{:>8.2},{},{}",
                tag, i, config.target, key, nsolved, v.0, v.1, v.2,
            );
        } else {
            println!(
                "\"{}\",{},\"{}{}\",{:3},{:>5},{},",
                tag, i, config.target, key, nsolved, config.timeout, "",
            );
        }
    }
    Ok(())
}
