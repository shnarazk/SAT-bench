use {
    sat_bench::{ANS_PREFIX, bench19::SCB, utils::parse_result},
    std::{collections::HashMap, fs},
    structopt::StructOpt,
};

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
    #[structopt(long = "timeout", default_value = "600")]
    pub timeout: usize,
    /// Name for the target set, ending with a delimitor
    #[structopt(long = "target", default_value = "SR19main/")]
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
        if fname.starts_with(ANS_PREFIX) {
            let cnf = &fname[5..];
            for (_n, key) in SCB.1.iter() {
                if *key == cnf {
                    if None != hash.get(key) {
                        panic!("duplicated {}", cnf);
                    }
                    if let Some((t, ss, m)) = parse_result(f.path()) {
                        if let Some(s) = ss {
                            if s {
                                nsat += 1;
                            } else {
                                nunsat += 1;
                            }
                            hash.insert(key, (timeout.min(t), s, m));
                        }
                    }
                    break;
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
    println!("solver,num,target,nsolved,time,strategy,satisfiability");
    let mut nsolved = 0;
    for (i, key) in SCB.1.iter() {
        if let Some(v) = hash.get(key) {
            nsolved += 1;
            println!(
                "\"{}\",{},\"{}{}\",{},{:.2},{},{}",
                tag, i, config.target, key, nsolved, v.0, v.1, v.2,
            );
        } else {
            println!(
                "\"{}\",{},\"{}{}\",{},{},,",
                tag, i, config.target, key, nsolved, config.timeout,
            );
        }
    }
    Ok(())
}
