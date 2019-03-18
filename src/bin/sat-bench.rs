/// A simple SAT benchmarker
/// Usage: sat-bench [OPTIONS] [solvers]
/// # Examples:
/// - sat-bench -s minisat                      # run on structured problems
/// - sat-bench -3 -U 225 mios                  # 3-SAT from 200 to 225 vars
/// - sat-bench -o "\-cla-decay\ 0.9" glucose   # options to solver
/// - sat-bench -t ../g2-ACG-15-10p1.cnf splr   # -t for a CNF file
use chrono::offset::TimeZone;
use chrono::{DateTime, Local};
use lazy_static::lazy_static;
use regex::Regex;
use std::fs;
use std::io::{stdout, Write};
use std::path::PathBuf;
use std::process::Command;
use std::time::{SystemTime, UNIX_EPOCH};
use structopt::StructOpt;

const VERSION: &str = "sat-bench 0.90.1";
const SAT_PROBLEMS: [(usize, &str); 7] = [
    (100, "3-SAT/UF100"),
    (125, "3-SAT/UF125"),
    (150, "3-SAT/UF150"),
    (175, "3-SAT/UF175"),
    (200, "3-SAT/UF200"),
    (225, "3-SAT/UF225"),
    (250, "3-SAT/UF250"),
];
const UNSAT_PROBLEMS: [(usize, &str); 1] = [(250, "3-SAT/UUF250")];
const STRUCTURED_PROBLEMS: [(&str, &str); 4] = [
    ("itox", "SR2015/itox_vc1130.cnf"),
    ("m283", "SR2015/manthey_DimacsSorter_28_3.cnf"),
    ("38b", "SR2015/38bits_10.dimacs.cnf"),
    ("44b", "SR2015/44bits_11.dimacs.cnf"),
];

#[derive(Clone, Debug, StructOpt)]
#[structopt(name = "sat-bench", about = "Run simple SAT benchmarks")]
struct Config {
    solvers: Vec<String>,
    #[structopt(long = "targets", short = "t", default_value = "")]
    targets: String,
    #[structopt(long = "from", short = "L", default_value = "200")]
    range_from: usize,
    #[structopt(long = "upto", short = "U", default_value = "250")]
    range_to: usize,
    #[structopt(long = "3SAT", short = "3")]
    three_sat_set: bool,
    #[structopt(long = "structured", short = "s")]
    structured_set: bool,
    #[structopt(long = "timeout", short = "T", default_value = "510")]
    timeout: usize,
    #[structopt(long = "terminate-hook", default_value = "finished")]
    terminate_hook: String,
    #[structopt(long = "options", default_value = "")]
    solver_options: String,
    #[structopt(long = "header", short = "H", default_value = "")]
    header: String,
    #[structopt(long = "message", short = "M", default_value = "")]
    message: String,
    #[structopt(long = "aux-key", short = "K", default_value = "")]
    aux_key: String,
}

fn main() {
    let mut config = Config::from_args();
    let base = env!("PWD");
    let single_solver = match config.solvers.len() {
        0 => {
            println!("Abort: no solver");
            return;
        }
        1 => true,
        _ => false,
    };
    let extra_message = if config.message == "" {
        "".to_string()
    } else {
        format!(", {}", config.message)
    };
    let host = Command::new("hostname")
        .arg("-s")
        .output()
        .expect("failed to execute process")
        .stdout;
    let h = String::from_utf8_lossy(&host[..host.len() - 1]);
    if config.solver_options.is_empty() {
        println!(
            "# {}, timeout:{} on {} @ {}{}",
            VERSION,
            config.timeout,
            h,
            system_time_to_date_time(SystemTime::now())
                .format("%F-%m-%dT%H:%M:%S")
                .to_string(),
            extra_message
        );
    } else {
        println!(
            "# {}, timeout:{}, options:'{}' on {} @ {}{}",
            VERSION,
            config.timeout,
            config.solver_options,
            h,
            system_time_to_date_time(SystemTime::now())
                .format("%F-%m-%dT%H:%M:%S")
                .to_string(),
            extra_message
        );
    }
    if single_solver {
        print_solver(&config.solvers[0]);
    }
    match config.header.as_ref() {
        "" => println!(
            "{:<14}{:>3},{:>16}{:>8}",
            "solver,", "num", "target,", "time"
        ),
        _ => println!("{}", config.header),
    }
    if !config.three_sat_set && !config.structured_set && config.targets.is_empty() {
        config.three_sat_set = true;
    }
    for solver in &config.solvers {
        if !single_solver {
            print_solver(solver);
        }
        let mut num: usize = 1;
        if config.three_sat_set {
            for cat in &[SAT_PROBLEMS.to_vec(), UNSAT_PROBLEMS.to_vec()] {
                for (n, s) in cat {
                    if config.range_from <= *n && *n <= config.range_to {
                        let dir = format!("{}/{}", base, s);
                        execute_3sats(&config, solver, num, *n, &dir);
                        num += 1;
                    }
                }
            }
        }
        if config.structured_set {
            for (k, s) in &STRUCTURED_PROBLEMS {
                let cnf = format!("{}/{}", base, s);
                execute(&config, solver, num, k, &cnf);
                num += 1;
            }
        }
        for t in config.targets.split_whitespace() {
            execute(&config, solver, num, t, t);
            num += 1;
        }
    }
    if !config.terminate_hook.is_empty() {
        let _ = Command::new(config.terminate_hook).output();
    }
}

/// show the average or total result of SAT problems
#[allow(unused_variables)]
fn execute_3sats(config: &Config, solver: &str, num: usize, n: usize, dir: &str) {
    let solver_name = format!("{}{}", solver, config.aux_key);
    let mut count: usize = 0;
    let start = SystemTime::now();
    for e in fs::read_dir(dir).unwrap() {
        if let Ok(f) = e {
            print!(
                "\x1B[1G\x1B[032mRunning on {}...\x1B[000m",
                f.path().file_name().unwrap().to_str().unwrap()
            );
            stdout().flush().unwrap();
            let mut run = Command::new("timeout");
            let mut command = run.arg(format!("{}", config.timeout)).set_solver(solver);
            for opt in config.solver_options.split_whitespace() {
                command = command.arg(&opt[opt.starts_with('\\') as usize..]);
            }
            match command
                .arg(f.path())
                .check_result(solver, &start, config.timeout as f64)
            {
                Some(_) => {
                    count += 1;
                }
                None => {
                    println!(
                        "\x1B[1G\x1B[0K{:<14}{:>3},{:>16}{}",
                        &format!("\"{}\",", solver_name),
                        num,
                        &format!("\"UF{}\",", n),
                        &format!("TIMEOUT at {}", f.file_name().to_str().unwrap(),),
                    );
                    return;
                }
            }
        }
    }
    let end: f64 = match start.elapsed() {
        Ok(e) => e.as_secs() as f64 + f64::from(e.subsec_millis()) / 1000.0f64,
        Err(_) => 0.0f64,
    };
    println!(
        "\x1B[1G\x1B[0K{:<14}{:>3},{:>16}{:>8.3}",
        &format!("\"{}\",", solver_name),
        num,
        &format!(
            "\"{}({})\",",
            PathBuf::from(dir).file_name().unwrap().to_string_lossy(),
            count
        ),
        end,
    );
}

#[allow(unused_variables)]
fn execute(config: &Config, solver: &str, num: usize, name: &str, target: &str) {
    let solver_name = format!("{}{}", solver, config.aux_key);
    for e in target.split_whitespace() {
        let f = PathBuf::from(e);
        if f.is_file() {
            print!(
                "\x1B[1G\x1B[032mRunning on {}...\x1B[000m",
                f.file_name().unwrap().to_str().unwrap()
            );
            stdout().flush().unwrap();
            let start = SystemTime::now();
            let mut run = Command::new("timeout");
            let mut command = run.arg(format!("{}", config.timeout)).set_solver(solver);
            for opt in config.solver_options.split_whitespace() {
                command = command.arg(&opt[opt.starts_with('\\') as usize..]);
            }
            match command
                .arg(f.as_os_str())
                .check_result(solver, &start, config.timeout as f64)
            {
                Some(end) => {
                    println!(
                        "\x1B[1G\x1B[0K{:<14}{:>3},{:>16}{:>8.3}",
                        &format!("\"{}\",", solver_name),
                        num,
                        &format!("\"{}\",", name),
                        end,
                    );
                }
                None => {
                    println!(
                        "\x1B[1G\x1B[0K{:<14}{:>3},{:>16}{:>8}",
                        &format!("\"{}\",", solver_name),
                        num,
                        &format!("\"{}\",", name),
                        "TIMEOUT",
                    );
                }
            };
        }
    }
}

trait SolverHandling {
    fn set_solver(&mut self, solver: &str) -> &mut Self;
    fn check_result(&mut self, solver: &str, start: &SystemTime, timeout: f64) -> Option<f64>;
}

impl SolverHandling for Command {
    fn set_solver(&mut self, solver: &str) -> &mut Command {
        lazy_static! {
            static ref GLUCOSE: Regex = Regex::new(r"\bglucose").expect("wrong regex");
            // static ref lingeling: Regex = Regex::new(r"\blingeling").expect("wrong regex");
            // static ref minisat: Regex = Regex::new(r"\bminisat").expect("wrong regex");
            // static ref mios: Regex = Regex::new(r"\bmios").expect("wrong regex");
            static ref SPLR: Regex = Regex::new(r"\bsplr").expect("wrong regex");
        }
        if SPLR.is_match(solver) {
            self.args(&[solver, "-r", "-"])
        } else if GLUCOSE.is_match(solver) {
            self.args(&[solver, "-verb=0"])
        } else {
            self.arg(solver)
        }
    }
    fn check_result(&mut self, solver: &str, start: &SystemTime, timeout: f64) -> Option<f64> {
        lazy_static! {
            static ref MINISAT_LIKE: Regex =
                Regex::new(r"\b(glucose|minisat)").expect("wrong regex");
        }
        let result = self.output();
        match result {
            Ok(ref done) => {
                match done.status.code() {
                    Some(10) | Some(20) if MINISAT_LIKE.is_match(solver) => (),
                    Some(0) => (),
                    _ => return None,
                }
                match start.elapsed() {
                    Ok(e) => {
                        let end = e.as_secs() as f64 + f64::from(e.subsec_millis()) / 1000.0f64;
                        if end < timeout {
                            Some(end)
                        } else {
                            None
                        }
                    }
                    Err(_) => None,
                }
            }
            Err(_) => None,
        }
    }
}

fn print_solver(solver: &str) -> Option<String> {
    let mut which = match Command::new("which").arg(&solver).output() {
        Ok(o) => String::from_utf8_lossy(&o.stdout).to_string(),
        _ => return None,
    };
    which.pop();
    // printf 更新時刻とフルパス、バージョンのみ表示
    let version = match Command::new(solver).arg("--version").output() {
        Ok(o) => String::from_utf8_lossy(&o.stdout[..o.stdout.len() - 1]).to_string(),
        _ => String::from("???"),
    };
    print!("# {} ({})", which, version);
    let at = fs::metadata(&which);
    if let Ok(meta) = at {
        if let Ok(time) = meta.modified() {
            println!(
                " @ {}",
                system_time_to_date_time(time)
                    .format("%F-%m-%dT%H:%M:%S")
                    .to_string()
            );
        }
    }
    Some(which.to_string())
}

// See https://users.rust-lang.org/t/convert-std-time-systemtime-to-chrono-datetime-datetime
fn system_time_to_date_time(t: SystemTime) -> DateTime<Local> {
    let (sec, nsec) = match t.duration_since(UNIX_EPOCH) {
        Ok(dur) => (dur.as_secs() as i64, dur.subsec_nanos()),
        Err(e) => {
            // unlikely but should be handled
            let dur = e.duration();
            let (sec, nsec) = (dur.as_secs() as i64, dur.subsec_nanos());
            if nsec == 0 {
                (-sec, 0)
            } else {
                (-sec - 1, 1_000_000_000 - nsec)
            }
        }
    };
    Local.timestamp(sec, nsec)
}
