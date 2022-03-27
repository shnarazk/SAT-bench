/// A simple SAT benchmarker
/// Usage: sat-bench [OPTIONS] [solvers]
/// # Examples:
/// - sat-bench -s minisat                      # run on structured problems
/// - sat-bench -3 -U 225 -L 200 mios           # 3-SAT from 200 to 225 vars
/// - sat-bench -o "\-cla-decay\ 0.9" glucose   # options to solver
/// - sat-bench -t ../g2-ACG-15-10p1.cnf splr   # -t for a CNF file
use {
    clap::Parser,
    once_cell::sync::OnceCell,
    regex::Regex,
    sat_bench::{
        regex,
        utils::{current_date_time, system_time_to_date_time},
    },
    std::{
        cmp::Ordering,
        collections::VecDeque,
        env, fs,
        io::{stdout, Write},
        path::PathBuf,
        process::Command,
        sync::RwLock,
        thread,
        time::SystemTime,
    },
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const RED: &str = "\x1B[001m\x1B[031m";
#[allow(dead_code)]
const GREEN: &str = "\x1B[001m\x1B[032m";
#[allow(dead_code)]
const BLUE: &str = "\x1B[001m\x1B[034m";
const MAGENTA: &str = "\x1B[001m\x1B[035m";
const RESET: &str = "\x1B[000m";
const CLEAR: &str = "\x1B[1G\x1B[0K";

/// Abnormal termination flags.
#[derive(Debug)]
pub enum SolverException {
    TimeOut,
    Abort,
}

type SolveResultPromise = Option<(String, Result<f64, SolverException>)>;

static PQUEUE: OnceCell<RwLock<VecDeque<(usize, String, String)>>> = OnceCell::new();
static RESVEC: OnceCell<RwLock<Vec<SolveResultPromise>>> = OnceCell::new();
static NREPORT: OnceCell<RwLock<usize>> = OnceCell::new();
static TOTALTIME: OnceCell<RwLock<Vec<f64>>> = OnceCell::new();

const SAT_PROBLEMS: [(usize, &str); 18] = [
    (100, "3-SAT/UF100"),
    (125, "3-SAT/UF125"),
    (150, "3-SAT/UF150"),
    (175, "3-SAT/UF175"),
    (200, "3-SAT/UF200"),
    (225, "3-SAT/UF225"),
    (250, "3-SAT/UF250"),
    (360, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360"),
    (380, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/380"),
    (400, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/400"),
    (420, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/420"),
    (440, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/440"),
    (460, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/460"),
    (480, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/480"),
    (500, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/500"),
    (520, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/520"),
    (540, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/540"),
    (560, "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/560"),
];
const UNSAT_PROBLEMS: [(usize, &str); 12] = [
    (250, "3-SAT/UUF250"),
    (360, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360"),
    (380, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/380"),
    (400, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/400"),
    (420, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/420"),
    (440, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/440"),
    (460, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/460"),
    (480, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/480"),
    (500, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/500"),
    (520, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/520"),
    (540, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/540"),
    (560, "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/560"),
];
const MATH_PROBLEMS: [(&str, &str); 20] = [
    (
        "3SAT/360  S144043535-002",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S144043535-002.cnf",
    ),
    (
        "3SAT/360  S722433227-030",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S722433227-030.cnf",
    ),
    (
        "3SAT/360 S1459690542-033",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1459690542-033.cnf",
    ),
    (
        "3SAT/360 S2032263657-035",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S2032263657-035.cnf",
    ),
    (
        "3SAT/360 S1293537826-039",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1293537826-039.cnf",
    ),
    (
        "3SAT/360  S368632549-051",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S368632549-051.cnf",
    ),
    (
        "3SAT/360 S1448866403-060",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1448866403-060.cnf",
    ),
    (
        "3SAT/360 S1684547485-073",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1684547485-073.cnf",
    ),
    (
        "3SAT/360 S1826927554-087",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1826927554-087.cnf",
    ),
    (
        "3SAT/360 S1711406314-093",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1711406314-093.cnf",
    ),
    (
        "3UNS/360  S404185236-001",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S404185236-001.cnf",
    ),
    (
        "3UNS/360 S1369720750-015",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S1369720750-015.cnf",
    ),
    (
        "3UNS/360   S23373420-028",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S23373420-028.cnf",
    ),
    (
        "3UNS/360  S367138237-029",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S367138237-029.cnf",
    ),
    (
        "3UNS/360  S305156909-031",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S305156909-031.cnf",
    ),
    (
        "3UNS/360  S680239195-053",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S680239195-053.cnf",
    ),
    (
        "3UNS/360 S2025517367-061",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S2025517367-061.cnf",
    ),
    (
        "3UNS/360  S253750560-086",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S253750560-086.cnf",
    ),
    (
        "3UNS/360 S1906521511-089",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S1906521511-089.cnf",
    ),
    (
        "3UNS/360 S1028159446-096",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S1028159446-096.cnf",
    ),
];
const STRUCTURED_PROBLEMS: [(&str, &str); 4] = [
    ("SR2015/itox", "SatRace2015/itox_vc1130.cnf"),
    ("SR2015/m283", "SatRace2015/manthey_DimacsSorter_28_3.cnf"),
    ("SR2015/38b", "SatRace2015/38bits_10.dimacs.cnf"),
    ("SR2015/44b", "SatRace2015/44bits_11.dimacs.cnf"),
];
const BIG_PROBLEMS: [(&str, &str); 7] = [
    ("SC21/b04_s_unknown[SAT]", "SC21/b04_s_unknown_pre.cnf"),
    ("SC21/quad_r21_m22 [SAT]", "SC21/quad_res_r21_m22.cnf"),
    (
        "SC21/toughsat_895s[SAT]",
        "SC21/toughsat_factoring_895s.cnf",
    ),
    ("SC21/assoc_mult_e3[UNS]", "SC21/assoc_mult_err_3.c.cnf"),
    ("SC21/dist4.c      [UNS]", "SC21/dist4.c.cnf"),
    ("SC21/p01_lb_05    [UNS]", "SC21/p01_lb_05.cnf"),
    ("SC21/shift1add    [UNS]", "SC21/shift1add.26949.cnf"),
];

const BENCHMARK: [(&str, &str); 20] = [
    (
        "3SAT/360  S722433227-030",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S722433227-030.cnf",
    ),
    (
        "3SAT/360 S2032263657-035",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S2032263657-035.cnf",
    ),
    (
        "3SAT/360  S368632549-051",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S368632549-051.cnf",
    ),
    (
        "3SAT/360 S1684547485-073",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1684547485-073.cnf",
    ),
    (
        "3SAT/360 S1711406314-093",
        "SAT09/RANDOM/MEDIUM/3SAT/SATISFIABLE/360/unif-k3-r4.25-v360-c1530-S1711406314-093.cnf",
    ),
    (
        "3UNS/360 S1369720750-015",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S1369720750-015.cnf",
    ),
    (
        "3UNS/360  S367138237-029",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S367138237-029.cnf",
    ),
    (
        "3UNS/360  S680239195-053",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S680239195-053.cnf",
    ),
    (
        "3UNS/360  S253750560-086",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S253750560-086.cnf",
    ),
    (
        "3UNS/360 S1028159446-096",
        "SAT09/RANDOM/MEDIUM/3SAT/UNKNOWN/360/unif-k3-r4.25-v360-c1530-S1028159446-096.cnf",
    ),
    (
        "[SAT] SR2015/m283,  3553",
        "SatRace2015/manthey_DimacsSorter_28_3.cnf",
    ),
    (
        "[SAT] SR2015/38b,    448",
        "SatRace2015/38bits_10.dimacs.cnf",
    ),
    (
        "[SAT] SR2015/44b,    609",
        "SatRace2015/44bits_11.dimacs.cnf",
    ),
    ("[SAT] SC21/b04_s_unknown", "SC21/b04_s_unknown_pre.cnf"),
    ("[SAT] SC21/quad_r21_m22 ", "SC21/quad_res_r21_m22.cnf"),
    (
        "[SAT] SC21/toughsat_895s",
        "SC21/toughsat_factoring_895s.cnf",
    ),
    ("[UNS] SC21/assoc_mult_e3", "SC21/assoc_mult_err_3.c.cnf"),
    ("[UNS] SC21/dist4.c      ", "SC21/dist4.c.cnf"),
    ("[UNS] SC21/p01_lb_05    ", "SC21/p01_lb_05.cnf"),
    ("[UNS] SC21/shift1add    ", "SC21/shift1add.26949.cnf"),
];

#[derive(Clone, Debug, Parser)]
#[clap(author, version, name = "sat-bench", about = "Run simple SAT benchmarks")]
struct Config {
    /// solvers names
    solvers: Vec<String>,
    /// a list of CNF files
    #[clap(long = "target", default_value = "")]
    targets: String,
    /// Lower limit of #vars of 3-SAT instances
    #[clap(long = "from", short = 'L', default_value = "250")]
    range_from: usize,
    /// Upper limit of #vars of 3-SAT instances
    #[clap(long = "upto", short = 'U', default_value = "360")]
    range_to: usize,
    /// 3-SAT instances
    #[clap(long = "3SAT", short = '3')]
    three_sat_set: bool,
    #[clap(long = "benchmark", short = 'B')]
    benchmark_set: bool,
    /// Big instances
    #[clap(long = "big", short = 'b')]
    big_problem_set: bool,
    /// Structured instances
    #[clap(long = "structured", short = 's')]
    structured_set: bool,
    /// SAT/UNSAT 360 3SAT instances
    #[clap(long = "massive", short = 'm')]
    massive_3sat_set: bool,
    /// only UNSAT 360 3SAT instances
    #[clap(long = "unsat360", short = 'u')]
    unsat_360_3sat_set: bool,
    /// time out in seconds
    #[clap(long = "timeout", short = 't', default_value = "2000")]
    timeout: usize,
    /// command to be executed after a run
    #[clap(long = "hook", default_value = "finished")]
    hook: String,
    /// arguments passed to solvers
    #[clap(long = "options", short = 'O', default_value = "")]
    solver_opts: String,
    ///  additinal string used in header
    #[clap(long = "message", short = 'M', default_value = "")]
    message: String,
    /// additional string following solver name
    #[clap(long = "aux-key", short = 'K', default_value = "")]
    aux_key: String,
    /// data directory
    #[clap(long = "lib", default_value = "")]
    lib_dir: String,
    /// the number of jobs in parallel
    #[clap(long = "jobs", short = 'j', default_value = "4")]
    num_jobs: usize,
    /// disable realtime report
    #[clap(long = "no-report", short = 'Q')]
    no_report: bool,
}

fn main() {
    let _ = PQUEUE.set(RwLock::new(VecDeque::new())); // : RwLock<VecDeque<(usize, String, String)>> =
    let _ = RESVEC.set(RwLock::new(Vec::new())); // : RwLock<Vec<SolveResultPromise>> = ;
    let _ = NREPORT.set(RwLock::new(0)); // : RwLock<usize> = ;
    let _ = TOTALTIME.set(RwLock::new(Vec::new())); // : RwLock<Vec<f64>> = ;
    let mut config = Config::parse();
    let base = if config.lib_dir.is_empty() {
        match option_env!("SATBENCHLIB") {
            Some(dir) => dir,
            None => env!("PWD"),
        }
    } else {
        &config.lib_dir
    };
    let single_solver = match config.solvers.len() {
        0 => {
            println!("Abort: no solver");
            return;
        }
        1 => true,
        _ => false,
    };
    let extra_message = if config.message.is_empty() {
        "".to_string()
    } else {
        format!(", {}", config.message)
    };
    if !config.three_sat_set
        && !config.big_problem_set
        && !config.structured_set
        && !config.massive_3sat_set
        && !config.unsat_360_3sat_set
        && config.targets.is_empty()
    {
        config.three_sat_set = true;
        config.range_to = 250;
        config.benchmark_set = true;
    }
    let host = Command::new("hostname")
        .arg("-s")
        .output()
        .expect("failed to execute process")
        .stdout;
    let h = String::from_utf8_lossy(&host[..host.len() - 1]);
    if config.solver_opts.is_empty() {
        println!(
            "# {}, timeout:{} on {} @ {}{}",
            VERSION,
            config.timeout,
            h,
            current_date_time().format("%FT%H:%M:%S"),
            extra_message
        );
    } else {
        println!(
            "# {}, timeout:{}, options:'{}' on {} @ {}{}",
            VERSION,
            config.timeout,
            config.solver_opts,
            h,
            current_date_time().format("%FT%H:%M:%S"),
            extra_message
        );
    }
    if single_solver {
        print_solver(&config.solvers[0]);
    }
    println!(
        "{:<14}{:>3},{:>30}{:>9}",
        "solver,", "num", "target,", "time"
    );
    for solver in &config.solvers {
        if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
            t.clear();
        }
        if !single_solver {
            print_solver(solver);
        }
        let solver_name = format!("{}{}", solver, config.aux_key);
        let mut num: usize = 1;
        if config.three_sat_set {
            for (n, s) in &SAT_PROBLEMS {
                if config.range_from <= *n && *n <= config.range_to {
                    let dir = format!("{}/{}", base, s);
                    execute_3sats(&config, solver, "UF", num, *n, &dir);
                    num += 1;
                }
            }
            for (n, s) in &UNSAT_PROBLEMS {
                if config.range_from <= *n && *n <= config.range_to {
                    let dir = format!("{}/{}", base, s);
                    execute_3sats(&config, solver, "UUF", num, *n, &dir);
                    num += 1;
                }
            }
        }
        if config.massive_3sat_set {
            threaded_execute(&config, &solver_name, &MATH_PROBLEMS, &mut num, base);
        } else if config.unsat_360_3sat_set {
            threaded_execute(&config, &solver_name, &MATH_PROBLEMS[10..], &mut num, base);
        }
        if config.benchmark_set {
            threaded_execute(&config, &solver_name, &BENCHMARK, &mut num, base);
        }
        if config.big_problem_set {
            threaded_execute(&config, &solver_name, &BIG_PROBLEMS, &mut num, base);
        }
        if config.structured_set {
            threaded_execute(&config, &solver_name, &STRUCTURED_PROBLEMS, &mut num, base);
        }
        for t in config.targets.split_whitespace() {
            execute(&config, solver, num, t, t);
            num += 1;
        }
        if let Ok(t) = TOTALTIME.get().unwrap().read() {
            let mut v: Vec<f64> = t.iter().copied().collect();
            v.sort_by(|a, b| match (a.is_nan(), b.is_nan()) {
                (true, true) => Ordering::Equal,
                (true, false) => Ordering::Less,
                (false, true) => Ordering::Greater,
                (_, _) if (a - b).abs() < 0.001 => Ordering::Equal,
                (_, _) if a < b => Ordering::Less,
                _ => Ordering::Greater,
            });
            let median = if v.is_empty() {
                0.0
            } else if v.len() % 2 == 0 {
                let m = v.len() / 2;
                0.5 * (v[m - 1] + v[m])
            } else {
                v[v.len() / 2]
            };
            let to = t.iter().filter(|v| v.is_nan()).count();
            v = v
                .iter()
                .filter(|x| !x.is_nan())
                .copied()
                .collect::<Vec<_>>();
            println!(
                "{}med:{:>10.3}, max:{:>10.3},{:>16}:{:>9.3}",
                CLEAR,
                median,
                v.last().map_or(0.0, |r| *r),
                {
                    if 0 < to {
                        format!("total except {} timeouts", to)
                    } else {
                        "total".to_string()
                    }
                },
                v.iter().sum::<f64>(),
            );
        }
    }
    if !config.hook.is_empty() {
        let _ = Command::new(config.hook).output();
    }
}

fn threaded_execute(
    config: &Config,
    solver: &str,
    ps: &[(&str, &str)],
    num: &mut usize,
    dir: &str,
) {
    if let Ok(mut q) = PQUEUE.get().unwrap().write() {
        *q = VecDeque::new();
    }
    if let Ok(mut v) = RESVEC.get().unwrap().write() {
        *v = Vec::new()
    }
    if let Ok(mut r) = NREPORT.get().unwrap().write() {
        *r = 0;
    }
    let offset = *num;
    if let Ok(mut q) = PQUEUE.get().unwrap().write() {
        if let Ok(mut v) = RESVEC.get().unwrap().write() {
            for (i, desc) in ps.iter().enumerate() {
                q.push_back((i, desc.0.to_string(), format!("{}/{}", dir, desc.1)));
                *num += 1;
                v.push(None);
            }
        }
    }
    let mut hs = Vec::new();
    let solver_name = format!("{}{}", solver, config.aux_key);
    for _ in 0..config.num_jobs {
        let cfg = config.clone();
        let slv = solver.to_string();
        let sln = solver_name.to_string();
        hs.push(thread::spawn(move || worker(cfg, slv, sln, offset)));
    }
    for h in hs {
        let _ = h.join();
    }
}

fn worker(config: Config, solver: String, solver_name: String, offset: usize) {
    let mut i = 0;
    let mut n = String::new();
    let mut p = String::new();
    loop {
        if let Ok(mut q) = PQUEUE.get().unwrap().write() {
            if q.is_empty() {
                return;
            } else if let Some(desc) = q.pop_front() {
                i = desc.0;
                n = desc.1;
                p = desc.2;
            }
        }
        let res = worker_execute(&config, &solver, &n, &p);
        if let Ok(mut v) = RESVEC.get().unwrap().write() {
            v[i] = res;
            if let Ok(mut r) = NREPORT.get().unwrap().write() {
                for j in *r..v.len() {
                    if let Some(r) = &v[j] {
                        worker_report(&solver_name, j + offset, &r.0, &r.1);
                    } else {
                        *r = j;
                        if !config.no_report {
                            print!(
                                "{}\x1B[032mRunning on {}th problem...\x1B[000m",
                                CLEAR,
                                j + offset
                            );
                            stdout().flush().unwrap();
                        }
                        break;
                    }
                }
            }
        }
    }
}

fn worker_execute(config: &Config, solver: &str, name: &str, path: &str) -> SolveResultPromise {
    let f = PathBuf::from(path);
    if !f.is_file() {
        return None;
    }
    if !config.no_report {
        print!(
            "{}\x1B[032mRunning on {}...\x1B[000m",
            CLEAR,
            f.file_name().unwrap().to_str().unwrap()
        );
        stdout().flush().unwrap();
    }
    let start = SystemTime::now();
    let mut run = Command::new("timeout");
    let mut command = run.arg(format!("{}", config.timeout)).set_solver(solver);
    for opt in config.solver_opts.split_whitespace() {
        command = command.arg(&opt[opt.starts_with('\\') as usize..]);
    }
    Some((
        name.to_string(),
        command
            .arg(f.as_os_str())
            .check_result(solver, &start, config.timeout as f64),
    ))
}

fn worker_report(solver: &str, num: usize, name: &str, res: &Result<f64, SolverException>) {
    match res {
        Ok(end) => {
            println!(
                "{}{:<14}{:>3},{:>30}{:>9.3}",
                CLEAR,
                &format!("\"{}\",", solver),
                num,
                &format!("\"{}\",", name),
                end,
            );
            if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
                t.push(*end);
            }
        }
        Err(SolverException::TimeOut) => {
            println!(
                "{}{:<14}{:>3},{:>30}{}{:>9}{}",
                CLEAR,
                &format!("\"{}\",", solver),
                num,
                &format!("\"{}\",", name),
                MAGENTA,
                "TIMEOUT",
                RESET,
            );
            if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
                t.push(f64::NAN);
            }
        }
        Err(SolverException::Abort) => {
            println!(
                "{}{:<14}{:>3},{:>30}{}{:>9}{}",
                CLEAR,
                &format!("\"{}\",", solver),
                num,
                &format!("\"{}\",", name),
                RED,
                "ABORT",
                RESET,
            );
            if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
                t.push(f64::NAN);
            }
        }
    };
}

/// show the average or total result of SAT problems
#[allow(unused_variables)]
fn execute_3sats(config: &Config, solver: &str, name: &str, num: usize, n: usize, dir: &str) {
    let solver_name = format!("{}{}", solver, config.aux_key);
    // let spinner = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏'];
    let mut count: usize = 0;
    let start = SystemTime::now();
    let _tag = PathBuf::from(dir).file_name().unwrap().to_string_lossy();
    for f in fs::read_dir(dir).unwrap().flatten() {
        if !config.no_report {
            print!(
                "{}\x1B[032mRunning on {:>2}th problem: {}...\x1B[000m",
                CLEAR,
                // &spinner[count % spinner.len()],
                count,
                f.path().file_name().unwrap().to_str().unwrap(),
            );
            stdout().flush().unwrap();
        }
        let mut run = Command::new("timeout");
        let mut command = run.arg(format!("{}", config.timeout)).set_solver(solver);
        for opt in config.solver_opts.split_whitespace() {
            command = command.arg(&opt[opt.starts_with('\\') as usize..]);
        }
        match command
            .arg(f.path())
            .check_result(solver, &start, config.timeout as f64)
        {
            Ok(_) => {
                count += 1;
            }
            Err(SolverException::TimeOut) => {
                println!(
                    "{}{:<14}{:>3},{:>30} {}TIMEOUT{} at {}",
                    CLEAR,
                    &format!("\"{}\",", solver_name),
                    num,
                    &format!("\"{}{}({})\",", name, n, count),
                    // &format!("\"{}({})\",", tag, count),
                    RED,
                    RESET,
                    f.file_name().to_str().unwrap(),
                );
                return;
            }
            Err(SolverException::Abort) => {
                println!(
                    "{}{:<14}{:>3},{:>30} {}ABORT{} at {}",
                    CLEAR,
                    &format!("\"{}\",", solver_name),
                    num,
                    &format!("\"{}{}({})\",", name, n, count),
                    RED,
                    RESET,
                    f.file_name().to_str().unwrap(),
                );
                return;
            }
        }
    }
    let end: f64 = match start.elapsed() {
        Ok(e) => e.as_secs() as f64 + f64::from(e.subsec_millis()) / 1000.0f64,
        Err(_) => 0.0f64,
    };
    println!(
        "{}{:<14}{:>3},{:>30}{:>9.3}",
        CLEAR,
        &format!("\"{}\",", solver_name),
        num,
        &format!("\"{}{}({})\",", name, n, count),
        // &format!("\"{}({})\",", tag, count),
        end,
    );
    if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
        t.push(end);
    }
}

trait SolverHandling {
    fn set_solver(&mut self, solver: &str) -> &mut Self;
    fn check_result(
        &mut self,
        solver: &str,
        start: &SystemTime,
        timeout: f64,
    ) -> Result<f64, SolverException>;
}

impl SolverHandling for Command {
    fn set_solver(&mut self, solver: &str) -> &mut Command {
        let glucose = regex!(r"\bglucose");
        // let minisat_like = regex!(r"\b(cadical|glucose|minisat|splr)");
        // let lingeling = regex!(r"\blingeling");
        // let minisat = regex!(r"\bminisat");
        // let mios = regex!(r"\bmios");
        let splr = regex!(r"\bsplr");
        let cadical = regex!(r"\bcadical");
        if splr.is_match(solver) {
            self.args(&[solver, "-r", "-", "-q"])
        } else if cadical.is_match(solver) {
            self.args(&[solver, "-f"])
        } else if glucose.is_match(solver) {
            self.args(&[solver, "-verb=0"])
        } else {
            self.arg(solver)
        }
    }
    fn check_result(
        &mut self,
        solver: &str,
        start: &SystemTime,
        timeout: f64,
    ) -> Result<f64, SolverException> {
        let minisat_like = regex!(r"\b(cadical|glucose|minisat|splr)");
        let result = self.output();
        match &result {
            Ok(r)
                if String::from_utf8(r.stderr.clone())
                    .unwrap()
                    .contains("thread 'main' panicked") =>
            {
                Err(SolverException::Abort)
            }
            Ok(ref done) => {
                match done.status.code() {
                    Some(124) => return Err(SolverException::TimeOut),
                    Some(10) | Some(20) if minisat_like.is_match(solver) => (),
                    Some(0) => (),
                    e => {
                        println!("unknown exit code {:?}", e);
                        println!("Abort stdout => {}", String::from_utf8_lossy(&done.stdout));
                        println!("Abort stderr => {}", String::from_utf8_lossy(&done.stderr));
                        return Err(SolverException::Abort);
                    }
                }
                match start.elapsed() {
                    Ok(e) => {
                        let end = e.as_secs() as f64 + f64::from(e.subsec_millis()) / 1000.0f64;
                        if end < timeout {
                            Ok(end)
                        } else {
                            Err(SolverException::TimeOut)
                        }
                    }
                    Err(_) => Err(SolverException::Abort),
                }
            }
            Err(e) => {
                println!("Abort by {}", e);
                Err(SolverException::Abort)
            }
        }
    }
}

fn print_solver(solver: &str) -> Option<String> {
    let mut which = match Command::new("which").arg(&solver).output() {
        Ok(o) => String::from_utf8_lossy(&o.stdout).to_string(),
        _ if PathBuf::from(solver).exists() => PathBuf::from(solver)
            .canonicalize()
            .unwrap()
            .to_string_lossy()
            .into_owned(),
        _ => return None,
    };
    which = which.trim_end_matches('\n').to_string();
    let at = fs::metadata(&which);
    if let Ok(home) = env::var("HOME") {
        let _ = home.trim_end_matches('/');
        let hr = Regex::new(&home).expect("wrong regex");
        which = hr.replace(&which, "~").to_string();
    }
    // printf 更新時刻とフルパス、バージョンのみ表示
    let version = match Command::new(solver).arg("--version").output() {
        Ok(o) => String::from_utf8_lossy(&o.stdout[..o.stdout.len() - 1]).to_string(),
        _ => String::from("???"),
    };
    print!("# {} ({})", which, version);
    if let Ok(meta) = at {
        if let Ok(time) = meta.modified() {
            println!(
                " @ {}",
                system_time_to_date_time(time).format("%FT%H:%M:%S")
            );
        }
    } else {
        println!();
    }
    Some(which)
}

#[allow(unused_variables)]
fn execute(config: &Config, solver: &str, num: usize, name: &str, target: &str) {
    let solver_name = format!("{}{}", solver, config.aux_key);
    for e in target.split_whitespace() {
        let f = PathBuf::from(e);
        if f.is_file() {
            if !config.no_report {
                print!(
                    "{}\x1B[032mRunning on {}...\x1B[000m",
                    CLEAR,
                    f.file_name().unwrap().to_str().unwrap()
                );
                stdout().flush().unwrap();
            }
            let start = SystemTime::now();
            let mut run = Command::new("timeout");
            let mut command = run.arg(format!("{}", config.timeout)).set_solver(solver);
            for opt in config.solver_opts.split_whitespace() {
                command = command.arg(&opt[opt.starts_with('\\') as usize..]);
            }
            match command
                .arg(f.as_os_str())
                .check_result(solver, &start, config.timeout as f64)
            {
                Ok(end) => {
                    println!(
                        "{}{:<14}{:>3},{:>30}{:>9.3}",
                        CLEAR,
                        &format!("\"{}\",", solver_name),
                        num,
                        &format!("\"{}\",", name),
                        end,
                    );
                    if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
                        t.push(end);
                    }
                }
                Err(SolverException::TimeOut) => {
                    println!(
                        "{}{:<14}{:>3},{:>30}{}{:>9}{}",
                        CLEAR,
                        &format!("\"{}\",", solver_name),
                        num,
                        &format!("\"{}\",", name),
                        MAGENTA,
                        "TIMEOUT",
                        RESET
                    );
                    if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
                        t.push(f64::NAN);
                    }
                }
                Err(SolverException::Abort) => {
                    println!(
                        "{}{:<14}{:>3},{:>30}{}{:>9}{}",
                        CLEAR,
                        &format!("\"{}\",", solver_name),
                        num,
                        &format!("\"{}\",", name),
                        RED,
                        "ABORT",
                        RESET,
                    );
                    if let Ok(mut t) = TOTALTIME.get().unwrap().write() {
                        t.push(f64::NAN);
                    }
                }
            };
        }
    }
}
