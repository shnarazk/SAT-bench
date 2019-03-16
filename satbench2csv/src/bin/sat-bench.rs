// use std::io::{self, Write};
use std::process::Command;
/// A simple benchmarker to dump a result of testrun(s)
/// Requirement: GNU parallel installed in your PATH
/// Usage: sat-benchmark [OPTIONS] [solvers]
/// # Examples:
///   - sat-benchmark -3 250 minisat             # 3-SAT from 150 (default) to 250 vars
///   - sat-benchmark -3 250 -s mios             # 3-SATs and your set of structured problems
///   - sat-benchmark -o "-cla-decay 0.9" -s glucose     # options to solver
///   - sat-benchmark -t ./g2-ACG-15-10p1.cnf -s glucose # -t for a CNF file
///   - sat-benchmark -t '../test / *.cnf' -s glucose      # -t for CNF files
use structopt::StructOpt;

const VERSION: &'static str = "sat-benchmark 0.15.0";
const SET_ENV: &'static str = "export LC_ALL=C; export TIMEFORMAT=\" %2U\"";
const BASE_DIR: &'static str = "/Repositories/SATbench";
const STRUCTURED_PROBLEMS: [(&'static str, &'static str); 4] = [
    ("itox", "SR2015/itox_vc1130.cnf"),
    ("m283", "SR2015/manthey_DimacsSorter_28_3.cnf"),
    ("38b", "SR2015/38bits_10.dimacs.cnf"),
    ("44b", "SR2015/44bits_11.dimacs.cnf"),
];

#[derive(Debug, StructOpt)]
#[structopt(name = "sat-bench", about = "Run simple benchmark")]
struct Config {
    solvers: Vec<String>,
    #[structopt(long = "targets", short = "t", default_value = "")]
    targets: String,
    #[structopt(long = "from", short = "L", default_value = "250")]
    rangeFrom: usize,
    #[structopt(long = "upto", short = "U", default_value = "250")]
    rangeTo: usize,
    #[structopt(long = "3SAT", short = "3")]
    threeSATSet: bool,
    #[structopt(long = "structured", short = "s")]
    structuredSATSet: bool,
    #[structopt(long = "timeout", short = "T", default_value = "510")]
    timeout: usize,
    #[structopt(long = "parallel", short = "j", default_value = "1")]
    inParallel: usize,
    #[structopt(long = "dump-all", short = "A")]
    dumpAll: bool,
    #[structopt(long = "terminate-hook", default_value = "finished")]
    terminateHook: String,
    #[structopt(long = "options", default_value = "")]
    solverOptions: String,
    #[structopt(long = "show-output", short = "S")]
    devNull: bool,
    #[structopt(long = "header", short = "H", default_value = "")]
    header: String,
    #[structopt(long = "message", short = "M", default_value = "")]
    message: String,
    #[structopt(long = "aux-key", short = "K", default_value = "")]
    auxKey: String,
    #[structopt(long = "output-suffix", short = "O", default_value = "")]
    outputSuffix: String,
}

fn main() {
    println!("{}", VERSION);
    let config = Config::from_args();
    let _home = "$HOME";
    let base = "$HOME".to_owned() + BASE_DIR;
    //   let base = baseDir home
    let singleSolver = match config.solvers.len() {
        0 => panic!("no solver"),
        1 => true,
        _ => false,
    };
    let extraMessage = if config.message == "" {
        "".to_string()
    } else {
        format!(", {}", config.message)
    };
    let date = Command::new("date")
        .arg("-Iseconds")
        .output()
        .expect("failed to execute process")
        .stdout;
    let d = String::from_utf8_lossy(&date[..date.len() - 1]);
    let host = Command::new("hostname")
        .arg("-s")
        .output()
        .expect("failed to execute process")
        .stdout;
    let h = String::from_utf8_lossy(&host[..host.len() - 1]);
    // io::stdout().write_all(&output.stdout).unwrap();
    println!(
        "# {}, j={}, t={}, p='{}' on {} @ {}{}",
        VERSION, config.inParallel, config.timeout, config.solverOptions, h, d, extraMessage
    );
    match config.header.as_ref() {
        "" if config.dumpAll => println!("solver, num, seq, target, time"),
        "" => println!("solver, num, target, time"),
        _ => println!("{}", config.header),
    }
    let opts = &config.solverOptions;
    if singleSolver {
        print_solver(&config.solvers.get(0).unwrap());
    }
    //forM_ (solvers conf) $ \solver -> do
    for solver in &config.solvers {
        if !singleSolver {
            print_solver(solver);
        }
        let threes: Vec<usize> = vec![25, 50, 75, 100, 125, 150, 175, 200, 225, 250];
        let mut num: usize = 1;
        if config.targets.is_empty() {
            if config.threeSATSet {
                for n in &threes {
                    if config.rangeFrom <= *n && *n <= config.rangeTo {
                        execute3SATs(&config, solver, opts, &base, num, *n);
                        num += 1;
                    }
                }
            }
            if config.structuredSATSet {
                for (k, s) in &STRUCTURED_PROBLEMS {
                    execute(&config, solver, opts, &base, num, k, s);
                    num += 1;
                }
            }
        } else {
            for t in config.targets.split_whitespace() {
                execute(&config, solver, opts, &base, num, t, t);
                num += 1;
            }
        }
    }
    if !config.terminateHook.is_empty() {
        println!("terminate hook");
    }
}

fn print_solver(solver: &str) -> Option<String> {
    let mut which = match Command::new("which").arg(&solver).output() {
        Ok(o) => String::from_utf8_lossy(&o.stdout).to_string(),
        _ => return None,
    };
    which.pop();
    //      system $
    // printf 更新時刻とフルパス、バージョンのみ表示
    let version = match Command::new(solver).arg("--version").output() {
        Ok(o) => String::from_utf8_lossy(&o.stdout[..o.stdout.len() - 1]).to_string(),
        _ => String::from("???"),
    };
    let at = match Command::new("date").arg("--iso-8601=seconds").output() {
        Ok(o) => String::from_utf8_lossy(&o.stdout).to_string(),
        _ => String::from("???"),
    };
    println!("# {} ({}) @ {}", which, version, at);
    Some(which.to_string())
}

// \\nが出てきたら改行文字に置き換え：正規表現がよさそう
fn undecode_newline(from: &str) -> String {
    from.to_string()
}

#[allow(unused_variables)]
fn execute3SATs(config: &Config, solver: &str, opts: &str, base: &str, num: usize, n: usize) {}

#[allow(unused_variables)]
fn execute(
    config: &Config,
    solver: &str,
    opts: &str,
    base: &str,
    num: usize,
    name: &str,
    target: &str,
) {
}

// -- | target is a list of files (for SAT-RACE benchmark)
// executeTargets conf solver options files = do
//   hFlush stdout
//   let flagJ = "-j " ++ show (inParallel conf)
//   let solverName = solver ++ auxKey conf
//   let outputPattern = outputSuffix conf
//   if devNull conf
//     then system $ printf "%s; (parallel -k --joblog satbench.log %s \"echo -n '\\\"%s\\\", {#}, \\\"{}\\\", '; time timeout %d %s %s {} %s > /dev/null \" ::: %s ;) 2>&1"
//                          setEnv flagJ solverName (timeout conf) solver options outputPattern files
//     else system $ printf "%s; (parallel -k --joblog satbench.log %s \"echo -n '\\\"%s\\\", {#}, \\\"{}\\\", '; time timeout %d %s %s {} %s \" ::: %s ;) 2>&1"
//                          setEnv flagJ solverName (timeout conf) solver options outputPattern files
//   hFlush stdout

// -- | [dump for DEBUG] target is a list of files (for SAT-RACE benchmark)
// execute3SATs conf@(dumpAll -> True) solver options dir (num, targets) = do
//   hFlush stdout
//   let flagJ = "-j " ++ show (inParallel conf)
//   let solverName = solver ++ auxKey conf
//   if devNull conf
//      then system $ printf "%s; (parallel -k %s \"echo -n '\\\"%s\\\", %d, {#}, \\\"{}\\\", '; time timeout %d %s %s {} > /dev/null\" ::: %s/3-SAT/UF%s/uf*.cnf;) 2>&1"
//                           setEnv flagJ solverName num (timeout conf) solver options dir (show targets)
//      else system $ printf "%s; (parallel -k %s \"echo -n '\\\"%s\\\", %d {#}, \\\"{}\\\", '; time timeout %d %s %s {} \" ::: %s/3-SAT/UF%s/uf*.cnf;) 2>&1"
//                           setEnv flagJ solverName num (timeout conf) solver options dir (show targets)

// -- | only show the average or total result of targets (for 3-SAT problems)
// execute3SATs conf solver options dir (num, targets) = do
//   let q s = "\"" ++ s ++ "\""
//   let solverName = solver ++ auxKey conf
//   let flagJ = "-j " ++ show (inParallel conf)
//   putStr $ q solverName ++ ", " ++ show num ++ ", " ++ show targets ++ ",\t"
//   hFlush stdout
//   if devNull conf
//      then system $ printf "%s; (time timeout %d parallel -k %s \"%s %s {} > /dev/null\" ::: %s/3-SAT/UF%s/uf*.cnf;) 2>&1"
//                           setEnv (timeout conf) flagJ solver options dir (show targets)
//      else system $ printf "%s; (time timeout %d parallel -k %s \"%s %s {} \" ::: %s/3-SAT/UF%s/uf*.cnf;) 2>&1"
//                           setEnv (timeout conf) flagJ solver options dir (show targets)

// -- | [dump for DEBUG] only show the average or total result of targets (for 3-SAT problems)
// execute conf@(dumpAll -> True) solver options dir (num, (key, target)) = do
//   hFlush stdout
//   let flagJ = "-j " ++ show (inParallel conf)
//   let solverName = solver ++ auxKey conf
//   if devNull conf
//     then system $ printf "%s; (parallel -k %s \"echo -n '\\\"%s\\\", %d, {#}, \\\"{}\\\", '; time timeout %d %s %s {} > /dev/null \" ::: %s/%s ; ) 2>&1"
//                          setEnv flagJ solverName num (timeout conf) solver options dir target
//     else system $ printf "%s; (parallel -k %s \"echo -n '\\\"%s\\\", %d, {#}, \\\"{}\\\", '; time timeout %d %s %s {} \" ::: %s/%s ; ) 2>&1"
//                          setEnv flagJ solverName num (timeout conf) solver options dir target
//   hFlush stdout

// -- | target is one of defined problem sets: fundamentalProblems, structuredProblems
// execute conf solver options dir (num, (key, target)) = do
//   let q s = "\"" ++ s ++ "\""
//   let flagJ = "-j " ++ show (inParallel conf)
//   let solverName = solver ++ auxKey conf
//   putStr $ q solverName ++ ", " ++ show num ++ ", " ++ q key ++ ",\t"
//   hFlush stdout
//   if devNull conf
//     then system $ printf "%s; (parallel -k %s \"time timeout %d %s %s {} > /dev/null \" ::: %s/%s ; ) 2>&1"
//                          setEnv flagJ (timeout conf) solver options dir target
//     else system $ printf "%s; (parallel -k %s \"time timeout %d %s %s {} \" ::: %s/%s ; ) 2>&1"
//                          setEnv flagJ (timeout conf) solver options dir target
//   hFlush stdout
