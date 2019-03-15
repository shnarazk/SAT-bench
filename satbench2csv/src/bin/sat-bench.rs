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
    let home = "$HOME";
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
    if singleSolver {
        if let Some(solver) = &config.solvers.get(0) {
            // echo -n \\# $(ls -g -G --time-style=long-iso `which %s`|sed -e 's/[-rwx]* [1-9] [0-9]* //' -e 's| \\([0-9][0-9]:[0-9][0-9]\\).*|T\\1|') '%s; '; %s --version 2>/dev/null|egrep -v '^.$'|head
            // solver,
            // solver,
            // solver,
        }
    }
    let opts = &config.solverOptions;
    for solver in &config.solvers {

    }
    //forM_ (solvers conf) $ \solver -> do
    //  val <- system $ "which " ++ solver ++ " > /dev/null"
    //  when (val == ExitSuccess) $ do
    //    unless singleSolver $ do
    //      system $
    //          printf "echo -n \\# $(ls -g -G --time-style=long-iso `which %s` | sed -e 's/[-rwx]* [1-9] [0-9]* //' -e 's| \\([0-9][0-9]:[0-9][0-9]\\).*|T\\1|') '%s; '; %s --version 2> /dev/null | egrep -v '^.$' | head"
    //                 solver solver solver
    //      return ()
    //    let
    //      threes = [rangeFrom conf, rangeFrom conf + 25 .. rangeTo conf]
    //      nums :: Int -> [Int]
    //      nums 0 = [ 1 :: Int .. ]
    //      nums 1 = drop 0 $ nums 0
    //      nums 2 = drop (if threeSATSet conf then length threes else 0) $ nums 1
    //      nums _ = nums 0
    //      withNum n l = zip (nums n) l
    //    case targets conf of
    //      Just s -> executeTargets conf solver opts s
    //      Nothing -> do
    //        when (threeSATSet conf) $ mapM_ (execute3SATs conf solver opts base) $ withNum 1 threes
    //        when (structuredSATSet conf) $ mapM_ (execute conf solver opts base) $ withNum 2 structuredProblems
    //    unless (null (terminateHook conf)) $ void (system (terminateHook co
}

// \\nが出てきたら改行文字に置き換え：正規表現がよさそう
fn undecode_newline(from: &str) -> String {
    from.to_string()
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
