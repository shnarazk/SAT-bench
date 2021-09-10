/// A simple SAT benchmarker with Matrix monitor
use {
    lazy_static::lazy_static,
    regex::Regex,
    sat_bench::{
        matrix,
        utils::{current_date_time, make_verifier, parse_result},
        ANS_PREFIX,
    },
    std::{
        collections::HashMap,
        env, fs,
        io::{stdout, BufWriter, Write},
        path::{Path, PathBuf},
        process::Command,
        str,
        sync::RwLock,
        time,
    },
    structopt::StructOpt,
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const CLEAR: &str = "\x1B[1G\x1B[0K";
lazy_static! {
    static ref MINISAT_LIKE: Regex = Regex::new(r"\b(glucose|minisat|cadical|splr)").expect("wrong regex");
    static ref CADICAL: Regex = Regex::new(r"\bcadical").expect("wrong regex");
    static ref GLUCOSE: Regex = Regex::new(r"\bglucose").expect("wrong regex");
    static ref MINISAT: Regex = Regex::new(r"\bminisat").expect("wrong regex");
    static ref MIOS: Regex = Regex::new(r"\bmios").expect("wrong regex");
    static ref SPLR: Regex = Regex::new(r"\bsplr").expect("wrong regex");
    pub static ref DIFF: RwLock<String> = RwLock::new(String::new());
    /// problem queue
    pub static ref PQ: RwLock<Vec<(usize, String)>> = RwLock::new(Vec::new());
    /// - the number of tried: usize
    /// - the number of reported: usize
    /// - the number of solved: usize
    pub static ref PROCESSED: RwLock<(usize, usize, usize)> = RwLock::new((0, 0, 0));
    pub static ref RESULT: RwLock<Vec<SolveResultPromise>> = RwLock::new(Vec::new());
}

/// Abnormal termination flags.
#[derive(Debug)]
pub enum SolverException {
    TimeOut,
    Abort,
}

type SolveResultPromise = Option<(String, Result<f64, SolverException>)>;

#[derive(Clone, Debug, StructOpt)]
#[structopt(name = "sat-bench", about = "A SAT Competition benchmark runner")]
pub struct Config {
    /// the problem
    #[structopt(long = "benchmark", short = "B", default_value = "SC21")]
    pub benchmark_name: String,
    /// a branch number
    #[structopt(long = "number", short = "N", default_value = "0")]
    pub seq_num: usize,
    /// the problem
    #[structopt(skip)]
    pub benchmark: &'static [(usize, &'static str)],
    /// solver names
    #[structopt(long = "solver", short = "s", default_value = "")]
    pub solver: String,
    /// start of the range of target problems
    #[structopt(long = "from", default_value = "0")]
    pub target_from: usize,
    /// end of the range of target problems
    #[structopt(long = "to", default_value = "400")]
    pub target_to: usize,
    /// step of choosen target problems
    #[structopt(long = "step", default_value = "1")]
    pub target_step: usize,
    /// time out in seconds
    #[structopt(long = "timeout", short = "T", default_value = "300")]
    pub timeout: usize,
    /// number of workers
    #[structopt(long = "jobs", short = "j", default_value = "3")]
    pub num_jobs: usize,
    /// arguments passed to solvers
    #[structopt(long = "options", default_value = "")]
    pub solver_options: String,
    /// data directory
    #[structopt(long = "data", default_value = "~/Library/SAT/SC21")]
    pub data_dir: PathBuf,
    /// solver repository
    #[structopt(long = "repo", default_value = "~/Repositories/splr")]
    pub repo_dir: PathBuf,
    /// cloud sharing directory
    #[structopt(long = "sync", default_value = "~/Desktop/splr-exp")]
    pub sync_dir: PathBuf,
    /// cloud sync command
    #[structopt(long = "sync-cmd", default_value = "")]
    pub sync_cmd: String,
    /// Rebuild report
    #[structopt(long = "rereport", default_value = "")]
    pub rereport: String,
    /// a directory to place the result under `sync_dir`
    #[structopt(skip)]
    pub dump_dir: PathBuf,
    /// an identifier for benchmarking
    #[structopt(skip)]
    pub run_id: String,
    /// host
    #[structopt(skip)]
    pub host: String,
    /// user id to post to Matrix
    #[structopt(long = "mid", default_value = "")]
    pub matrix_id: String,
    /// user password to post to Matrix
    #[structopt(long = "mpasswd", default_value = "")]
    pub matrix_password: String,
    /// The Matrix room
    #[structopt(long = "mroom", default_value = "")]
    pub matrix_room: String,
    #[structopt(skip)]
    pub matrix_token: Option<String>,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            benchmark_name: sat_bench::bench21::SCB.0.to_string(),
            seq_num: 0,
            benchmark: &sat_bench::bench21::SCB.1,
            solver: String::from("splr"),
            target_from: 0,
            target_to: 400,
            target_step: 1,
            timeout: 120,
            num_jobs: 2,
            solver_options: String::new(),
            data_dir: PathBuf::new(),
            repo_dir: PathBuf::new(),
            sync_dir: PathBuf::new(),
            sync_cmd: String::new(),
            rereport: "".to_string(),
            dump_dir: PathBuf::new(),
            run_id: String::new(),
            host: String::new(),
            matrix_id: String::new(),
            matrix_password: String::new(),
            matrix_room: String::new(),
            matrix_token: None,
        }
    }
}

impl Config {
    fn post<S: AsRef<str>>(&self, msg: S) {
        matrix::post(
            &self.matrix_room,
            &self.matrix_token,
            &format!("{}: {}", self.run_id, msg.as_ref()),
        );
    }
    fn dump_stream(&self, cnf: &Path, stream: &str) -> std::io::Result<()> {
        let outname = self.dump_dir.join(format!(
            "{}_{}",
            ANS_PREFIX,
            cnf.file_name().unwrap().to_string_lossy()
        ));
        let outfile = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(&outname)?;
        let mut outbuf = BufWriter::new(outfile);
        write!(outbuf, "{}", stream)?;
        Ok(())
    }
    fn is_new_record(&self, r: &(usize, usize, usize)) -> bool {
        (match (self.benchmark_name.as_str(), self.timeout) {
            ("SR19Core", 100) => [5, 12, 17, 24, 28, 28, 29, 31, 37, 37][(r.1 - 1) / 10],
            ("SR19Core", 300) => [5, 14, 21, 29, 36, 38, 40, 45, 51, 59][(r.1 - 1) / 10],
            ("SR19", 100) => [6, 8, 11, 12, 12, 16, 17, 18, 22, 29][(r.1 - 1) / 40],
            ("SR19", 200) => [6, 8, 12, 14, 14, 18, 21, 23, 35, 36][(r.1 - 1) / 40],
            ("SR19", 400) => [10, 15, 17, 19, 20, 25, 28, 32, 37, 37][(r.1 - 1) / 40],
            _ => r.2,
        }) < r.2
    }
    fn next_task(&self) -> Option<(usize, PathBuf)> {
        if let Ok(mut processed) = PROCESSED.write() {
            // - processed.0 -- the last queued task id.
            // - processed.1 -- the index to check from.
            // - processed.2 -- the number of solved (process teminated normally).
            if let Ok(mut q) = PQ.write() {
                if let Some((index, top)) = q.pop() {
                    processed.0 = index;
                    return Some((index, self.data_dir.join(top)));
                }
            }
        }
        None
    }
    fn solver_command(&self) -> Command {
        if SPLR.is_match(&self.solver) {
            let mut command = Command::new(&self.solver);
            command.args(&[
                "--timeout",
                &format!("{}", self.timeout),
                "-o",
                &self.dump_dir.to_string_lossy(),
                "-q",
            ]);
            command
        } else if CADICAL.is_match(&self.solver) {
            let mut command = Command::new(&self.solver);
            command.args(&["-t", &format!("{}", self.timeout), "--report=false"]);
            command
        } else if GLUCOSE.is_match(&self.solver) {
            let mut command = Command::new(&self.solver);
            command.args(&[&format!("-cpu-lim={}", self.timeout)]);
            command
        } else {
            Command::new(&self.solver)
        }
    }
    fn worker(self) {
        while let Some((i, p)) = self.next_task() {
            check_result(&self);
            let res: SolveResultPromise = self.execute(p);
            if let Ok(mut v) = RESULT.write() {
                v[i - 1] = res; // RESULT starts from 0, while tasks start from 1.
            }
        }
    }
    fn execute(&self, cnf: PathBuf) -> SolveResultPromise {
        assert!(cnf.is_file(), "{} does not exist.", cnf.to_string_lossy());
        let target: String = cnf.file_name().unwrap().to_string_lossy().to_string();
        // println!("\x1B[032m{}\x1B[000m", target);
        let mut command: Command = self.solver_command();
        for opt in self.solver_options.split_whitespace() {
            command.arg(&opt[opt.starts_with('\\') as usize..]);
        }
        Some((target, command.arg(cnf.as_os_str()).map_to_result(self, &cnf)))
    }
}

#[allow(clippy::trivial_regex)]
fn main() {
    let mut config = Config::from_args();
    let tilde = Regex::new("~").expect("wrong regex");
    let home = env::var("HOME").expect("No home");
    let compiled_solver = config.solver.starts_with('/');
    config.benchmark_name = match config.benchmark_name.as_str() {
        "SC21" => config.benchmark_name,
        "SC20" => config.benchmark_name,
        "SR19" => config.benchmark_name,
        _ => "SC21".to_string(),
    };
    config.benchmark = match config.benchmark_name.as_str() {
        "SC21" => &sat_bench::bench21::SCB.1,
        "SC20" => &sat_bench::bench20::SCB.1,
        "SR19" => &sat_bench::bench19::SCB.1,
        _ => &sat_bench::bench21::SCB.1,
    };
    config.data_dir = PathBuf::from(
        tilde
            .replace(&config.data_dir.to_string_lossy(), home.as_str())
            .to_string(),
    );
    config.sync_dir = PathBuf::from(
        tilde
            .replace(&config.sync_dir.to_string_lossy(), home.as_str())
            .to_string(),
    );
    config.repo_dir = PathBuf::from(
        tilde
            .replace(&config.repo_dir.to_string_lossy(), home.as_str())
            .to_string(),
    );
    config.target_to = config.target_to.min(config.benchmark.len());
    if !config.matrix_id.is_empty() {
        let mut map: HashMap<&str, &str> = HashMap::new();
        map.insert("user", &config.matrix_id);
        map.insert("password", &config.matrix_password);
        config.matrix_token = matrix::get_token(&mut map);
        println!("ready to post to matrix; user: {}", config.matrix_id);
    }
    if !compiled_solver && config.solver.is_empty() && config.rereport.is_empty() {
        config.solver = "splr".to_string();
        for f in config
            .repo_dir
            .join("src/bin")
            .read_dir()
            .expect("no repo")
            .flatten()
        {
            let splr = f.path().file_stem().unwrap().to_string_lossy().to_string();
            if splr.contains("splr") {
                print!("\x1B[032mCompiling {}...\x1B[000m", config.solver);
                stdout().flush().unwrap();
                Command::new("cargo")
                    .current_dir(&config.repo_dir)
                    .args(&["install", "--path", ".", "--force", "--features", "cli"])
                    .output()
                    .expect("fail to compile");
                config.solver = splr;
                println!("\x1B[032mdone.\x1B[000m");
                break;
            }
        }
    }
    config.host = {
        let h = Command::new("hostname")
            .arg("-s")
            .output()
            .expect("failed to execute process")
            .stdout;
        String::from_utf8_lossy(&h[..h.len() - 1]).to_string()
    };
    if config.rereport.is_empty() {
        let timestamp = current_date_time().format("%Y%m%d").to_string();
        let solver_id = if compiled_solver {
            format!(
                "{}-{}",
                PathBuf::from(&config.solver)
                    .file_name()
                    .unwrap()
                    .to_string_lossy(),
                timestamp
            )
        } else {
            let commit_id_u8 = Command::new("git")
                .current_dir(&config.repo_dir)
                .args(&["log", "-1", "--format=format:%h"])
                .output()
                .expect("fail to git")
                .stdout;
            let commit_id = String::from_utf8(commit_id_u8).expect("strange commit id");
            format!("{}-{}-{}", config.solver, timestamp, commit_id)
        };
        config.run_id = format!(
            "{}{}-{}",
            solver_id,
            if config.seq_num == 0 {
                "".to_string()
            } else {
                format!("-{}", config.seq_num)
            },
            &config.benchmark_name,
        );
        config.dump_dir = PathBuf::from(&config.sync_dir).join(&config.run_id);
        start_benchmark(config);
    } else {
        config.run_id = format!(
            "{}{}-{}",
            &config.rereport,
            if config.seq_num == 0 {
                "".to_string()
            } else {
                format!("-{}", config.seq_num)
            },
            &config.benchmark_name,
        );
        config.dump_dir = PathBuf::from(&config.sync_dir).join(&config.run_id);
        report(&config, config.target_to).expect("fail to execute");
    }
}

fn start_benchmark(config: Config) {
    let diff = {
        let diff8 = Command::new("git")
            .current_dir(&config.repo_dir)
            .args(&["diff"])
            .output()
            .expect("fail to git diff")
            .stdout;
        String::from_utf8(diff8).expect("strange diff")
    };
    if !config.solver.starts_with('/') {
        if let Ok(mut d) = DIFF.write() {
            *d = diff.clone();
        }
    }
    if config.dump_dir.exists() {
        println!("WARNING: {} exists.", config.dump_dir.to_string_lossy());
    } else {
        fs::create_dir(&config.dump_dir).expect("fail to mkdir");
    }
    if let Ok(mut queue) = PQ.write() {
        if let Ok(mut v) = RESULT.write() {
            for s in config
                .benchmark
                .iter()
                .take(config.target_to)
                .skip(config.target_from)
            {
                if s.0 % config.target_step == 0 {
                    queue.push((s.0 - config.target_from, s.1.to_string()));
                }
                v.push(None);
            }
        }
        queue.reverse();
    }
    report(&config, 0).unwrap();
    config.post(format!(
        "A new {} parallel, {} timeout benchmark starts.",
        config.num_jobs, config.timeout
    ));
    if !diff.is_empty() {
        config.post("WARNING: There're unregistered modifications!");
    }
    println!("Benchmark: {}", &config.run_id);
    crossbeam::scope(|s| {
        for i in 0..config.num_jobs {
            let c = config.clone();
            s.spawn(move |_| {
                std::thread::sleep(time::Duration::from_millis((2 + 2 * i as u64) * 1000));
                c.worker();
            });
        }
    })
    .expect("fail to exit crossbeam::scope");
    check_result(&config);
    let mut np = PROCESSED.read().map(|v| *v).unwrap_or((0, 0, 0));
    let (s, u) = report(&config, np.1).unwrap_or((0, 0));
    np.2 = s + u;
    println!(
        "Benchmark {} finished, {} problems, {} solutions",
        config.run_id, np.1, np.2
    );
    config.post(format!(
        "A {} timeout benchmark {} ended, {} problems, {} solutions",
        config.timeout, config.run_id, np.1, np.2
    ));
    make_verifier(config.benchmark, &config.dump_dir, &config.data_dir)
        .expect("fail to create verify.sh");
    let tarfile = config.sync_dir.join(&format!("{}.tar.xz", config.run_id));
    Command::new("tar")
        .args(&[
            "cvJf",
            &tarfile.to_string_lossy(),
            &config.dump_dir.to_string_lossy(),
        ])
        .output()
        .expect("fail to tar");
    if !config.sync_cmd.is_empty() {
        Command::new(&config.sync_cmd)
            .output()
            .expect("fail to run sync command");
    }
}

fn check_result(config: &Config) {
    let mut new_solution = false;
    if let Ok(mut processed) = PROCESSED.write() {
        // - processed.0 -- the last queued task id.
        // - processed.1 -- the index to check from.
        // - processed.2 -- the number of solved (process teminated normally).
        if let Ok(v) = RESULT.write() {
            for j in processed.1..v.len() {
                // skip all the processed
                // I have the resposibility to print the (j-1) th task's result.
                let task_id = j + 1;
                if let Some(r) = &v[j] {
                    processed.1 = j + 1;
                    if r.1.is_ok() {
                        processed.2 += 1;
                        new_solution = true;
                    }
                    print!("{}", CLEAR);
                    // Note again: j is an index for RESULT,
                    // and it corresponds to (j + 1) th task.
                    if new_solution {
                        if config.is_new_record(&processed) {
                            config.post(format!("*{:>3},{:>3}", task_id, processed.2));
                            println!("*{:>3},{:>3},{}", task_id, processed.2, &r.0);
                        } else {
                            config.post(format!(" {:>3},{:>3}", task_id, processed.2));
                            println!(" {:>3},{:>3},{}", task_id, processed.2, &r.0);
                        }
                    }
                    if j % config.num_jobs == 0 {
                        // The other processes might dump further results already.
                        // So `report` may return a larger number than `processed.2`.
                        // We should not update.
                        report(config, task_id).unwrap();
                    }
                } else {
                    // re display the current running task id(s)
                    debug_assert!(task_id <= processed.0);
                    if task_id < config.benchmark.len() {
                        let mut fname = config.benchmark[task_id].1.to_string();
                        fname.truncate(40);
                        if task_id == processed.0 {
                            print!(
                                "{}\x1B[032mRunning on the {}th problem {}...\x1B[000m",
                                CLEAR, task_id, fname
                            );
                        } else {
                            print!(
                                "{}\x1B[032mRunning on the {}-{}th problem {}...\x1B[000m",
                                CLEAR, task_id, processed.0, fname
                            );
                        }
                        stdout().flush().unwrap();
                    }
                    break;
                }
            }
        }
    }
}

fn report(config: &Config, nprocessed: usize) -> std::io::Result<(usize, usize)> {
    let outname = config.sync_dir.join(format!("{}.csv", config.run_id));
    let mut nsat = 0;
    let mut nunsat = 0;
    {
        let outfile = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(&outname)?;
        let mut outbuf = BufWriter::new(outfile);
        // * key: problem name
        // * value:
        //    * elapsed time or None
        //    * used strategy
        //    * satisfiability or None
        let mut problem: HashMap<&str, (Option<f64>, String, Option<bool>)> = HashMap::new();
        let mut strategy: HashMap<String, (usize, usize)> = HashMap::new();
        let mut found = 0;
        let timeout = config.timeout as f64;
        for e in config.dump_dir.read_dir()? {
            let f = e?;
            if !f.file_type()?.is_file() {
                continue;
            }
            let fname = f.file_name().to_string_lossy().to_string();
            if fname.starts_with(ANS_PREFIX) {
                let cnf = fname.strip_prefix(ANS_PREFIX).expect("invalid answer file");
                for (_n, key) in config.benchmark.iter() {
                    if *key == cnf {
                        if None != problem.get(key) {
                            panic!("duplicated {}", cnf);
                        }
                        if let Some((t, s, m)) = parse_result(f.path()) {
                            found += 1;
                            match s {
                                Some(b) => {
                                    if b {
                                        nsat += 1;
                                    } else {
                                        nunsat += 1;
                                    }
                                    if let Some(p) = strategy.get_mut(&m.to_string()) {
                                        p.0 += 1;
                                    } else {
                                        strategy.insert(m.clone(), (1, 0));
                                    }
                                    problem.insert(key, (Some(timeout.min(t)), m, Some(b)));
                                }
                                None => {
                                    if let Some(p) = strategy.get_mut(&m.to_string()) {
                                        p.1 += 1;
                                    } else {
                                        strategy.insert(m.clone(), (0, 1));
                                    }
                                    problem.insert(key, (None, m, None));
                                }
                            }
                        }
                        break;
                    }
                }
            }
        }
        // show header line
        writeln!(
            outbuf,
            "#{} on {} by benchm.rs {}",
            config.run_id, config.host, VERSION,
        )?;
        // show summary of settings
        writeln!(
            outbuf,
            "# benchmark: {} from {} to {}, process: {}, timeout: {}",
            config.benchmark_name,
            config.target_from,
            config.target_to,
            config.num_jobs,
            config.timeout,
        )?;
        // show summary of solutions
        writeln!(
            outbuf,
            "# Procesed: {} (found {}), total answers: {} (SAT: {}, UNSAT: {}) so far",
            nprocessed,
            found,
            nsat + nunsat,
            nsat,
            nunsat,
        )?;
        // show summary of each strategy
        write!(outbuf, "# ")?;
        let mut sv = strategy.iter().collect::<Vec<_>>();
        sv.sort();
        for (s, n) in &sv {
            write!(outbuf, "{}:{:?}, ", *s, *n)?;
        }
        writeln!(outbuf)?;
        // show options
        if !config.solver_options.is_empty() {
            if config.solver_options.starts_with('\\') {
                let str = config.solver_options.chars().skip(1).collect::<String>();
                writeln!(outbuf, "# options: {}", &str)?;
            } else {
                writeln!(outbuf, "# options: {}", &config.solver_options)?;
            }
        }
        // show diff
        if let Ok(diff) = DIFF.write() {
            for l in diff.lines() {
                writeln!(outbuf, "# {}", l)?;
            }
        }
        writeln!(
            outbuf,
            "solver,num,target,nsolved,time,strategy,satisfiability"
        )?;
        let mut nsolved = 0;
        for (i, key) in config.benchmark.iter() {
            if let Some(v) = problem.get(key) {
                match v {
                    (Some(time_), str_, Some(sat_)) => {
                        nsolved += 1;
                        writeln!(
                            outbuf,
                            "\"{}\",{},\"{}/{}\",{},{:.2},{},{}",
                            config.run_id,
                            i,
                            config.benchmark_name,
                            key,
                            nsolved,
                            time_,
                            str_,
                            sat_,
                        )?;
                    }
                    (_, str_, _) => writeln!(
                        outbuf,
                        "\"{}\",{},\"{}/{}\",{},{},{},",
                        config.run_id,
                        i,
                        config.benchmark_name,
                        key,
                        nsolved,
                        config.timeout + 10, // Sometimes a run ends in just the timeout.
                        str_
                    )?,
                }
            } else {
                writeln!(
                    outbuf,
                    "\"{}\",{},\"{}/{}\",{},{},,",
                    config.run_id,
                    i,
                    config.benchmark_name,
                    key,
                    nsolved,
                    config.timeout + 10, // Sometimes a run ends in just the timeout.
                )?;
            }
        }
    }
    Command::new("make")
        .current_dir(&config.sync_dir)
        .output()?;
    if !config.sync_cmd.is_empty() {
        Command::new(&config.sync_cmd).output()?;
    }
    Ok((nsat, nunsat))
}

trait SolverHandling {
    fn map_to_result(&mut self, config: &Config, cnf: &Path) -> Result<f64, SolverException>;
}

impl SolverHandling for Command {
    fn map_to_result(&mut self, config: &Config, cnf: &Path) -> Result<f64, SolverException> {
        match &self.output() {
            Ok(r) => {
                match (
                    r.status.code(),
                    String::from_utf8_lossy(&r.stdout),
                    String::from_utf8_lossy(&r.stderr),
                ) {
                    (Some(0), ref s, _) if s.contains("SATISFIABLE: ") => Ok(0.0),
                    (Some(0), ref s, _) if s.contains("UNSAT: ") => Ok(0.0),
                    (Some(10), ref s, _) if s.contains("s SATISFIABLE") => {
                        if !SPLR.is_match(&config.solver) {
                            config.dump_stream(cnf, s).unwrap();
                        }
                        Ok(0.0)
                    }
                    (Some(20), ref s, _) if s.contains("s UNSATISFIABLE") => {
                        if !SPLR.is_match(&config.solver) {
                            config.dump_stream(cnf, s).unwrap();
                        }
                        Ok(0.0)
                    }
                    (_, ref s, _)
                        if s.contains("s UNKNOWN")
                            || s.contains("TimeOut")
                            || s.contains("s INDETERMINATE") =>
                    {
                        if !SPLR.is_match(&config.solver) {
                            config.dump_stream(cnf, s).unwrap();
                        }
                        Err(SolverException::TimeOut)
                    }
                    (_, ref s, _) if s.contains("thread 'main' panicked") => {
                        println!("{}: {}", cnf.to_string_lossy(), s);
                        // config.dump_stream(cnf, s).unwrap();
                        Err(SolverException::Abort)
                    }
                    (_, _, ref e) if e.contains("thread 'main' panicked") => {
                        println!("{}: {}", cnf.to_string_lossy(), e);
                        // config.dump_stream(cnf, e).unwrap();
                        Err(SolverException::Abort)
                    }
                    (_, s, e) if s == e => {
                        println!("{}: {}", cnf.to_string_lossy(), s);
                        Err(SolverException::Abort)
                    }
                    (_, s, e) => {
                        println!("{}: {}{}", cnf.to_string_lossy(), s, e);
                        Err(SolverException::Abort)
                    }
                }
            }
            Err(r) => {
                println!("{}", r);
                Err(SolverException::Abort)
            }
        }
    }
}
