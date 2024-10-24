/// A simple SAT benchmarker with Matrix monitor
use {
    clap::Parser,
    once_cell::sync::OnceCell,
    regex::Regex,
    sat_bench::{
        matrix::{connect_to_matrix, Matrix},
        utils::{current_date_time, make_verifier, parse_result},
        ANS_PREFIX,
    },
    std::{
        collections::HashMap,
        env, fs,
        io::{stdout, BufWriter, Write},
        path::{Path, PathBuf},
        process::Command,
        str, time,
    },
    tokio::sync::{
        mpsc::{self, Receiver, Sender},
        Mutex,
    },
};

const VERSION: &str = env!("CARGO_PKG_VERSION");
const CLEAR: &str = "\x1B[1G\x1B[0K";
static MINISAT_LIKE: OnceCell<Regex> = OnceCell::new();
static CADICAL: OnceCell<Regex> = OnceCell::new();
static GLUCOSE: OnceCell<Regex> = OnceCell::new();
static MINISAT: OnceCell<Regex> = OnceCell::new();
static MIOS: OnceCell<Regex> = OnceCell::new();
static SPLR: OnceCell<Regex> = OnceCell::new();
static DIFF: OnceCell<String> = OnceCell::new();

/// problem queue
static PQ: OnceCell<Mutex<Vec<(usize, String)>>> = OnceCell::new();

/// Abnormal termination flags.
#[derive(Clone, Debug)]
pub enum SolverException {
    TimeOut,
    Abort,
}

type SolveResult = (usize, String, Result<bool, SolverException>);
type SolveResultPromise = Option<SolveResult>;

#[derive(Clone, Debug)]
enum MsgKind {
    Start(usize),
    Result(SolveResult),
}

#[derive(Clone, Debug, Parser)]
#[clap(name = "sat-bench", about = "A SAT Competition benchmark runner")]
pub struct Config {
    /// the problem
    #[clap(long = "benchmark", short = 'B', default_value = "SC21")]
    pub benchmark_name: String,
    /// a branch number
    #[clap(long = "number", short = 'N', default_value = "0")]
    pub seq_num: usize,
    /// the problem
    #[clap(skip)]
    pub benchmark: &'static [(usize, &'static str)],
    /// solver names
    #[clap(long = "solver", short = 's', default_value = "")]
    pub solver: String,
    /// start of the range of target problems
    #[clap(long = "from", default_value = "0")]
    pub target_from: usize,
    /// end of the range of target problems
    #[clap(long = "to", default_value = "400")]
    pub target_to: usize,
    /// step of choosen target problems
    #[clap(long = "step", default_value = "1")]
    pub target_step: usize,
    /// time out in seconds
    #[clap(long = "timeout", short = 'T', default_value = "300")]
    pub timeout: usize,
    /// number of workers
    #[clap(long = "jobs", short = 'j', default_value = "3")]
    pub num_jobs: usize,
    /// arguments passed to solvers
    #[clap(long = "options", default_value = "")]
    pub solver_options: String,
    /// data directory
    #[clap(long = "data", default_value = "~/Library/SAT/SC21")]
    pub data_dir: PathBuf,
    /// solver repository
    #[clap(long = "repo", default_value = "~/Repositories/splr")]
    pub repo_dir: PathBuf,
    /// cloud sharing directory
    #[clap(long = "sync", default_value = "~/Desktop/splr-exp")]
    pub sync_dir: PathBuf,
    /// cloud sync command
    #[clap(long = "sync-cmd", default_value = "")]
    pub sync_cmd: String,
    /// Rebuild report
    #[clap(long = "rereport", default_value = "")]
    pub rereport: String,
    /// a directory to place the result under `sync_dir`
    #[clap(skip)]
    pub dump_dir: PathBuf,
    /// an identifier for benchmarking
    #[clap(skip)]
    pub run_id: String,
    /// hnum_task: usize
    #[clap(skip)]
    pub host: String,
    /// user id to post to Matrix
    #[clap(long = "mid", default_value = "")]
    pub matrix_id: String,
    /// user password to post to Matrix
    #[clap(long = "mpasswd", default_value = "")]
    pub matrix_password: String,
    /// The Matrix room
    #[clap(long = "mroom", default_value = "")]
    pub matrix_room: String,
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
        }
    }
}

impl Config {
    #[allow(unused_variables)]
    fn dump_stream(&self, cnf: &Path, stream: &str) -> std::io::Result<()> {
        let outname = self.dump_dir.join(format!(
            "{}_{}",
            ANS_PREFIX,
            cnf.file_name().unwrap().to_string_lossy()
        ));
        let outfile = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(outname)?;
        let mut outbuf = BufWriter::new(outfile);
        write!(outbuf, "{stream}")?;
        Ok(())
    }
    fn is_new_record(&self, r: &(usize, usize, usize)) -> bool {
        (match (self.benchmark_name.as_str(), self.timeout) {
            ("SC21", 400) => [8, 22, 32, 41, 44, 53, 60, 66, 72, 76][(r.1 - 1) / 40],
            ("SR19", 100) => [6, 8, 11, 12, 12, 16, 17, 18, 22, 29][(r.1 - 1) / 40],
            ("SR19", 200) => [6, 8, 12, 14, 14, 18, 21, 23, 35, 36][(r.1 - 1) / 40],
            ("SR19", 400) => [10, 15, 17, 19, 20, 25, 28, 32, 37, 37][(r.1 - 1) / 40],
            ("SR19Core", 100) => [5, 12, 17, 24, 28, 28, 29, 31, 37, 37][(r.1 - 1) / 10],
            ("SR19Core", 300) => [5, 14, 21, 29, 36, 38, 40, 45, 51, 59][(r.1 - 1) / 10],
            _ => r.2,
        }) < r.2
    }
    async fn next_task(&self) -> Option<(usize, PathBuf)> {
        let mut q = PQ.get().unwrap().lock().await;
        let (index, top) = q.pop()?;
        Some((index, self.data_dir.join(top)))
    }
    fn solver_command(&self) -> Command {
        if SPLR.get().unwrap().is_match(&self.solver) {
            let mut command = Command::new(&self.solver);
            command.args([
                "--timeout",
                &format!("{}", self.timeout),
                "-o",
                &self.dump_dir.to_string_lossy(),
                "-q",
            ]);
            command
        } else if CADICAL.get().unwrap().is_match(&self.solver) {
            let mut command = Command::new(&self.solver);
            command.args(["-t", &format!("{}", self.timeout), "--report=false"]);
            command
        } else if GLUCOSE.get().unwrap().is_match(&self.solver) {
            let mut command = Command::new(&self.solver);
            command.args([&format!("-cpu-lim={}", self.timeout)]);
            command
        } else {
            Command::new(&self.solver)
        }
    }
    async fn worker(self, matrix: Sender<MsgKind>) -> Result<bool, Box<dyn std::error::Error>> {
        while let Some((i, p)) = self.next_task().await {
            matrix.send(MsgKind::Start(i)).await?;
            let res: SolveResultPromise = self.execute(i, p);
            matrix.send(MsgKind::Result(res.unwrap())).await?;
        }
        Ok(true)
    }
    fn execute(&self, index: usize, cnf: PathBuf) -> SolveResultPromise {
        assert!(cnf.is_file(), "{} does not exist.", cnf.to_string_lossy());
        let target: String = cnf.file_name().unwrap().to_string_lossy().to_string();
        // println!("\x1B[032m{}\x1B[000m", target);
        let mut command: Command = self.solver_command();
        for opt in self.solver_options.split_whitespace() {
            command.arg(&opt[opt.starts_with('\\') as usize..]);
        }
        Some((
            index,
            target,
            command.arg(cnf.as_os_str()).map_to_result(self, &cnf),
        ))
    }
}

#[allow(clippy::trivial_regex)]
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let _ = PQ.set(Mutex::new(Vec::new()));
    let _ = MINISAT_LIKE.set(Regex::new(r"\b(glucose|minisat|cadical|splr)").expect("wrong regex"));
    let _ = CADICAL.set(Regex::new(r"\bcadical").expect("wrong regex"));
    let _ = GLUCOSE.set(Regex::new(r"\bglucose").expect("wrong regex"));
    let _ = MINISAT.set(Regex::new(r"\bminisat").expect("wrong regex"));
    let _ = MIOS.set(Regex::new(r"\bmios").expect("wrong regex"));
    let _ = SPLR.set(Regex::new(r"\bsplr").expect("wrong regex"));
    let mut config = Config::parse();
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
                    .args(["install", "--path", ".", "--force", "--features", "cli"])
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
                .args(["log", "-1", "--format=format:%h"])
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
        let num_task = {
            let mut queue = PQ.get().unwrap().lock().await;
            let mut task_id = 0;
            for s in config
                .benchmark
                .iter()
                .take(config.target_to)
                .skip(config.target_from)
            {
                if s.0 % config.target_step == 0 {
                    queue.push((task_id, s.1.to_string()));
                    task_id += 1;
                    // queue.push((s.0 - config.target_from, s.1.to_string()));
                }
            }
            queue.reverse();
            queue.len()
        };
        let (sx, rx): (Sender<MsgKind>, Receiver<MsgKind>) = mpsc::channel(20);
        async fn reporter(
            cfg: Config,
            mut rx: Receiver<MsgKind>,
            num_task: usize,
        ) -> Result<(), Box<dyn std::error::Error>> {
            // (the number of tried, the number of reported, the number of solved)
            let mut processed: (usize, usize, usize) = (0, 0, 0);
            let mut results: Vec<SolveResultPromise> = vec![None; num_task];
            let mut matrix =
                connect_to_matrix(&cfg.matrix_id, &cfg.matrix_password, &cfg.matrix_room).await;
            start_benchmark(&cfg, &mut matrix).await;
            let mut done = 0;
            report(&cfg, done).await?;
            while let Some(res) = rx.recv().await {
                match res {
                    MsgKind::Start(i) => {
                        processed.0 = i;
                    }
                    MsgKind::Result(r) => {
                        check_result(&cfg, &mut matrix, r, &mut processed, &mut results).await;
                        done += 1;
                        if done == num_task {
                            break;
                        }
                    }
                }
            }
            report(&cfg, done).await?;
            finalize_benchmark(cfg, &mut matrix, &mut processed).await;
            Ok(())
        }
        for i in 0..config.num_jobs {
            let c = config.clone();
            let sx = sx.clone();
            tokio::spawn(async move {
                std::thread::sleep(time::Duration::from_millis((2 + 2 * i as u64) * 1000));
                c.worker(sx).await.unwrap();
            });
        }
        reporter(config, rx, num_task).await
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
        report(&config, config.target_to).await?;
        Ok(())
    }
}

async fn start_benchmark(config: &Config, matrix: &mut Option<Matrix>) {
    let diff = {
        let diff8 = Command::new("git")
            .current_dir(&config.repo_dir)
            .args(["diff"])
            .output()
            .expect("fail to git diff")
            .stdout;
        String::from_utf8(diff8).expect("strange diff")
    };
    if !config.solver.starts_with('/') {
        DIFF.set(diff.clone()).expect("L429");
    }
    if config.dump_dir.exists() {
        println!("WARNING: {} exists.", config.dump_dir.to_string_lossy());
    } else {
        fs::create_dir(&config.dump_dir).expect("fail to mkdir");
    }
    if let Some(m) = matrix {
        m.post(format!(
            "A new {} parallel, {} timeout benchmark starts.",
            config.num_jobs, config.timeout
        ))
        .await;
        if !diff.is_empty() {
            m.post("WARNING: There're unregistered modifications!".to_string())
                .await;
        }
    }
    println!("Benchmark: {}", &config.run_id);
}

async fn finalize_benchmark(
    config: Config,
    matrix: &mut Option<Matrix>,
    processed: &mut (usize, usize, usize),
) {
    let (s, u) = report(&config, processed.1).await.unwrap_or((0, 0));
    processed.2 = s + u;
    println!(
        "Benchmark {} finished, {} problems, {} solutions",
        config.run_id, processed.1, processed.2
    );
    if let Some(m) = matrix {
        m.post(format!(
            "A {} timeout benchmark {} ended, {} problems, {} solutions",
            config.timeout, config.run_id, processed.1, processed.2
        ))
        .await;
    }
    make_verifier(config.benchmark, &config.dump_dir, &config.data_dir)
        .expect("fail to create verify.sh");
    let tarfile = config.sync_dir.join(format!("{}.tar.xz", config.run_id));
    Command::new("tar")
        .args([
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

async fn check_result(
    config: &Config,
    matrix: &mut Option<Matrix>,
    result: SolveResult,
    processed: &mut (usize, usize, usize),
    results: &mut [SolveResultPromise],
) {
    let mut new_solution = false;
    // - processed.0 -- the last queued task id.
    // - processed.1 -- the index to check from.
    // - processed.2 -- the number of solved (process teminated normally).
    let i = result.0;
    results[i] = Some(result);
    // for j in processed.1..results.len() {
    for (j, res) in results.iter_mut().enumerate().skip(processed.1) {
        // skip all the processed
        // I have the resposibility to print the (j-1) th task's result.
        let task_id = j + 1;
        if let Some(r) = res {
            processed.1 = task_id;
            if r.2.is_ok() {
                processed.2 += 1;
                new_solution = true;
            }
            print!("{CLEAR}");
            // Note again: j is an index for RESULT,
            // and it corresponds to (j + 1) th task.
            if new_solution {
                if config.is_new_record(processed) {
                    let msg: String = format!("🎉 {:>3},{:>3},{}", task_id, processed.2, &r.1);
                    if let Some(m) = matrix {
                        m.post(msg).await;
                    }
                    println!("🎉 {:>3},{:>3},{}", task_id, processed.2, &r.1);
                } else {
                    if let Some(m) = matrix {
                        m.post(format!(" {:>3},{:>3},{}", task_id, processed.2, &r.1))
                            .await;
                    }
                    println!(" {:>3},{:>3},{}", task_id, processed.2, &r.1);
                };
            }
            if j % config.num_jobs == 0 {
                report(config, task_id).await.unwrap();
            }
        } else {
            // re-display the current running task id(s)
            // The following might be right under matrix configuration.
            // debug_assert!(task_id <= processed.0);
            if task_id < config.benchmark.len() {
                let mut fname = config.benchmark[task_id].1.to_string();
                fname.truncate(40);
                if task_id == processed.0 {
                    print!(
                        "{CLEAR}\x1B[032mRunning on the {task_id}th problem {fname}...\x1B[000m"
                    );
                } else {
                    print!(
                        "{CLEAR}\x1B[032mRunning on the {task_id}-{}th problem {fname}...\x1B[000m",
                        processed.0
                    );
                }
                stdout().flush().unwrap();
            }
            break;
        }
    }
}

async fn report(config: &Config, nprocessed: usize) -> std::io::Result<(usize, usize)> {
    let outname = config.sync_dir.join(format!("{}.csv", config.run_id));
    let mut nsat = 0;
    let mut nunsat = 0;
    {
        let outfile = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open(outname)?;
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
                        if problem.contains_key(key) {
                            panic!("duplicated {cnf}");
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
        if let Some(diff) = DIFF.get() {
            for l in diff.lines() {
                writeln!(outbuf, "# {l}")?;
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
    fn map_to_result(&mut self, config: &Config, cnf: &Path) -> Result<bool, SolverException>;
}

impl SolverHandling for Command {
    fn map_to_result(&mut self, config: &Config, cnf: &Path) -> Result<bool, SolverException> {
        match &self.output() {
            Ok(r) => {
                match (
                    r.status.code(),
                    String::from_utf8_lossy(&r.stdout),
                    String::from_utf8_lossy(&r.stderr),
                ) {
                    (Some(0), ref s, _) if s.contains("SATISFIABLE: ") => Ok(true),
                    (Some(0), ref s, _) if s.contains("UNSAT: ") => Ok(false),
                    (Some(10), ref s, _) if s.contains("s SATISFIABLE") => {
                        if !SPLR.get().unwrap().is_match(&config.solver) {
                            config.dump_stream(cnf, s).unwrap();
                        }
                        Ok(true)
                    }
                    (Some(20), ref s, _) if s.contains("s UNSATISFIABLE") => {
                        if !SPLR.get().unwrap().is_match(&config.solver) {
                            config.dump_stream(cnf, s).unwrap();
                        }
                        Ok(false)
                    }
                    (_, ref s, _)
                        if s.contains("s UNKNOWN")
                            || s.contains("TimeOut")
                            || s.contains("s INDETERMINATE") =>
                    {
                        if !SPLR.get().unwrap().is_match(&config.solver) {
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
                println!("{r}");
                Err(SolverException::Abort)
            }
        }
    }
}
