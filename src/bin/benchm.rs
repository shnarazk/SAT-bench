/// A simple SAT benchmarker with Matrix monitor
use {
    lazy_static::lazy_static,
    regex::Regex,
    sat_bench::{
        bench19::{BENCHMARK, SCB},
        matrix,
        utils::{current_date_time, parse_result},
    },
    std::{
        collections::HashMap,
        env, fs,
        io::{stdout, BufWriter, Write},
        path::PathBuf,
        process::Command,
        str,
        sync::RwLock,
        time,
    },
    structopt::StructOpt,
};

const VERSION: &str = "benchbot 0.7.4";
const CLEAR: &str = "\x1B[1G\x1B[0K";

/// Abnormal termination flags.
#[derive(Debug)]
pub enum SolverException {
    TimeOut,
    Abort,
}

type SolveResultPromise = Option<(String, Result<f64, SolverException>)>;

lazy_static! {
    pub static ref CONFIG: RwLock<Config> = RwLock::new(Config::default());
    pub static ref DIFF: RwLock<String> = RwLock::new(String::new());
    pub static ref PQ: RwLock<Vec<(usize, String)>> = RwLock::new(Vec::new());
    pub static ref PROCESSED: RwLock<usize> = RwLock::new(0);
    pub static ref ANSWERED: RwLock<usize> = RwLock::new(0);
    pub static ref RUN_ID: RwLock<String> = RwLock::new(String::new());
    pub static ref RUN_NAME: RwLock<String> = RwLock::new(String::new());
    pub static ref RESVEC: RwLock<Vec<SolveResultPromise>> = RwLock::new(Vec::new());
    /// - the number of try: usize
    /// - the number of solved: usize
    pub static ref NREPORT: RwLock<(usize, usize)> = RwLock::new((0, 0));
}

#[derive(Clone, Debug, StructOpt)]
#[structopt(name = "sat-bench", about = "Run the SAT Competition benchmark")]
pub struct Config {
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
    #[structopt(long = "timeout", short = "T", default_value = "600")]
    pub timeout: usize,
    /// number of workers
    #[structopt(long = "jobs", short = "j", default_value = "4")]
    pub num_jobs: usize,
    /// arguments passed to solvers
    #[structopt(long = "options", default_value = "")]
    pub solver_options: String,
    /// data directory
    #[structopt(long = "data", default_value = "~/Library/SAT/SR19main")]
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
    /// Don't assign
    #[structopt(long = "dump", default_value = "")]
    pub dump_dir: PathBuf,
    /// Don't assign
    #[structopt(long = "run", default_value = "")]
    pub run_id: String,
    /// Don't assign
    #[structopt(skip)]
    pub run_name: String,
    /// user id to post to Matrix
    #[structopt(long = "matrix-id", default_value = "")]
    pub matrix_id: String,
    /// user password to post to Matrix
    #[structopt(long = "matrix-password", default_value = "")]
    pub matrix_password: String,
    /// The Matrix room
    #[structopt(long = "matrix-room", default_value = "")]
    pub matrix_room: String,
    #[structopt(skip)]
    pub matrix_token: Option<String>,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            solver: String::from("splr"),
            target_from: 0,
            target_to: 400,
            target_step: 1,
            timeout: 5000,
            num_jobs: 3,
            solver_options: String::new(),
            data_dir: PathBuf::new(),
            repo_dir: PathBuf::new(),
            sync_dir: PathBuf::new(),
            sync_cmd: String::new(),
            dump_dir: PathBuf::new(),
            run_id: String::new(),
            run_name: String::new(),
            matrix_id: String::new(),
            matrix_password: String::new(),
            matrix_room: String::new(),
            matrix_token: None,
        }
    }
}

impl Config {
    fn post(&self, msg: &str) {
        if let Ok(id) = RUN_NAME.read() {
            matrix::post(
                &self.matrix_room,
                &self.matrix_token,
                &format!("{}: {}", id, msg),
            );
        }
    }
}

fn main() {
    let mut config = Config::from_args();
    let tilde = Regex::new("~").expect("wrong regex");
    let home = env::var("HOME").expect("No home");
    config.data_dir = PathBuf::from(
        tilde
            .replace(&config.data_dir.to_string_lossy(), &home[..])
            .to_string(),
    );
    config.sync_dir = PathBuf::from(
        tilde
            .replace(&config.sync_dir.to_string_lossy(), &home[..])
            .to_string(),
    );
    config.repo_dir = PathBuf::from(
        tilde
            .replace(&config.repo_dir.to_string_lossy(), &home[..])
            .to_string(),
    );
    if !config.matrix_id.is_empty() {
        let mut map: HashMap<&str, &str> = HashMap::new();
        map.insert("user", &config.matrix_id);
        map.insert("password", &config.matrix_password);
        config.matrix_token = matrix::get_token(&map);
        println!(
            "ready to post to matrix; user: {}, token: {:?}",
            config.matrix_id, config.matrix_token,
        );
    }
    config.post("A test post from benchm.");
    if let Ok(mut conf) = CONFIG.write() {
        *conf = config.clone();
    }
    start_benchmark();
}

fn start_benchmark() {
    let mut config = if let Ok(conf) = CONFIG.read() {
        conf.clone()
    } else {
        Config::from_args()
    };
    if config.solver.is_empty() {
        config.solver = "splr".to_string();
        for e in config.repo_dir.join("src/bin").read_dir().expect("no repo") {
            if let Ok(f) = e {
                let splr = f.path().file_stem().unwrap().to_string_lossy().to_string();
                if splr.contains("splr") {
                    print!("\x1B[032mCompiling {}...\x1B[000m", config.solver);
                    stdout().flush().unwrap();
                    Command::new("cargo")
                        .current_dir(&config.repo_dir)
                        .args(&["install", "--path", ".", "--force"])
                        .output()
                        .expect("fail to compile");
                    config.solver = splr;
                    println!("\x1B[032mdone.\x1B[000m");
                    break;
                }
            }
        }
    }
    let host = {
        let h = Command::new("hostname")
            .arg("-s")
            .output()
            .expect("failed to execute process")
            .stdout;
        String::from_utf8_lossy(&h[..h.len() - 1]).to_string()
    };
    {
        let commit_id_u8 = Command::new("git")
            .current_dir(&config.repo_dir)
            .args(&["log", "-1", "--format=format:%h"])
            .output()
            .expect("fail to git")
            .stdout;
        let commit_id = String::from_utf8(commit_id_u8).expect("strange commit id");
        let timestamp = current_date_time().format("%F").to_string();
        config.run_name = format!("{}-{}", config.solver, commit_id);
        config.run_id = format!("{}-{}-{}", config.run_name, timestamp, host);
    }
    let diff = {
        let diff8 = Command::new("git")
            .current_dir(&config.repo_dir)
            .args(&["diff"])
            .output()
            .expect("fail to git diff")
            .stdout;
        String::from_utf8(diff8).expect("strange diff")
    };
    if let Ok(mut d) = DIFF.write() {
        *d = diff.clone();
    }
    if let Ok(mut id) = RUN_ID.write() {
        *id = config.run_id.clone();
    }
    if let Ok(mut name) = RUN_NAME.write() {
        *name = config.run_name.clone();
    }
    config.dump_dir = PathBuf::from(&config.run_id);
    if config.dump_dir.exists() {
        println!("WARNING: {} exists.", config.dump_dir.to_string_lossy());
    } else {
        fs::create_dir(&config.dump_dir).expect("fail to mkdir");
    }
    if let Ok(mut queue) = PQ.write() {
        if let Ok(mut v) = RESVEC.write() {
            v.push(None);
            for s in SCB.iter().take(config.target_to).skip(config.target_from) {
                if s.0 % config.target_step == 0 {
                    queue.push((s.0, s.1.to_string()));
                }
                v.push(None);
            }
        }
        queue.reverse();
    }
    if let Ok(mut processed) = PROCESSED.write() {
        *processed = config.target_from;
    }
    if let Ok(mut answered) = ANSWERED.write() {
        let (s, u) = report(&config).unwrap_or((0, 0));
        *answered = s + u;
    }
    config.post(&format!(
        "A new {} parallel benchmark starts.",
        config.num_jobs
    ));
    if !diff.is_empty() {
        config.post(&format!(
            "**WARNING: unregistered modifications**\n```diff\n{}```\n",
            diff
        ));
    }
    crossbeam::scope(|s| {
        for i in 0..config.num_jobs {
            let c = config.clone();
            s.spawn(move |_| {
                std::thread::sleep(time::Duration::from_millis((2 + 2 * i as u64) * 1000));
                worker(c);
            });
        }
    })
    .expect("fail to exit crossbeam::scope");

    let (s, u) = report(&config).unwrap_or((0, 0));
    if let Ok(mut answered) = ANSWERED.write() {
        let sum = s + u;
        *answered = sum;
    }
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
    println!("Benchmark {} finished.", config.run_id);
    let pro = PROCESSED.read().and_then(|v| Ok(*v)).unwrap_or(0);
    check_result(&config);
    config.post(&format!(
        "Benchmark ended, {} problems, {} solutions",
        pro,
        s + u
    ));
    if let Ok(mut conf) = CONFIG.write() {
        *conf = config.clone();
    }
}

fn worker(config: Config) {
    loop {
        if let Some((i, p)) = next_task(&config) {
            check_result(&config);
            let res: SolveResultPromise = execute(&config, p);
            if let Ok(mut v) = RESVEC.write() {
                v[i] = res;
            }
        } else {
            return;
        }
    }
}

fn next_task(config: &Config) -> Option<(usize, PathBuf)> {
    let p: Option<(usize, PathBuf)> = if let Ok(mut q) = PQ.write() {
        if let Some((index, top)) = q.pop() {
            if let Ok(mut processed) = PROCESSED.write() {
                *processed = index;
            }
            Some((index, config.data_dir.join(top)))
        } else {
            None
        }
    } else {
        None
    };
    p
}

fn check_result(config: &Config) {
    let mut new_solution = false;
    let mut new_record = false;
    let processed = if let Ok(p) = PROCESSED.read() { *p } else { 0 };
    if let Ok(mut n) = NREPORT.write() {
        // - n.0 -- target id to be checked firstly
        // - n.1 -- the number of process teminated normally
        if let Ok(v) = RESVEC.read() {
            for j in n.0..v.len() { // skip all the processed
                if let Some(r) = &v[j] {
                    n.0 = j + 1;
                    if r.1.is_ok() {
                        n.1 += 1;
                        new_solution = true;
                        if j % config.num_jobs == 0 {
                            let (s, u) = report(&config).unwrap_or((0, 0));
                            if let Ok(mut answered) = ANSWERED.write() {
                                let sum = s + u;
                                if *answered < sum {
                                    new_solution = true;
                                    *answered = sum;
                                }
                            }
                        }
                        // TODO: this is for SR2018
                        new_record = config.timeout == 1000
                            && 4 <= config.num_jobs
                            && ((n.0 == 20 && 3 < n.1)
                                || (n.0 == 40 && 4 < n.1)
                                || (n.0 == 60 && 20 < n.1)
                                || (n.0 == 80 && 26 < n.1)
                                || (n.0 == 100 && 44 < n.1)
                                || (n.0 == 120 && 54 < n.1)
                                || (n.0 == 140 && 56 < n.1)
                                || (n.0 == 160 && 59 < n.1)
                                || (n.0 == 180 && 66 < n.1)
                                || (n.0 == 200 && 73 < n.1)
                                || (n.0 == 220 && 81 < n.1)
                                || (n.0 == 240 && 92 < n.1)
                                || (n.0 == 260 && 99 < n.1)
                                || (n.0 == 280 && 104 < n.1)
                                || (n.0 == 300 && 113 < n.1)
                                || (n.0 == 320 && 123 < n.1)
                                || (n.0 == 340 && 135 < n.1)
                                || (n.0 == 360 && 148 < n.1)
                                || (n.0 == 380 && 154 < n.1)
                                || (n.0 == 400 && 155 < n.1));
                    }
                    print!("{}", CLEAR);
                    if new_record {
                        config.post(&format!("*{:>3},{:>3}", j + 1, n.1));
                        print!("*");
                    } else {
                        if new_solution {
                            config.post(&format!(" {:>3},{:>3}", j + 1, n.1));
                        }
                        print!(" ");
                    }
                    println!("{:>3},{:>3},{}", j + 1, n.1, &r.0);
                } else {
                    if j == processed {
                        print!(
                            "{}\x1B[032mRunning on the {} th problem {}...\x1B[000m",
                            CLEAR,
                            j + 1,
                            SCB[j].1
                        );
                    } else {
                        print!(
                            "{}\x1B[032mRunning on the {}-{} th problem {}...\x1B[000m",
                            CLEAR,
                            j + 1,
                            processed,
                            SCB[j].1
                        );
                    }
                    stdout().flush().unwrap();
                    break;
                }
            }
        }
    }
}

fn execute(config: &Config, cnf: PathBuf) -> SolveResultPromise {
    assert!(
        cnf.is_file(),
        format!("{} does not exist.", cnf.to_string_lossy())
    );
    let target: String = cnf.file_name().unwrap().to_string_lossy().to_string();
    // println!("\x1B[032m{}\x1B[000m", target);
    let mut command: Command = solver_command(config);
    for opt in config.solver_options.split_whitespace() {
        command.arg(&opt[opt.starts_with('\\') as usize..]);
    }
    Some((
        target,
        command.arg(cnf.as_os_str()).to_result(&config.solver),
    ))
}

fn solver_command(config: &Config) -> Command {
    lazy_static! {
        static ref GLUCOSE: Regex = Regex::new(r"\bglucose").expect("wrong regex");
        // static ref lingeling: Regex = Regex::new(r"\blingeling").expect("wrong regex");
        // static ref minisat: Regex = Regex::new(r"\bminisat").expect("wrong regex");
        // static ref mios: Regex = Regex::new(r"\bmios").expect("wrong regex");
        static ref SPLR: Regex = Regex::new(r"\bsplr").expect("wrong regex");
    }
    if SPLR.is_match(&config.solver) {
        let mut command = Command::new(&config.solver);
        command.args(&[
            "--to",
            &format!("{}", config.timeout),
            "-o",
            &config.dump_dir.to_string_lossy(),
        ]);
        command
    } else if GLUCOSE.is_match(&config.solver) {
        let mut command = Command::new(&config.solver);
        command.args(&["-verb=0", &format!("-cpu-lim={}", config.timeout)]);
        command
    } else {
        Command::new(&config.solver)
    }
}

fn report(config: &Config) -> std::io::Result<(usize, usize)> {
    let outname = config.sync_dir.join(config.run_id.to_string() + ".csv");
    let mut nsat = 0;
    let mut nunsat = 0;
    {
        let outfile = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(&outname)?;
        let mut outbuf = BufWriter::new(outfile);
        let mut hash: HashMap<&str, (f64, String, bool)> = HashMap::new();
        let timeout = config.timeout as f64;
        let processed = if let Ok(p) = PROCESSED.read() { *p } else { 0 };
        for e in config.dump_dir.read_dir()? {
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
                            hash.insert(key, (timeout.min(t), m, s));
                            break;
                        }
                    }
                }
            }
        }
        writeln!(
            outbuf,
            "#{} by {}: from {} to {}\n# process: {}, timeout: {}\n# Procesed: {}, total answers: {} (SAT: {}, UNSAT: {}) so far",
            config.run_id,
            VERSION,
            config.target_from,
            config.target_to,
            config.num_jobs,
            config.timeout,
            processed,
            nsat + nunsat,
            nsat,
            nunsat,
        )?;
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
        for (i, key) in SCB.iter() {
            if let Some(v) = hash.get(key) {
                nsolved += 1;
                writeln!(
                    outbuf,
                    "\"{}\",{},\"{}/{}\",{},{:.2},{},{}",
                    config.dump_dir.to_string_lossy(),
                    i,
                    BENCHMARK,
                    key,
                    nsolved,
                    v.0,
                    v.1,
                    v.2,
                )?;
            } else {
                writeln!(
                    outbuf,
                    "\"{}\",{},\"{}/{}\",{},{},,",
                    config.dump_dir.to_string_lossy(),
                    i,
                    BENCHMARK,
                    key,
                    nsolved,
                    config.timeout,
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
    fn set_solver(&mut self, solver: &str) -> &mut Self;
    fn to_result(&mut self, solver: &str) -> Result<f64, SolverException>;
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
    fn to_result(&mut self, solver: &str) -> Result<f64, SolverException> {
        lazy_static! {
            static ref MINISAT_LIKE: Regex =
                Regex::new(r"\b(glucose|minisat)").expect("wrong regex");
        }
        let result = self.output();
        match &result {
            Ok(r)
                if String::from_utf8(r.stderr.clone())
                    .unwrap()
                    .contains("thread 'main' panicked") =>
            {
                Err(SolverException::Abort)
            }
            Ok(r)
                if String::from_utf8(r.stdout.clone())
                    .unwrap()
                    .contains("TimeOut") =>
            {
                Err(SolverException::TimeOut)
            }
            Ok(ref done) => match done.status.code() {
                Some(0) => Ok(0.0),
                Some(10) | Some(20) if MINISAT_LIKE.is_match(solver) => Ok(0.0),
                _ => return Err(SolverException::Abort),
            },
            Err(_) => Err(SolverException::Abort),
        }
    }
}
