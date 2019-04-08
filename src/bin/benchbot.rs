/// A simple SAT benchmarker
use lazy_static::lazy_static;
use regex::Regex;
use sat_bench::utils::current_date_time;
use sat_bench::{bench18::SCB, utils::parse_result};
use serenity::builder::GetMessages;
use serenity::client::{Client, Context, EventHandler};
use serenity::command;
use serenity::framework::standard::StandardFramework;
use serenity::model::channel::Channel;
use serenity::model::event::TypingStartEvent;
use serenity::model::gateway::Game;
use serenity::model::id::ChannelId;
use serenity::model::user::OnlineStatus;
use std::collections::HashMap;
use std::fs;
use std::io::{stdout, BufWriter, Write};
use std::path::PathBuf;
use std::process::Command;
use std::str;
use std::sync::RwLock;
use std::{env, process, thread, time};
use structopt::StructOpt;

const VERSION: &str = "benchbot 0.1.4";

lazy_static! {
    pub static ref CHID: RwLock<u64> = RwLock::new(0);
    pub static ref CONFIG: RwLock<Config> = RwLock::new(Config::default());
    pub static ref DIFF: RwLock<String> = RwLock::new(String::new());
    pub static ref PQ: RwLock<Vec<String>> = RwLock::new(Vec::new());
    pub static ref PROCESSED: RwLock<usize> = RwLock::new(0);
    pub static ref ANSWERED: RwLock<usize> = RwLock::new(0);
    pub static ref M: RwLock<String> = RwLock::new(String::new());
    pub static ref RUN: RwLock<String> = RwLock::new(String::new());
    pub static ref N: RwLock<usize> = RwLock::new(0);
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
    /// time out in seconds
    #[structopt(long = "timeout", short = "T", default_value = "2000")]
    pub timeout: usize,
    /// number of workers
    #[structopt(long = "jobs", short = "j", default_value = "3")]
    pub num_jobs: usize,
    /// arguments passed to solvers
    #[structopt(long = "options", default_value = "")]
    pub solver_options: String,
    /// data directory
    #[structopt(long = "data", default_value = "~/Documents/SAT-RACE/SC18main")]
    pub data_dir: PathBuf,
    /// solver repository
    #[structopt(long = "repo", default_value = "~/Repositories/splr")]
    pub repo_dir: PathBuf,
    /// cloud sharing directory
    #[structopt(long = "sync", default_value = "~/Documents/ownCloud/splr-exp")]
    pub sync_dir: PathBuf,
    /// cloud sync command
    #[structopt(long = "sync-cmd", default_value = "syncCloud")]
    pub sync_cmd: String,
    /// Don't assign
    #[structopt(long = "dump", default_value = "")]
    pub dump_dir: PathBuf,
    /// Don't assign
    #[structopt(long = "run", default_value = "")]
    pub run_name: String,
    /// Discord chhannel
    #[structopt(long = "channel", default_value = "")]
    pub discord_channel: String,
    /// Discord token
    #[structopt(long = "token", default_value = "")]
    pub discord_token: String,
}

impl Default for Config {
    fn default() -> Config {
        Config {
            solver: String::from("splr"),
            target_from: 0,
            target_to: 400,
            timeout: 2000,
            num_jobs: 3,
            solver_options: String::new(),
            data_dir: PathBuf::new(),
            repo_dir: PathBuf::new(),
            sync_dir: PathBuf::new(),
            sync_cmd: String::new(),
            dump_dir: PathBuf::new(),
            run_name: String::new(),
            discord_channel: String::new(),
            discord_token: String::new(),
        }
    }
}

fn main() {
    let mut config = Config::from_args();
    let tilde = Regex::new("~").expect("wrong reex");
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
    config.run_name = {
        let commit_id_u8 = Command::new("git")
            .current_dir(format!("{}/Repositories/splr", home))
            .args(&["log", "-1", "--format=format:%h"])
            .output()
            .expect("fail to git")
            .stdout;
        let commit_id = unsafe { String::from_utf8_unchecked(commit_id_u8) };
        let timestamp = current_date_time().format("%F").to_string();
        format!("{}-{}-{}-{}", config.solver, commit_id, timestamp, host)
    };
    let diff = {
        let diff8 = Command::new("git")
            .current_dir(format!("{}/Repositories/splr", home))
            .args(&["diff"])
            .output()
            .expect("fail to git diff")
            .stdout;
        unsafe { String::from_utf8_unchecked(diff8) }
    };
    if let Ok(mut d) = DIFF.write() {
        *d = diff.clone();
    }
    if let Ok(mut run) = RUN.write() {
        *run = config.run_name.clone();
    }
    config.dump_dir = PathBuf::from(&config.run_name);
    if config.dump_dir.exists() {
        println!("WARNING: {} exists.", config.dump_dir.to_string_lossy());
    } else {
        fs::create_dir(&config.dump_dir).expect("fail to mkdir");
    }
    if let Ok(mut cid) = CHID.write() {
        *cid = if config.discord_channel.is_empty() {
            env::var("DISCORD_CHANNEL")
                .expect("no channel")
                .parse::<u64>()
                .unwrap()
        } else {
            config.discord_channel.parse::<u64>().unwrap()
        };
    }
    let mut client = if !config.discord_token.is_empty() {
        Client::new(&config.discord_token, Handler).expect("create client")
    } else {
        Client::new(&env::var("DISCORD_TOKEN").expect("token"), Handler).expect("create client")
    };
    client.with_framework(
        StandardFramework::new()
            .configure(|c| c.prefix("."))
            .cmd("clear", clean)
            .cmd("draw", draw)
            .cmd("who", who)
            .cmd("whatsup", whatsup)
            .cmd("help", help)
            .cmd("bye", bye),
    );
    if let Ok(mut queue) = PQ.write() {
        for s in SCB.iter().take(config.target_to).skip(config.target_from) {
            queue.push(s.to_string());
        }
        queue.reverse();
    }
    if let Ok(mut processed) = PROCESSED.write() {
        *processed = config.target_from;
    }
    report(&config).unwrap();
    for i in 0..config.num_jobs {
        let c = config.clone();
        thread::spawn(move || {
            thread::sleep(time::Duration::from_millis((2 + 2 * i as u64) * 1000));
            worker(c);
        });
    }
    post(&format!(
        "A new {} parallel benchmark starts.",
        config.num_jobs,
    ));
    if !diff.is_empty() {
        post(&format!(
            "**WARNING: unregistered modifications**\n```diff\n{}```\n",
            diff
        ));
    }
    if let Ok(mut conf) = CONFIG.write() {
        *conf = config.clone();
    }
    if let Err(why) = client.start() {
        println!("An error occurred while running the client: {:?}", why);
    }
}

fn worker(config: Config) {
    loop {
        let mut p: PathBuf;
        let num: usize;
        if let Ok(mut q) = PQ.write() {
            if let Some(top) = q.pop() {
                p = config.data_dir.join(top);
                num = q.len();
                if let Ok(mut processed) = PROCESSED.write() {
                    *processed += 1;
                }
            } else {
                break;
            }
        } else {
            break;
        }
        execute(&config, num, &p);
        if num == 0 {
            // I'm the last one.
            state("");
            let (s, u) = report(&config).unwrap_or((0, 0));
            if let Ok(processed) = PROCESSED.read() {
                if let Ok(mut answered) = ANSWERED.write() {
                    let sum = s + u;
                    *answered = sum;
                    post(&format!(
                        "All {} problems were solved, answered {}.",
                        processed, sum,
                    ));
                }
            }
            let tarfile = config.sync_dir.join(&format!("{}.tar.xz", config.run_name));
            Command::new("tar")
                .args(&[
                    "cvf",
                    &tarfile.to_string_lossy(),
                    &config.dump_dir.to_string_lossy(),
                ])
                .output()
                .expect("fail to tar");
            if !config.sync_cmd.is_empty() {
                Command::new(&config.sync_cmd)
                    .output()
                    .expect("fail to sync");
            }
        // process::exit(0);
        } else if (400 - num) % 10 == 0 {
            let (s, u) = report(&config).unwrap_or((0, 0));
            if let Ok(mut answered) = ANSWERED.write() {
                let sum = s + u;
                if *answered < sum {
                    *answered = sum;
                    post(&format!(
                        "The {}-th job is done, answered {}.",
                        400 - num,
                        sum
                    ));
                }
            }
        }
    }
}

fn execute(config: &Config, _num: usize, cnf: &PathBuf) {
    let f = PathBuf::from(cnf);
    if f.is_file() {
        let target: String = f.file_name().unwrap().to_str().unwrap().to_string();
        println!("\x1B[032mRunning on {}...\x1B[000m", target);
        if let Ok(processed) = PROCESSED.read() {
            state(&format!("#{}, {}", *processed, &target));
        }
        let mut command: Command = solver_command(config);
        for opt in config.solver_options.split_whitespace() {
            command.arg(&opt[opt.starts_with('\\') as usize..]);
        }
        if command.arg(f.as_os_str()).output().is_err() {
            post(&format!("Something happened to {}.", &target));
        }
    }
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

fn post(mes: &str) {
    if let Ok(cid) = CHID.read() {
        if let Ok(channel) = ChannelId(*cid).to_channel() {
            if let Channel::Guild(gchannel) = &channel {
                let ch = gchannel.read();
                if let Ok(id) = RUN.read() {
                    ch.say(&format!("{}: {}", *id, mes)).expect("fail to say");
                }
            }
        }
    }
}

fn state(s: &str) {
    if let Ok(cid) = CHID.read() {
        if let Ok(channel) = ChannelId(*cid).to_channel() {
            if let Channel::Guild(gchannel) = &channel {
                if let Ok(mut mes) = M.write() {
                    *mes = s.to_string();
                }
                let ch = gchannel.read();
                ch.broadcast_typing().expect("typing");
            }
        }
    }
}

fn report(config: &Config) -> std::io::Result<(usize, usize)> {
    let outname = config.sync_dir.join(config.run_name.to_string() + ".csv");
    let mut nsat = 0;
    let mut nunsat = 0;
    {
        let outfile = fs::OpenOptions::new()
            .write(true)
            .create(true)
            .open(&outname)?;
        let mut outbuf = BufWriter::new(outfile);
        let mut hash: HashMap<&str, (f64, bool, String)> = HashMap::new();
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
                for key in SCB.iter() {
                    if *key == cnf {
                        if None != hash.get(key) {
                            panic!("duplicated {}", cnf);
                        }
                        if let Some((t, s, m)) = parse_result(f.path()) {
                            hash.insert(key, (timeout.min(t), s, m));
                            if s {
                                nsat += 1;
                            } else {
                                nunsat += 1;
                            }
                            break;
                        }
                    }
                }
            }
        }
        writeln!(
            outbuf,
            "#{} by {}: from {} to {}\n# process: {}, timeout: {}\n# Procesed: {}, total answers: {} (SAT: {}, UNSAT: {}) so far",
            config.run_name,
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
            "solver, num, target, time, satisfiability, strategy"
        )?;
        for (i, key) in SCB.iter().enumerate() {
            if let Some(v) = hash.get(key) {
                writeln!(
                    outbuf,
                    "\"{}\",{},\"{}{}\",{:>8.2},{},{}",
                    config.dump_dir.to_string_lossy(),
                    i + 1,
                    "SC18main/",
                    key,
                    v.0,
                    v.1,
                    v.2,
                )?;
            } else {
                writeln!(
                    outbuf,
                    "\"{}\",{},\"{}{}\",{:>5},{},",
                    config.dump_dir.to_string_lossy(),
                    i + 1,
                    "SC18main/",
                    key,
                    config.timeout,
                    "",
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

struct Handler;
impl EventHandler for Handler {
    fn typing_start(&self, ctx: Context, _: TypingStartEvent) {
        if let Ok(mes) = M.read() {
            if mes.is_empty() {
                ctx.set_presence(None, OnlineStatus::Idle);
            } else {
                let name = format!("{}", *mes);
                ctx.set_game(Game::playing(&name));
            }
        }
    }
}

command!(bye(_context, _message) {
    process::exit(0);
});

command!(clean(context, message) {
    context.idle();
    let ch = message.channel_id;
    let retriever = GetMessages::default().before(message.id).limit(100);
    match ch.messages(|_| retriever) {
        Ok(ref v) => {
            let len = v.len();
            let mut n = 0;
            for (i, m) in v.iter().enumerate() {
                match m.delete() {
                    Ok(_) => n += 1,
                    Err(why) => println!("{} ({}/{}): {}", n, i, len, why),
                }
            }
        }
        Err(e) => {
            ch.say(&format!("Error {}", e))?;
        }
    }
});

command!(draw(_context, message) {
    message.channel_id.say(&format!("not yet implemented"))?;
    // message.channel_id.send_files(&[]);
});

command!(who(_context, message) {
    if let Ok(conf) = CONFIG.read() {
        message.channel_id.say(&format!("I am {}: ```rust\n{:?}\n```", VERSION, conf))?;
    }
});

command!(help(_context, message) {
    message.channel_id.say(&format!("I accept `bye`, `clean`, `whatsup`, `who`"))?;
});

command!(whatsup(_context, message) {
    if let Ok(p) = PROCESSED.read() {
        if let Ok(a) = ANSWERED.read() {
            message.channel_id.say(&format!("tried {} problems, answered {}.", p, a))?;
        }
    }
});
