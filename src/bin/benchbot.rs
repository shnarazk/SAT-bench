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
use serenity::model::prelude::Ready;
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

const VERSION: &str = "benchbot 0.5.12";

lazy_static! {
    pub static ref CHID: RwLock<u64> = RwLock::new(0);
    pub static ref CONFIG: RwLock<Config> = RwLock::new(Config::default());
    pub static ref DIFF: RwLock<String> = RwLock::new(String::new());
    pub static ref PQ: RwLock<Vec<(usize, String)>> = RwLock::new(Vec::new());
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
    /// step of choosen target problems
    #[structopt(long = "step", default_value = "1")]
    pub target_step: usize,
    /// time out in seconds
    #[structopt(long = "timeout", short = "T", default_value = "1000")]
    pub timeout: usize,
    /// number of workers
    #[structopt(long = "jobs", short = "j", default_value = "4")]
    pub num_jobs: usize,
    /// arguments passed to solvers
    #[structopt(long = "options", default_value = "")]
    pub solver_options: String,
    /// data directory
    #[structopt(long = "data", default_value = "~/Library/SAT/SC18main")]
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
            target_step: 1,
            timeout: 5000,
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
    let mut client = if !config.discord_token.is_empty() {
        Client::new(&config.discord_token, Handler).expect("Can't tcreate client")
    } else {
        Client::new(&env::var("DISCORD_TOKEN").expect("NO token"), Handler)
            .expect("Can't create client")
    };
    if let Ok(mut conf) = CONFIG.write() {
        *conf = config.clone();
    }
    // start_benchmark();
    client.with_framework(
        StandardFramework::new()
            .configure(|c| c.prefix("."))
            .cmd("clear", clean)
            .cmd("draw", draw)
            .cmd("start", start)
            .cmd("whatsup", whatsup)
            .cmd("who", who)
            .cmd("help", help)
            .cmd("bye", bye),
    );
    if let Err(why) = client.start() {
        println!("An error occurred while running the client: {:?}", why);
    } else {
        println!("Start a discord client.");
    }
}

fn start_benchmark() {
    let mut config = if let Ok(conf) = CONFIG.read() {
        conf.clone()
    } else {
        Config::from_args()
    };
    let home = env::var("HOME").expect("No home");
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
    if let Ok(mut queue) = PQ.write() {
        for s in SCB.iter().take(config.target_to).skip(config.target_from) {
            if s.0 % config.target_step == 0 {
                queue.push((s.0, s.1.to_string()));
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
}

fn worker(config: Config) {
    loop {
        let mut p: PathBuf;
        let num: usize;
        let n: usize;
        if let Ok(mut q) = PQ.write() {
            if let Some((index, top)) = q.pop() {
                n = index;
                p = config.data_dir.join(top);
                num = q.len();
                if let Ok(mut processed) = PROCESSED.write() {
                    *processed = index;
                }
            } else {
                break;
            }
        } else {
            break;
        }
        execute(&config, n, &p);
        if num == 0 {
            // I'm the last one.
            state("");
            let (s, u) = report(&config).unwrap_or((0, 0));
            if let Ok(mut answered) = ANSWERED.write() {
                let sum = s + u;
                *answered = sum;
                post(&format!(
                    "All ({} + {}, {}) problems were solved, answered {}.{}",
                    config.target_from,
                    config.target_step,
                    config.target_to,
                    sum,
                    if 172 < sum { " It's a new record!" } else { "" },
                ));
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
                    .expect("fail to run sync command");
            }
            println!("Benchmark {} has been done.", config.run_name);
        // process::exit(0);
        } else {
            let pro = {
                if let Ok(processed) = PROCESSED.read() {
                    *processed
                } else {
                    0
                }
            };
            if pro % config.num_jobs == 0 {
                let (s, u) = report(&config).unwrap_or((0, 0));
                if let Ok(mut answered) = ANSWERED.write() {
                    let sum = s + u;
                    *answered = sum;
                }
            }
            let ans = {
                if let Ok(answered) = ANSWERED.read() {
                    *answered
                } else {
                    0
                }
            };
            if (num == 20 && 3 < ans && config.timeout == 1000)
                || (num == 40 && 4 < ans)
                || (num == 60 && 19 < ans)
                || (num == 80 && 24 < ans)
                || (num == 100 && 42 < ans)
                || (num == 120 && 51 < ans)
                || (num == 140 && 53 < ans)
                || (num == 160 && 56 < ans)
                || (num == 180 && 61 < ans)
                || (num == 200 && 67 < ans)
                || (num == 220 && 75 < ans)
                || (num == 240 && 84 < ans)
                || (num == 260 && 90 < ans)
                || (num == 280 && 95 < ans)
                || (num == 300 && 99 < ans)
                || (num == 320 && 110 < ans)
                || (num == 340 && 122 < ans)
                || (num == 360 && 133 < ans)
                || (num == 380 && 134 < ans)
                || (num == 400 && 135 < ans)
            {
                post(&format!(
                    "New record: {} solutions at {}-th problem.",
                    ans, pro
                ));
            }
        }
    }
}

fn execute(config: &Config, num: usize, cnf: &PathBuf) {
    let f = PathBuf::from(cnf);
    if f.is_file() {
        let target: String = f.file_name().unwrap().to_str().unwrap().to_string();
        println!("\x1B[032mRunning on {}...\x1B[000m", target);
        if let Ok(answered) = ANSWERED.read() {
            state(&format!("{}#{},{}", *answered, num, &target));
        }
        let mut command: Command = solver_command(config);
        for opt in config.solver_options.split_whitespace() {
            command.arg(&opt[opt.starts_with('\\') as usize..]);
        }
        let result = command.arg(f.as_os_str()).output();
        match &result {
            Err(_) => post(&format!("Something happened to {}.", &target)),
            Ok(r) if String::from_utf8(r.stderr.clone()).unwrap().contains("thread 'main' panicked") => {
                post(&format!("**Panic at {}.**", &target))
            }
            _ => (),
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
            "solver, num, target, nsolved, time, strategy, satisfiability"
        )?;
        let mut nsolved = 0;
        for (i, key) in SCB.iter() {
            if let Some(v) = hash.get(key) {
                nsolved += 1;
                writeln!(
                    outbuf,
                    "\"{}\",{},\"SC18main/{}\",{:>3},{:>8.2},{},{}",
                    config.dump_dir.to_string_lossy(),
                    i,
                    key,
                    nsolved,
                    v.0,
                    v.1,
                    v.2,
                )?;
            } else {
                writeln!(
                    outbuf,
                    "\"{}\",{},\"SC18main/{}\",{:>3},{:>5},,",
                    config.dump_dir.to_string_lossy(),
                    i,
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

struct Handler;
impl EventHandler for Handler {
    fn ready(&self, ctx: Context, _: Ready) {
        println!("booted up");
        ctx.idle();
    }
    fn typing_start(&self, ctx: Context, _: TypingStartEvent) {
        if let Ok(mes) = M.read() {
            if mes.is_empty() {
                ctx.set_presence(None, OnlineStatus::Idle);
            } else {
                let name = mes.to_string();
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
    println!("{:?}", message.content);
    if let Ok(conf) = CONFIG.read() {
        let host = {
            let h = Command::new("hostname")
                .arg("-s")
                .output()
                .expect("failed to execute process")
                .stdout;
            String::from_utf8_lossy(&h[..h.len() - 1]).to_string()
        };
        // ./mkCactusL.R rio.runs 400 "" $(THR)
        if Command::new("./mkCactusL.R")
            .current_dir(&conf.sync_dir)
            .args(&[&format!("{}.run", host), "400", "", &format!("{}", conf.timeout)])
            .output()
            .is_err()
        {
            message.channel_id.say("Failed to draw a cactus graph.").unwrap();
        }
        let cactus = conf.sync_dir.join(&format!("CactusL-{}.png", host));
        if cactus.exists() {
            if message.channel_id.send_files(&[cactus], |m| m.content("Cactus Plot")).is_err() {
                message.channel_id.say("Failed to upload the file.").unwrap();
            }
        } else {
            message.channel_id.say(&format!("{} doesn't exist.", cactus.to_string_lossy())).unwrap();
        }
    }
});

command!(start(_context, _message) {
    start_benchmark();
});

command!(who(_context, message) {
    if let Ok(conf) = CONFIG.read() {
        message.channel_id.say(&format!("I am {}: ```rust\n{:?}\n```", VERSION, conf))?;
    }
});

command!(help(_context, message) {
    message.channel_id.say("I accept `bye`, `clear`, `draw`, `whatsup`, `who`".to_string())?;
});

command!(whatsup(_context, message) {
    if let Ok(p) = PROCESSED.read() {
        if let Ok(a) = ANSWERED.read() {
            message.channel_id.say(&format!("tried {} problems, answered {}.", p, a))?;
        }
    }
});
