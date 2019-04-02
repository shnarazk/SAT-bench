/// A simple SAT benchmarker
use lazy_static::lazy_static;
use regex::Regex;
use serenity::builder::GetMessages;
use serenity::client::{Client, Context, EventHandler};
use serenity::command;
use serenity::framework::standard::StandardFramework;
use serenity::model::channel::Channel;
use serenity::model::event::TypingStartEvent;
use serenity::model::gateway::Game;
use serenity::model::id::ChannelId;
use std::path::PathBuf;
use std::process::Command;
use structopt::StructOpt;
// use serenity::model::prelude::Message;
use serenity::model::user::OnlineStatus;
use std::sync::RwLock;
use std::{env, process, thread};
use sat_bench::{bench18::SCB, utils::parse_result};
use sat_bench::utils::current_date_time;
use std::collections::HashMap;
use std::fs::{create_dir, read_dir};
use std::str;

lazy_static! {
    pub static ref PQ: RwLock<Vec<String>> = RwLock::new(Vec::new());
    pub static ref PROCESSED: RwLock<usize> = RwLock::new(0);
    pub static ref ANSWERED: RwLock<usize> = RwLock::new(0);
    pub static ref M: RwLock<String> = RwLock::new(String::new());
    pub static ref N: RwLock<usize> = RwLock::new(0);
    pub static ref CHID: RwLock<u64> = RwLock::new(0);
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

fn main() {
    let mut config = Config::from_args();
    config.solver = "splr-013".to_string();
    let base: String = if config.lib_dir.is_empty() {
        match option_env!("SATBENCHLIB") {
            Some(dir) => dir.to_string(),
            None => env!("PWD").to_string(),
        }
    } else {
        config.lib_dir.to_string()
    };
    let host = {
        let h = Command::new("hostname")
            .arg("-s")
            .output()
            .expect("failed to execute process")
            .stdout;
        String::from_utf8_lossy(&h[..h.len() - 1]).to_string()
    };
    let target_dir = {
        let home = env::var("HOME").expect("No home");
        let commit_id_u8 = Command::new("git")
            .current_dir(format!("{}/Repositories/splr", home))
            .args(&["log", "-1", "--format=format:%h"])
            .output()
            .expect("fail to git")
            .stdout;
        let commit_id = unsafe { String::from_utf8_unchecked(commit_id_u8) };
        let timestamp = current_date_time()
            .format("%F-%m-%dT%H:%M:%S")
            .to_string();
        PathBuf::from(format!("{}-{}-{}", config.solver, commit_id, timestamp))
    };
    if target_dir.exists() {
        panic!(format!("{} exists.", target_dir.to_string_lossy()));
    } else {
        create_dir(&target_dir).expect("fail to mkdir");
    }
    if let Ok(mut cid) = CHID.write() {
        *cid = env::var("DISCORD_CHANNEL")
            .expect("no channel")
            .parse::<u64>()
            .unwrap();
    }
    let mut client = Client::new(&env::var("DISCORD_TOKEN").expect("token"), Handler)
        .expect("Error creating client");
    client.with_framework(
        StandardFramework::new()
            .configure(|c| c.prefix("."))
            .cmd("clear", clean),
    );
    if let Ok(mut queue) = PQ.write() {
        for s in SCB.iter().rev() {
            queue.push(s.to_string());
        }
    }
    post(&format!("{}: Start benchmark @ {} now.", VERSION, host));
    for i in 0..config.jobs {
        let c = config.clone();
        let b = base.to_string();
        let t = target_dir.clone();
        post(&format!("start worker {}", i));
        thread::spawn(move || {
            worker(c, b, t);
        });
    }
    if let Err(why) = client.start() {
        println!("An error occurred while running the client: {:?}", why);
    }
}

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

const VERSION: &str = "benchbot 0.0.1";

#[derive(Clone, Debug, StructOpt)]
#[structopt(name = "sat-bench", about = "Run simple SAT benchmarks")]
struct Config {
    /// solver names
    solver: String,
    /// time out in seconds
    #[structopt(long = "timeout", short = "T", default_value = "100")]
    timeout: usize,
    /// number of workers
    #[structopt(long = "jobs", short = "j", default_value = "3")]    
    jobs: usize,
    /// arguments passed to solvers
    #[structopt(long = "options", default_value = "")]
    solver_options: String,
    /// data directory
    #[structopt(long = "lib", default_value = "/home/narazaki/Documents/SAT-RACE/SC18main")]
    lib_dir: String,
}

fn worker(config: Config, base: String, target_dir: PathBuf) {
    loop {
        let mut p: String;
        let num: usize;
        if let Ok(mut q) = PQ.write() {
            if let Some(top) = q.pop() {
                p = format!("{}/{}", base, top);
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
        execute(&config, num, &target_dir, &p);
        if num == 0 {
            state("");
            post("Done. Bye.");
            process::exit(0);
        } else if (400 - num) % 20 == 0 {
            let (s,u) = report(&config, &target_dir).unwrap_or((0, 0));
            if let Ok(mut answered) = ANSWERED.write() {
                if *answered < s + u {
                    *answered = s + u;
                    post(&format!("The {}-th job is done, answered {} {}.", 400 - num, s, u));
                }
            }
        }
    }
}

fn execute(config: &Config, _num: usize, target_dir: &PathBuf, cnf: &str) {
    let f = PathBuf::from(cnf);
    if f.is_file() {
        let target: String = f.file_name().unwrap().to_str().unwrap().to_string();
        println!("\x1B[032mRunning on {}...\x1B[000m", target);
        if let Ok(processed) = PROCESSED.read() {
            state(&format!("#{}, {}", *processed, &target));
        }
        let mut command: Command = solver_command(&config.solver, config, target_dir);
        for opt in config.solver_options.split_whitespace() {
            command.arg(&opt[opt.starts_with('\\') as usize..]);
        }
        if command.arg(f.as_os_str()).output().is_err() {
            post(&format!("Something happened to {}.", &target));
        }
    }
}

fn solver_command(solver: &str, config: &Config, dir: &PathBuf) -> Command {
    lazy_static! {
        static ref GLUCOSE: Regex = Regex::new(r"\bglucose").expect("wrong regex");
        // static ref lingeling: Regex = Regex::new(r"\blingeling").expect("wrong regex");
        // static ref minisat: Regex = Regex::new(r"\bminisat").expect("wrong regex");
        // static ref mios: Regex = Regex::new(r"\bmios").expect("wrong regex");
        static ref SPLR: Regex = Regex::new(r"\bsplr").expect("wrong regex");
    }
    if SPLR.is_match(solver) {
        let mut command = Command::new(&solver);
        command.args(&["--to", &format!("{}", config.timeout), "-o", &dir.to_string_lossy()]);
        command
    } else if GLUCOSE.is_match(solver) {
        let mut command = Command::new(&solver);
        command.args(&["-verb=0", &format!("-cpu-lim={}", config.timeout)]);
        command
    } else {
        Command::new(&solver)
    }
}

fn post(mes: &str) {
    if let Ok(cid) = CHID.read() {
        if let Ok(channel) = ChannelId(*cid).to_channel() {
            if let Channel::Guild(gchannel) = &channel {
                let ch = gchannel.read();
                ch.say(&format!("{}", mes)).expect("fail to say");
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

fn report(config: &Config, target_dir: &PathBuf) -> std::io::Result<(usize, usize)> {
    let mut hash: HashMap<&str, (f64, bool, String)> = HashMap::new();
    let timeout = config.timeout as f64;
    let mut nsat = 0;
    let mut nunsat = 0;
    for e in read_dir(target_dir)? {
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
    println!(
        "# SAT: {}, UNSAT: {}, total: {} so far",
        nsat,
        nunsat,
        nsat + nunsat
    );
    println!("solver, num, target, time, satisfiability, strategy");
    for (i, key) in SCB.iter().enumerate() {
        if let Some(v) = hash.get(key) {
            println!(
                "\"{}\",{},\"{}{}\",{:>8.2},{},{}",
                target_dir.to_string_lossy(),
                i + 1,
                "SC18main/",
                key,
                v.0,
                v.1,
                v.2,
            );
        } else {
            println!(
                "\"{}\",{},\"{}{}\",{:>5},{},",
                target_dir.to_string_lossy(),
                i + 1,
                "SC18main/",
                key,
                config.timeout,
                "",
            );
        }
    }
    Ok((nsat, nunsat))
}
