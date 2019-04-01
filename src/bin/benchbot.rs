/// A simple SAT benchmarker
#[macro_use]
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

lazy_static! {
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
    thread::spawn(move || {
        main_loop();
    });
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

const VERSION: &str = "benchbot 0.0.0";
const STRUCTURED_PROBLEMS: [(&str, &str); 4] = [
    ("SR2015/itox", "SR2015/itox_vc1130.cnf"),
    ("SR2015/m283", "SR2015/manthey_DimacsSorter_28_3.cnf"),
    ("SR2015/38b", "SR2015/38bits_10.dimacs.cnf"),
    ("SR2015/44b", "SR2015/44bits_11.dimacs.cnf"),
];

#[derive(Clone, Debug, StructOpt)]
#[structopt(name = "sat-bench", about = "Run simple SAT benchmarks")]
struct Config {
    /// solver names
    solver: String,
    /// time out in seconds
    #[structopt(long = "timeout", short = "T", default_value = "510")]
    timeout: usize,
    /// arguments passed to solvers
    #[structopt(long = "options", default_value = "")]
    solver_options: String,
    /// additional string following solver name
    #[structopt(long = "aux-key", short = "K", default_value = "")]
    aux_key: String,
    /// data directory
    #[structopt(long = "lib", default_value = "")]
    lib_dir: String,
}

pub fn main_loop() {
    let mut config = Config::from_args();
    config.solver = "splr-013".to_string();
    let base = if config.lib_dir.is_empty() {
        match option_env!("SATBENCHLIB") {
            Some(dir) => dir,
            None => env!("PWD"),
        }
    } else {
        &config.lib_dir
    };
    let host = Command::new("hostname")
        .arg("-s")
        .output()
        .expect("failed to execute process")
        .stdout;
    let h = String::from_utf8_lossy(&host[..host.len() - 1]);
    post(&format!("Start benchmark @ {} now.", h));
    let mut num: usize = 1;
    for (k, s) in &STRUCTURED_PROBLEMS {
        let cnf = format!("{}/{}", base, s);
        execute(&config, &config.solver, num, k, &cnf);
        num += 1;
    }
    state("");
    post("Done. Bye.");
    process::exit(0);
}

fn execute(config: &Config, solver: &str, _num: usize, _name: &str, target: &str) {
    let f = PathBuf::from(target);
    if f.is_file() {
        let target: String = f.file_name().unwrap().to_str().unwrap().to_string();
        println!("\x1B[032mRunning on {}...\x1B[000m", target);
        state(&target);
        let mut command: Command = solver_command(solver);
        for opt in config.solver_options.split_whitespace() {
            command.arg(&opt[opt.starts_with('\\') as usize..]);
        }
        if command.arg(f.as_os_str()).output().is_err() {
            post(&format!("Something happened to {}.", &target));
        }
    }
}

fn solver_command(solver: &str) -> Command {
    lazy_static! {
        static ref GLUCOSE: Regex = Regex::new(r"\bglucose").expect("wrong regex");
        // static ref lingeling: Regex = Regex::new(r"\blingeling").expect("wrong regex");
        // static ref minisat: Regex = Regex::new(r"\bminisat").expect("wrong regex");
        // static ref mios: Regex = Regex::new(r"\bmios").expect("wrong regex");
        static ref SPLR: Regex = Regex::new(r"\bsplr").expect("wrong regex");
    }
    if SPLR.is_match(solver) {
        let mut command = Command::new(&solver);
        command.args(&["-r", "-", "--to", "5"]);
        command
    } else if GLUCOSE.is_match(solver) {
        let mut command = Command::new(&solver);
        command.args(&["-verb=0", "-cpu-lim=2500"]);
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
