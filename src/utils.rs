use chrono::offset::TimeZone;
use chrono::{DateTime, Local};
use regex::Regex;
use std::fs::File;
use std::io::*;
use std::path::PathBuf;
use std::time::{SystemTime, UNIX_EPOCH};

pub fn parse_result(fname: PathBuf) -> Option<(f64, bool, String)> {
    let f;
    match File::open(fname) {
        Ok(fin) => f = fin,
        Err(_) => return None,
    }
    let mut input = BufReader::new(f);
    let sat = Regex::new(r"\bSATISFIABLE\b").expect("wrong regex");
    let unsat = Regex::new(r"\bUNSATISFIABLE\b").expect("wrong regex");
    let splr = Regex::new(r"^c +Strategy\|mode: +([^,]+), time: +([.0-9]+)").expect("wrong regex");
    let glucose = Regex::new(r"^c CPU time +: ([.0-9]+)").expect("wrong regex");
    let mut buf = String::new();
    let mut time: Option<f64> = None;
    let mut found: Option<bool> = None;
    let mut strategy: String = "".to_string();
    while let Ok(k) = input.read_line(&mut buf) {
        if k == 0 {
            break;
        }
        if sat.is_match(&buf) {
            assert_eq!(found, None);
            found = Some(true);
        } else if unsat.is_match(&buf) {
            assert_eq!(found, None);
            found = Some(false);
        } else if let Some(c) = splr.captures(&buf) {
            strategy = c[1].to_string();
            if let Ok(v) = c[2].parse() {
                time = Some(v);
            }
        } else if let Some(c) = glucose.captures(&buf) {
            if let Ok(v) = c[1].parse() {
                time = Some(v);
            }
        }
        buf.clear();
    }
    match (time, found) {
        (Some(t), Some(f)) => Some((t, f, strategy)),
        _ => None,
    }
}

// See https://users.rust-lang.org/t/convert-std-time-systemtime-to-chrono-datetime-datetime
pub fn system_time_to_date_time(t: SystemTime) -> DateTime<Local> {
    let (sec, nsec) = match t.duration_since(UNIX_EPOCH) {
        Ok(dur) => (dur.as_secs() as i64, dur.subsec_nanos()),
        Err(e) => {
            // unlikely but should be handled
            let dur = e.duration();
            let (sec, nsec) = (dur.as_secs() as i64, dur.subsec_nanos());
            if nsec == 0 {
                (-sec, 0)
            } else {
                (-sec - 1, 1_000_000_000 - nsec)
            }
        }
    };
    Local.timestamp(sec, nsec)
}

pub fn current_date_time() -> DateTime<Local> {
    system_time_to_date_time(SystemTime::now())
}
