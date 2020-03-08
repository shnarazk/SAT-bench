use {
    chrono::{offset::TimeZone, DateTime, Local},
    regex::Regex,
    std::{
        fs::{File, OpenOptions},
        io::*,
        path::{Path, PathBuf},
        time::{SystemTime, UNIX_EPOCH},
    },
};

pub fn parse_result(fname: PathBuf) -> Option<(f64, Option<bool>, String)> {
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
    time.map(|t| (t, found, strategy))
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

pub fn make_verifier<P: AsRef<Path>>(
    problems: &[(usize, &str)],
    sync_dir: P,
    repo_dir: P,
) -> std::io::Result<()> {
    let outname = sync_dir.as_ref().join("verify.sh");
    let mut outfile = OpenOptions::new()
        .write(true)
        .create(true)
        .open(&outname)
        .expect("fail to create velify.sh");
    let path = repo_dir.as_ref();
    let mut sats = Vec::new();
    let mut unsats = Vec::new();

    for (n, key) in problems.iter() {
        let fname = sync_dir.as_ref().join(PathBuf::from(format!(".ans_{}", key)));
        if fname.exists() {
            if let Some((_, s, _)) = parse_result(fname) {
                match s {
                    Some(true) => sats.push((n, key)),
                    Some(false) => unsats.push((n, key)),
                    _ => (),
                }
            }
        }
    }
    writeln!(outfile, "# SAT")?;
    for (n, (i, p)) in sats.iter().enumerate() {
        let key = path.join(p);
        let f = key.to_string_lossy();
        writeln!(outfile, "# - SAT:{}, {:>3}, {}", n + 1, i, p)?;
        writeln!(outfile, "    dmcr {}", f)?;
    }
    writeln!(outfile, "# UNSAT")?;
    for (n, (i, p)) in unsats.iter().enumerate() {
        let key = path.join(p);
        let f = key.to_string_lossy();
        writeln!(outfile, "# - UNSAT:{}, {:>3}, {}", n, i, p)?;
        writeln!(outfile, "    echo '# UNSAT:{}, {:>3}, {}'", n, i, p)?;
        writeln!(outfile, "    splr -c {} > /dev/null", f)?;
        writeln!(outfile, "    egrep -v '^[cs]' < proof.out > {}.drat", p)?;
        writeln!(
            outfile,
            "    gratgen {} {}.drat -o {}.grad -j 4 > /dev/null",
            f, p, p
        )?;
        writeln!(outfile, "    gratchk unsat {} {}.grad", f, p)?;
    }
    Ok(())
}
