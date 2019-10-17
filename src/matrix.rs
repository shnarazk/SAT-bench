/// Matrix.org IF
use serde_json::Value;
use std::collections::HashMap;

//use std::fs;
//use std::io::{stdout, BufWriter, Write};
//use std::path::PathBuf;
//use std::process::Command;
//use std::str;
//use std::sync::RwLock;
//use std::{env, process, time};
//use structopt::StructOpt;

pub fn get_token(map: &HashMap<&str, &str>) -> Option<String> {
    let url = "https://matrix.org/_matrix/client/r0/login";
    let mut map: HashMap<&str, &str> = map.clone();
    map.insert("type", "m.login.password");
    let client = reqwest::Client::new();
    let res = client.post(url).json(&map).send();
    match res {
        Err(e) => println!("failed to post: {:?}.", e),
        Ok(mut r) => {
            let body: String = r.text().unwrap();
            let v: Value = serde_json::from_str(&body).unwrap();
            return Some(v["access_token"].to_string());
        }
    }
    None
}

pub fn post(maybe_token: &Option<String>, msg: &str) {
    if let Some(ref token) = maybe_token {
        let room = "!mflwVjLifrqjTvoAnJ:matrix.org";
        let url = format!("https://matrix.org/_matrix/client/r0/rooms/{}/send/m.room.message?access_token={}", room, token);
        let mut map: HashMap<&str, &str> = HashMap::new();
        map.insert("msgtype", "m.text");
        map.insert("body", msg);
        let client = reqwest::Client::new();
        let res = client.post(&url).json(&map).send();
        if res.is_err() {
            println!("failed to post.");
        }
    }
}
