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

//    map.insert("user", "nash8192");
//    map.insert("password", "wino-294-mAt.");

pub fn get_token(map: &mut HashMap<String, String>) -> Option<String> {
    let url = "https://matrix.org/_matrix/client/r0/login";
    // let mut map = HashMap::new();
    map.insert("type".to_string(), "m.login.password".to_string());
    let client = reqwest::Client::new();
    let mut res = client.post(url).json(map).send();
    match res {
        Err(e) => println!("failed to post: {:?}.", e),
        Ok(mut r) => {
            let body: String = r.text().unwrap();
            println!("got {}.", body);
            let v: Value = serde_json::from_str(&body).unwrap();
            println!("got {} => {}.", body, v["access_token"]);
            return Some(v["access_token"].to_string());
        }
    }
    None
}

pub fn post(token: &str, msg: &str) {
    let room = "!mflwVjLifrqjTvoAnJ:matrix.org";
    let url = format!("https://matrix.org/_matrix/client/r0/rooms/{}/send/m.room.message?access_token={}", room, token);
    let mut map = HashMap::new();
    map.insert("msgtype", "m.text");
    map.insert("body", msg);
    let client = reqwest::Client::new();
    let res = client.post(&url).json(&map).send();
    if res.is_err() {
        println!("failed to post.");
    }
}
