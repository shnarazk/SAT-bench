/// Matrix.org IF
use serde_json::Value;
use std::collections::HashMap;

pub fn get_token<S: ::std::hash::BuildHasher>(map: &mut HashMap<&str, &str, S>) -> Option<String> {
    let url = "https://matrix.org/_matrix/client/r0/login";
    map.insert("type", "m.login.password");
    let client = reqwest::blocking::Client::new();
    let res = client.post(url).json(&map).send();
    match res {
        Err(e) => println!("failed to post: {:?}.", e),
        Ok(r) => {
            let body: String = r.text().unwrap();
            let v: Value = serde_json::from_str(&body).unwrap();
            let token = v["access_token"].to_string();
            assert!(!token.is_empty());
            let s = &token[1..token.len() - 1];
            return Some(s.to_string());
        }
    }
    None
}

pub fn post(room: &str, maybe_token: &Option<String>, msg: &str) {
    if let Some(ref token) = maybe_token {
        let url = format!(
            "https://matrix.org/_matrix/client/r0/rooms/{}/send/m.room.message?access_token={}",
            room, token
        );
        let mut map: HashMap<&str, &str> = HashMap::new();
        map.insert("msgtype", "m.text");
        map.insert("body", msg);
        let client = reqwest::blocking::Client::new();
        let res = client.post(&url).json(&map).send();
        if res.is_err() {
            println!("failed to post.");
        }
    }
}
