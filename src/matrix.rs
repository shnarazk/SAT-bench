/// Matrix.org IF
use {futures::executor::block_on, std::collections::HashMap, serde::Deserialize};

#[derive(Deserialize)]
struct Token {
    access_token: String
}

pub fn get_token<S: ::std::hash::BuildHasher>(map: &mut HashMap<&str, &str, S>) -> Option<String> {
    let url = "https://matrix.org/_matrix/client/r0/login";
    map.insert("type", "m.login.password");
    let client = reqwest::Client::new();
    let s = async {
        let content = client.post(url).json(&map).send().await.unwrap();
        content.json::<Token>().await.map(|j| j.access_token).ok()
    };
    block_on(s)
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
        let client = reqwest::Client::new();
        let fut = async { client.post(&url).json(&map).send().await };
        block_on(fut).expect("failed to post");
    }
}
