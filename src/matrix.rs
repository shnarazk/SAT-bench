/// Matrix.org IF
// use {futures::executor::block_on, std::collections::HashMap, serde::Deserialize};
use {
    matrix_sdk::{
        config::SyncSettings,
        room::{Joined, Room},
        ruma::events::room::message::RoomMessageEventContent,
        Client,
    },
    std::env,
    url::Url,
};

// #[derive(Deserialize)]
// struct Token {
//     access_token: String
// }

// pub fn get_token<S: ::std::hash::BuildHasher>(map: &mut HashMap<&str, &str, S>) -> Option<String> {
//     let url = "https://matrix.org/_matrix/client/r0/login";
//     map.insert("type", "m.login.password");
//     let client = reqwest::Client::new();
//     let s = async {
//         let content = client.post(url).json(&map).send().await.unwrap();
//         content.json::<Token>().await.map(|j| j.access_token).ok()
//     };
//     block_on(s)
// }

// pub fn post(room: &str, maybe_token: &Option<String>, msg: &str) {
//     if let Some(ref token) = maybe_token {
//         let url = format!(
//             "https://matrix.org/_matrix/client/r0/rooms/{}/send/m.room.message?access_token={}",
//             room, token
//         );
//         let mut map: HashMap<&str, &str> = HashMap::new();
//         map.insert("msgtype", "m.text");
//         map.insert("body", msg);
//         let client = reqwest::Client::new();
//         let fut = async { client.post(&url).json(&map).send().await };
//         block_on(fut).expect("failed to post");
//     }
// }

#[derive(Debug, Default)]
pub struct Matrix {
    room: Option<Joined>,
}

impl Matrix {
    pub async fn post(&mut self, message: &str) {
        if let Some(room) = &self.room {
            // let content = RoomMessageEventContent::text_plain("🎉🎊🥳 let's PARTY!! 🥳🎊🎉");
            let content = RoomMessageEventContent::text_plain(message);
            room.send(content, None).await.unwrap();
        }
    }
}

#[tokio::main]
pub async fn connect_to_matrix() -> Result<Matrix, Box<dyn std::error::Error>> {
    let mut matrix = Matrix::default();
    let mid: String = env::var("MID").expect("Provide MID");
    let mpasswd: String = env::var("MPASSWD").expect("Provide MPASSWD");
    let mroom: String = env::var("MROOM").expect("Provide MROOM");
    dbg!(&mroom);
    let server = Url::parse("https://matrix.org").unwrap();
    let client = Client::new(server).await.unwrap();
    client.login_username(&mid, &mpasswd).send().await?;
    client.sync_once(SyncSettings::default()).await?;
    // let settings = SyncSettings::default().token(client.sync_token().await.unwrap());
    // client.sync(settings).await?;
    let room_id = mroom.as_str().try_into().unwrap();
    dbg!(&room_id);
    if let Room::Joined(room) = client.get_room(room_id).unwrap() {
        // matrix.client = Some(client);
        matrix.room = Some(room);
    }
    // matrix.post().await;
    Ok(matrix)
}
