/// Matrix.org IF
use {
    matrix_sdk::{
        config::SyncSettings,
        room::{Joined, Room},
        ruma::events::room::message::RoomMessageEventContent,
        Client,
    },
    url::Url,
};

#[derive(Debug, Default)]
pub struct Matrix {
    room: Option<Joined>,
}

impl Matrix {
    pub async fn post<S: AsRef<str>>(&self, message: S) {
        if let Some(room) = &self.room {
            let content = RoomMessageEventContent::text_plain(message.as_ref());
            room.send(content, None).await.unwrap();
        }
    }
}

pub async fn connect_to_matrix(
    mid: &str,
    mpasswd: &str,
    mroom: &str,
) -> Result<Matrix, Box<dyn std::error::Error>> {
    let mut matrix = Matrix::default();
    let server = Url::parse("https://matrix.org").unwrap();
    let client = Client::new(server).await.unwrap();
    client
        .login_username(mid, mpasswd)
        .initial_device_display_name("SAT-bench Matrix bot")
        .send()
        .await?;
    client.sync_once(SyncSettings::default()).await?;
    // let settings = SyncSettings::default().token(client.sync_token().await.unwrap());
    // client.sync(settings).await?;
    let room_id = mroom.try_into().unwrap();
    if let Room::Joined(room) = client.get_room(room_id).unwrap() {
        matrix.room = Some(room);
    }
    Ok(matrix)
}
