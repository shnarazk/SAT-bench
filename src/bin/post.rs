use {
    matrix_sdk::{
        config::SyncSettings, room::Room, ruma::events::room::message::RoomMessageEventContent,
        Client,
    },
    std::env,
    url::Url,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
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
        dbg!(&room);
        let content = RoomMessageEventContent::text_plain("🎉🎊🥳 let's PARTY!! 🥳🎊🎉");
        room.send(content, None).await.unwrap();
    }
    Ok(())
}
