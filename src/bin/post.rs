use {matrix_sdk::Client, std::env, url::Url};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let server_url = Url::parse("https://matrix.org").unwrap();
    let client = Client::new(server_url).await.unwrap();
    let mpasswd: String = env::var("MPASSWD").expect("Provide mpasswd");
    let mid: String = env::var("MID").expect("Provide mid");
    client.login_username(&mid, &mpasswd).send().await?;
    Ok(())
}
