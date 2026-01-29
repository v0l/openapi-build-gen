use axum::http::header::CONTENT_TYPE;
use axum::{routing::get, Router};
use std::net::SocketAddr;

mod api;

// Include the generated OpenAPI module
mod openapi {
    include!(concat!(env!("OUT_DIR"), "/openapi.rs"));
}

#[tokio::main]
async fn main() {
    let app = create_app();

    let addr: SocketAddr = "127.0.0.1:8080".parse().unwrap();
    let listener = tokio::net::TcpListener::bind(addr).await.unwrap();

    println!("Server running on http://{}", addr);
    println!("OpenAPI spec available at http://{}/openapi.json", addr);

    axum::serve(listener, app).await.unwrap();
}

fn create_app() -> Router {
    Router::new().merge(api::routes()).route(
        "/openapi.json",
        get(|| async { ([(CONTENT_TYPE, "application/json")], openapi::OPENAPI_JSON) }),
    )
}
