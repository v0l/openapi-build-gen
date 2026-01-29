use axum::{
    extract::{Json, Path},
    routing::{delete, get, post, put},
    Router,
};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct User {
    pub id: u64,
    pub first_name: String,
    pub last_name: String,
    pub email: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct CreateUserRequest {
    pub first_name: String,
    pub last_name: String,
    pub email: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UpdateUserRequest {
    pub first_name: Option<String>,
    pub last_name: Option<String>,
    pub email: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Product {
    pub id: u64,
    pub name: String,
    pub price: f64,
    pub description: Option<String>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct CreateProductRequest {
    pub name: String,
    pub price: f64,
    pub description: Option<String>,
}

pub fn routes() -> Router {
    Router::new()
        .route("/users", get(list_users))
        .route("/users", post(create_user))
        .route("/users/{id}", get(get_user))
        .route("/users/{id}", put(update_user))
        .route("/users/{id}", delete(delete_user))
        .route("/products", get(list_products))
        .route("/products", post(create_product))
        .route("/products/{id}", get(get_product))
        .route("/health", get(health_check))
}

async fn list_users() -> Json<Vec<User>> {
    Json(vec![User {
        id: 1,
        first_name: "John".to_string(),
        last_name: "Doe".to_string(),
        email: "john@example.com".to_string(),
    }])
}

async fn create_user(Json(req): Json<CreateUserRequest>) -> Json<User> {
    Json(User {
        id: 1,
        first_name: req.first_name,
        last_name: req.last_name,
        email: req.email,
    })
}

async fn get_user(Path(id): Path<u64>) -> Json<User> {
    Json(User {
        id,
        first_name: "John".to_string(),
        last_name: "Doe".to_string(),
        email: "john@example.com".to_string(),
    })
}

async fn update_user(Path(id): Path<u64>, Json(req): Json<UpdateUserRequest>) -> Json<User> {
    Json(User {
        id,
        first_name: req.first_name.unwrap_or_else(|| "John".to_string()),
        last_name: req.last_name.unwrap_or_else(|| "Doe".to_string()),
        email: req.email.unwrap_or_else(|| "john@example.com".to_string()),
    })
}

async fn delete_user(Path(_id): Path<u64>) -> Json<()> {
    Json(())
}

async fn list_products() -> Json<Vec<Product>> {
    Json(vec![Product {
        id: 1,
        name: "Widget".to_string(),
        price: 19.99,
        description: Some("A useful widget".to_string()),
    }])
}

async fn create_product(Json(req): Json<CreateProductRequest>) -> Json<Product> {
    Json(Product {
        id: 1,
        name: req.name,
        price: req.price,
        description: req.description,
    })
}

async fn get_product(Path(id): Path<u64>) -> Json<Product> {
    Json(Product {
        id,
        name: "Widget".to_string(),
        price: 19.99,
        description: Some("A useful widget".to_string()),
    })
}

async fn health_check() -> &'static str {
    "OK"
}
