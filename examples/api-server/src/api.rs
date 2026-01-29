use axum::routing::any;
use axum::{
    extract::{Json, Path, Query},
    routing::{get, post},
    Router,
};
use serde::{Deserialize, Serialize};

pub type ApiResponse<T> = Result<Json<T>, &'static str>;

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum UserRole {
    Admin,
    User,
    Guest,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct User {
    pub id: u64,
    pub first_name: String,
    pub last_name: String,
    pub email: String,
    pub role: UserRole,
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

#[derive(Debug, Serialize, Deserialize)]
pub struct PageResult<T> {
    data: Vec<T>,
    page: i32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct SearchQuery {
    pub q: String,
    pub page: Option<i32>,
    pub limit: Option<i32>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProductFilters {
    pub category: Option<String>,
    pub min_price: Option<f64>,
    pub max_price: Option<f64>,
}

// Example of flatten usage
#[derive(Debug, Serialize, Deserialize)]
pub struct PaginationParams {
    pub page: i32,
    pub page_size: i32,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct ProductSearchQuery {
    #[serde(flatten)]
    pub pagination: PaginationParams,
    pub search: Option<String>,
    pub category: Option<String>,
}

pub fn routes() -> Router {
    Router::new()
        .route("/users", get(list_users))
        .route("/users", post(create_user))
        .route(
            "/users/{id}",
            get(get_user).put(update_user).delete(delete_user),
        )
        .route("/users/search", get(search_users))
        .route("/products", get(list_products))
        .route("/products/search", get(search_products))
        .route("/products", post(create_product))
        .route("/products/{id}", get(get_product))
        .route("/health", get(health_check))
        .route(
            "/other-result",
            any(
                async |Json(_req): Json<CreateProductRequest>| -> ApiResponse<PageResult<Product>> {
                    Ok(Json(PageResult {
                        page: 1,
                        data: Vec::new(),
                    }))
                },
            ),
        )
}

async fn list_users() -> Json<Vec<User>> {
    Json(vec![User {
        id: 1,
        first_name: "John".to_string(),
        last_name: "Doe".to_string(),
        email: "john@example.com".to_string(),
        role: UserRole::Admin,
    }])
}

async fn create_user(Json(req): Json<CreateUserRequest>) -> Json<User> {
    Json(User {
        id: 1,
        first_name: req.first_name,
        last_name: req.last_name,
        email: req.email,
        role: UserRole::User,
    })
}

async fn get_user(Path(id): Path<u64>) -> Json<User> {
    Json(User {
        id,
        first_name: "John".to_string(),
        last_name: "Doe".to_string(),
        email: "john@example.com".to_string(),
        role: UserRole::User,
    })
}

async fn update_user(Path(id): Path<u64>, Json(req): Json<UpdateUserRequest>) -> Json<User> {
    Json(User {
        id,
        first_name: req.first_name.unwrap_or_else(|| "John".to_string()),
        last_name: req.last_name.unwrap_or_else(|| "Doe".to_string()),
        email: req.email.unwrap_or_else(|| "john@example.com".to_string()),
        role: UserRole::User,
    })
}

async fn delete_user(Path(_id): Path<u64>) -> Json<()> {
    Json(())
}

async fn search_users(Query(query): Query<SearchQuery>) -> Result<Json<Vec<User>>, String> {
    // Simulate search based on query parameters
    Ok(Json(vec![User {
        id: 1,
        first_name: format!("Search result for: {}", query.q),
        last_name: "Doe".to_string(),
        email: "john@example.com".to_string(),
        role: UserRole::User,
    }]))
}

async fn list_products() -> Json<Vec<Product>> {
    Json(vec![Product {
        id: 1,
        name: "Widget".to_string(),
        price: 19.99,
        description: Some("A useful widget".to_string()),
    }])
}

async fn search_products(Query(params): Query<ProductSearchQuery>) -> Json<Vec<Product>> {
    // Demonstrates flatten - params has page, page_size (from PaginationParams), search, category
    Json(vec![Product {
        id: 1,
        name: format!("Search result for page {}", params.pagination.page),
        price: 19.99,
        description: params.search,
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
