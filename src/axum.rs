use crate::parser::{HttpMethod, PathParam, Route, TypeInfo};
use syn::{Expr, ExprMethodCall, FnArg, ItemFn, Stmt};

/// Extract routes from an Axum router function
pub fn extract_routes_from_function(func: &ItemFn) -> Option<Vec<Route>> {
    let mut routes = Vec::new();

    // Look through the function body for Router::new() and .route() calls
    for stmt in &func.block.stmts {
        if let Stmt::Local(local) = stmt {
            if let Some(init) = &local.init {
                let routes_from_expr = extract_routes_from_expr(&init.expr);
                routes.extend(routes_from_expr);
            }
        } else if let Stmt::Expr(expr, _) = stmt {
            let routes_from_expr = extract_routes_from_expr(expr);
            routes.extend(routes_from_expr);
        }
    }

    if routes.is_empty() {
        None
    } else {
        Some(routes)
    }
}

/// Recursively extract routes from an expression
fn extract_routes_from_expr(expr: &Expr) -> Vec<Route> {
    extract_routes_from_expr_with_prefix(expr, "")
}

/// Recursively extract routes from an expression with a path prefix
fn extract_routes_from_expr_with_prefix(expr: &Expr, prefix: &str) -> Vec<Route> {
    let mut routes = Vec::new();

    match expr {
        Expr::MethodCall(method_call) => {
            // Check if this is a .route() call
            if method_call.method == "route" {
                if let Some(mut route) = parse_route_call(method_call) {
                    // Prepend the prefix to the route path
                    if !prefix.is_empty() {
                        route.path = format!("{}{}", prefix, route.path);
                        // Re-extract path params since the path changed
                        route.path_params = extract_path_params(&route.path);
                    }
                    routes.push(route);
                }
                // Recursively check the receiver with the same prefix
                routes.extend(extract_routes_from_expr_with_prefix(
                    &method_call.receiver,
                    prefix,
                ));
            } else if method_call.method == "nest" {
                // Check if this is a .nest() call
                if let Some((_nest_path, nested_routes)) = parse_nest_call(method_call, prefix) {
                    routes.extend(nested_routes);
                    // Recursively check the receiver with the same prefix
                    routes.extend(extract_routes_from_expr_with_prefix(
                        &method_call.receiver,
                        prefix,
                    ));
                }
            } else {
                // For other method calls, recursively check the receiver
                routes.extend(extract_routes_from_expr_with_prefix(
                    &method_call.receiver,
                    prefix,
                ));
            }
        }
        Expr::Call(call) => {
            // Check arguments for nested expressions
            for arg in &call.args {
                routes.extend(extract_routes_from_expr_with_prefix(arg, prefix));
            }
        }
        _ => {}
    }

    routes
}

/// Parse a single .route() method call
fn parse_route_call(method_call: &ExprMethodCall) -> Option<Route> {
    if method_call.args.len() < 2 {
        return None;
    }

    // First argument is the path
    let path = match &method_call.args[0] {
        Expr::Lit(lit) => {
            if let syn::Lit::Str(s) = &lit.lit {
                s.value()
            } else {
                return None;
            }
        }
        _ => return None,
    };

    // Second argument is the method routing (e.g., get(handler))
    let (method, handler) = parse_method_routing(&method_call.args[1])?;

    // Extract path parameters from the path
    let path_params = extract_path_params(&path);

    Some(Route {
        path,
        method,
        handler,
        request_body: None,
        response_body: None,
        path_params,
        query_params: Vec::new(),
    })
}

/// Parse a single .nest() method call
fn parse_nest_call(
    method_call: &ExprMethodCall,
    current_prefix: &str,
) -> Option<(String, Vec<Route>)> {
    if method_call.args.is_empty() {
        return None;
    }

    // First argument is the path prefix
    let nest_path = match &method_call.args[0] {
        Expr::Lit(lit) => {
            if let syn::Lit::Str(s) = &lit.lit {
                s.value()
            } else {
                return None;
            }
        }
        _ => return None,
    };

    // Combine current prefix with nest path
    let full_prefix = if current_prefix.is_empty() {
        nest_path.clone()
    } else {
        format!("{}{}", current_prefix, nest_path)
    };

    // Second argument is the nested router (if present)
    let nested_routes = if method_call.args.len() >= 2 {
        extract_routes_from_expr_with_prefix(&method_call.args[1], &full_prefix)
    } else {
        Vec::new()
    };

    Some((full_prefix, nested_routes))
}

/// Parse method routing like get(handler), post(handler), etc.
fn parse_method_routing(expr: &Expr) -> Option<(HttpMethod, String)> {
    if let Expr::Call(call) = expr {
        if let Expr::Path(path) = &*call.func {
            let method_name = path.path.segments.last()?.ident.to_string();
            let method = match method_name.as_str() {
                "get" => HttpMethod::Get,
                "post" => HttpMethod::Post,
                "put" => HttpMethod::Put,
                "delete" => HttpMethod::Delete,
                "patch" => HttpMethod::Patch,
                "head" => HttpMethod::Head,
                "options" => HttpMethod::Options,
                _ => return None,
            };

            // Extract handler name
            let handler = if let Some(handler_expr) = call.args.first() {
                match handler_expr {
                    Expr::Path(p) => p.path.segments.last()?.ident.to_string(),
                    _ => "unknown".to_string(),
                }
            } else {
                "unknown".to_string()
            };

            return Some((method, handler));
        }
    }
    None
}

/// Extract path parameters from a path like "/users/:id" or "/users/{id}"
fn extract_path_params(path: &str) -> Vec<PathParam> {
    let mut params = Vec::new();

    // Split by / and look for parameters
    for segment in path.split('/') {
        if segment.starts_with(':') {
            params.push(PathParam {
                name: segment[1..].to_string(),
                type_name: "String".to_string(),
            });
        } else if segment.starts_with('{') && segment.ends_with('}') {
            params.push(PathParam {
                name: segment[1..segment.len() - 1].to_string(),
                type_name: "String".to_string(),
            });
        }
    }

    params
}

/// Parse a handler function to extract request/response types
pub fn parse_handler_function(func: &ItemFn) -> (Option<TypeInfo>, Option<TypeInfo>) {
    let mut request_type = None;
    let mut response_type = None;

    // Extract from function parameters
    for input in &func.sig.inputs {
        if let FnArg::Typed(pat_type) = input {
            // Check for Json<T> extractor
            if let syn::Type::Path(type_path) = &*pat_type.ty {
                if let Some(segment) = type_path.path.segments.last() {
                    if segment.ident == "Json" {
                        if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                            if let Some(syn::GenericArgument::Type(syn::Type::Path(inner_type))) =
                                args.args.first()
                            {
                                if let Some(type_name) = inner_type.path.segments.last() {
                                    request_type = Some(TypeInfo {
                                        name: type_name.ident.to_string(),
                                        fields: Vec::new(),
                                        is_array: false,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Extract from return type
    if let syn::ReturnType::Type(_, return_type) = &func.sig.output {
        // Look for Result<Json<T>, _> or Json<T>
        response_type = extract_response_type(return_type);
    }

    (request_type, response_type)
}

fn extract_response_type(ty: &syn::Type) -> Option<TypeInfo> {
    match ty {
        syn::Type::Path(type_path) => {
            if let Some(segment) = type_path.path.segments.last() {
                // Handle Result<T, E>
                if segment.ident == "Result" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(ok_type)) = args.args.first() {
                            return extract_response_type(ok_type);
                        }
                    }
                }

                // Handle Json<T>
                if segment.ident == "Json" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(syn::Type::Path(inner_type))) =
                            args.args.first()
                        {
                            if let Some(type_name) = inner_type.path.segments.last() {
                                return Some(TypeInfo {
                                    name: type_name.ident.to_string(),
                                    fields: Vec::new(),
                                    is_array: false,
                                });
                            }
                        }
                    }
                }
            }
        }
        _ => {}
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_path_params_colon_syntax() {
        let path = "/users/:id/posts/:post_id";
        let params = extract_path_params(path);

        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "id");
        assert_eq!(params[0].type_name, "String");
        assert_eq!(params[1].name, "post_id");
        assert_eq!(params[1].type_name, "String");
    }

    #[test]
    fn test_extract_path_params_brace_syntax() {
        let path = "/users/{id}/posts/{post_id}";
        let params = extract_path_params(path);

        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "id");
        assert_eq!(params[1].name, "post_id");
    }

    #[test]
    fn test_extract_path_params_no_params() {
        let path = "/users/list";
        let params = extract_path_params(path);

        assert_eq!(params.len(), 0);
    }

    #[test]
    fn test_extract_path_params_mixed_syntax() {
        let path = "/users/:id/posts/{post_id}";
        let params = extract_path_params(path);

        assert_eq!(params.len(), 2);
        assert_eq!(params[0].name, "id");
        assert_eq!(params[1].name, "post_id");
    }

    #[test]
    fn test_parse_handler_function_with_json_request() {
        let code = r#"
            async fn create_user(Json(user): Json<CreateUserRequest>) -> Json<User> {
                Json(User { id: 1 })
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let (request, response) = parse_handler_function(&parsed);

        assert!(request.is_some());
        assert_eq!(request.unwrap().name, "CreateUserRequest");

        assert!(response.is_some());
        assert_eq!(response.unwrap().name, "User");
    }

    #[test]
    fn test_parse_handler_function_with_result() {
        let code = r#"
            async fn get_user() -> Result<Json<User>, StatusCode> {
                Ok(Json(User { id: 1 }))
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let (request, response) = parse_handler_function(&parsed);

        assert!(request.is_none());
        assert!(response.is_some());
        assert_eq!(response.unwrap().name, "User");
    }

    #[test]
    fn test_parse_handler_function_no_json() {
        let code = r#"
            async fn health_check() -> &'static str {
                "OK"
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let (request, response) = parse_handler_function(&parsed);

        assert!(request.is_none());
        assert!(response.is_none());
    }

    #[test]
    fn test_extract_routes_from_function() {
        let code = r#"
            fn routes() -> Router {
                Router::new()
                    .route("/users", get(list_users))
                    .route("/users/:id", get(get_user))
                    .route("/users", post(create_user))
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let routes = extract_routes_from_function(&parsed);

        assert!(routes.is_some());
        let routes = routes.unwrap();
        assert_eq!(routes.len(), 3);

        // Routes are extracted in reverse order (bottom-up in AST)
        assert_eq!(routes[0].path, "/users");
        assert_eq!(routes[0].method, HttpMethod::Post);
        assert_eq!(routes[0].handler, "create_user");

        assert_eq!(routes[1].path, "/users/:id");
        assert_eq!(routes[1].method, HttpMethod::Get);
        assert_eq!(routes[1].handler, "get_user");
        assert_eq!(routes[1].path_params.len(), 1);
        assert_eq!(routes[1].path_params[0].name, "id");

        assert_eq!(routes[2].path, "/users");
        assert_eq!(routes[2].method, HttpMethod::Get);
        assert_eq!(routes[2].handler, "list_users");
    }

    #[test]
    fn test_parse_method_routing_all_methods() {
        let test_cases = vec![
            ("get(handler)", HttpMethod::Get),
            ("post(handler)", HttpMethod::Post),
            ("put(handler)", HttpMethod::Put),
            ("delete(handler)", HttpMethod::Delete),
            ("patch(handler)", HttpMethod::Patch),
            ("head(handler)", HttpMethod::Head),
            ("options(handler)", HttpMethod::Options),
        ];

        for (code, expected_method) in test_cases {
            let expr: Expr = syn::parse_str(code).unwrap();
            let result = parse_method_routing(&expr);

            assert!(result.is_some());
            let (method, handler) = result.unwrap();
            assert_eq!(method, expected_method);
            assert_eq!(handler, "handler");
        }
    }

    #[test]
    fn test_extract_routes_with_nest() {
        let code = r#"
            fn routes() -> Router {
                Router::new()
                    .route("/health", get(health))
                    .nest("/api", Router::new()
                        .route("/users", get(list_users))
                        .route("/users/:id", get(get_user))
                    )
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let routes = extract_routes_from_function(&parsed);

        assert!(routes.is_some());
        let routes = routes.unwrap();
        assert_eq!(routes.len(), 3);

        // Check that nested routes have the correct prefix
        let api_routes: Vec<_> = routes
            .iter()
            .filter(|r| r.path.starts_with("/api"))
            .collect();
        assert_eq!(api_routes.len(), 2);

        // Find the specific routes
        let users_route = routes.iter().find(|r| r.path == "/api/users").unwrap();
        assert_eq!(users_route.method, HttpMethod::Get);
        assert_eq!(users_route.handler, "list_users");

        let user_by_id = routes.iter().find(|r| r.path == "/api/users/:id").unwrap();
        assert_eq!(user_by_id.method, HttpMethod::Get);
        assert_eq!(user_by_id.handler, "get_user");
        assert_eq!(user_by_id.path_params.len(), 1);
        assert_eq!(user_by_id.path_params[0].name, "id");

        // Check the non-nested route
        let health_route = routes.iter().find(|r| r.path == "/health").unwrap();
        assert_eq!(health_route.method, HttpMethod::Get);
        assert_eq!(health_route.handler, "health");
    }

    #[test]
    fn test_extract_routes_with_nested_nest() {
        let code = r#"
            fn routes() -> Router {
                Router::new()
                    .nest("/api", Router::new()
                        .nest("/v1", Router::new()
                            .route("/users", get(list_users))
                            .route("/posts", get(list_posts))
                        )
                        .route("/status", get(status))
                    )
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let routes = extract_routes_from_function(&parsed);

        assert!(routes.is_some());
        let routes = routes.unwrap();
        assert_eq!(routes.len(), 3);

        // Check deeply nested routes
        let users_route = routes.iter().find(|r| r.path == "/api/v1/users").unwrap();
        assert_eq!(users_route.method, HttpMethod::Get);
        assert_eq!(users_route.handler, "list_users");

        let posts_route = routes.iter().find(|r| r.path == "/api/v1/posts").unwrap();
        assert_eq!(posts_route.method, HttpMethod::Get);
        assert_eq!(posts_route.handler, "list_posts");

        // Check single-level nested route
        let status_route = routes.iter().find(|r| r.path == "/api/status").unwrap();
        assert_eq!(status_route.method, HttpMethod::Get);
        assert_eq!(status_route.handler, "status");
    }

    #[test]
    fn test_extract_routes_with_nest_and_path_params() {
        let code = r#"
            fn routes() -> Router {
                Router::new()
                    .nest("/users/:user_id", Router::new()
                        .route("/posts", get(user_posts))
                        .route("/posts/:post_id", get(user_post))
                    )
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let routes = extract_routes_from_function(&parsed);

        assert!(routes.is_some());
        let routes = routes.unwrap();
        assert_eq!(routes.len(), 2);

        // Check that path params are correctly extracted from nested paths
        let posts_route = routes
            .iter()
            .find(|r| r.path == "/users/:user_id/posts")
            .unwrap();
        assert_eq!(posts_route.path_params.len(), 1);
        assert_eq!(posts_route.path_params[0].name, "user_id");

        let post_route = routes
            .iter()
            .find(|r| r.path == "/users/:user_id/posts/:post_id")
            .unwrap();
        assert_eq!(post_route.path_params.len(), 2);
        assert_eq!(post_route.path_params[0].name, "user_id");
        assert_eq!(post_route.path_params[1].name, "post_id");
    }

    #[test]
    fn test_extract_routes_with_multiple_nests() {
        let code = r#"
            fn routes() -> Router {
                Router::new()
                    .nest("/api", Router::new()
                        .route("/users", get(list_users))
                    )
                    .nest("/admin", Router::new()
                        .route("/users", get(admin_users))
                    )
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let routes = extract_routes_from_function(&parsed);

        assert!(routes.is_some());
        let routes = routes.unwrap();
        assert_eq!(routes.len(), 2);

        // Check that both nests are correctly handled
        let api_users = routes.iter().find(|r| r.path == "/api/users").unwrap();
        assert_eq!(api_users.handler, "list_users");

        let admin_users = routes.iter().find(|r| r.path == "/admin/users").unwrap();
        assert_eq!(admin_users.handler, "admin_users");
    }
}
