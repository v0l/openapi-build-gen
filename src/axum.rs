use crate::parser::{HttpMethod, PathParam, RawRoute, TypeAlias, TypeInfo};
use std::collections::HashMap;
use syn::{Expr, ExprMethodCall, FnArg, ItemFn, Stmt};

/// Extract routes from an Axum router function
pub fn extract_routes_from_function(func: &ItemFn) -> Option<Vec<RawRoute>> {
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
fn extract_routes_from_expr(expr: &Expr) -> Vec<RawRoute> {
    extract_routes_from_expr_with_prefix(expr, "")
}

/// Recursively extract routes from an expression with a path prefix
fn extract_routes_from_expr_with_prefix(expr: &Expr, prefix: &str) -> Vec<RawRoute> {
    let mut routes = Vec::new();

    match expr {
        Expr::MethodCall(method_call) => {
            // Check if this is a .route() call
            if method_call.method == "route" {
                if let Some(mut parsed_routes) = parse_route_call(method_call) {
                    // Prepend the prefix to the route paths
                    if !prefix.is_empty() {
                        for route in &mut parsed_routes {
                            route.path = format!("{}{}", prefix, route.path);
                            // Re-extract path params since the path changed
                            route.path_params = extract_path_params(&route.path);
                        }
                    }
                    routes.extend(parsed_routes);
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
fn parse_route_call(method_call: &ExprMethodCall) -> Option<Vec<RawRoute>> {
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

    // Second argument is the method routing (e.g., get(handler) or get(handler).post(other))
    // Parse all methods from the chain - now returns (method, handler_name, handler_expr)
    let methods = parse_method_routing_chain_with_expr(&method_call.args[1]);

    if methods.is_empty() {
        return None;
    }

    // Extract path parameters from the path
    let path_params = extract_path_params(&path);

    // Create a route for each method
    let routes: Vec<RawRoute> = methods
        .into_iter()
        .map(|(method, handler, handler_expr)| {
            // Try to extract types from inline handlers
            let (request_type, response_type, error_type) = if let Some(expr) = handler_expr {
                extract_types_from_handler_expr(&expr)
            } else {
                (None, None, None)
            };

            RawRoute {
                path: path.clone(),
                method,
                handler,
                request_body_type: request_type,
                response_body_type: response_type,
                error_response_type: error_type,
                query_type: None, // Will be filled in during handler enrichment
                path_params: path_params.clone(),
                query_params: Vec::new(),
            }
        })
        .collect();

    Some(routes)
}

/// Parse a single .nest() method call
fn parse_nest_call(
    method_call: &ExprMethodCall,
    current_prefix: &str,
) -> Option<(String, Vec<RawRoute>)> {
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

/// Extract types from an inline handler expression (async block or closure)
fn extract_types_from_handler_expr(
    expr: &Expr,
) -> (Option<syn::Type>, Option<syn::Type>, Option<syn::Type>) {
    match expr {
        Expr::Closure(closure) => {
            // Extract from closure signature
            extract_types_from_closure(closure)
        }
        _ => (None, None, None),
    }
}

/// Recursively find and extract the inner type T from Json<T> wrapper anywhere in the type tree
/// This handles patterns like:
/// - Json<User> -> User
/// - Result<Json<User>, E> -> User
/// - Result<Json<Page<User>>, E> -> Page<User>
/// - Option<Json<User>> -> User
fn find_json_inner_type(ty: &syn::Type) -> Option<syn::Type> {
    match ty {
        syn::Type::Path(type_path) => {
            // Check each segment in the path (not just the last one!)
            for segment in &type_path.path.segments {
                // Found Json<T> - return T
                if segment.ident == "Json" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                            return Some(inner_type.clone());
                        }
                    }
                }

                // Recursively search in generic arguments
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(inner_type) = arg {
                            if let Some(found) = find_json_inner_type(inner_type) {
                                return Some(found);
                            }
                        }
                    }
                }
            }
        }
        syn::Type::Tuple(tuple) => {
            // Handle (StatusCode, Json<T>) patterns
            for elem in &tuple.elems {
                if let Some(found) = find_json_inner_type(elem) {
                    return Some(found);
                }
            }
        }
        _ => {}
    }
    None
}

/// Extract types from closure
fn extract_types_from_closure(
    closure: &syn::ExprClosure,
) -> (Option<syn::Type>, Option<syn::Type>, Option<syn::Type>) {
    let mut request_type = None;
    let mut response_type = None;
    let mut error_type = None;

    // Extract request type from closure inputs
    // Look for Json<T> pattern in parameters using the unified finder
    for input in &closure.inputs {
        if let syn::Pat::Type(pat_type) = input {
            if let Some(inner_type) = find_json_inner_type(&pat_type.ty) {
                request_type = Some(inner_type);
                break; // Only take the first Json parameter
            }
        }
    }

    // Extract response type from closure output
    if let syn::ReturnType::Type(_, return_type) = &closure.output {
        response_type = Some((**return_type).clone());
        // Also try to extract error type
        error_type = extract_error_type_raw(&**return_type);
    }

    (request_type, response_type, error_type)
}

/// Parse method routing chain and return (method, handler_name, optional_handler_expr)
fn parse_method_routing_chain_with_expr(expr: &Expr) -> Vec<(HttpMethod, String, Option<Expr>)> {
    let mut methods = Vec::new();

    // Handle method chains like get(handler).post(other)
    if let Expr::MethodCall(method_call) = expr {
        // Recursively parse the receiver first
        methods.extend(parse_method_routing_chain_with_expr(&method_call.receiver));

        // Parse the current method
        let method_name = method_call.method.to_string();
        let method = match method_name.as_str() {
            "get" => Some(HttpMethod::Get),
            "post" => Some(HttpMethod::Post),
            "put" => Some(HttpMethod::Put),
            "delete" => Some(HttpMethod::Delete),
            "patch" => Some(HttpMethod::Patch),
            "head" => Some(HttpMethod::Head),
            "options" => Some(HttpMethod::Options),
            "any" => Some(HttpMethod::Post), // Default 'any' to POST for now
            _ => None,
        };

        if let Some(method) = method {
            // Extract handler name and expression from the argument
            let (handler, handler_expr) = if let Some(handler_arg) = method_call.args.first() {
                match handler_arg {
                    Expr::Path(p) => (
                        p.path
                            .segments
                            .last()
                            .map(|s| s.ident.to_string())
                            .unwrap_or_else(|| "unknown".to_string()),
                        None,
                    ),
                    expr @ (Expr::Async(_) | Expr::Closure(_)) => {
                        // For inline lambda/closure handlers, generate a unique name and save expr
                        (
                            format!("inline_handler_{}_{}", method.as_str(), line!()),
                            Some(expr.clone()),
                        )
                    }
                    _ => ("unknown".to_string(), None),
                }
            } else {
                ("unknown".to_string(), None)
            };

            methods.push((method, handler, handler_expr));
        }
    } else if let Expr::Call(call) = expr {
        // Single method call like get(handler)
        if let Expr::Path(path) = &*call.func {
            let method_name = path
                .path
                .segments
                .last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default();

            // Verify it's an Axum routing method
            let is_axum_method = if path.path.segments.len() == 1 {
                true
            } else {
                path.path.segments.iter().any(|seg| seg.ident == "routing")
            };

            if !is_axum_method {
                return methods;
            }

            let method = match method_name.as_str() {
                "get" => Some(HttpMethod::Get),
                "post" => Some(HttpMethod::Post),
                "put" => Some(HttpMethod::Put),
                "delete" => Some(HttpMethod::Delete),
                "patch" => Some(HttpMethod::Patch),
                "head" => Some(HttpMethod::Head),
                "options" => Some(HttpMethod::Options),
                "any" => Some(HttpMethod::Post), // Default 'any' to POST for now
                _ => None,
            };

            if let Some(method) = method {
                // Extract handler name and expression
                let (handler, handler_expr) = if let Some(handler_arg) = call.args.first() {
                    match handler_arg {
                        Expr::Path(p) => (
                            p.path
                                .segments
                                .last()
                                .map(|s| s.ident.to_string())
                                .unwrap_or_else(|| "unknown".to_string()),
                            None,
                        ),
                        expr @ (Expr::Async(_) | Expr::Closure(_)) => {
                            // For inline lambda/closure handlers, save the expression
                            (
                                format!("inline_handler_{}_{}", method.as_str(), line!()),
                                Some(expr.clone()),
                            )
                        }
                        _ => ("unknown".to_string(), None),
                    }
                } else {
                    ("unknown".to_string(), None)
                };

                methods.push((method, handler, handler_expr));
            }
        }
    }

    methods
}

/// Parse method routing like get(handler), post(handler), etc.
/// Also handles chained methods like get(handler).post(other)
#[allow(dead_code)]
fn parse_method_routing_chain(expr: &Expr) -> Vec<(HttpMethod, String)> {
    let mut methods = Vec::new();

    // Check if this is a method call chain like get(handler).post(other)
    if let Expr::MethodCall(method_call) = expr {
        // Recursively parse the receiver to get earlier methods in the chain
        methods.extend(parse_method_routing_chain(&method_call.receiver));

        // Parse the current method
        let method_name = method_call.method.to_string();
        let method = match method_name.as_str() {
            "get" => Some(HttpMethod::Get),
            "post" => Some(HttpMethod::Post),
            "put" => Some(HttpMethod::Put),
            "delete" => Some(HttpMethod::Delete),
            "patch" => Some(HttpMethod::Patch),
            "head" => Some(HttpMethod::Head),
            "options" => Some(HttpMethod::Options),
            _ => None,
        };

        if let Some(method) = method {
            // Extract handler name from the argument
            let handler = if let Some(handler_expr) = method_call.args.first() {
                match handler_expr {
                    Expr::Path(p) => p
                        .path
                        .segments
                        .last()
                        .map(|s| s.ident.to_string())
                        .unwrap_or_else(|| "unknown".to_string()),
                    Expr::Async(_) | Expr::Closure(_) => {
                        // For inline lambda/closure handlers, generate a unique name
                        format!("inline_handler_{}_{}", method.as_str(), line!())
                    }
                    _ => "unknown".to_string(),
                }
            } else {
                "unknown".to_string()
            };

            methods.push((method, handler));
        }
    } else if let Expr::Call(call) = expr {
        // Single method call like get(handler)
        if let Expr::Path(path) = &*call.func {
            // Check if this is an Axum routing method by verifying the path
            // We look for patterns like:
            // - get (simple)
            // - axum::routing::get
            // - routing::get
            let method_name = path
                .path
                .segments
                .last()
                .map(|s| s.ident.to_string())
                .unwrap_or_default();

            // Verify it's an Axum routing method by checking if the path contains "routing" or is a standalone method name
            let is_axum_method = if path.path.segments.len() == 1 {
                // Standalone like `get(handler)` - assume it's from axum::routing
                true
            } else {
                // Check if path contains "routing" like axum::routing::get or routing::get
                path.path.segments.iter().any(|seg| seg.ident == "routing")
            };

            if !is_axum_method {
                return methods;
            }

            let method = match method_name.as_str() {
                "get" => Some(HttpMethod::Get),
                "post" => Some(HttpMethod::Post),
                "put" => Some(HttpMethod::Put),
                "delete" => Some(HttpMethod::Delete),
                "patch" => Some(HttpMethod::Patch),
                "head" => Some(HttpMethod::Head),
                "options" => Some(HttpMethod::Options),
                _ => None,
            };

            if let Some(method) = method {
                // Extract handler name
                let handler = if let Some(handler_expr) = call.args.first() {
                    match handler_expr {
                        Expr::Path(p) => p
                            .path
                            .segments
                            .last()
                            .map(|s| s.ident.to_string())
                            .unwrap_or_else(|| "unknown".to_string()),
                        Expr::Async(_) | Expr::Closure(_) => {
                            // For inline lambda/closure handlers, generate a unique name
                            format!("inline_handler_{}_{}", method.as_str(), line!())
                        }
                        _ => "unknown".to_string(),
                    }
                } else {
                    "unknown".to_string()
                };

                methods.push((method, handler));
            }
        }
    }

    methods
}

/// Parse method routing like get(handler), post(handler), etc.
#[allow(dead_code)]
fn parse_method_routing(expr: &Expr) -> Option<(HttpMethod, String)> {
    let methods = parse_method_routing_chain(expr);
    methods.into_iter().next()
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

/// Extract error type from Result<T, E> as raw syn::Type
/// Recursively searches for Result<T, E> and returns E
fn extract_error_type_raw(ty: &syn::Type) -> Option<syn::Type> {
    match ty {
        syn::Type::Path(type_path) => {
            // Search all segments, not just the last one
            for segment in &type_path.path.segments {
                let segment_name = segment.ident.to_string();

                // Handle Result<T, E> - return the second type argument (E)
                if segment_name == "Result" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if args.args.len() >= 2 {
                            if let Some(syn::GenericArgument::Type(error_type)) =
                                args.args.iter().nth(1)
                            {
                                return Some(error_type.clone());
                            }
                        }
                    }
                }

                // Recursively search in generic arguments
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(inner_type) = arg {
                            if let Some(result) = extract_error_type_raw(inner_type) {
                                return Some(result);
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

/// Parse a handler function to extract raw request/response types (as syn::Type)
pub fn parse_handler_function_raw(
    func: &ItemFn,
) -> (
    Option<syn::Type>,
    Option<syn::Type>,
    Option<syn::Type>,
    Option<syn::Type>,
) {
    let mut request_type = None;
    let mut response_type = None;
    let mut error_type = None;
    let mut query_type = None;

    // Extract from function parameters
    for input in &func.sig.inputs {
        if let FnArg::Typed(pat_type) = input {
            // Check for Json<T>
            if request_type.is_none() {
                if let Some(inner_type) = find_json_inner_type(&pat_type.ty) {
                    request_type = Some(inner_type);
                }
            }
            // Check for Query<T>
            if query_type.is_none() {
                if let Some(query_inner) = find_query_inner_type(&pat_type.ty) {
                    query_type = Some(query_inner);
                }
            }
        }
    }

    // Extract from return type
    if let syn::ReturnType::Type(_, return_type) = &func.sig.output {
        response_type = Some((**return_type).clone());
        // Also try to extract error type from Result<T, E>
        error_type = extract_error_type_raw(&**return_type);
    }

    (request_type, response_type, error_type, query_type)
}

/// Find Query<T> in a type and return the inner type T
/// Similar to find_json_inner_type but looks for Query wrapper
fn find_query_inner_type(ty: &syn::Type) -> Option<syn::Type> {
    match ty {
        syn::Type::Path(type_path) => {
            // Check each segment in the path
            for segment in &type_path.path.segments {
                // Found Query<T> - return T
                if segment.ident == "Query" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                            return Some(inner_type.clone());
                        }
                    }
                }
            }
        }
        _ => {}
    }
    None
}

/// Parse a handler function to extract request/response types
pub fn parse_handler_function(
    func: &ItemFn,
    type_aliases: &HashMap<String, TypeAlias>,
) -> (Option<TypeInfo>, Option<TypeInfo>, Option<TypeInfo>) {
    let mut request_type = None;
    let mut response_type = None;
    let mut error_type = None;

    // Extract from function parameters - use unified Json finder
    for input in &func.sig.inputs {
        if let FnArg::Typed(pat_type) = input {
            if let Some(inner_type) = find_json_inner_type(&pat_type.ty) {
                // Convert the found type to TypeInfo
                request_type = resolve_type_info(&inner_type, type_aliases);
                break;
            }
        }
    }

    // Extract from return type
    if let syn::ReturnType::Type(_, return_type) = &func.sig.output {
        // Look for Result<Json<T>, _> or Json<T>
        response_type = extract_response_type(return_type, type_aliases);
        // Extract error type
        error_type = extract_error_type(return_type, type_aliases);
    }

    (request_type, response_type, error_type)
}

fn extract_response_type(
    ty: &syn::Type,
    type_aliases: &HashMap<String, TypeAlias>,
) -> Option<TypeInfo> {
    // First, try to find Json<T> anywhere in the type tree
    if let Some(json_inner) = find_json_inner_type(ty) {
        // Now convert the inner type to TypeInfo
        return extract_json_inner_type(&json_inner, type_aliases);
    }

    // If no Json<T> found, check for type aliases and other patterns
    match ty {
        syn::Type::Path(type_path) => {
            // Search all segments, not just the last one
            for segment in &type_path.path.segments {
                let segment_name = segment.ident.to_string();

                // Check if this is a type alias first
                if let Some(_type_alias) = type_aliases.get(&segment_name) {
                    // For type aliases with generic arguments like ApiResult<User>,
                    // we extract the generic argument directly
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(generic_type)) = args.args.first() {
                            return extract_json_inner_type(generic_type, type_aliases);
                        }
                    }
                }

                // Handle Result<T, E> - look in first type argument
                if segment_name == "Result" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(ok_type)) = args.args.first() {
                            return extract_response_type(ok_type, type_aliases);
                        }
                    }
                }

                // Handle Option<T>
                if segment_name == "Option" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                            return extract_response_type(inner_type, type_aliases);
                        }
                    }
                }

                // Recursively search in generic arguments
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(inner_type) = arg {
                            if let Some(result) = extract_response_type(inner_type, type_aliases) {
                                return Some(result);
                            }
                        }
                    }
                }
            }
        }
        syn::Type::Tuple(tuple) => {
            // Handle (StatusCode, Json<T>) or similar patterns
            for elem in &tuple.elems {
                if let Some(type_info) = extract_response_type(elem, type_aliases) {
                    return Some(type_info);
                }
            }
        }
        _ => {}
    }
    None
}

/// Extract the inner type from Json<T>, handling Vec<T> and other types
fn extract_json_inner_type(
    ty: &syn::Type,
    _type_aliases: &HashMap<String, TypeAlias>,
) -> Option<TypeInfo> {
    match ty {
        syn::Type::Path(type_path) => {
            // Get the last segment as the primary type
            if let Some(segment) = type_path.path.segments.last() {
                let segment_name = segment.ident.to_string();

                // Handle Vec<T> specially - we want to mark it as an array
                if segment_name == "Vec" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                            // Recursively extract the element type
                            if let Some(mut element_type) =
                                extract_json_inner_type(inner_type, _type_aliases)
                            {
                                element_type.is_array = true;
                                return Some(element_type);
                            }
                        }
                    }
                }

                // Extract generic arguments from this type
                let mut generic_args = Vec::new();
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(generic_type) = arg {
                            // Extract the name of the generic type argument
                            if let syn::Type::Path(gen_path) = generic_type {
                                if let Some(gen_segment) = gen_path.path.segments.last() {
                                    generic_args.push(gen_segment.ident.to_string());
                                }
                            }
                        }
                    }
                }

                // For all other types (including Page<T>, ApiData<T>, etc.),
                // we return the type with its generic arguments
                return Some(TypeInfo {
                    name: segment.ident.to_string(),
                    fields: Vec::new(),
                    is_array: false,
                    is_enum: false,
                    enum_variants: vec![],
                    generic_args,
                });
            }
        }
        _ => {}
    }
    None
}

/// Extract the error type from Result<T, E> patterns
fn extract_error_type(
    ty: &syn::Type,
    type_aliases: &HashMap<String, TypeAlias>,
) -> Option<TypeInfo> {
    // First, get the raw error type from Result<T, E>
    let error_type_raw = extract_error_type_raw(ty)?;

    // Now check if the error is wrapped in Json<E>
    if let Some(json_inner) = find_json_inner_type(&error_type_raw) {
        // Convert to TypeInfo
        return extract_json_inner_type(&json_inner, type_aliases);
    }

    // If not wrapped in Json, just extract the error type directly
    extract_error_inner_type(&error_type_raw, type_aliases)
}

/// Extract the inner type from an error type, handling Json<E> wrappers
fn extract_error_inner_type(
    ty: &syn::Type,
    type_aliases: &HashMap<String, TypeAlias>,
) -> Option<TypeInfo> {
    // First check if it's wrapped in Json<E>
    if let Some(json_inner) = find_json_inner_type(ty) {
        return extract_json_inner_type(&json_inner, type_aliases);
    }

    // Otherwise, just extract the type directly
    match ty {
        syn::Type::Path(type_path) => {
            // Search all segments
            for segment in &type_path.path.segments {
                // Handle wrapper types with generics - recursively search
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(inner_type) = arg {
                            if let Some(result) = extract_error_inner_type(inner_type, type_aliases)
                            {
                                return Some(result);
                            }
                        }
                    }
                }
            }

            // Return the outermost type name as-is
            if let Some(segment) = type_path.path.segments.last() {
                // Extract generic arguments for error types too
                let mut generic_args = Vec::new();
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(generic_type) = arg {
                            if let syn::Type::Path(gen_path) = generic_type {
                                if let Some(gen_segment) = gen_path.path.segments.last() {
                                    generic_args.push(gen_segment.ident.to_string());
                                }
                            }
                        }
                    }
                }

                return Some(TypeInfo {
                    name: segment.ident.to_string(),
                    fields: Vec::new(),
                    is_array: false,
                    is_enum: false,
                    enum_variants: vec![],
                    generic_args,
                });
            }
        }
        _ => {}
    }
    None
}

/// Public function to resolve a raw type (used for request bodies)
pub fn resolve_type_info(
    ty: &syn::Type,
    _type_aliases: &HashMap<String, TypeAlias>,
) -> Option<TypeInfo> {
    // For request bodies, we just extract the type name directly
    match ty {
        syn::Type::Path(type_path) => {
            if let Some(segment) = type_path.path.segments.last() {
                // Extract generic arguments
                let mut generic_args = Vec::new();
                if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                    for arg in &args.args {
                        if let syn::GenericArgument::Type(generic_type) = arg {
                            if let syn::Type::Path(gen_path) = generic_type {
                                if let Some(gen_segment) = gen_path.path.segments.last() {
                                    generic_args.push(gen_segment.ident.to_string());
                                }
                            }
                        }
                    }
                }

                Some(TypeInfo {
                    name: segment.ident.to_string(),
                    fields: Vec::new(),
                    is_array: false,
                    is_enum: false,
                    enum_variants: vec![],
                    generic_args,
                })
            } else {
                None
            }
        }
        _ => None,
    }
}

/// Public function to resolve a response type with type alias resolution
pub fn resolve_response_type(
    ty: &syn::Type,
    type_aliases: &HashMap<String, TypeAlias>,
) -> Option<TypeInfo> {
    extract_response_type(ty, type_aliases)
}

/// Public function to resolve an error type with type alias resolution
pub fn resolve_error_type(
    ty: &syn::Type,
    type_aliases: &HashMap<String, TypeAlias>,
) -> Option<TypeInfo> {
    extract_error_inner_type(ty, type_aliases)
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
        let type_aliases = HashMap::new();
        let (request, response, _error) = parse_handler_function(&parsed, &type_aliases);

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
        let type_aliases = HashMap::new();
        let (request, response, error) = parse_handler_function(&parsed, &type_aliases);

        assert!(request.is_none());
        assert!(response.is_some());
        assert_eq!(response.unwrap().name, "User");

        // StatusCode is not a Json type, so error extraction won't find it
        assert!(error.is_some());
        assert_eq!(error.unwrap().name, "StatusCode");
    }

    #[test]
    fn test_parse_handler_function_no_json() {
        let code = r#"
            async fn health_check() -> &'static str {
                "OK"
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let type_aliases = HashMap::new();
        let (request, response, _error) = parse_handler_function(&parsed, &type_aliases);

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

    #[test]
    fn test_parse_method_routing_with_explicit_path() {
        // Test that we correctly parse axum::routing::get syntax
        let code = r#"
            pub fn routes() -> Router {
                Router::new()
                    .route("/test", axum::routing::get(handler))
                    .route("/post", routing::post(create))
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let routes = extract_routes_from_function(&parsed);

        assert!(routes.is_some());
        let routes = routes.unwrap();
        assert_eq!(routes.len(), 2);

        // Don't assume order since routes are collected during tree traversal
        let get_route = routes.iter().find(|r| r.path == "/test").unwrap();
        assert_eq!(get_route.method, HttpMethod::Get);
        assert_eq!(get_route.handler, "handler");

        let post_route = routes.iter().find(|r| r.path == "/post").unwrap();
        assert_eq!(post_route.method, HttpMethod::Post);
        assert_eq!(post_route.handler, "create");
    }

    #[test]
    fn test_parse_handler_with_nested_generics() {
        // Test Result<Json<Page<User>>> pattern
        let code = r#"
            async fn get_users() -> Result<Json<Page<User>>, Json<ApiError>> {
                Ok(Json(Page { items: vec![] }))
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let type_aliases = HashMap::new();
        let (request, response, error) = parse_handler_function(&parsed, &type_aliases);

        assert!(request.is_none());
        assert!(response.is_some());
        assert_eq!(response.unwrap().name, "Page");

        assert!(error.is_some());
        assert_eq!(error.unwrap().name, "ApiError");
    }

    #[test]
    fn test_find_json_inner_type_deeply_nested() {
        // Test that we can find Json<T> deep in the type tree
        let type_str = "Result<Option<Json<User>>, Error>";
        let ty: syn::Type = syn::parse_str(type_str).unwrap();

        let result = find_json_inner_type(&ty);
        assert!(result.is_some());

        // Should extract User from Json<User>
        if let Some(syn::Type::Path(type_path)) = result {
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(name, "User");
        } else {
            panic!("Expected Type::Path");
        }
    }

    #[test]
    fn test_find_json_in_tuple() {
        // Test (StatusCode, Json<T>) pattern
        let type_str = "(StatusCode, Json<User>)";
        let ty: syn::Type = syn::parse_str(type_str).unwrap();

        let result = find_json_inner_type(&ty);
        assert!(result.is_some());

        if let Some(syn::Type::Path(type_path)) = result {
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(name, "User");
        } else {
            panic!("Expected Type::Path");
        }
    }

    #[test]
    fn test_extract_query_inner_type() {
        // Test extracting T from Query<T>
        let type_str = "Query<SearchParams>";
        let ty: syn::Type = syn::parse_str(type_str).unwrap();

        let result = find_query_inner_type(&ty);
        assert!(result.is_some());

        if let Some(syn::Type::Path(type_path)) = result {
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(name, "SearchParams");
        } else {
            panic!("Expected Type::Path");
        }
    }

    #[test]
    fn test_parse_handler_with_query_params() {
        // Test extracting Query<T> from handler
        let code = r#"
            async fn search(Query(params): Query<SearchParams>) -> Json<Vec<User>> {
                Json(vec![])
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let (request, response, error, query) = parse_handler_function_raw(&parsed);

        assert!(request.is_none());
        assert!(response.is_some());
        assert!(error.is_none());
        assert!(query.is_some());

        if let Some(syn::Type::Path(type_path)) = query {
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(name, "SearchParams");
        } else {
            panic!("Expected Type::Path for query");
        }
    }

    #[test]
    fn test_parse_handler_with_query_and_json() {
        // Test extracting both Query<T> and Json<T>
        let code = r#"
            async fn create_with_filter(
                Query(filter): Query<FilterParams>,
                Json(body): Json<CreateRequest>
            ) -> Json<Response> {
                Json(Response {})
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let (request, response, error, query) = parse_handler_function_raw(&parsed);

        assert!(request.is_some());
        if let Some(syn::Type::Path(type_path)) = request {
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(name, "CreateRequest");
        }

        assert!(response.is_some());
        assert!(error.is_none());

        assert!(query.is_some());
        if let Some(syn::Type::Path(type_path)) = query {
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(name, "FilterParams");
        }
    }

    #[test]
    fn test_extract_query_with_generic_type() {
        // Test extracting Query<Page<T>> - should get Page<T>, not just T
        let type_str = "Query<Page<User>>";
        let ty: syn::Type = syn::parse_str(type_str).unwrap();

        let result = find_query_inner_type(&ty);
        assert!(result.is_some());

        // The result should be Page<User>, not User
        if let Some(syn::Type::Path(type_path)) = result {
            // Check the main type is Page
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(name, "Page");

            // Check it has generic arguments
            let segment = type_path.path.segments.last().unwrap();
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                assert_eq!(args.args.len(), 1, "Page should have 1 generic argument");

                // Check the generic argument is User
                if let Some(syn::GenericArgument::Type(syn::Type::Path(inner_path))) =
                    args.args.first()
                {
                    let inner_name = inner_path.path.segments.last().unwrap().ident.to_string();
                    assert_eq!(inner_name, "User");
                }
            } else {
                panic!("Expected Page to have generic arguments");
            }
        } else {
            panic!("Expected Type::Path");
        }
    }

    #[test]
    fn test_parse_handler_with_generic_query_params() {
        // Test handler with Query<Page<T>>
        let code = r#"
            async fn search_paginated(
                Query(params): Query<PageParams<SearchFilter>>
            ) -> Json<Vec<User>> {
                Json(vec![])
            }
        "#;

        let parsed: ItemFn = syn::parse_str(code).unwrap();
        let (request, response, error, query) = parse_handler_function_raw(&parsed);

        assert!(request.is_none());
        assert!(response.is_some());
        assert!(error.is_none());
        assert!(query.is_some());

        // Verify we extracted PageParams<SearchFilter>, not just SearchFilter
        if let Some(syn::Type::Path(type_path)) = query {
            let name = type_path.path.segments.last().unwrap().ident.to_string();
            assert_eq!(
                name, "PageParams",
                "Should extract PageParams, not just the inner type"
            );

            // Verify generic argument is preserved
            let segment = type_path.path.segments.last().unwrap();
            if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                assert_eq!(args.args.len(), 1);
            } else {
                panic!("Expected PageParams to have generic arguments");
            }
        } else {
            panic!("Expected Type::Path for query");
        }
    }
}
