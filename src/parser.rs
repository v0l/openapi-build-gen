use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Route {
    pub path: String,
    pub method: HttpMethod,
    pub handler: String,
    pub request_body: Option<TypeInfo>,
    pub response_body: Option<TypeInfo>,
    pub path_params: Vec<PathParam>,
    pub query_params: Vec<QueryParam>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Patch,
    Head,
    Options,
}

impl HttpMethod {
    pub fn as_str(&self) -> &str {
        match self {
            HttpMethod::Get => "get",
            HttpMethod::Post => "post",
            HttpMethod::Put => "put",
            HttpMethod::Delete => "delete",
            HttpMethod::Patch => "patch",
            HttpMethod::Head => "head",
            HttpMethod::Options => "options",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct TypeInfo {
    pub name: String,
    pub fields: Vec<Field>,
    pub is_array: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Field {
    pub name: String,
    pub type_name: String,
    pub required: bool,
    pub description: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PathParam {
    pub name: String,
    pub type_name: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueryParam {
    pub name: String,
    pub type_name: String,
    pub required: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_http_method_as_str() {
        assert_eq!(HttpMethod::Get.as_str(), "get");
        assert_eq!(HttpMethod::Post.as_str(), "post");
        assert_eq!(HttpMethod::Put.as_str(), "put");
        assert_eq!(HttpMethod::Delete.as_str(), "delete");
        assert_eq!(HttpMethod::Patch.as_str(), "patch");
        assert_eq!(HttpMethod::Head.as_str(), "head");
        assert_eq!(HttpMethod::Options.as_str(), "options");
    }

    #[test]
    fn test_type_info_creation() {
        let type_info = TypeInfo {
            name: "User".to_string(),
            fields: vec![
                Field {
                    name: "id".to_string(),
                    type_name: "u64".to_string(),
                    required: true,
                    description: None,
                },
                Field {
                    name: "name".to_string(),
                    type_name: "String".to_string(),
                    required: true,
                    description: Some("User's full name".to_string()),
                },
            ],
            is_array: false,
        };

        assert_eq!(type_info.name, "User");
        assert_eq!(type_info.fields.len(), 2);
        assert_eq!(type_info.is_array, false);
    }

    #[test]
    fn test_field_required_vs_optional() {
        let required_field = Field {
            name: "email".to_string(),
            type_name: "String".to_string(),
            required: true,
            description: None,
        };

        let optional_field = Field {
            name: "phone".to_string(),
            type_name: "String".to_string(),
            required: false,
            description: None,
        };

        assert!(required_field.required);
        assert!(!optional_field.required);
    }

    #[test]
    fn test_route_creation() {
        let route = Route {
            path: "/users/:id".to_string(),
            method: HttpMethod::Get,
            handler: "get_user".to_string(),
            request_body: None,
            response_body: Some(TypeInfo {
                name: "User".to_string(),
                fields: vec![],
                is_array: false,
            }),
            path_params: vec![PathParam {
                name: "id".to_string(),
                type_name: "String".to_string(),
            }],
            query_params: vec![],
        };

        assert_eq!(route.path, "/users/:id");
        assert_eq!(route.method, HttpMethod::Get);
        assert_eq!(route.handler, "get_user");
        assert!(route.request_body.is_none());
        assert!(route.response_body.is_some());
        assert_eq!(route.path_params.len(), 1);
    }
}
