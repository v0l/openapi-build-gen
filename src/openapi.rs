use crate::parser::{HttpMethod, Route, TypeInfo};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Serialize, Deserialize)]
pub struct OpenApiSpec {
    pub openapi: String,
    pub info: Info,
    pub paths: HashMap<String, PathItem>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub components: Option<Components>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Info {
    pub title: String,
    pub version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub contact: Option<Contact>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub license: Option<License>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Contact {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub name: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub email: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct License {
    pub name: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub url: Option<String>,
}

/// Builder for customizing OpenAPI Info
#[derive(Debug, Clone)]
pub struct InfoBuilder {
    title: String,
    version: String,
    description: Option<String>,
    contact: Option<Contact>,
    license: Option<License>,
}

impl InfoBuilder {
    pub fn new(title: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            title: title.into(),
            version: version.into(),
            description: None,
            contact: None,
            license: None,
        }
    }

    pub fn description(mut self, description: impl Into<String>) -> Self {
        self.description = Some(description.into());
        self
    }

    pub fn contact(
        mut self,
        name: Option<String>,
        url: Option<String>,
        email: Option<String>,
    ) -> Self {
        self.contact = Some(Contact { name, url, email });
        self
    }

    pub fn license(mut self, name: impl Into<String>, url: Option<String>) -> Self {
        self.license = Some(License {
            name: name.into(),
            url,
        });
        self
    }

    pub fn build(self) -> Info {
        Info {
            title: self.title,
            version: self.version,
            description: self.description,
            contact: self.contact,
            license: self.license,
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
pub struct PathItem {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub get: Option<Operation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub post: Option<Operation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub put: Option<Operation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub delete: Option<Operation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub patch: Option<Operation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub head: Option<Operation>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub options: Option<Operation>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Operation {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub parameters: Option<Vec<Parameter>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub request_body: Option<RequestBody>,
    pub responses: HashMap<String, Response>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Parameter {
    pub name: String,
    #[serde(rename = "in")]
    pub location: String,
    pub required: bool,
    pub schema: Schema,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct RequestBody {
    pub required: bool,
    pub content: HashMap<String, MediaType>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Response {
    pub description: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub content: Option<HashMap<String, MediaType>>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct MediaType {
    pub schema: Schema,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(untagged)]
pub enum Schema {
    Ref {
        #[serde(rename = "$ref")]
        reference: String,
    },
    Object {
        #[serde(rename = "type")]
        type_name: String,
        #[serde(skip_serializing_if = "Option::is_none")]
        properties: Option<HashMap<String, Schema>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        required: Option<Vec<String>>,
        #[serde(skip_serializing_if = "Option::is_none")]
        items: Option<Box<Schema>>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Components {
    pub schemas: HashMap<String, Schema>,
}

impl OpenApiSpec {
    pub fn new(
        title: &str,
        version: &str,
        routes: Vec<Route>,
        type_schemas: HashMap<String, TypeInfo>,
    ) -> Self {
        Self::from_info(
            InfoBuilder::new(title, version).build(),
            routes,
            type_schemas,
        )
    }

    pub fn from_info(
        info: Info,
        routes: Vec<Route>,
        type_schemas: HashMap<String, TypeInfo>,
    ) -> Self {
        let mut paths: HashMap<String, PathItem> = HashMap::new();

        for route in routes {
            let path_item = paths.entry(route.path.clone()).or_insert(PathItem {
                get: None,
                post: None,
                put: None,
                delete: None,
                patch: None,
                head: None,
                options: None,
            });

            let operation = create_operation(&route);

            match route.method {
                HttpMethod::Get => path_item.get = Some(operation),
                HttpMethod::Post => path_item.post = Some(operation),
                HttpMethod::Put => path_item.put = Some(operation),
                HttpMethod::Delete => path_item.delete = Some(operation),
                HttpMethod::Patch => path_item.patch = Some(operation),
                HttpMethod::Head => path_item.head = Some(operation),
                HttpMethod::Options => path_item.options = Some(operation),
            }
        }

        // Build component schemas from TypeInfo
        let components = if !type_schemas.is_empty() {
            let mut schemas = HashMap::new();
            for (name, type_info) in type_schemas {
                schemas.insert(name, build_schema_from_type_info(&type_info));
            }
            Some(Components { schemas })
        } else {
            None
        };

        OpenApiSpec {
            openapi: "3.0.0".to_string(),
            info,
            paths,
            components,
        }
    }

    /// Update the Info section of the OpenAPI spec
    pub fn with_info(mut self, info: Info) -> Self {
        self.info = info;
        self
    }

    /// Update the title
    pub fn with_title(mut self, title: impl Into<String>) -> Self {
        self.info.title = title.into();
        self
    }

    /// Update the version
    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.info.version = version.into();
        self
    }

    /// Convert to JSON string
    pub fn to_json(&self) -> anyhow::Result<String> {
        Ok(serde_json::to_string_pretty(self)?)
    }

    /// Write JSON to a file
    pub fn write_json_to_file(&self, path: impl AsRef<std::path::Path>) -> anyhow::Result<()> {
        let json = self.to_json()?;
        std::fs::write(path, json)?;
        Ok(())
    }

    /// Generate Rust source code that defines the spec as a JSON string constant
    pub fn to_rust_code(&self, _const_name: &str) -> anyhow::Result<String> {
        let json = self.to_json()?;

        let mut code = String::new();
        code.push_str("// Auto-generated by openapi-build-gen\n");
        code.push_str("// This file contains the OpenAPI specification for your API\n\n");
        code.push_str("/// The OpenAPI specification as a JSON string\n");
        code.push_str("pub const OPENAPI_JSON: &str = r###\"");
        code.push_str(&json);
        code.push_str("\"###;\n");

        Ok(code)
    }

    /// Write Rust source code to a file
    pub fn write_rust_to_file(
        &self,
        path: impl AsRef<std::path::Path>,
        const_name: &str,
    ) -> anyhow::Result<()> {
        let code = self.to_rust_code(const_name)?;
        std::fs::write(path, code)?;
        Ok(())
    }
}

fn build_schema_from_type_info(type_info: &TypeInfo) -> Schema {
    if type_info.is_array {
        return Schema::Object {
            type_name: "array".to_string(),
            properties: None,
            required: None,
            items: Some(Box::new(Schema::Ref {
                reference: format!("#/components/schemas/{}", type_info.name),
            })),
        };
    }

    let mut properties = HashMap::new();
    let mut required_fields = Vec::new();

    for field in &type_info.fields {
        if field.required {
            required_fields.push(field.name.clone());
        }

        properties.insert(field.name.clone(), field_type_to_schema(&field.type_name));
    }

    Schema::Object {
        type_name: "object".to_string(),
        properties: Some(properties),
        required: if required_fields.is_empty() {
            None
        } else {
            Some(required_fields)
        },
        items: None,
    }
}

fn field_type_to_schema(type_name: &str) -> Schema {
    // Check for Vec<T> or array types
    if type_name.starts_with("Vec<") && type_name.ends_with('>') {
        let inner_type = &type_name[4..type_name.len() - 1];
        return Schema::Object {
            type_name: "array".to_string(),
            properties: None,
            required: None,
            items: Some(Box::new(field_type_to_schema(inner_type))),
        };
    }

    let openapi_type = match type_name {
        "String" | "str" => "string",
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" => "integer",
        "u8" | "u16" | "u32" | "u64" | "u128" | "usize" => "integer",
        "f32" | "f64" => "number",
        "bool" => "boolean",
        _ => {
            // It's probably a reference to another schema
            return Schema::Ref {
                reference: format!("#/components/schemas/{}", type_name),
            };
        }
    };

    Schema::Object {
        type_name: openapi_type.to_string(),
        properties: None,
        required: None,
        items: None,
    }
}

fn create_operation(route: &Route) -> Operation {
    let mut parameters = Vec::new();

    // Add path parameters
    for param in &route.path_params {
        parameters.push(Parameter {
            name: param.name.clone(),
            location: "path".to_string(),
            required: true,
            schema: type_to_schema(&param.type_name),
        });
    }

    // Add query parameters
    for param in &route.query_params {
        parameters.push(Parameter {
            name: param.name.clone(),
            location: "query".to_string(),
            required: param.required,
            schema: type_to_schema(&param.type_name),
        });
    }

    // Create request body if present
    let request_body = route.request_body.as_ref().map(|type_info| {
        let mut content = HashMap::new();
        content.insert(
            "application/json".to_string(),
            MediaType {
                schema: Schema::Ref {
                    reference: format!("#/components/schemas/{}", type_info.name),
                },
            },
        );
        RequestBody {
            required: true,
            content,
        }
    });

    // Create responses
    let mut responses = HashMap::new();

    if let Some(type_info) = &route.response_body {
        let mut content = HashMap::new();
        content.insert(
            "application/json".to_string(),
            MediaType {
                schema: Schema::Ref {
                    reference: format!("#/components/schemas/{}", type_info.name),
                },
            },
        );
        responses.insert(
            "200".to_string(),
            Response {
                description: "Successful response".to_string(),
                content: Some(content),
            },
        );
    } else {
        responses.insert(
            "200".to_string(),
            Response {
                description: "Successful response".to_string(),
                content: None,
            },
        );
    }

    Operation {
        summary: Some(route.handler.clone()),
        description: None,
        parameters: if parameters.is_empty() {
            None
        } else {
            Some(parameters)
        },
        request_body,
        responses,
    }
}

fn type_to_schema(type_name: &str) -> Schema {
    let openapi_type = match type_name {
        "String" | "str" => "string",
        "i32" | "i64" | "u32" | "u64" | "isize" | "usize" => "integer",
        "f32" | "f64" => "number",
        "bool" => "boolean",
        _ => "string",
    };

    Schema::Object {
        type_name: openapi_type.to_string(),
        properties: None,
        required: None,
        items: None,
    }
}
