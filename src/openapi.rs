use crate::parser::{HttpMethod, Route, TypeInfo};
use indexmap::IndexMap;
use openapiv3::*;

/// Wrapper around openapiv3::OpenAPI with convenience methods
pub struct OpenApiSpec {
    pub spec: OpenAPI,
}

/// Builder for customizing OpenAPI Info (keeping API compatibility)
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
        self.contact = Some(Contact {
            name,
            url,
            email,
            extensions: Default::default(),
        });
        self
    }

    pub fn license(mut self, name: impl Into<String>, url: Option<String>) -> Self {
        self.license = Some(License {
            name: name.into(),
            url,
            extensions: Default::default(),
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
            terms_of_service: None,
            extensions: Default::default(),
        }
    }
}

impl OpenApiSpec {
    pub fn new(
        title: &str,
        version: &str,
        routes: Vec<Route>,
        type_schemas: std::collections::HashMap<String, TypeInfo>,
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
        type_schemas: std::collections::HashMap<String, TypeInfo>,
    ) -> Self {
        let mut paths = Paths::default();

        for route in routes {
            let path_item = paths
                .paths
                .entry(route.path.clone())
                .or_insert_with(|| ReferenceOr::Item(PathItem::default()));

            if let ReferenceOr::Item(path_item) = path_item {
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
        }

        // Build component schemas from TypeInfo
        let components = if !type_schemas.is_empty() {
            let mut schemas = IndexMap::new();
            for (name, type_info) in type_schemas {
                schemas.insert(
                    name,
                    ReferenceOr::Item(build_schema_from_type_info(&type_info)),
                );
            }
            Some(Components {
                schemas,
                responses: Default::default(),
                parameters: Default::default(),
                examples: Default::default(),
                request_bodies: Default::default(),
                headers: Default::default(),
                security_schemes: Default::default(),
                links: Default::default(),
                callbacks: Default::default(),
                extensions: Default::default(),
            })
        } else {
            None
        };

        let spec = OpenAPI {
            openapi: "3.0.3".to_string(),
            info,
            servers: vec![],
            paths,
            components,
            security: None,
            tags: vec![],
            external_docs: None,
            extensions: Default::default(),
        };

        OpenApiSpec { spec }
    }

    /// Update the Info section of the OpenAPI spec
    pub fn with_info(mut self, info: Info) -> Self {
        self.spec.info = info;
        self
    }

    /// Update the title
    pub fn with_title(mut self, title: impl Into<String>) -> Self {
        self.spec.info.title = title.into();
        self
    }

    /// Update the version
    pub fn with_version(mut self, version: impl Into<String>) -> Self {
        self.spec.info.version = version.into();
        self
    }

    /// Convert to JSON string
    pub fn to_json(&self) -> anyhow::Result<String> {
        Ok(serde_json::to_string_pretty(&self.spec)?)
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
    // Handle enums
    if type_info.is_enum {
        return Schema {
            schema_kind: SchemaKind::Type(Type::String(StringType {
                enumeration: type_info
                    .enum_variants
                    .iter()
                    .map(|v| Some(v.clone()))
                    .collect(),
                ..Default::default()
            })),
            schema_data: Default::default(),
        };
    }

    if type_info.is_array {
        return Schema {
            schema_kind: SchemaKind::Type(Type::Array(ArrayType {
                items: Some(ReferenceOr::ref_(&format!(
                    "#/components/schemas/{}",
                    type_info.name
                ))),
                min_items: None,
                max_items: None,
                unique_items: false,
            })),
            schema_data: Default::default(),
        };
    }

    let mut properties = IndexMap::new();
    let mut required_fields = Vec::new();

    for field in &type_info.fields {
        if field.required {
            required_fields.push(field.name.clone());
        }

        properties.insert(
            field.name.clone(),
            ReferenceOr::boxed_item(field_type_to_schema(&field.type_name)),
        );
    }

    Schema {
        schema_kind: SchemaKind::Type(Type::Object(ObjectType {
            properties,
            required: required_fields,
            additional_properties: None,
            min_properties: None,
            max_properties: None,
        })),
        schema_data: Default::default(),
    }
}

fn field_type_to_schema(type_name: &str) -> Schema {
    // Check for Vec<T> or array types
    if type_name.starts_with("Vec<") && type_name.ends_with('>') {
        let inner_type = &type_name[4..type_name.len() - 1];
        return Schema {
            schema_kind: SchemaKind::Type(Type::Array(ArrayType {
                items: Some(ReferenceOr::boxed_item(field_type_to_schema(inner_type))),
                min_items: None,
                max_items: None,
                unique_items: false,
            })),
            schema_data: Default::default(),
        };
    }

    match type_name {
        "String" | "str" => Schema {
            schema_kind: SchemaKind::Type(Type::String(StringType::default())),
            schema_data: Default::default(),
        },
        "i8" | "i16" | "i32" | "i64" | "i128" | "isize" | "u8" | "u16" | "u32" | "u64" | "u128"
        | "usize" => Schema {
            schema_kind: SchemaKind::Type(Type::Integer(IntegerType::default())),
            schema_data: Default::default(),
        },
        "f32" | "f64" => Schema {
            schema_kind: SchemaKind::Type(Type::Number(NumberType::default())),
            schema_data: Default::default(),
        },
        "bool" => Schema {
            schema_kind: SchemaKind::Type(Type::Boolean(BooleanType::default())),
            schema_data: Default::default(),
        },
        _ => {
            // For custom types, create a reference schema
            Schema {
                schema_kind: SchemaKind::OneOf {
                    one_of: vec![ReferenceOr::ref_(&format!(
                        "#/components/schemas/{}",
                        type_name
                    ))],
                },
                schema_data: Default::default(),
            }
        }
    }
}

fn create_operation(route: &Route) -> Operation {
    let mut parameters = Vec::new();

    // Add path parameters
    for param in &route.path_params {
        parameters.push(ReferenceOr::Item(Parameter::Path {
            parameter_data: ParameterData {
                name: param.name.clone(),
                description: None,
                required: true,
                deprecated: None,
                format: ParameterSchemaOrContent::Schema(type_to_schema(&param.type_name)),
                example: None,
                examples: Default::default(),
                explode: None,
                extensions: Default::default(),
            },
            style: PathStyle::Simple,
        }));
    }

    // Add query parameters
    for param in &route.query_params {
        parameters.push(ReferenceOr::Item(Parameter::Query {
            parameter_data: ParameterData {
                name: param.name.clone(),
                description: None,
                required: param.required,
                deprecated: None,
                format: ParameterSchemaOrContent::Schema(type_to_schema(&param.type_name)),
                example: None,
                examples: Default::default(),
                explode: None,
                extensions: Default::default(),
            },
            allow_reserved: false,
            style: QueryStyle::Form,
            allow_empty_value: None,
        }));
    }

    // Create request body if present
    let request_body = route.request_body.as_ref().map(|type_info| {
        let mut content = IndexMap::new();
        let schema_name = type_info.full_name(); // Use full_name() for generic types
        let schema = if type_info.is_array {
            ReferenceOr::Item(Schema {
                schema_kind: SchemaKind::Type(Type::Array(ArrayType {
                    items: Some(ReferenceOr::ref_(&format!(
                        "#/components/schemas/{}",
                        schema_name
                    ))),
                    min_items: None,
                    max_items: None,
                    unique_items: false,
                })),
                schema_data: Default::default(),
            })
        } else {
            ReferenceOr::ref_(&format!("#/components/schemas/{}", schema_name))
        };

        content.insert(
            "application/json".to_string(),
            MediaType {
                schema: Some(schema),
                example: None,
                examples: Default::default(),
                encoding: Default::default(),
                extensions: Default::default(),
            },
        );

        ReferenceOr::Item(RequestBody {
            description: None,
            content,
            required: true,
            extensions: Default::default(),
        })
    });

    // Create responses
    let mut responses = Responses::default();

    if let Some(type_info) = &route.response_body {
        let mut content = IndexMap::new();
        let schema_name = type_info.full_name(); // Use full_name() for generic types
        let schema = if type_info.is_array {
            ReferenceOr::Item(Schema {
                schema_kind: SchemaKind::Type(Type::Array(ArrayType {
                    items: Some(ReferenceOr::ref_(&format!(
                        "#/components/schemas/{}",
                        schema_name
                    ))),
                    min_items: None,
                    max_items: None,
                    unique_items: false,
                })),
                schema_data: Default::default(),
            })
        } else {
            ReferenceOr::ref_(&format!("#/components/schemas/{}", schema_name))
        };

        content.insert(
            "application/json".to_string(),
            MediaType {
                schema: Some(schema),
                example: None,
                examples: Default::default(),
                encoding: Default::default(),
                extensions: Default::default(),
            },
        );

        responses.responses.insert(
            StatusCode::Code(200),
            ReferenceOr::Item(Response {
                description: "Successful response".to_string(),
                content,
                headers: Default::default(),
                links: Default::default(),
                extensions: Default::default(),
            }),
        );
    } else {
        responses.responses.insert(
            StatusCode::Code(200),
            ReferenceOr::Item(Response {
                description: "Successful response".to_string(),
                content: Default::default(),
                headers: Default::default(),
                links: Default::default(),
                extensions: Default::default(),
            }),
        );
    }

    // Add error response if present
    if let Some(error_info) = &route.error_response {
        let mut content = IndexMap::new();
        let schema_name = error_info.full_name(); // Use full_name() for generic types
        let schema = ReferenceOr::ref_(&format!("#/components/schemas/{}", schema_name));

        content.insert(
            "application/json".to_string(),
            MediaType {
                schema: Some(schema),
                example: None,
                examples: Default::default(),
                encoding: Default::default(),
                extensions: Default::default(),
            },
        );

        responses.responses.insert(
            StatusCode::Code(500),
            ReferenceOr::Item(Response {
                description: "Internal server error".to_string(),
                content,
                headers: Default::default(),
                links: Default::default(),
                extensions: Default::default(),
            }),
        );
    }

    Operation {
        summary: Some(route.handler.clone()),
        description: None,
        parameters,
        request_body,
        responses,
        tags: vec![],
        deprecated: false,
        security: None,
        servers: vec![],
        callbacks: Default::default(),
        external_docs: None,
        operation_id: None,
        extensions: Default::default(),
    }
}

fn type_to_schema(type_name: &str) -> ReferenceOr<Schema> {
    match type_name {
        "String" | "str" => ReferenceOr::Item(Schema {
            schema_kind: SchemaKind::Type(Type::String(StringType::default())),
            schema_data: Default::default(),
        }),
        "i32" | "i64" | "u32" | "u64" | "isize" | "usize" => ReferenceOr::Item(Schema {
            schema_kind: SchemaKind::Type(Type::Integer(IntegerType::default())),
            schema_data: Default::default(),
        }),
        "f32" | "f64" => ReferenceOr::Item(Schema {
            schema_kind: SchemaKind::Type(Type::Number(NumberType::default())),
            schema_data: Default::default(),
        }),
        "bool" => ReferenceOr::Item(Schema {
            schema_kind: SchemaKind::Type(Type::Boolean(BooleanType::default())),
            schema_data: Default::default(),
        }),
        _ => ReferenceOr::Item(Schema {
            schema_kind: SchemaKind::Type(Type::String(StringType::default())),
            schema_data: Default::default(),
        }),
    }
}
