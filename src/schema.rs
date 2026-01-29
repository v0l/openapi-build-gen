use crate::parser::{Field, TypeInfo};
use std::collections::HashMap;
use syn::{Attribute, Expr, Fields, ItemEnum, ItemStruct, Lit, Meta, Type};

/// Extract struct information including serde attributes
pub fn extract_struct_info(item_struct: &ItemStruct) -> TypeInfo {
    extract_struct_info_with_flatten(item_struct, &HashMap::new())
}

/// Extract struct information with flatten support
pub fn extract_struct_info_with_flatten(
    item_struct: &ItemStruct,
    all_structs: &HashMap<String, TypeInfo>,
) -> TypeInfo {
    let name = item_struct.ident.to_string();
    let mut fields = Vec::new();

    // Get struct-level rename_all attribute
    let rename_all = get_serde_rename_all(&item_struct.attrs);

    if let Fields::Named(named_fields) = &item_struct.fields {
        for field in &named_fields.named {
            if let Some(field_ident) = &field.ident {
                let original_name = field_ident.to_string();

                // Check if field should be skipped
                if should_skip_field(&field.attrs) {
                    continue;
                }

                // Check if field is flattened
                if is_flattened_field(&field.attrs) {
                    // Extract the type name and look it up
                    let type_name = type_to_base_name(&field.ty);
                    if let Some(flattened_type) = all_structs.get(&type_name) {
                        // Merge all fields from the flattened struct
                        fields.extend(flattened_type.fields.clone());
                    }
                    // If we can't find the struct, skip it (could be an external type)
                    continue;
                }

                // Get the JSON name (considering rename and rename_all)
                let json_name = get_field_json_name(&field.attrs, &original_name, &rename_all);

                let type_name = type_to_string(&field.ty);

                // Check if field is Option<T> to determine if it's required
                let (is_required, clean_type) = if type_name.starts_with("Option<") {
                    (false, type_name[7..type_name.len() - 1].to_string())
                } else {
                    (true, type_name)
                };

                fields.push(Field {
                    name: json_name,
                    type_name: clean_type,
                    required: is_required,
                    description: None,
                });
            }
        }
    }

    TypeInfo {
        name,
        fields,
        is_array: false,
        is_enum: false,
        enum_variants: vec![],
        generic_args: vec![], // Structs defined in source don't have concrete generic args yet
    }
}

/// Extract enum information including serde attributes
pub fn extract_enum_info(item_enum: &ItemEnum) -> TypeInfo {
    let name = item_enum.ident.to_string();
    let mut enum_variants = Vec::new();

    // Get enum-level rename_all attribute
    let rename_all = get_serde_rename_all(&item_enum.attrs);

    for variant in &item_enum.variants {
        let original_name = variant.ident.to_string();

        // Get the variant name (considering rename and rename_all)
        let variant_name = get_field_json_name(&variant.attrs, &original_name, &rename_all);
        enum_variants.push(variant_name);
    }

    TypeInfo {
        name,
        fields: vec![],
        is_array: false,
        is_enum: true,
        enum_variants,
        generic_args: vec![], // Enums defined in source don't have concrete generic args yet
    }
}

/// Get the serde rename_all attribute from struct attributes
fn get_serde_rename_all(attrs: &[Attribute]) -> Option<String> {
    for attr in attrs {
        if attr.path().is_ident("serde") {
            if let Ok(meta_list) = attr.meta.require_list() {
                // Parse the nested meta items
                if let Ok(nested) = meta_list.parse_args_with(
                    syn::punctuated::Punctuated::<Meta, syn::Token![,]>::parse_terminated,
                ) {
                    for meta in nested {
                        if let Meta::NameValue(nv) = meta {
                            if nv.path.is_ident("rename_all") {
                                if let Expr::Lit(expr_lit) = &nv.value {
                                    if let Lit::Str(lit_str) = &expr_lit.lit {
                                        return Some(lit_str.value());
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    None
}

/// Get the JSON name for a field, considering serde rename and rename_all
fn get_field_json_name(
    attrs: &[Attribute],
    original_name: &str,
    rename_all: &Option<String>,
) -> String {
    // First check for field-level rename attribute
    for attr in attrs {
        if attr.path().is_ident("serde") {
            if let Ok(meta_list) = attr.meta.require_list() {
                if let Ok(nested) = meta_list.parse_args_with(
                    syn::punctuated::Punctuated::<Meta, syn::Token![,]>::parse_terminated,
                ) {
                    for meta in nested {
                        if let Meta::NameValue(nv) = meta {
                            if nv.path.is_ident("rename") {
                                if let Expr::Lit(expr_lit) = &nv.value {
                                    if let Lit::Str(lit_str) = &expr_lit.lit {
                                        return lit_str.value();
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    // Apply rename_all transformation if present
    if let Some(convention) = rename_all {
        return apply_rename_all(original_name, convention);
    }

    original_name.to_string()
}

/// Check if a field should be skipped based on serde attributes
fn should_skip_field(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if attr.path().is_ident("serde") {
            if let Ok(meta_list) = attr.meta.require_list() {
                if let Ok(nested) = meta_list.parse_args_with(
                    syn::punctuated::Punctuated::<Meta, syn::Token![,]>::parse_terminated,
                ) {
                    for meta in nested {
                        if let Meta::Path(path) = meta {
                            if path.is_ident("skip") || path.is_ident("skip_serializing") {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

/// Check if a field is flattened based on serde attributes
fn is_flattened_field(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if attr.path().is_ident("serde") {
            if let Ok(meta_list) = attr.meta.require_list() {
                if let Ok(nested) = meta_list.parse_args_with(
                    syn::punctuated::Punctuated::<Meta, syn::Token![,]>::parse_terminated,
                ) {
                    for meta in nested {
                        if let Meta::Path(path) = meta {
                            if path.is_ident("flatten") {
                                return true;
                            }
                        }
                    }
                }
            }
        }
    }
    false
}

/// Extract just the base type name from a Type (e.g., "User" from "User" or "Option<User>")
fn type_to_base_name(ty: &Type) -> String {
    match ty {
        Type::Path(type_path) => {
            if let Some(segment) = type_path.path.segments.last() {
                let type_name = segment.ident.to_string();

                // If it's Option<T>, extract T
                if type_name == "Option" {
                    if let syn::PathArguments::AngleBracketed(args) = &segment.arguments {
                        if let Some(syn::GenericArgument::Type(inner_type)) = args.args.first() {
                            return type_to_base_name(inner_type);
                        }
                    }
                }

                type_name
            } else {
                "unknown".to_string()
            }
        }
        _ => "unknown".to_string(),
    }
}

/// Apply rename_all convention to a field name
fn apply_rename_all(name: &str, convention: &str) -> String {
    match convention {
        "lowercase" => name.to_lowercase(),
        "UPPERCASE" => name.to_uppercase(),
        "PascalCase" => to_pascal_case(name),
        "camelCase" => to_camel_case(name),
        "snake_case" => to_snake_case(name),
        "SCREAMING_SNAKE_CASE" => to_snake_case(name).to_uppercase(),
        "kebab-case" => to_kebab_case(name),
        "SCREAMING-KEBAB-CASE" => to_kebab_case(name).to_uppercase(),
        _ => name.to_string(),
    }
}

fn to_pascal_case(s: &str) -> String {
    let mut result = String::new();
    let mut capitalize_next = true;

    for ch in s.chars() {
        if ch == '_' {
            capitalize_next = true;
        } else if capitalize_next {
            result.push(ch.to_ascii_uppercase());
            capitalize_next = false;
        } else {
            result.push(ch);
        }
    }
    result
}

fn to_camel_case(s: &str) -> String {
    let pascal = to_pascal_case(s);
    let mut chars = pascal.chars();
    match chars.next() {
        None => String::new(),
        Some(first) => first.to_lowercase().collect::<String>() + chars.as_str(),
    }
}

fn to_snake_case(s: &str) -> String {
    let mut result = String::new();

    for (i, ch) in s.chars().enumerate() {
        if ch.is_uppercase() && i > 0 {
            result.push('_');
            result.push(ch.to_ascii_lowercase());
        } else {
            result.push(ch.to_ascii_lowercase());
        }
    }
    result
}

fn to_kebab_case(s: &str) -> String {
    to_snake_case(s).replace('_', "-")
}

/// Convert a syn::Type to a string representation
fn type_to_string(ty: &Type) -> String {
    match ty {
        Type::Path(type_path) => {
            let segments: Vec<String> = type_path
                .path
                .segments
                .iter()
                .map(|seg| {
                    let ident = seg.ident.to_string();
                    if let syn::PathArguments::AngleBracketed(args) = &seg.arguments {
                        let inner_args: Vec<String> = args
                            .args
                            .iter()
                            .filter_map(|arg| {
                                if let syn::GenericArgument::Type(inner_ty) = arg {
                                    Some(type_to_string(inner_ty))
                                } else {
                                    None
                                }
                            })
                            .collect();
                        if inner_args.is_empty() {
                            ident
                        } else {
                            format!("{}<{}>", ident, inner_args.join(", "))
                        }
                    } else {
                        ident
                    }
                })
                .collect();
            segments.join("::")
        }
        Type::Reference(type_ref) => type_to_string(&type_ref.elem),
        _ => "unknown".to_string(),
    }
}

/// Check if a struct has Serialize/Deserialize derives
pub fn has_serde_derives(item_struct: &ItemStruct) -> bool {
    check_serde_derives(&item_struct.attrs)
}

/// Check if an enum has Serialize/Deserialize derives
pub fn has_serde_derives_enum(item_enum: &ItemEnum) -> bool {
    check_serde_derives(&item_enum.attrs)
}

/// Internal function to check for serde derives on attributes
fn check_serde_derives(attrs: &[Attribute]) -> bool {
    for attr in attrs {
        if attr.path().is_ident("derive") {
            if let Ok(derive_input) = attr.parse_args::<syn::Meta>() {
                let derive_str = format!("{:?}", derive_input);
                if derive_str.contains("Serialize") || derive_str.contains("Deserialize") {
                    return true;
                }
            }

            // Alternative parsing
            if let syn::Meta::List(meta_list) = &attr.meta {
                let tokens_str = meta_list.tokens.to_string();
                if tokens_str.contains("Serialize") || tokens_str.contains("Deserialize") {
                    return true;
                }
            }
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use syn::parse_quote;

    #[test]
    fn test_to_camel_case() {
        assert_eq!(to_camel_case("user_id"), "userId");
        assert_eq!(to_camel_case("first_name"), "firstName");
        assert_eq!(to_camel_case("last_name"), "lastName");
        assert_eq!(to_camel_case("api_key"), "apiKey");
        assert_eq!(to_camel_case("some_long_field_name"), "someLongFieldName");
    }

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(to_pascal_case("user_id"), "UserId");
        assert_eq!(to_pascal_case("first_name"), "FirstName");
        assert_eq!(to_pascal_case("api_key"), "ApiKey");
    }

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("userId"), "user_id");
        assert_eq!(to_snake_case("firstName"), "first_name");
        assert_eq!(to_snake_case("APIKey"), "a_p_i_key");
        assert_eq!(to_snake_case("already_snake"), "already_snake");
    }

    #[test]
    fn test_to_kebab_case() {
        assert_eq!(to_kebab_case("user_id"), "user-id");
        assert_eq!(to_kebab_case("first_name"), "first-name");
        assert_eq!(to_kebab_case("userId"), "user-id");
    }

    #[test]
    fn test_apply_rename_all() {
        assert_eq!(apply_rename_all("user_id", "camelCase"), "userId");
        assert_eq!(apply_rename_all("user_id", "PascalCase"), "UserId");
        assert_eq!(apply_rename_all("user_id", "kebab-case"), "user-id");
        assert_eq!(
            apply_rename_all("user_id", "SCREAMING_SNAKE_CASE"),
            "USER_ID"
        );
        assert_eq!(
            apply_rename_all("user_id", "SCREAMING-KEBAB-CASE"),
            "USER-ID"
        );
        assert_eq!(apply_rename_all("UserID", "lowercase"), "userid");
        assert_eq!(apply_rename_all("user_id", "UPPERCASE"), "USER_ID");
        assert_eq!(apply_rename_all("user_id", "snake_case"), "user_id");
    }

    #[test]
    fn test_extract_struct_info_basic() {
        let parsed: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct User {
                pub id: u64,
                pub name: String,
            }
        };

        let type_info = extract_struct_info(&parsed);

        assert_eq!(type_info.name, "User");
        assert_eq!(type_info.fields.len(), 2);
        assert_eq!(type_info.is_array, false);

        assert_eq!(type_info.fields[0].name, "id");
        assert_eq!(type_info.fields[0].type_name, "u64");
        assert!(type_info.fields[0].required);

        assert_eq!(type_info.fields[1].name, "name");
        assert_eq!(type_info.fields[1].type_name, "String");
        assert!(type_info.fields[1].required);
    }

    #[test]
    fn test_extract_struct_info_with_option() {
        let parsed: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct User {
                pub id: u64,
                pub email: Option<String>,
            }
        };

        let type_info = extract_struct_info(&parsed);

        assert_eq!(type_info.fields.len(), 2);
        assert!(type_info.fields[0].required);
        assert!(!type_info.fields[1].required);
        assert_eq!(type_info.fields[1].type_name, "String");
    }

    #[test]
    fn test_extract_struct_info_with_rename_all() {
        let parsed: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            #[serde(rename_all = "camelCase")]
            pub struct UserProfile {
                pub user_id: u64,
                pub first_name: String,
                pub last_name: String,
            }
        };

        let type_info = extract_struct_info(&parsed);

        assert_eq!(type_info.fields.len(), 3);
        assert_eq!(type_info.fields[0].name, "userId");
        assert_eq!(type_info.fields[1].name, "firstName");
        assert_eq!(type_info.fields[2].name, "lastName");
    }

    #[test]
    fn test_extract_struct_info_with_field_rename() {
        let parsed: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct Product {
                pub id: u64,
                #[serde(rename = "productName")]
                pub name: String,
            }
        };

        let type_info = extract_struct_info(&parsed);

        assert_eq!(type_info.fields.len(), 2);
        assert_eq!(type_info.fields[0].name, "id");
        assert_eq!(type_info.fields[1].name, "productName");
    }

    #[test]
    fn test_extract_struct_info_with_skip() {
        let parsed: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct User {
                pub id: u64,
                pub name: String,
                #[serde(skip)]
                pub password_hash: String,
                #[serde(skip_serializing)]
                pub internal_field: String,
            }
        };

        let type_info = extract_struct_info(&parsed);

        assert_eq!(type_info.fields.len(), 2);
        assert_eq!(type_info.fields[0].name, "id");
        assert_eq!(type_info.fields[1].name, "name");
        // password_hash and internal_field should be skipped
    }

    #[test]
    fn test_has_serde_derives() {
        let with_serde: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct User {
                pub id: u64,
            }
        };

        let without_serde: ItemStruct = parse_quote! {
            #[derive(Debug, Clone)]
            pub struct User {
                pub id: u64,
            }
        };

        assert!(has_serde_derives(&with_serde));
        assert!(!has_serde_derives(&without_serde));
    }

    #[test]
    fn test_rename_all_overridden_by_field_rename() {
        let parsed: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            #[serde(rename_all = "camelCase")]
            pub struct User {
                pub user_id: u64,
                #[serde(rename = "custom_name")]
                pub full_name: String,
            }
        };

        let type_info = extract_struct_info(&parsed);

        assert_eq!(type_info.fields[0].name, "userId"); // uses rename_all
        assert_eq!(type_info.fields[1].name, "custom_name"); // uses field rename
    }

    #[test]
    fn test_flatten_support() {
        // Create base struct
        let base_struct: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct BaseFields {
                pub id: u64,
                pub created_at: String,
            }
        };

        // Create struct with flatten
        let with_flatten: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct User {
                #[serde(flatten)]
                pub base: BaseFields,
                pub name: String,
            }
        };

        // First extract base struct
        let base_info = extract_struct_info(&base_struct);
        let mut all_structs = HashMap::new();
        all_structs.insert("BaseFields".to_string(), base_info);

        // Extract struct with flatten
        let user_info = extract_struct_info_with_flatten(&with_flatten, &all_structs);

        // Should have 3 fields: id, created_at (from BaseFields), and name
        assert_eq!(user_info.fields.len(), 3);
        assert_eq!(user_info.fields[0].name, "id");
        assert_eq!(user_info.fields[0].type_name, "u64");
        assert_eq!(user_info.fields[1].name, "created_at");
        assert_eq!(user_info.fields[1].type_name, "String");
        assert_eq!(user_info.fields[2].name, "name");
        assert_eq!(user_info.fields[2].type_name, "String");
    }

    #[test]
    fn test_flatten_with_option() {
        // Create base struct
        let base_struct: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct Timestamps {
                pub created_at: String,
                pub updated_at: Option<String>,
            }
        };

        // Create struct with optional flatten
        let with_flatten: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            pub struct Record {
                pub id: u64,
                #[serde(flatten)]
                pub timestamps: Option<Timestamps>,
            }
        };

        // Extract base struct
        let timestamps_info = extract_struct_info(&base_struct);
        let mut all_structs = HashMap::new();
        all_structs.insert("Timestamps".to_string(), timestamps_info);

        // Extract struct with flatten
        let record_info = extract_struct_info_with_flatten(&with_flatten, &all_structs);

        // Should have 3 fields: id, created_at, updated_at
        assert_eq!(record_info.fields.len(), 3);
        assert_eq!(record_info.fields[0].name, "id");
        assert_eq!(record_info.fields[1].name, "created_at");
        assert_eq!(record_info.fields[2].name, "updated_at");
    }

    #[test]
    fn test_flatten_with_rename_all() {
        // Create base struct with rename_all
        let base_struct: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            #[serde(rename_all = "camelCase")]
            pub struct CommonFields {
                pub user_id: u64,
                pub first_name: String,
            }
        };

        // Create struct with flatten
        let with_flatten: ItemStruct = parse_quote! {
            #[derive(Serialize, Deserialize)]
            #[serde(rename_all = "snake_case")]
            pub struct UserProfile {
                #[serde(flatten)]
                pub common: CommonFields,
                pub LastLogin: String,
            }
        };

        // Extract base struct
        let common_info = extract_struct_info(&base_struct);
        let mut all_structs = HashMap::new();
        all_structs.insert("CommonFields".to_string(), common_info);

        // Extract struct with flatten
        let profile_info = extract_struct_info_with_flatten(&with_flatten, &all_structs);

        // Should have 3 fields
        assert_eq!(profile_info.fields.len(), 3);
        // Fields from flattened struct keep their original naming
        assert_eq!(profile_info.fields[0].name, "userId"); // from CommonFields camelCase
        assert_eq!(profile_info.fields[1].name, "firstName"); // from CommonFields camelCase
                                                              // Own fields use parent's rename_all
        assert_eq!(profile_info.fields[2].name, "last_login"); // snake_case applied
    }
}
