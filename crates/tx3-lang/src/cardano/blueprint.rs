use std::collections::HashMap;
use std::collections::HashSet;

/// Decodes JSON pointer escapes (~1 -> /, ~0 -> ~).
fn decode_json_pointer_escapes(s: &str) -> String {
    s.replace("~1", "/").replace("~0", "~")
}

/// Strips known prefixes (Option, Pairs) and extracts the last segment
pub fn aiken_prettify_name(name: &str) -> String {
    let decoded = decode_json_pointer_escapes(name);

    if let Some(inner) = decoded.strip_prefix("Option$") {
        let inner_pretty = aiken_prettify_name(inner);
        return format!("Option{}", inner_pretty);
    }

    if let Some(inner) = decoded.strip_prefix("Option<") {
        let inner = inner.strip_suffix(">").unwrap_or(inner);
        let inner_pretty = aiken_prettify_name(inner);
        return format!("Option{}", inner_pretty);
    }

    if let Some(inner) = decoded.strip_prefix("Pairs$") {
        let parts: Vec<&str> = inner.split('_').collect();
        let prettified: Vec<String> = parts.iter().map(|p| aiken_prettify_name(p)).collect();
        return format!("Pairs{}", prettified.join(""));
    }

    if let Some(inner) = decoded.strip_prefix("Pairs<") {
        let inner = inner.strip_suffix(">").unwrap_or(inner);
        let parts: Vec<&str> = inner.split(',').collect();
        let prettified: Vec<String> = parts
            .iter()
            .map(|p| aiken_prettify_name(p.trim()))
            .collect();
        return format!("Pairs{}", prettified.join(""));
    }

    extract_last_segment(&decoded)
}

fn extract_last_segment(path: &str) -> String {
    path.rsplit('/').next().unwrap_or(path).to_string()
}

/// Converts a path to upper camel case handling nested generic types like Option and Pairs
pub fn path_to_upper_camel(name: &str) -> String {
    let decoded = decode_json_pointer_escapes(name);

    if let Some(inner) = decoded.strip_prefix("Option$") {
        let inner_camel = path_to_upper_camel(inner);
        return format!("Option{}", inner_camel);
    }

    if let Some(inner) = decoded.strip_prefix("Option<") {
        let inner = inner.strip_suffix(">").unwrap_or(inner);
        let inner_camel = path_to_upper_camel(inner);
        return format!("Option{}", inner_camel);
    }

    if let Some(inner) = decoded.strip_prefix("Pairs$") {
        let parts: Vec<&str> = inner.split('_').collect();
        let prettified: Vec<String> = parts.iter().map(|p| path_to_upper_camel(p)).collect();
        return format!("Pairs{}", prettified.join(""));
    }

    if let Some(inner) = decoded.strip_prefix("Pairs<") {
        let inner = inner.strip_suffix(">").unwrap_or(inner);
        let parts: Vec<&str> = inner.split(',').collect();
        let prettified: Vec<String> = parts
            .iter()
            .map(|p| path_to_upper_camel(p.trim()))
            .collect();
        return format!("Pairs{}", prettified.join(""));
    }

    path_segments_to_camel(&decoded)
}

fn path_segments_to_camel(path: &str) -> String {
    path.split('/')
        .map(|segment| {
            let mut chars = segment.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().to_string() + chars.as_str(),
            }
        })
        .collect()
}

pub fn build_aiken_name_mapping(keys: &[&str]) -> HashMap<String, String> {
    // collect simplified names and detect clashes
    let mut simple_to_originals: HashMap<String, Vec<String>> = HashMap::new();

    for &key in keys {
        let simple = aiken_prettify_name(key);
        simple_to_originals
            .entry(simple)
            .or_default()
            .push(key.to_string());
    }

    let mut result = HashMap::new();
    let mut used_names: HashSet<String> = HashSet::new();

    for &key in keys {
        let simple = aiken_prettify_name(key);
        let originals = simple_to_originals.get(&simple).unwrap();

        let final_name = if originals.len() == 1 {
            simple.clone()
        } else {
            path_to_upper_camel(key)
        };

        // ensure uniqueness
        let mut unique_name = final_name.clone();
        let mut counter = 1;
        while used_names.contains(&unique_name) {
            unique_name = format!("{}{}", final_name, counter);
            counter += 1;
        }

        used_names.insert(unique_name.clone());
        result.insert(key.to_string(), unique_name);
    }

    result
}

/// Sanitizes a type name to be a valid tx3 identifier
pub fn generic_sanitizer(name: &str) -> String {
    name.replace("~1", "_")
        .replace('/', "_")
        .replace('$', "_")
        .replace('<', "_")
        .replace('>', "")
        .replace(',', "_")
        .replace(' ', "")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_aiken_prettify_name_basic() {
        assert_eq!(aiken_prettify_name("cardano/address/Address"), "Address");
        assert_eq!(aiken_prettify_name("aiken/crypto/ScriptHash"), "ScriptHash");
        assert_eq!(aiken_prettify_name("types/SettingsDatum"), "SettingsDatum");

        assert_eq!(aiken_prettify_name("Int"), "Int");
        assert_eq!(aiken_prettify_name("Bool"), "Bool");
        assert_eq!(aiken_prettify_name("ByteArray"), "ByteArray");
    }

    #[test]
    fn test_aiken_prettify_name_option_and_pairs() {
        assert_eq!(
            aiken_prettify_name("Option$cardano/address/Address"),
            "OptionAddress"
        );
        assert_eq!(
            aiken_prettify_name("Option$cardano/address/StakeCredential"),
            "OptionStakeCredential"
        );

        assert_eq!(
            aiken_prettify_name("Pairs$cardano/assets/AssetName_Int"),
            "PairsAssetNameInt"
        );

        assert_eq!(
            aiken_prettify_name("Pairs$cardano/assets/PolicyId_Pairs$cardano/assets/AssetName_Int"),
            "PairsPolicyIdPairsAssetNameInt"
        );
    }

    #[test]
    fn test_aiken_prettify_name_url_encoded() {
        assert_eq!(aiken_prettify_name("cardano~1address~1Address"), "Address");
    }

    #[test]
    fn test_aiken_prettify_name_generics() {
        assert_eq!(
            aiken_prettify_name("Option<cardano/address/StakeCredential>"),
            "OptionStakeCredential"
        );

        assert_eq!(
            aiken_prettify_name("Pairs<cardano/assets/AssetName, Int>"),
            "PairsAssetNameInt"
        );
        assert_eq!(
            aiken_prettify_name("Pairs<cardano/assets/AssetName, Int>"),
            "PairsAssetNameInt"
        );
    }

    #[test]
    fn test_path_to_upper_camel_generics() {
        assert_eq!(
            path_to_upper_camel("Option<cardano/address/StakeCredential>"),
            "OptionCardanoAddressStakeCredential"
        );

        assert_eq!(
            path_to_upper_camel("Pairs<cardano/assets/AssetName, Int>"),
            "PairsCardanoAssetsAssetNameInt"
        );
    }

    #[test]
    fn test_build_aiken_name_mapping_no_clashes() {
        let keys = vec![
            "cardano/address/Address",
            "cardano/assets/AssetName",
            "types/SettingsDatum",
            "Int",
        ];
        let mapping = build_aiken_name_mapping(&keys);

        assert_eq!(mapping.get("cardano/address/Address").unwrap(), "Address");
        assert_eq!(
            mapping.get("cardano/assets/AssetName").unwrap(),
            "AssetName"
        );
        assert_eq!(mapping.get("types/SettingsDatum").unwrap(), "SettingsDatum");
        assert_eq!(mapping.get("Int").unwrap(), "Int");
    }

    #[test]
    fn test_build_aiken_name_mapping_with_clashes() {
        // Both cardano/transaction/Datum and types/Datum map into Datum
        let keys = vec!["cardano/transaction/Datum", "types/Datum"];
        let mapping = build_aiken_name_mapping(&keys);

        assert_eq!(
            mapping.get("cardano/transaction/Datum").unwrap(),
            "CardanoTransactionDatum"
        );
        assert_eq!(mapping.get("types/Datum").unwrap(), "TypesDatum");
    }
    #[test]
    fn test_generic_sanitizer() {
        assert_eq!(generic_sanitizer("simple"), "simple");
        assert_eq!(generic_sanitizer("path/to/something"), "path_to_something");
        assert_eq!(generic_sanitizer("Option<Int>"), "Option_Int");
        assert_eq!(generic_sanitizer("Map<K, V>"), "Map_K_V");
        assert_eq!(
            generic_sanitizer("some ~1 weird ~0 thing"),
            "some_weird~0thing"
        );
    }
}
