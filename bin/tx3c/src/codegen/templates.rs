//! Built-in templates compiled into tx3c.
//!
//! A template is one use case in one language, such as `rust-client`. Its
//! files live in `bin/tx3c/templates/<name>/` and choose their language
//! backend through the helpers they call. `tx3c codegen --template <name>`
//! renders a built-in template; `--template <dir>` renders a custom one.
//! Names match trix's built-in plugin names, so trix passes them through.

/// One template file, addressed by its path relative to the template root.
pub struct File {
    pub path: &'static str,
    pub content: &'static str,
}

macro_rules! template {
    ($name:literal: $($path:literal),+ $(,)?) => {
        ($name, &[$(File {
            path: $path,
            content: include_str!(concat!("../../templates/", $name, "/", $path)),
        }),+])
    };
}

/// Every built-in template, by name.
pub const BUILT_IN: &[(&str, &[File])] = &[
    template!("ts-client": "README.md.hbs", "package.json.hbs", "protocol.ts.hbs", "tsconfig.json.hbs"),
    template!("rust-client": "Cargo.toml.hbs", "README.md.hbs", "lib.rs.hbs"),
    template!("python-client": "README.md.hbs", "__init__.py.hbs", "requirements.txt.hbs"),
    template!("go-client": "README.md.hbs", "go.mod.hbs", "protocol.go.hbs"),
];

/// The built-in template called `name`, if there is one.
pub fn built_in(name: &str) -> Option<&'static [File]> {
    BUILT_IN
        .iter()
        .find(|(candidate, _)| *candidate == name)
        .map(|(_, files)| *files)
}

/// Comma-separated built-in template names, for error messages.
pub fn names() -> String {
    BUILT_IN
        .iter()
        .map(|(name, _)| *name)
        .collect::<Vec<_>>()
        .join(", ")
}

#[cfg(test)]
mod tests {
    use std::{collections::BTreeMap, fs, path::Path};

    use super::*;

    /// The registry must list exactly the template directories and files on
    /// disk, so a template added without being registered fails the tests.
    #[test]
    fn registry_matches_the_template_directories() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("templates");

        let mut on_disk: Vec<String> = fs::read_dir(&root)
            .unwrap()
            .map(|entry| entry.unwrap().file_name().to_string_lossy().into_owned())
            .collect();
        on_disk.sort();
        let mut registered: Vec<String> =
            BUILT_IN.iter().map(|(name, _)| name.to_string()).collect();
        registered.sort();
        assert_eq!(registered, on_disk, "template directories");

        for (name, files) in BUILT_IN {
            let dir = root.join(name);
            let expected: BTreeMap<String, String> = walkdir::WalkDir::new(&dir)
                .into_iter()
                .map(Result::unwrap)
                .filter(|entry| entry.file_type().is_file())
                .map(|entry| {
                    let relative = entry.path().strip_prefix(&dir).unwrap();
                    (
                        relative.to_string_lossy().replace('\\', "/"),
                        fs::read_to_string(entry.path()).unwrap(),
                    )
                })
                .collect();
            let listed: BTreeMap<String, String> = files
                .iter()
                .map(|file| (file.path.to_string(), file.content.to_string()))
                .collect();
            assert_eq!(listed, expected, "files of template {name}");
        }
    }
}
