# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

## [0.14.4](https://github.com/tx3-lang/tx3/releases/tag/tx3c-v0.14.4) - 2026-02-24

### Added

- *(tx3c)* add comprehensive support for TII format
- *(tx3c)* support build profiles ([#275](https://github.com/tx3-lang/tx3/pull/275))
- *(tx3c)* improve build command options ([#274](https://github.com/tx3-lang/tx3/pull/274))
- introduce tx3c binary
- implement naive eval mechanism ([#3](https://github.com/tx3-lang/tx3/pull/3))

### Fixed

- *(bin)* support adding profiles without needing an env file
- ensure a default "local" profile is present in the emitted TII ([#281](https://github.com/tx3-lang/tx3/pull/281))
- *(tx3c)* improve parsing of non-string dotenv values
- *(tx3c)* enable version command

### Other

- fix some lint warnings
- *(tx3c)* improve module organization
- remove experimental label ([#73](https://github.com/tx3-lang/tx3/pull/73))
- add better examples to README ([#8](https://github.com/tx3-lang/tx3/pull/8))
- define scope
