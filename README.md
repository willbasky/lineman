# lineman

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)

## Description

Lineman traverses directory recursively and run command by condition

## Installation

    git clone https://github.com/willbasky/lineman
    cd lineman

And either

    stack install

or

    cabal install

## Usage

    lineman /path/to/toml/config/

## Toml config example

```toml

# targetDirectory consume 'rel', 'abs' and '~'' paths
targetDirectory = "/path/to/directory/for/traversing/"

[[configElement]]
# Target directory has files
hasFiles         = ["readme.txt"]

# Target directory has directories
hasDirectories   = []

# Target directory has extensions.
# It consume exts with and without '.'
hasExtensions    = []

# Command to run in searched directories
command          = "rm"

# Command's arguments
args             = ["readme.txt"]

```

According to the config `lineman`
- traverses target directory recursively
- finds directories that have `readme.txt` file
- apply `rm readme.txt` in found directories

Groups of `[[configElement]]` can be unlimited. They are run one by one.

## Environment variables

There are envvars for managing `lineman` behavior.

- `LINEMAN_SEVERITY` is for severity set. It receives `Error`, `Warning`, `Info`, `Debug` strings. By default it is set to `Info`.
- `LINEMAN_ASYNC` is for running particular command asynchronously. It is experimental feature hence it is buggy. By default it is turned off.
- `LINEMAN_RICH_LOG` is for rich log output. By default it is simple.

How to pass envvar?

    export LINEMAN_SEVERITY=Error && lineman /path-to-config/config.toml
