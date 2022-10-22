# lineman

[![MIT license](https://img.shields.io/badge/license-MIT-blue.svg)](LICENSE)
[![Haskell-CI](https://github.com/willbasky/lineman/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/willbasky/lineman/actions/workflows/haskell-ci.yml)

## Description

Lineman traverses directory recursively and run command by condition.

For example supposed there a many haskell projects with building files (like `.stack-work` or `dist-newstyle`) that take enormous space of disk. It takes time to purge them all manually. The lineman is able to carry out this job automatically. All you need is to configure lineman's config and run it.

## Installation

    git clone https://github.com/willbasky/lineman
    cd lineman

And either

    stack install

or

    cabal install

## Usage

    lineman /path/to/toml/config/

## Toml config

[Config example in file](./config.toml)

The description of config fields:

```toml

# targetDirectory where you plan lineman to run.
# It is able to consume 'rel', 'abs' and '~'' paths.
targetDirectory = "/path/to/directory/for/traversing/"

# configElement is a group that contains conditions and command with args.
# The conditions define directories where lineman run.
[[configElement]]

# Target directory has files
hasFiles         = ["stack.yaml"]

# Target directory has directories
hasDirectories   = [".stack-work"]

# Target directory has extensions.
# It consume exts with and without '.'
hasExtensions    = []

# Command that run in detected directories.
command          = "stack"

# Command's arguments.
args             = ["purge"]

```

According to the config `lineman`:
- traverses target directory recursively
- finds directories that have `stack.yaml` file and `.stack-work` directory.
- apply `stack purge` in the found directories.

The number of `[[configElement]]` can be unlimited. They run one by one.

## Environment variables

There are envvars for managing `lineman` behavior.

- `LINEMAN_SEVERITY` is for severity set. It receives `Error`, `Warning`, `Info`, `Debug` strings. By default it is set to `Info`.
- `LINEMAN_ASYNC` is for running particular command asynchronously. It is experimental feature hence it is buggy. By default it is turned off.
- `LINEMAN_RICH_LOG` is for rich log output. By default it is simple.

How to pass envvar?

    export LINEMAN_SEVERITY=Error && lineman /path-to-config/config.toml
