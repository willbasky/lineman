# lineman

[![BSD 3](https://img.shields.io/badge/license-BSD3-blue.svg)](LICENSE)
[![Haskell-CI](https://github.com/willbasky/lineman/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/willbasky/lineman/actions/workflows/haskell-ci.yml)

## Description

Lineman traverses directory recursively and run command by conditions.

For example supposed there a many haskell projects with building files (like `.stack-work` or `dist-newstyle`) that take enormous space of disk. It takes time to purge them all manually. The lineman is able to carry out this job automatically. All you need is to configure lineman's config and run it.

## Installation

    git clone https://github.com/willbasky/lineman
    cd lineman

And either

    cabal install

## Usage

    lineman /path/to/toml/config/

## Lineman's config

Lineman uses [dhall](https://dhall-lang.org) configuration. See [Config](./lineman.dhall) example.

## Term policy

- `Action` - a single command that run in a directory due to particular `conditions`
- `Conditions` describe the directory has to have to run `action`
- `EntryPoint` is a parent directory where `lineman` starts seeking for `targets`
- `Target` is a directory that aligns to `condition` 
- `Swarm` is a bunch of `actions` that match to `condition`
- `Hive` is a collection of `swarms`

## Features 

- Both `actions` in a `swarm` and `swarms` in a `hive` can be run concurrently or successively 
- Both `actions` and `swarms` can be interspersed with breaks
- `Lineman` starts seeking `targets` from `entryPoint` recursively and finds directories that have particular subdirectories, files or extensions. And then it run `Action` in found `targets`. 

## Use cases

- Clear build artefact in bunch of projects. 

