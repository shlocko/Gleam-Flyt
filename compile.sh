#!/usr/bin/env bash

set -e  # Exit on any error

gleam run

cp program.jef ../../rust/vm/program.jef

cd ../../rust/vm/
cargo run
