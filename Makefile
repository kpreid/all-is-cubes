# Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
# in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

# This Makefile contains no cleverness and is merely a set of useful commands
# bundled together with conventional names rather than a collection of shell
# scripts.

.PHONY: all lint test run-server update

all:
	cargo build  # ignores wasm-only code
	(cd all-is-cubes-wasm && npm run-script build)

lint:
	cargo clippy  # ignores wasm-only code
	(cd all-is-cubes-wasm && cargo clippy --target=wasm32-unknown-unknown)
	# TODO: add JS linting

test:
	cargo test  # ignores wasm-only code
	(cd all-is-cubes-wasm && npm test)

run-server:
	(cd all-is-cubes-wasm && npm start)

update:
	cargo update
	(cd all-is-cubes-wasm && npm update)
