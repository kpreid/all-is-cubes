# Copyright 2020 Kevin Reid under the terms of the MIT License as detailed
# in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

# This Makefile contains no cleverness and is merely a set of useful commands
# bundled together with conventional names rather than a collection of shell
# scripts.

.PHONY: all lint test run-dev run-console run-game-server update try-publish-all

all:
	cargo build --package all-is-cubes
	(cd all-is-cubes-wasm && npm run-script build)
	# Server statically embeds results of wasm build, so it must be run last to be fresh, and
	# we must copy the files into the actual package directory.
	rsync --archive all-is-cubes-wasm/dist/ all-is-cubes-server/static-all-is-cubes-wasm/
	cargo build --package all-is-cubes-server

lint:
	cargo clippy  # ignores wasm-only code
	(cd all-is-cubes-wasm && cargo clippy --target=wasm32-unknown-unknown)
	# TODO: add JS linting

test:
	cargo test  # ignores wasm-only code
	(cd all-is-cubes-wasm && npm test)

run-dev:
	# Live-reloading webpack dev server; not a game server
	(cd all-is-cubes-wasm && npm start)

run-console: all
	cargo run --bin aic-console

run-game-server: all
	cargo run --bin aic-server

update:
	cargo update
	(cd all-is-cubes-wasm && npm update)

# This depends on `all` to ensure the server's static files are up to date.
try-publish-all: all
	(cd all-is-cubes && cargo publish --dry-run)
	# static-all-is-cubes-wasm counts as dirty despite .gitignore so we must use --allow-dirty
	(cd all-is-cubes-server && cargo publish --dry-run --allow-dirty)
