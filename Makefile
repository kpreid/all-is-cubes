# Copyright 2020-2021 Kevin Reid under the terms of the MIT License as detailed
# in the accompanying file README.md or <http://opensource.org/licenses/MIT>.

# This Makefile contains no cleverness and is merely a set of useful commands
# bundled together with conventional names rather than a collection of shell
# scripts.

.PHONY: all lint test run-dev run-game-server update try-publish-all actually-publish-all

all: all-is-cubes-wasm/node_modules/.bin/webpack
	cargo build --package all-is-cubes
	cargo build --package all-is-cubes-desktop
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
	cargo build --all-targets # Confirm benchmarks etc. compile, even if we don't run them

run-dev:
	# Live-reloading webpack dev server; not a game server
	(cd all-is-cubes-wasm && npm start)

run-game-server: all
	cargo run --bin aic-server

update:
	cargo update
	(cd all-is-cubes-wasm && npm -depth 9 update)

# This depends on `all` to ensure the server's static files are up to date.
try-publish-all: all
	(cd all-is-cubes && cargo publish --dry-run)
	(cd all-is-cubes-desktop && cargo publish --dry-run)
	# static-all-is-cubes-wasm counts as dirty despite .gitignore so we must use --allow-dirty
	(cd all-is-cubes-server && cargo publish --dry-run --allow-dirty)
actually-publish-all: all
	(cd all-is-cubes && cargo publish)
	sleep 10  # Let crates.io pick up the new all-is-cubes version or dependents will fail
	(cd all-is-cubes-desktop && cargo publish)
	# static-all-is-cubes-wasm counts as dirty despite .gitignore so we must use --allow-dirty
	(cd all-is-cubes-server && cargo publish --allow-dirty)

# Run npm install if needed to get build commands
all-is-cubes-wasm/node_modules/.bin/webpack:
	(cd all-is-cubes-wasm && npm install)
