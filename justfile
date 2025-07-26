# List the available just commands
list:
    just --list

# Run the autodocodec example
run:
    nix-shell --run "cabal run autodojsonschema"

# Build the project
build:
    nix-shell --run "cabal build"

# Clean build artifacts
clean:
    nix-shell --run "cabal clean"

# Enter nix shell
shell:
    nix-shell

# Update cabal package list
update:
    nix-shell --run "cabal update"
