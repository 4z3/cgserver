# This makex file is just a convenient interface for some useful commands.

.PHONY: all build doc env install

all: cgserver.nix

build:
	load-env-cgserver-1 cabal build

doc:
	load-env-cgserver-1 cabal haddock --executable

env:
	nix-env -f env.nix -i

install:
	nix-env -f . -i

%.nix: %.cabal
	cabal2nix . > $@
