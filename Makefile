project = TSLogAnalyzer

.PHONY: all build clean configure haddock nix-shell repl tags

all: build haddock tags

build: configure
	cabal new-build -j8

configure:
	cabal new-configure

clean:
	-@cabal clean
	-@rm -rf *.tix
	-@rm -rf .hpc
	-@rm -rf cabal.project.local
	-@rm -rf dist-newstyle

haddock:
	cabal haddock --hyperlink-source

default.nix: $(project).cabal
	cabal2nix ./. > default.nix

nix-shell: default.nix
	make clean
	nix-shell --run $$SHELL
	make clean

repl:
	cabal new-repl lib:$(project)

tags:
	hasktags -e .
