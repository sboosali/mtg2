#------------------------------------------------#
# Makefile Settings -----------------------------#
#------------------------------------------------#

SHELL=bash

.EXPORT_ALL_VARIABLES:

#------------------------------------------------#
# Makefile Variables ----------------------------#
#------------------------------------------------#
# Components...

Package      ?=mtg

CabalTargets ?=all
CabalTarget  ?=$(Package):lib:$(Package)
CabalProgram ?=$(Package):exe:$(Package)

#------------------------------------------------#
# Programs...

Cabal ?=cabal
Ghc   ?=ghc

#------------------------------------------------#
# Paths...

DataDirectory ?=./data

#------------------------------------------------#
# Miscellaneous...

HaskellRepository ?=

#------------------------------------------------#
# Options...

CabalOptions ?=

#------------------------------------------------#
# Subcommands...

CabalBuild ?=cabal new-build $(CabalOptions)
CabalTest  ?=cabal new-test --enable-tests $(CabalOptions)
CabalBench ?=cabal new-bench --enable-benchmarks $(CabalOptions)

#------------------------------------------------#
# Environment Variables...

LC_ALL=C.UTF-8

#------------------------------------------------#
# Makefile Targets: Standard --------------------#
#------------------------------------------------#

build:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Building..."

	$(Cabal) new-build $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: build

#------------------------------------------------#

check:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Testing..."

	$(CabalTest) $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: check

#------------------------------------------------#

install:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Installing..."

	$(Cabal) new-install $(CabalOptions) $(CabalProgram)

	@printf "\n%s\n" "========================================"

.PHONY: install

#------------------------------------------------#

dist:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Archiving..."

	$(Cabal) new-sdist $(CabalOptions) $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: dist

#------------------------------------------------#

clean:

	rm -rf dist/ dist-newstyle/ .sboo/
	rm -f *.project.local .ghc.environment.*

	rm -rf "result/" result-*/
	find .  -type f  -name 'TAGS'  -exec  rm -f '{}'  \+

#	rm -fr _
#	killall _

.PHONY: clean

#------------------------------------------------#

all: mtg-types mtg-json mtg-csv mtg-sql dist

.PHONY: all

#------------------------------------------------#
# Makefile Targets: Custom ----------------------#
#------------------------------------------------#
# « mtg-* » (Haskell Packages)...

mtg-types:

	$(CabalBuild) -f"+develop" "lib:mtg-types"

.PHONY: mtg-types

#------------------------------------------------#

mtg-json:

	$(CabalBuild) -f"+develop" "lib:mtg-json"

.PHONY: mtg-json

#------------------------------------------------#

mtg-sql:

	$(CabalBuild) -f"+develop" "lib:mtg-sql"

.PHONY: mtg-sql

#------------------------------------------------#

mtg-csv:

	$(CabalBuild) -f"+develop" "lib:mtg-csv"

.PHONY: mtg-csv

#------------------------------------------------#

mtg-scryfall:

	$(CabalBuild) -f"+develop" "lib:mtg-scryfall"

.PHONY: mtg-scryfall

#------------------------------------------------#
# Data...

fetch:

	mkdir -p "$(DataDirectory)/json"
	mkdir -p "$(DataDirectory)/gitignored"

	(cd "$(DataDirectory)/gitignored" && wget "https://mtgjson.com/json/Keywords.json")
	(cd "$(DataDirectory)/json"       && wget "https://mtgjson.com/json/CardTypes.json")
	(cd "$(DataDirectory)/json"       && wget "https://mtgjson.com/json/Vintage.json.zip")

.PHONY: fetch

#------------------------------------------------#
# Tests...

checkdocs:
	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Testing Documentation..."

	$(Cabal) new-test $(CabalOptions) --enable-tests "mtg-types:test:doc"
	$(Cabal) new-test $(CabalOptions) --enable-tests "mtg-json:test:doc"
	$(Cabal) new-test $(CabalOptions) --enable-tests "mtg-csv:test:doc"
	$(Cabal) new-test $(CabalOptions) --enable-tests "mtg-sql:test:doc"

	@printf "\n%s\n" "========================================"

	@printf "%s\n\n" "Generating Documentation..."

	$(Cabal) new-haddock $(CabalTargets)

	@printf "\n%s\n" "========================================"

.PHONY: checkdocs

#------------------------------------------------#

check-mtg-types:

	$(CabalTest) "mtg-types:test:doc"

.PHONY: check-mtg-types

#------------------------------------------------#

check-mtg-json:

	$(CabalTest) "mtg-json:test:doc"

.PHONY: check-mtg-json

#------------------------------------------------#

check-mtg-sql:

	$(CabalTest) "mtg-sql:test:doc"

.PHONY: check-mtg-sql

#------------------------------------------------#

check-mtg-csv:

	$(CabalTest) "mtg-csv:test:doc"

.PHONY: check-mtg-csv

#------------------------------------------------#

check-mtg-scryfall:

	$(CabalTest) "mtg-scryfall:test:doc"

.PHONY: check-mtg-scryfall

#------------------------------------------------#
# Cabal...

docs:

	$(Cabal) new-haddock $(CabalTargets)

.PHONY: docs

#------------------------------------------------#

update:

	cabal new-update $(HaskellRepository)

.PHONY: update

#------------------------------------------------#
# Miscellaneous...



#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#