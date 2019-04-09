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

	$(Cabal) new-test $(CabalOptions) $(CabalTargets)

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

	$(Cabal) new-build $(CabalOptions) -f"+develop" "lib:mtg-types"

.PHONY: mtg-types

#------------------------------------------------#

mtg-json:

	$(Cabal) new-build $(CabalOptions) -f"+develop" "lib:mtg-json"

.PHONY: mtg-json

#------------------------------------------------#

mtg-sql:

	$(Cabal) new-build $(CabalOptions) -f"+develop" "lib:mtg-sql"

.PHONY: mtg-sql

#------------------------------------------------#

mtg-csv:

	$(Cabal) new-build $(CabalOptions) -f"+develop" "lib:mtg-csv"

.PHONY: mtg-csv

#------------------------------------------------#

mtg-scryfall:

	$(Cabal) new-build $(CabalOptions) -f"+develop" "lib:mtg-scryfall"

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