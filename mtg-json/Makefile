#------------------------------------------------#
# Makefile Settings -----------------------------#
#------------------------------------------------#

SHELL=bash

.EXPORT_ALL_VARIABLES:

#------------------------------------------------#
# Makefile Variables ----------------------------#
#------------------------------------------------#
# Components...

CabalTargets ?=all
CabalTarget  ?=mtg-json:lib:mtg-json
CabalProgram ?=mtg-json:exe:mtg-json

#------------------------------------------------#
# Programs...

Cabal ?=cabal
Ghc   ?=ghc

#------------------------------------------------#
# Paths...

DataDirectory ?=./data

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

all: build check install dist

.PHONY: all

#------------------------------------------------#

clean:

	rm -rf "result/" result-*/
	find .  -type f  -name 'TAGS'  -exec  rm -f '{}'  \+

#	rm -fr _
#	killall _

.PHONY: clean

#------------------------------------------------#
# Makefile Targets: Custom ----------------------#
#------------------------------------------------#

fetch:

	wget "https://mtgjson.com/json/Keywords.json"
	wget "https://mtgjson.com/json/CardTypes.json"
	wget "https://mtgjson.com/json/Vintage.json.zip"

.PHONY: fetch

#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#