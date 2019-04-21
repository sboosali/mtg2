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

Open          ?=xdg-open
ClipboardCopy ?=xclip -selection clipboard

CheckCabal            ?=$(Cabal) check
CheckBash             ?=shellcheck
CheckStaticExecutable ?=! ldd
  # ^ TODO on OSX, otool -o

#------------------------------------------------#
# Paths...

DataDirectory ?=./data

ShareDirectory ?=./gitignore/share
# ^ gitignore'd during development.
#  for a release, override to « ./share ».

BashCompletionDirectory ?=$(ShareDirectory)/bash-completion
ZshCompletionDirectory  ?=$(ShareDirectory)/zsh-completion
FishCompletionDirectory ?=$(ShareDirectory)/fish-completion

#------------------------------------------------#
# Miscellaneous...

HaskellRepository ?=

#------------------------------------------------#
# Options...

CabalOptions ?=

#------------------------------------------------#
# Subcommands...

CabalBuild   ?=$(Cabal) new-build $(CabalOptions)
CabalRun     ?=$(Cabal) new-run $(CabalOptions)
CabalInstall ?=$(Cabal) -v --overwrite-policy=always new-install

CabalTest    ?=$(Cabal) new-test --enable-tests $(CabalOptions)
CabalBench   ?=$(Cabal) new-bench --enable-benchmarks $(CabalOptions)
CabalDocs    ?=$(Cabal) new-haddock --enable-documentation $(CabalOptions)

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

	$(CabalRun) -f"+develop" "exe:mtg-json" -- --help

.PHONY: mtg-json

#------------------------------------------------#

mtg-sql:

	$(CabalRun) -f"+develop" "exe:mtg-sql" -- --help

.PHONY: mtg-sql

#------------------------------------------------#

mtg-csv:

	$(CabalRun) -f"+develop" "exe:mtg-csv" -- --help

.PHONY: mtg-csv

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
# Programs...

install-mtg-json:

	@printf "\n%s\n" "========================================"

	$(CabalInstall) "exe:mtg-json"

	@printf "\n%s\n" "========================================"

	@mkdir -p "$(BashCompletionDirectory)"
	mtg-json --bash-completion-script `which mtg-json` > "$(BashCompletionDirectory)/mtg-json.bash"

	@mkdir -p "$(ZshCompletionDirectory)"
	mtg-json --zsh-completion-script `which mtg-json` > "$(ZshCompletionDirectory)/mtg-json.zsh"

	@mkdir -p "$(FishCompletionDirectory)"
	mtg-json --fish-completion-script `which mtg-json` > "$(FishCompletionDirectory)/mtg-json.fish"

	@printf "\n%s\n" "========================================"

	$(CheckStaticExecutable) `which mtg-json` || true

	@printf "\n%s\n" "========================================"

	source `readlink -f "$(BashCompletionDirectory)/mtg-json.bash"`

	@printf "\n%s\n" "========================================"

.PHONY: install-mtg-json

#------------------------------------------------#

install-mtg-sql:

	$(CabalInstall) "exe:mtg-sql"

.PHONY: install-mtg-sql

#------------------------------------------------#

install-mtg-csv:

	$(CabalInstall) "exe:mtg-csv"

.PHONY: install-mtg-csv

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
# Cabal...

docs-mtg-json:

	stack haddock mtg-json
	$(Open) ./mtg-json/.stack-work/dist/x86_64-linux/Cabal-2.4.0.1/doc/html/mtg-json/index.html

.PHONY: docs-mtg-json

#------------------------------------------------#

update:

	cabal new-update $(HaskellRepository)

.PHONY: update

#------------------------------------------------#
# Miscellaneous...



#------------------------------------------------#
# EOF -------------------------------------------#
#------------------------------------------------#