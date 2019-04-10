;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "mtg-json:lib:mtg-json")
     (dante-project-root . "~/haskell/mtg")
     (compile-command    . "cabal new-build mtg-json:lib:mtg-json -fdevelop")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;