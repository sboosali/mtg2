;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "mtg-types:lib:mtg-types")
     (dante-project-root . "~/haskell/mtg/")
     (compile-command    . "cabal new-build -fdevelop mtg-types:lib:mtg-types")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;