;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "mtg-json:exe:mtg-json")
     (dante-project-root . "~/haskell/mtg")
     (compile-command    . "cabal new-run mtg-json:exe:mtg-json -- ")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;