;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "mtg-json:lib:srcdst")
     (dante-project-root . "~/haskell/mtg")
     (compile-command    . "cabal new-build mtg-json:lib:srcdst -fdevelop")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;