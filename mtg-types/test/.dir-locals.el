;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

((haskell-mode
  . (
     (dante-target       . "mtg-types:test:doc")
     (dante-project-root . "~/haskell/mtg/")
     (compile-command    . "cabal new-test --enable-tests mtg-types:test:doc")
     )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;