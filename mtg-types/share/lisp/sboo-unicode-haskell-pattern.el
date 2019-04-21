;;;###autoload
(defun sboo-haskell-insert-unicode-pattern-synonym (char name)

  "`insert' a « PatternSynonym » for Unicode Character CHAR with name NAME."

  (interactive (list
                (sboo-read-character)
                (sboo-read-character-name)
                ))

  (let* ((CHAR char)
         (NAME name)
         )

    (format "{- | @%c@ -}\npattern %s :: Char\npattern %s = '%c'\n" CHAR NAME NAME CHAR)))





(defun sboo-unicode-print-char (char)

  "Get the Unicode Character Database « 'name » of CHAR.

Inputs:

• CHAR — a character (an `integerp').

Examples:

    M-: (call-interactively #'sboo-unicode-print-char)
    Character: c
    ⇒ \"LATIN SMALL LETTER C\"

    M-: (sboo-unicode-print-char ?γ)
    ⇒ \"GREEK SMALL LETTER GAMMA\"

Related:

• `sboo-unicode-get-char-name'."

  (interactive (list (sboo-unicode-guess-char)))

  (let* ((CHAR char)
         (NAME (sboo-unicode-get-char-name CHAR))
         )

    (progn
      (when (called-interactively-p 'any)
        (message NAME))

      NAME)))

  