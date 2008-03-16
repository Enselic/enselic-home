(defun gimp-fix-satic-function-definition ()
  (interactive)
  (search-forward "(")
  (backward-char)
  (c-hungry-delete-backwards)
  (insert " ")
  (search-backward " ")
  (search-backward " ")
  (newline-and-indent)

  (while (not (string-match ";" (thing-at-point 'line)))
    (next-line)
    (indent-or-dabbrev-completion))

  (search-forward ";")
  (backward-char)
  (delete-char 1)
  (newline)
  (insert "{")
  (indent-according-to-mode)
  (insert "\n\n}")
  (previous-line)
  (indent-or-dabbrev-completion))



(provide 'gimp-elisp)
