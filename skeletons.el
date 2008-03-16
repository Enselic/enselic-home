(defun retrieve-format-string-conveniently ()
  (interactive)
  (replace-regexp-in-string ", $" ""
                            (replace-regexp-in-string "." "%\\&, " (read-string "Shortented format string: "))))

(define-skeleton printf-debug-output
  "" '(retrieve-format-string-conveniently)
  > "printf(\"" str "\\n\""   ("Variable: " ", " str)   ");" \n)

(define-skeleton glib-debug-output
  "" '(retrieve-format-string-conveniently)
  > "g_printerr (\"%s: " str "\\n\", G_STRFUNC"   ("Variable: " ", " str)   ");" \n)

(define-skeleton wx-debug-output
  "" '(retrieve-format-string-conveniently)
  > "wxLogDebug(wxT(\"%s: " str "\\n\"), wxT(\"TODO:\") "   ("Variable: " ", " str)   ");" \n)

(define-skeleton wx-message-output
  "" '(retrieve-format-string-conveniently)
  > "wxLogMessage(wxT(\"%s: " str "\\n\"), wxT(\"TODO:\") "   ("Variable: " ", " str)   ");" \n)

(defun backward-two-chars-and-delete-one-char ()
  (interactive)
  (backward-char 2)
  (backward-delete-char))

(define-abbrev c-mode-abbrev-table    "dop"   ""  'glib-debug-output)
(define-abbrev c++-mode-abbrev-table  "dop"   ""  'glib-debug-output)
