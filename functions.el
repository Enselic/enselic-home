(require 'thingatpt)

(defun highlight-symbol-next-auto-on ()
  (interactive)
  (if (not highlight-symbol-mode)
      (highlight-symbol-mode t))
  (highlight-symbol-next))

(defun highlight-symbol-prev-auto-on ()
  (interactive)
  (if (not highlight-symbol-mode)
      (highlight-symbol-mode t))
  (highlight-symbol-prev))

(defun enable-hl-line-mode ()
  (hl-line-mode 1))

(defun goto-scratch-buffer ()
  (interactive)
  (switch-to-buffer "*Scratch*"))

(defun line-to-top-of-window ()
  (interactive)
  (recenter 0))

(defun next-six-lines ()
  (interactive)
  (next-line 6))

(defun previous-six-lines ()
  (interactive)
  (previous-line 6))

(defun rename-symbol ()
  (interactive)
  (let* ((symbol-to-rename (thing-at-point 'symbol)))
    (beginning-of-thing 'symbol)
    (query-replace-regexp
     (concat "\\_<" symbol-to-rename "\\_>")
     (read-string "Rename to what?: " symbol-to-rename))))

(defun next-word ()
  (interactive)
  (forward-word)
  (forward-word)
  (backward-word))

(defun find-file-dot-emacs ()
  (interactive)
  (find-file user-init-file))

(defun kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun generic-enlarge-window ()
  (interactive)
  (enlarge-window 7) (enlarge-window 7 t))

(defun generic-shrink-window ()
  (interactive)
  (shrink-window 7) (shrink-window 7 t))

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer nil))

(defun cycle-other-buffer ()
  (interactive)
  (other-window 1))

(defun fix-m ()
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))

(defun indent-or-dabbrev-completion ()
  (interactive)
  (if mark-active
      (indent-region (region-beginning) (region-end))
    (let ((l (line-end-position))
          (p (point)))
      (indent-according-to-mode)
      (when (and (= l (line-end-position))
                 (= p (point))
                 (not (bolp))
                 (not (eq (char-syntax (following-char)) ?w))
                 (dabbrev-completion))))))

(defun improve-tab ()
  (interactive)
  (local-set-key (kbd "<tab>") 'indent-or-dabbrev-completion)
  (local-set-key (kbd "TAB")   'indent-or-dabbrev-completion))

(defun full-screen-buffer (buffer-name)
  (interactive)
  (delete-other-windows)
  (switch-to-buffer buffer-name))

(defun add-symbol-at-point-to-kill-ring ()
  (interactive)
  (message (kill-new (thing-at-point 'symbol))))

(defun message-and-kill-add-buffer-file-name ()
  (interactive)
  (message buffer-file-name)
  (kill-new buffer-file-name))

(defun delete-current-buffer-file ()
  (interactive)
  (delete-file buffer-file-name))

(defun adjust-tab-width ()
  (interactive)
  (setq tab-width (read-number "New tab-width: ")))

(defun symbol-at-point ()
  (thing-at-point 'symbol))

(defun safe-load-abbrevs ()
  (if (file-exists-p "~/.abbrev_defs")
      (read-abbrev-file "~/.abbrev_defs")))

(defun c-include-file-name ()
  (interactive)
  (let* ((line-string (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
         match-start)

    (setq match-start (string-match "[^\"<][<\"]\\(.*[/\\]\\)?\\(.*\\..*\\)[\">]" line-string))

    (if match-start
        (substring line-string (match-beginning 2) (match-end 2))
      nil)))


(defun c-find-other-file ()
  (interactive)

  (let ((file-name (c-include-file-name)))
    (cond
     (file-name
      (or (file-cache-ido-find-file file-name)
          (file-cache-ido-find-file (transform-h-to-idl
                                     (file-name-nondirectory file-name)))))
     (t
      (or (and (get-buffer (transform-h-to-c-and-vice-versa (buffer-name)))
               (switch-to-buffer (transform-h-to-c-and-vice-versa (buffer-name))))
          (file-cache-ido-find-file (transform-h-to-c-and-vice-versa
                                     (buffer-name))))))))


(defun transform-h-to-c-and-vice-versa (file-name)
  (interactive "MFile name:")
  (cond
   ((string-match "\\.c$" file-name)
    (replace-regexp-in-string "\\.c$" ".h" file-name))
   ((string-match "\\.h$" file-name)
    (replace-regexp-in-string "\\.h$" ".c" file-name))
   (t
    (message "Not a .c or .h file!"))))

(defun transform-h-to-idl (file-name)
  (interactive "MFile name:")
  (cond
   ((string-match "\\.h$" file-name)
    (replace-regexp-in-string "\\.h$" ".idl" file-name))
   (t
    (message "Not a .h file!"))))

(defun raise-svn-status-buffer ()
  (interactive)

  (let ((versioned-files-root (enspro-project-versioned-files-root enspro-active-project)))
    (if versioned-files-root
        (svn-status versioned-files-root))))

(defun create-file-cache-for-current-directory ()
  (let* ((file-cache-file (expand-file-name enspro-default-file-cache-file-name ".")))
    (if (file-exists-p file-cache-file)
        (message (format "The file %s already exists. Refusing to overwrite." file-cache-file))
      (file-cache-add-directory-recursively ".")
      (file-cache-save-cache-to-file file-cache-file)
      (message (format "Wrote %s." file-cache-file)))))

(defun get-buffer-regexp (regex)
  (find-if (lambda (x) (string-match regex (buffer-name x)))
           (buffer-list)))

(defun get-config-string (file variable)
  (save-excursion
    (let (regexp
          buffer
          result)

      (setq buffer (find-file-noselect file))
      (set-buffer buffer)

      (beginning-of-buffer)
      (setq regexp (concat variable " *= *\\(.*\\)"))
      (setq result (re-search-forward regexp (point-max) t))

      (if result
          (buffer-substring-no-properties (match-beginning 1) (match-end 1))
        nil))))

(defun directory-files-without-dots (directory)
  (let (directory-files-content
        result)
    (setq directory-files-content (directory-files directory))
    (dolist (item directory-files-content)
      (if (not (or (string= item ".") (string= item "..")))
          (add-to-list 'result item)))

    result))


(defun delete-file-if-exists (file)
  (if (file-exists-p file)
      (delete-file file)))


(defun delete-directory-if-exists (directory)
  (if (file-directory-p directory)
      (delete-directory directory)))


(defun multiply-string (string number)
  (let (result)
    (dotimes (i 5)
      (setq result (concat result string)))
    result))


(defun windowsify-path (path)
  (replace-regexp-in-string "/" "\\\\" path))


(defun on-windows-p ()
  (string-match "mingw" (version)))

(defun apply-patch ()
  (interactive)
  (shell-command (concat "patch -p0 < " (buffer-file-name))))

(defun apply-patch-in-buffer (&optional dir)
  "Applies the text in the current buffer as a patch."
  (interactive)
  (let ((temp-file (make-temp-file "patch")))
    (write-file temp-file)
    (if dir
        (cd dir))
    (shell-command (concat "patch -p0 < " temp-file))))
