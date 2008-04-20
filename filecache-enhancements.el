;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  filecache enhancements (from www.EmacsWiki.org)
;;

(defvar file-cache-files-matching-map nil)
(if file-cache-files-matching-map
    ()
  (setq file-cache-files-matching-map (make-sparse-keymap))
  (define-key file-cache-files-matching-map (kbd "RET") 'file-cache-files-matching-mode-select)
  (define-key file-cache-files-matching-map (kbd "n")   'next-line)
  (define-key file-cache-files-matching-map (kbd "p")   'previous-line))



(defun file-cache-files-matching-with-mode ()
  (interactive)
  (kill-buffer "*File Cache Files Matching*")
  (call-interactively 'file-cache-files-matching)
  (switch-to-buffer "*File Cache Files Matching*")
  (file-cache-files-matching-mode)
  (delete-other-windows)
  (setq buffer-read-only t))


(defun file-cache-files-matching-mode-select ()
  (interactive)
  (let (start
        end
        file-name)
    (setq start     (line-beginning-position)
          end       (line-end-position)
          file-name (buffer-substring-no-properties start end))
    (file-cache-ido-find-file file-name)))


(defun file-cache-files-matching-mode ()
  (kill-all-local-variables)
  (use-local-map file-cache-files-matching-map)
  (setq mode-name "FCFMM")
  (setq major-mode 'file-cache-files-matching-mode))


(defun file-cache-save-cache-to-file (file)
  "Save contents of `file-cache-alist' to FILE.
For later retrieval using `file-cache-read-cache-from-file'"
  (interactive "FFile: ")
  (with-temp-file (expand-file-name file)
    (prin1 file-cache-alist (current-buffer))))


(defun file-cache-read-cache-from-file (file)
  "Clear `file-cache-alist' and read cache from FILE.
The file cache can be saved to a file using
`file-cache-save-cache-to-file'."
  (interactive "fFile: ")
  (file-cache-clear-cache)
  (let ((buf (find-file-noselect file)))
    (setq file-cache-alist (read buf))
    (kill-buffer buf)))


(defun file-cache-ido-find-file (file)
  "Using ido, interactively open file from file cache'.
First select a file, matched using ido-switch-buffer against the contents
in `file-cache-alist'. If the file exist in more than one
directory, select directory. Lastly the file is opened."
  (interactive (list (file-cache-ido-read "File: "
                                          (mapcar
                                           (lambda (x)
                                             (car x))
                                           file-cache-alist))))
  (let* ((record (assoc file file-cache-alist)))
    (find-file
     (expand-file-name
      file
      (if (= (length record) 2)
          (car (cdr record))
        (file-cache-ido-read
         (format "Find %s in dir: " file) (cdr record)))))))


(defun file-cache-ido-read (prompt choices)
  (let ((ido-make-buffer-list-hook
	 (lambda ()
	   (setq ido-temp-list choices))))
    (ido-read-buffer prompt)))


(defun file-cache-add-this-file ()
  (and buffer-file-name
       (file-exists-p buffer-file-name)
       (file-cache-add-file buffer-file-name)))


(defun file-cache-number-of-items ()
  (length file-cache-alist))



(provide 'filecache-enhancements)
