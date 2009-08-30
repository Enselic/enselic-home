;;;
;;; egtk.el - Minor Mode for editing GTK+ code
;;;
;;; minor mode providing various utilities useful when
;;; editing GTK+ source code.
;;;
;;; Put the file somewhere in your emacs load path and add
;;; the following to your .emacs file:
;;;
;;;  (autoload 'egtk-mode "egtk" "mode for editing GTK+ code")
;;;  (add-hook 'c-mode-hook 'egtk-mode)
;;;
:;; v0.1 
;;; v0.2 - 17 Nov 1999: Fixed indenting of return pointer types.
;;;
;;; Author: Owen Taylor <otaylor@redhat.com>
;;; Copyright Red Hat, Inc. 1999
;;;

(defvar egtk-mode nil
  "Non-nil if egtk mode is enabled.
Don't change this variable directly, you must change it by one of the
functions that enable or disable view mode.")

(make-variable-buffer-local 'egtk-mode)

(defun egtk-mode (&optional arg)
  "Toggle Emacs minor mode for GTK+ editing
With arg, turn egtk mode on iff arg is positive

\\{egtk-mode-map}
"
  (interactive "P")
  (unless (and arg			; Do nothing if already OK.
	       (if (> (prefix-numeric-value arg) 0)
		   egtk-mode
		 (not egtk-mode)))
    (if egtk-mode (egtk-mode-disable)
      (egtk-mode-enable))))

(defun egtk-mode-enable ()
  (setq egtk-mode t))

(defun egtk-mode-disable ()
  (setq egtk-mode nil))

;;;
;;; Utility functions
;;;

(defun egtk-trim (str)
  (if (string-match "[ \n\t]*\\(.*[^ \n\t]+\\)[ \n\t]*" str)
      (substring str (match-beginning 1) (match-end 1))
    str))

(defun egtk-remove-white (str)
  (apply 'concat (split-string str)))

(defun egtk-list-max (a b)
  (if a
      (cons (max (car a) (car b))
	    (egtk-list-max (cdr a) (cdr b)))
    nil))

;; Like APL reduce
(defun egtk-fold (func list start)
  (if list
      (apply func (list (car list) (egtk-fold func (cdr list) start)))
    start))

(defun egtk-pad-string (str width)
  (concat str (make-string (- width (length str)) ?  )))

(defun egtk-pad-string-left (str width)
  (concat (make-string (- width (length str)) ?  ) str))

;;;
;;; Automatically insert cast for GTK+ function
;;;

(defun egtk-insert-cast ()
  (interactive)
  "Inserts casting macro for the current function at point"
  (insert 
   (upcase 
    (save-excursion
      ;; Find beginning of function name
      (search-backward "(")
      (skip-chars-backward " 	")
      (skip-chars-backward "A-Za-z0-9_")
      (let ((begin (point)))
	(skip-chars-forward "A-Za-z0-9")
	(skip-chars-forward "_")
	(skip-chars-forward "A-Za-z0-9")
	(buffer-substring begin (point)))))
   " ("))

;;;
;;; Code to align prototypes in the GTK+ style
;;;

(defun egtk-parse-parameter (str)
  (if (string-match "[ \n\t]*\\(.*[a-zA-Z0-9_]\\)\\([* \n\t]+\\)\\([a-zA-Z0-9_]+[ \t\n]*\\(\\[[^]]*\\]\\)?\\)[ \t\n]*" str)
      (let ((first (substring str (match-beginning 1) (match-end 1)))
	    (stars (substring str (match-beginning 2) (match-end 2)))
	    (second (substring str (match-beginning 3) (match-end 3))))
	(list first (egtk-remove-white stars) second))
    str))

(defun egtk-next-prototype ()
  (if (search-forward-regexp "^[ \t]*\\([^(/]+[ \t]+\\**\\)\\([a-zA-Z0-9_]+\\)[ \t]*(\\([^)]*\\))[ \t]*;" (point-max) t)
      (let* ((begin (match-beginning 0))
	     (end (match-end 0))
	     (return-tmp (match-string 1))
	     (function (match-string 2))
	     (argstr (match-string 3))
	     (return (egtk-trim return-tmp))
	     (args (mapcar 'egtk-parse-parameter
			   (split-string argstr "[ \t\n]*,[ \t\n]*"))))
	(list begin end return function args))
    nil))

(defun get-arg-widths (arg)
  (if (listp arg)
      (list (length (nth 0 arg))
	    (length (nth 1 arg)))
    (list (length arg)
	  0)))


;; Return the length of str, adding 1 if the string cannot be
;; immediatly followed by an identifier
;;
(defun egtk-type-length (str)
  (print str)
  (if (string-match "[ \t]\\**$" str)
      (+ (length str ) 1)
    (+ (length str) 2)))

(defun egtk-get-proto-widths (return function args)
  (append (list (egtk-type-length return) (length function))
	  (egtk-fold 'egtk-list-max (mapcar 'get-arg-widths args) '(0 0))))

(defun egtk-get-proto-widths-for-region ()
  (save-excursion
    (save-restriction
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (let ((working t)
	    (widths '()))
	(while working
	  (let ((res (egtk-next-prototype)))
	    (if res
		(let* ((return (nth 2 res))
		       (function (nth 3 res))
		       (args (nth 4 res)))
		  (setq widths (cons (egtk-get-proto-widths return function args) widths)))
	      (setq working nil))))
	(egtk-fold 'egtk-list-max widths '(0 0 0 0))))))

(defun egtk-format-arg (arg)
  (if (listp arg)
      (let ((type (nth 0 arg))
	    (stars (nth 1 arg))
	    (var (nth 2 arg)))
	(concat (egtk-pad-string type type-width)
		" "
		(egtk-pad-string-left stars stars-width)
		var))
    arg))

(defun egtk-format-args (args type-width stars-width)
  (mapconcat 'egtk-format-arg args ",\n"))

(defun egtk-format-protos ()
  (interactive)
  "Align prototypes in the current region in the GTK+ style"
  (let* ((widths (egtk-get-proto-widths-for-region))
	 (ret-width  (nth 0 widths))
	 (func-width (nth 1 widths))
	 (type-width (nth 2 widths))
	 (stars-width  (nth 3 widths)))
    (save-excursion
      (save-restriction
	(narrow-to-region (point) (mark))
	(goto-char (point-min))
	(let ((working t))
	  (while working
	    (let ((res (egtk-next-prototype)))
	      (if res
		  (let ((begin    (nth 0 res))
			(end      (nth 1 res))
			(return   (nth 2 res))
			(function (nth 3 res))
			(args     (nth 4 res)))
		    (delete-region begin end)
		    (insert (egtk-pad-string return ret-width)
			    (egtk-pad-string function func-width)
			    " ("
			    (egtk-format-args args type-width stars-width)
			    ");\n"))
		(setq working nil)))))
	(indent-region (point-min) (point-max) nil)))))

;; Install ourselves:

(unless (assq 'egtk-mode minor-mode-alist)
  (setq minor-mode-alist
	(cons '(egtk-mode "GTK") minor-mode-alist)))

(defvar egtk-menu-map
  (let ((map (make-sparse-keymap "GTK+")))
    (define-key map [format-protos]
      '("Format Prototypes" . egtk-format-protos))
    (define-key map [insert-cast]
      '("Insert Cast Macro" . egtk-insert-cast))
    map))

(defvar egtk-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-cg" 'egtk-insert-cast)
    (define-key map "\C-cp" 'egtk-format-protos)
    (define-key map [menu-bar GTK+] (cons "GTK+" egtk-menu-map))
    map))

(unless (assq 'egtk-mode minor-mode-map-alist)
  (setq minor-mode-map-alist
	(cons (cons 'egtk-mode egtk-mode-map) minor-mode-map-alist)))

;; Provide ourselves:

(provide 'egtk)


