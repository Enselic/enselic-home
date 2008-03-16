;;; highlight-symbol.el --- automatic and manual symbol highlighting
;;
;; Copyright (C) 2007-2008 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0.3
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-symbol/
;; Compatibility: GNU Emacs 22.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
;;
;;; Commentary:
;;
;; Add the following to your .emacs file:
;; (require 'highlight-symbol)
;; (global-set-key [(control f3)] 'highlight-symbol-at-point)
;; (global-set-key [f3] 'highlight-symbol-next)
;; (global-set-key [(shift f3)] 'highlight-symbol-prev)
;; (global-set-key [(meta f3)] 'highlight-symbol-prev)))
;;
;; Use `highlight-symbol-at-point' to toggle highlighting of the symbol at
;; point throughout the current buffer.  Use `highlight-symbol-mode' to keep the
;; symbol at point highlighted.
;;
;; The functions `highlight-symbol-next', `highlight-symbol-prev',
;; `highlight-symbol-next-in-defun' and `highlight-symbol-prev-in-defun'
;; allow for cycling through the locations of any symbol at point. If
;; you want to highlight the symbol while navigating with these
;; functions, customize `highlight-symbol-on-navigation'.
;;
;;; Changes Log:
;;
;; ????-??-?? (?.?.?)
;;    Made navigation with the `highlight-symbol-jump' back-end
;;    automatically handle highlight of the symbol, customizable
;;    through `highlight-symbol-on-navigation'.
;;    Patch from Martin Nordholts.
;;
;; 2008-02-26 (1.0.3)
;;    Added `highlight-symbol-remove-all'.
;;
;; 2007-09-06 (1.0.2)
;;    Fixed highlighting with delay set to 0.  (thanks to Stefan Persson)
;;
;; 2007-09-05 (1.0.1)
;;    Fixed completely broken temporary highlighting.
;;
;; 2007-07-30 (1.0)
;;    Keep temp highlight while jumping.
;;    Replaced `highlight-symbol-faces' with `highlight-symbol-colors'.
;;    Fixed dependency and Emacs 21 bug.  (thanks to Gregor Gorjanc)
;;    Prevent calling `highlight-symbol-at-point' on nil.
;;
;; 2007-04-20 (0.9.1)
;;    Fixed bug in `highlight-symbol-jump'.  (thanks to Per Nordlöw)
;;
;; 2007-04-06 (0.9)
;;    Initial release.
;;
;;; Code:

(require 'thingatpt)
(require 'hi-lock)
(eval-when-compile (require 'cl))

(push "^No symbol at point$" debug-ignored-errors)

(defgroup highlight-symbol nil
  "Automatic and manual symbols highlighting"
  :group 'faces
  :group 'matching)

(defface highlight-symbol-face
  '((((class color) (background dark))
     (:background "gray30"))
    (((class color) (background light))
     (:background "gray90")))
  "*Face used by `highlight-symbol-mode'."
  :group 'highlight-symbol)

(defcustom highlight-symbol-on-navigation nil
  "*Wether or not to temporary highlight the symbol when using
`highlight-symbol-jump' family of functions."
  :type 'boolean
  :group 'highlight-symbol)

(defcustom highlight-symbol-idle-delay 1.5
  "*Number of seconds of idle time before highlighting the current symbol.
If this variable is set to 0, no idle time is required.
Changing this does not take effect until `highlight-symbol-mode' has been
disabled for all buffers."
  :type 'number
  :group 'highlight-symbol)

(defcustom highlight-symbol-colors
  '("yellow" "DeepPink" "cyan" "MediumPurple1" "SpringGreen1"
    "DarkOrange" "HotPink1" "RoyalBlue1" "OliveDrab")
  "*Colors used by `highlight-symbol-at-point'.
highlighting the symbols will use these colors in order."
  :type '(repeat color)
  :group 'highlight-symbol)

(defvar highlight-symbol-color-index 0)
(make-variable-buffer-local 'highlight-symbol-color-index)

(defvar highlight-symbol-timer nil)
(make-variable-buffer-local 'highlight-symbol-timer)

(defvar highlight-symbol nil)
(make-variable-buffer-local 'highlight-symbol)

(defvar highlight-symbol-list nil)
(make-variable-buffer-local 'highlight-symbol-list)

(defconst highlight-symbol-border-pattern
  (if (>= emacs-major-version 22) '("\\_<" . "\\_>") '("\\<" . "\\>")))

;;;###autoload
(define-minor-mode highlight-symbol-mode
  "Minor mode that highlights the symbol under point throughout the buffer.
Highlighting takes place after `highlight-symbol-idle-delay'."
  nil " hl-s" nil
  (if highlight-symbol-mode
      ;; on
      (progn
        (unless hi-lock-mode (hi-lock-mode 1))
        (unless highlight-symbol-timer
          (setq highlight-symbol-timer
                (when (and highlight-symbol-idle-delay
                           (/= highlight-symbol-idle-delay 0))
                  (run-with-idle-timer highlight-symbol-idle-delay t
                                       'highlight-symbol-temp-highlight))))
        (add-hook 'post-command-hook 'highlight-symbol-mode-post-command nil t))
    ;; off
    (remove-hook 'post-command-hook 'highlight-symbol-mode-post-command t)
    (unless (null highlight-symbol-timer)
      (cancel-timer highlight-symbol-timer)
      (kill-local-variable 'highlight-symbol-timer))

    (highlight-symbol-mode-remove-temp)
    (kill-local-variable 'highlight-symbol)))

;;;###autoload
(define-minor-mode highlight-symbol-navigation-mode
  "Minor mode that is active when navigating with
`highlight-symbol-next' and `highlight-symbol-prev'."
  nil nil nil
  (if highlight-symbol-navigation-mode
      (progn
        (if (not (member (highlight-symbol-get-symbol) highlight-symbol-list))
            (highlight-symbol-at-point))
        (add-hook 'pre-command-hook 'highlight-symbol-navigation-mode-pre-command nil t))
    (progn
      (if (member (highlight-symbol-get-symbol) highlight-symbol-list)
          (highlight-symbol-at-point))
      (remove-hook 'pre-command-hook 'highlight-symbol-navigation-mode-pre-command t))))

;;;###autoload
(defun highlight-symbol-is-navigation-command (command)
  (or (eq command 'highlight-symbol-next)
      (eq command 'highlight-symbol-prev)
      (eq command 'highlight-symbol-next-in-defun)
      (eq command 'highlight-symbol-prev-in-defun)))

;;;###autoload
(defun highlight-symbol-navigation-mode-pre-command ()
  (if (not (highlight-symbol-is-navigation-command this-command))
      (highlight-symbol-navigation-mode -1)))

;;;###autoload
(defun highlight-symbol-at-point ()
  "Toggle highlighting of the symbol at point.
This highlights or unhighlights the symbol at point using the first
element in of `highlight-symbol-colors'."
  (interactive)
  (let ((symbol (highlight-symbol-get-symbol)))
    (unless symbol (error "No symbol at point"))
    (unless hi-lock-mode (hi-lock-mode 1))
    (if (member symbol highlight-symbol-list)
        ;; remove
        (progn
          (setq highlight-symbol-list (delete symbol highlight-symbol-list))
          (hi-lock-unface-buffer symbol))
      ;; add
      (when (equal symbol highlight-symbol)
        (highlight-symbol-mode-remove-temp))
      (let ((color (highlight-symbol-get-next-color)))
        ;; highlight
        (with-no-warnings
          (if (< emacs-major-version 22)
              (hi-lock-set-pattern `(,symbol (0 (quote ,color) t)))
            (hi-lock-set-pattern symbol color)))
        (push symbol highlight-symbol-list)))))

;;;###autoload
(defun highlight-symbol-get-next-color ()
  "Returns the next color to use."
      (let ((color (nth highlight-symbol-color-index
                        highlight-symbol-colors)))
        (if color ;; wrap
            (incf highlight-symbol-color-index)
          (setq highlight-symbol-color-index 0
                color (car highlight-symbol-colors)))
        `((background-color . ,color)
          (foreground-color . "black"))))

;;;###autoload
(defun highlight-symbol-remove-all ()
  "Remove symbol highlighting in buffer."
  (interactive)
  (mapc 'hi-lock-unface-buffer highlight-symbol-list)
  (setq highlight-symbol-list nil))

;;;###autoload
(defun highlight-symbol-next ()
  "Jump to the next location of the symbol at point within the function."
  (interactive)
  (highlight-symbol-jump 1))

;;;###autoload
(defun highlight-symbol-prev ()
  "Jump to the previous location of the symbol at point within the function."
  (interactive)
  (highlight-symbol-jump -1))

;;;###autoload
(defun highlight-symbol-next-in-defun ()
  "Jump to the next location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump 1)))

;;;###autoload
(defun highlight-symbol-prev-in-defun ()
  "Jump to the previous location of the symbol at point within the defun."
  (interactive)
  (save-restriction
    (narrow-to-defun)
    (highlight-symbol-jump -1)))

(defun highlight-symbol-get-symbol ()
  "Return a regular expression dandifying the symbol at point."
  (let ((symbol (thing-at-point 'symbol)))
    (when symbol (concat (car highlight-symbol-border-pattern)
                         (regexp-quote symbol)
                         (cdr highlight-symbol-border-pattern)))))

(defun highlight-symbol-temp-highlight ()
  "Highlight the current symbol until a command is executed."
  (when highlight-symbol-mode
    (let ((symbol (highlight-symbol-get-symbol)))
      (unless (or (equal symbol highlight-symbol)
                  (member symbol highlight-symbol-list))
        (highlight-symbol-mode-remove-temp)
        (when symbol
          (setq highlight-symbol symbol)
          (hi-lock-set-pattern symbol 'highlight-symbol-face))))))

(defun highlight-symbol-mode-remove-temp ()
  "Remove the temporary symbol highlighting."
  (when highlight-symbol
    (hi-lock-unface-buffer highlight-symbol)
    (setq highlight-symbol nil)))

(defun highlight-symbol-mode-post-command ()
  "After a command, change the temporary highlighting.
Remove the temporary symbol highlighting and, unless a timeout is specified,
create the new one."
  (unless (highlight-symbol-is-navigation-command this-command)
    (if highlight-symbol-timer
        (highlight-symbol-mode-remove-temp)
      (highlight-symbol-temp-highlight))))

(defun highlight-symbol-jump (dir)
  "Jump to the next or previous occurence of the symbol at point.
DIR has to be 1 or -1."
  (let ((symbol (highlight-symbol-get-symbol)))
    (if symbol
        (let* ((case-fold-search nil)
               (bounds (bounds-of-thing-at-point 'symbol))
               (offset (- (point) (if (< 0 dir) (cdr bounds) (car bounds)))))
          (unless (eq last-command 'highlight-symbol-jump)
            (push-mark))
          ;; move a little, so we don't find the same instance again
          (goto-char (- (point) offset))
          (let ((target (re-search-forward symbol nil t dir)))
            (unless target
              (goto-char (if (< 0 dir) (point-min) (point-max)))
              (setq target (re-search-forward symbol nil nil dir)))
            (goto-char (+ target offset)))

          (if highlight-symbol-on-navigation
              (highlight-symbol-navigation-mode 1)))
      (error "No symbol at point"))))

(provide 'highlight-symbol)

;;; highlight-symbol.el ends here
