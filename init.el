;; init.el - Enselic's Emacs customization starting point
;;
;; Copyright © 2007-2008 Martin Nordholts
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;
;; To get up and running, add this to ~/.emacs:
;;
;;   (setq user-init-file "~/enselic-elisp/init.el")
;;   (load user-init-file)
;;

(add-to-list 'load-path "~/enselic-elisp")

(require 'browse-kill-ring)
(require 'cc-mode)
(require 'egtk)
(require 'etags-select)
(require 'filecache)
(require 'filecache-enhancements)
(require 'gimp-elisp)
(require 'git)
(require 'grep+)
(require 'gtk-doc)
(require 'highlight-symbol)
(require 'iswitchb)
(require 'programming-project)
(require 'psvn)
(require 'session)
(require 'simple-project-management)
(require 'thingatpt)
(require 'tinyreplace)
(require 'vc-clearcase-auto)
(require 'whitespace)


(load "functions.el")
(load "keys.el")
(load "skeletons.el")


(setq-default cursor-type '(bar . 2))
(c-add-style "2sp-linux" '("linux" (c-basic-offset . 2)))
(c-add-style "3sp-linux" '("linux" (c-basic-offset . 3)))
(c-add-style "4sp-linux" '("linux" (c-basic-offset . 4)))
(fset 'yes-or-no-p 'y-or-n-p)
(set-background-color "white")


(add-hook 'after-init-hook  'safe-load-abbrevs)
(add-hook 'after-init-hook  'session-initialize)
(add-hook 'after-init-hook  'grep-compute-defaults)
(add-hook 'after-init-hook  'programming-project-init)
(add-hook 'after-init-hook  'simple-project-management-initialize)

(add-hook 'find-file-hook   'improve-tab)

(add-hook 'kill-emacs-hook  'write-abbrev-file)


(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode t)
 '(add-log-full-name "Martin Nordholts")
 '(add-log-mailing-address "martinn@svn.gnome.org")
 '(auto-save-default nil)
 '(backward-delete-char-untabify-method nil)
 '(browse-kill-ring-maximum-display-length 1000)
 '(c-basic-offset 2)
 '(c-block-comment-prefix "* ")
 '(c-default-style (quote ((c++-mode . "gnu") (java-mode . "java") (awk-mode . "awk") (other . "gnu"))))
 '(clearcase-checkout-policy (quote reserved))
 '(column-number-mode t)
 '(comment-fill-column 80)
 '(comment-style (quote aligned))
 '(compilation-scroll-output t)
 '(dabbrev-case-fold-search nil)
 '(dabbrev-case-replace t)
 '(desktop-base-file-name "DESKTOP")
 '(desktop-globals-to-clear nil)
 '(desktop-inhibit-find-file-warnings t)
 '(desktop-lazy-verbose nil)
 '(desktop-restore-eager 20)
 '(desktop-save t)
 '(desktop-save-mode t)
 '(echo-keystrokes 0.01)
 '(file-cache-filter-regexps (quote ("~$" "\\.o$" "\\.exe$" "\\.a$" "\\.elc$" ",v$" "\\.output$" "\\.$" "#$" "\\.class$" "/\\.")))
 '(fringe-mode (quote (1 . 0)) nil (fringe))
 '(global-auto-revert-mode t)
 '(grep-scroll-output t)
 '(grepp-default-regexp-fn (quote symbol-at-point))
 '(highlight-symbol-idle-delay 7200)
 '(highlight-symbol-on-navigation t)
 '(highlight-symbol-on-navigation-p t)
 '(history-delete-duplicates t)
 '(icicle-bind-top-level-commands-flag t)
 '(icicle-mode nil)
 '(icicle-reminder-prompt-flag 0)
 '(icicle-saved-completion-sets (quote (("files" . "/home/martin/Kallkod/cpp/ThemesCreator_trunk/icicles-files.cache"))))
 '(ido-mode (quote both) nil (ido))
 '(indent-tabs-mode nil)
 '(indicate-buffer-boundaries nil)
 '(inhibit-startup-screen t)
 '(kill-read-only-ok t)
 '(large-file-warning-threshold nil)
 '(longlines-wrap-follows-window-size t)
 '(make-backup-files nil)
 '(max-lisp-eval-depth 30000)
 '(max-specpdl-size 10000)
 '(menu-bar-mode nil)
 '(mode-require-final-newline nil)
 '(recentf-max-saved-items 40)
 '(safe-local-variable-values (quote ((c-basic-indent . 2) (c-set-style . gnu))))
 '(save-abbrevs nil)
 '(save-place t nil (saveplace))
 '(scroll-bar-mode (quote right))
 '(scroll-step 1)
 '(show-paren-delay 0.0)
 '(show-paren-mode t)
 '(show-paren-style (quote parenthesis))
 '(size-indication-mode nil)
 '(svn-status-default-diff-arguments (quote ("--diff-cmd" "diff" "-x" "-up")))
 '(svn-status-default-log-arguments (quote ("-v" "--limit" "1000")))
 '(svn-status-use-header-line nil)
 '(svn-status-verbose nil)
 '(tags-add-tables nil)
 '(tags-case-fold-search nil)
 '(tags-revert-without-query t)
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(truncate-lines t)
 '(undo-ask-before-discard t)
 '(undo-limit 200000)
 '(undo-strong-limit 300000)
 '(user-full-name "Martin Nordholts")
 '(user-mail-address "martinn@svn.gnome.org")
 '(vc-clearcase-diff-switches (quote ("-graphical")))
 '(vc-consult-headers nil)
 '(vc-display-status nil)
 '(version-control (quote never))
 '(which-function-mode t)
 '(whitespace-chars (quote (tabs spaces trailing)))
 '(whitespace-tab (quote whitespace-space))
 '(x-select-enable-clipboard t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(diff-added ((t (:foreground "#008B8B"))))
 '(diff-context ((((class color grayscale) (min-colors 88)) nil)))
 '(diff-file-header ((((class color) (min-colors 88) (background light)) (:inherit diff-header))))
 '(diff-function ((t (:inherit diff-hunk-header))))
 '(diff-header ((((class color) (min-colors 88) (background light)) (:foreground "#2E8B57" :weight bold))))
 '(diff-hunk-header ((t (:foreground "#A52A2A" :weight bold))))
 '(diff-index ((t (:foreground "#2E8B57"))))
 '(diff-indicator-added ((t (:foreground "#5f9ea0"))))
 '(diff-indicator-changed ((t (:foreground "#5f9ea0"))))
 '(diff-removed ((t (:foreground "#6A5ACD"))))
 '(dired-directory ((t (:foreground "Red"))))
 '(font-lock-builtin-face ((((class color) (min-colors 8)) (:inherit font-lock-keyword-face))))
 '(font-lock-comment-delimiter-face ((default (:inherit font-lock-comment-face)) (((class color) (min-colors 8) (background light)) nil)))
 '(font-lock-comment-face ((((class color) (min-colors 8) (background light)) (:foreground "#228b22"))))
 '(font-lock-constant-face ((((class color) (min-colors 8)) (:foreground "#000000"))))
 '(font-lock-doc-face ((t (:foreground "#228b22"))))
 '(font-lock-function-name-face ((((class color) (min-colors 8)) (:foreground "#8b008b" :weight bold))))
 '(font-lock-keyword-face ((((class color) (min-colors 8)) (:foreground "#0000ff"))))
 '(font-lock-preprocessor-face ((t (:inherit font-lock-builtin-face))))
 '(font-lock-string-face ((((class color) (min-colors 8)) (:foreground "#ff0000"))))
 '(font-lock-type-face ((((class color) (min-colors 8)) (:foreground "#4682b4"))))
 '(font-lock-variable-name-face ((((class color) (min-colors 8)) (:foreground "#000000"))))
 '(hl-line ((t (:background "#fffafa"))))
 '(isearch ((((class color) (min-colors 88) (background light)) (:background "#ffccff"))))
 '(mode-line ((t (:background "#000000" :foreground "#ffffff"))))
 '(mode-line-inactive ((default (:inherit mode-line)) (((class color) (min-colors 88) (background light)) (:background "#000000" :foreground "grey75" :box (:line-width -1 :color "grey75") :weight light))))
 '(region ((((class color) (min-colors 88) (background light)) (:background "#ccccff"))))
 '(show-ws-spaces ((((class color)) nil)))
 '(show-ws-tabs ((((class color)) (:inherit trailing-whitespace))))
 '(which-func ((((class color) (min-colors 88) (background light)) (:foreground "#ffffff" :weight bold))))
 '(whitespace-space ((((class color) (background light)) (:foreground "#8b1a1a"))))
 '(whitespace-trailing ((t (:inherit whitespace-space :background "red1" :weight bold)))))

(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)


(put 'downcase-region 'disabled nil)
