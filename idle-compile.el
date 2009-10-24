;; idle-compile.el - Compiles the current buffer after a certain time
;; of inactivity following a save.
;;
;; Copyright © 2009 Martin Nordholts
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

(provide 'idle-compile)


(defconst idle-compile-timeout 1
  "Timeout for compile after save.")


(defvar idle-compile-timeout-timer nil
  "Timer used for timeout.")
(make-variable-buffer-local 'idle-compile-timeout-timer)


(define-minor-mode idle-compile-mode
  "When enabled, the current buffer will be compiled after a
short time on inactivity following a save."
  nil " Idle-Compile" nil

  (if idle-compile-mode
      (idle-compile-turn-on)
    (idle-compile-turn-off)))


(defun idle-compile-turn-on ()
  "Turns on idle compile."
  (add-hook 'after-save-hook 'idle-compile-setup-timeout t t))


(defun idle-compile-turn-off ()
  "Turns off idle compile."
  (idle-compile-teardown-timeout)
  (remove-hook 'after-save-hook 'idle-compile-setup-timeout t))


(defun idle-compile-setup-timeout ()
  "Setup a timeout for compile."

  (idle-compile-teardown-timeout)

  (setq idle-compile-timeout-timer
        (run-with-idle-timer idle-compile-timeout nil
                             'idle-compile-compile)))


(defun idle-compile-teardown-timeout ()
  "Abort any timeout."

  (when idle-compile-timeout-timer
    (cancel-timer idle-compile-timeout-timer)
    (kill-local-variable 'idle-compile-timeout-timer)))


(defun idle-compile-compile ()
  "Compiles the current buffer."

  (when (and buffer-file-name
             (equal (file-name-extension buffer-file-name) "c"))

    ;; The current buffer is a C file, make it's object file
    (let* ((dirname (file-name-directory buffer-file-name))
           (c-name (file-name-nondirectory buffer-file-name))
           (basename (file-name-sans-extension c-name))
           (obj-name (concat basename ".o"))
           (original-compile-command compile-command))

      ;; Issue the compile command
      (setq compile-command (concat "make -C " dirname " " obj-name))
      (recompile)
      (setq compile-command original-compile-command)

      ;; Make the compile buffer as small as possible
      ;; (let ((compilation-buffer-window (get-buffer-window "*compilation*")) ;;
      ;;       error-symbol)                                                   ;;
      ;;   (if nil ;compilation-buffer-window                                  ;;
      ;;       (condition-case error-symbol                                    ;;
      ;;           (while t                                                    ;;
      ;;             (message "ja")                                            ;;
      ;;             (adjust-window-trailing-edge compilation-buffer-window    ;;
      ;;                                          -2 nil)                      ;;
      ;;             (sleep-for 1))                                            ;;
      ;;         (error (message error-symbol)))))                             ;;
      )))

