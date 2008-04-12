;; simple-programming-project.el - A simple programming project type
;; for simple-project.el
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
;; To initialize, put this in .emacs:
;;
;;   (add-hook 'after-init-hook 'simple-programming-project-init)
;;



(defconst simple-programming-project-tags-file-name                   "TAGS")
(defconst simple-programming-project-file-cache-file-name             "FILECACHE")
(defconst simple-programming-project-id-database-file-name            "ID")
(defconst simple-programming-project-config-file-name                 "programming-project-config.cfg")
(defconst simple-programming-project-source-root-config-key           "source_root")
(defconst simple-programming-project-binary-to-debug-config-key       "binary_to_debug")
(defconst simple-programming-project-additional-tag-files-config-key  "additional_tag_files")
(defconst simple-programming-project-default-file-cache-file-name     ".simple-project-file-cache")
(defconst simple-programming-project-grep-template-command-format     "grep -rn %s -e ")
(defconst simple-programming-project-lid-template-command-format      "cd %s && lid -f %s --result=grep ")
(defconst simple-programming-project-gdb-command-format               "libtool --mode=execute gdb --annotate=3 --args %s ")
(defconst simple-programming-project-default-compile-command-format   "make -k -j3 -C %s")
(defconst simple-programming-project-type                             "Programming Project")



(defun simple-programming-project-init ()
  (let ((project-type (make-simple-project-project-type-entry
                       :name                      simple-programming-project-type
                       :create-project-function  'simple-programming-project-create
                       :load-project-function    'simple-programming-project-load
                       :unload-project-function  'simple-programming-project-unload
                       :destroy-project-function 'simple-programming-project-destroy)))
    (simple-project-register-project-type project-type)))


(defun simple-programming-project-create (project-name)
  (let (source-root)
    (setq source-root (read-directory-name "Source code root directory: "))
    (simple-programming-project-create-config-file project-name source-root)))


(defun simple-programming-project-batch-create ()
  "To create a new project from the commandline, use this
command: `emacs -batch -l ~/.emacs -f
simple-programming-project-batch-create PROJECTNAME'"
  (let (project-name)
    (setq project-name (car command-line-args-left))
    (if project-name
        (simple-programming-project-create-complete-project-at-working-directory project-name)
      (error "simple-programming-project-batch-create-project: Must pass `project-name' on the commandline"))))

(defun simple-programming-project-load (project-name)

  (when (not (file-exists-p (simple-programming-project-get-config-file project-name)))
    (error "Failed to find project file %s, project corrupt, please recreate"))

  ;; Setup the new project
  (desktop-change-dir (simple-project-get-project-directory project-name))

  (let (source-root)
    (setq source-root (simple-programming-project-get-source-root project-name))
    (setq compile-command (format simple-programming-project-default-compile-command-format source-root)))

  (if (file-regular-p (simple-programming-project-get-file-cache-file project-name))
      (file-cache-read-cache-from-file (simple-programming-project-get-file-cache-file project-name)))

  (let (tags-file
        additional-tag-files)

    (setq tags-file            (simple-programming-project-get-tags-file            project-name)
          additional-tag-files (simple-programming-project-get-additional-tag-files project-name))

    (if (not additional-tag-files)
        (setq tags-file-name tags-file-name)
      (setq tags-table-list (cons tags-file additional-tag-files)))))


(defun simple-programming-project-debug-binary ()
  (interactive)
  (gdb (format simple-programming-project-gdb-command-format
               (simple-programming-project-get-debug-binary (simple-project-get-current-project)))))


(defun simple-programming-project-get-debug-binary (project-name)
  (simple-programming-project-get-config project-name
                                         simple-programming-project-binary-to-debug-config-key))

(defun simple-programming-project-unload (project-name)
  (save-some-buffers) 
  (simple-programming-project-save-file-cache project-name)
  (file-cache-clear-cache)
  (setq tags-file-name  nil
        tags-table-list nil))


(defun simple-programming-project-destroy (project-name)
  (delete-file-if-exists (simple-programming-project-get-config-file      project-name))
  (delete-file-if-exists (simple-programming-project-get-file-cache-file  project-name))
  (delete-file-if-exists (simple-programming-project-get-tags-file        project-name))
  (delete-file-if-exists (simple-programming-project-get-id-database-file project-name))
  (delete-file-if-exists (simple-programming-project-get-desktop-file     project-name)))


(defun simple-programming-project-gdb-wrapper ()
  (interactive))
  

(defun simple-programming-project-get-additional-tag-files (project-name)
  (let (tag-files-string)
    (setq tag-files-string
          (simple-programming-project-get-config project-name
                                                 simple-programming-project-additional-tag-files-config-key))
    (if tag-files-string
        (split-string tag-files-string ":")
      nil)))


(defun simple-programming-project-recreate-tags-command (output-file
                                                         source-root
                                                         project-name
                                                         synchroniously)
  (shell-command (format "ctags -e -o %s --recurse %s %s"
                         output-file
                         source-root
                         (if synchroniously "" "&"))
                 (format "*ctags (%s) Shell Command*"
                         project-name)))


(defun simple-programming-project-recreate-id-database (project-name)
  (let (output-file
        source-root)
    (setq output-file )))


(defun simple-programming-project-create-config-file (project-name source-root)
  (with-temp-buffer
    (insert simple-programming-project-source-root-config-key " = " source-root "\n")
    (write-file (simple-programming-project-get-config-file project-name))))
  

(defun simple-programming-project-recreate-id-dabatabse-command (output-file
                                                                 source-root
                                                                 project-name
                                                                 synchroniously)
  (shell-command (format "cd %s && mkid -o %s %s"
                         source-root
                         output-file
                         (if synchroniously "" "&"))
                 (format "*mkid (%s) Shell Command*" project-name)))


(defun simple-programming-project-get-source-root (project-name)
  (simple-programming-project-get-config project-name
                                         simple-programming-project-source-root-config-key))


(defun simple-programming-project-get-config (project-name variable)
  (get-config-string (simple-programming-project-get-config-file project-name)
                     variable))

(defun simple-programming-project-get-config-file (project-name)
  (expand-file-name simple-programming-project-config-file-name
                    (simple-project-get-project-directory project-name)))


(defun simple-programming-project-update-tags (project-name async)
  (let ((output-file (expand-file-name simple-programming-project-tags-file-name
                                       (simple-project-get-project-directory project-name)))
        (source-root (simple-programming-project-get-source-root project-name)))
    (simple-programming-project-recreate-tags-command output-file
                                                      source-root
                                                      project-name)))


(defun simple-programming-project-get-desktop-file (project-name)
  (expand-file-name desktop-base-file-name
                    (simple-project-get-project-directory project-name)))


(defun simple-programming-project-get-file-cache-file (project-name)
  (expand-file-name simple-programming-project-file-cache-file-name
                    (simple-project-get-project-directory project-name)))


(defun simple-programming-project-get-id-database-file (project-name)
  (expand-file-name simple-programming-project-id-database-file-name
                    (simple-project-get-project-directory project-name)))


(defun simple-programming-project-get-tags-file (project-name)
  (expand-file-name simple-programming-project-tags-file-name
                    (simple-project-get-project-directory project-name)))


(defun simple-programming-project-save-file-cache (project-name)
  (file-cache-save-cache-to-file (simple-programming-project-get-file-cache-file project-name)))


(defun simple-programming-project-create-complete-project-at-working-directory (project-name)
  (let (tag-output-file
        id-database-output-file
        source-root)
    (setq tag-output-file         (simple-programming-project-get-tags-file        project-name)
          id-database-output-file (simple-programming-project-get-id-database-file project-name)
          source-root             (expand-file-name "."))

    (message "\n\n\n==== BATCH CREATE PROJECT `%s' ====" project-name)

    (message "Making sure project directory %s exists" (simple-project-get-project-directory project-name))
    (simple-project-create-project-directory project-name)

    (message "Creating %s" simple-project-config-file-name)
    (simple-project-create-config-file project-name simple-programming-project-type)

    (message "Creating %s" simple-programming-project-config-file-name)
    (simple-programming-project-create-config-file project-name source-root)

    (message "Creating %s" simple-programming-project-tags-file-name)
    (simple-programming-project-recreate-tags-command tag-output-file
                                                      source-root
                                                      project-name
                                                      t)

    (message "Creating %s" simple-programming-project-id-database-file-name)
    (simple-programming-project-recreate-id-dabatabse-command id-database-output-file
                                                              source-root
                                                              project-name
                                                              t)

    (message "Creating %s" simple-programming-project-file-cache-file-name)
    (simple-programming-project-recreate-file-cache project-name)))


(defun simple-programming-project-recreate-file-cache (project-name)
  (file-cache-clear-cache)
  (file-cache-add-directory-recursively (simple-programming-project-get-source-root project-name))
  (simple-programming-project-save-file-cache project-name))


(defun simple-programming-project-recreate-current-project-file-cache ()
  (interactive)
  (simple-programming-project-recreate-file-cache (simple-project-get-current-project)))
  

(defun simple-programming-project-svn-status-current-source-root ()
  (interactive)
  (svn-status (simple-programming-project-get-source-root (simple-project-get-current-project))))


(defun simple-programming-project-add-current-project-source-root-to-file-cache ()
  (interactive)
  (let (current-project)
    (setq current-project (simple-project-get-current-project))
    (file-cache-add-directory-recursively (simple-programming-project-get-source-root current-project))))


(defun simple-programming-project-edit-current-project-file ()
  (interactive)
  (find-file (simple-programming-project-get-config-file (simple-project-get-current-project))))

(defun simple-programming-project-grep-frontend (arg)
  (interactive "P")
  (let (project-name
        source-root)

    (setq project-name (simple-project-get-current-project)
          source-root  (simple-programming-project-get-source-root project-name))
    
    (if (not arg)
        (setq grep-command
              (format simple-programming-project-lid-template-command-format
                      source-root
                      (simple-programming-project-get-id-database-file project-name)))
      (setq grep-command
            (format simple-programming-project-grep-template-command-format
                    source-root)))
    (call-interactively 'grep)))



(provide 'simple-programming-project)
