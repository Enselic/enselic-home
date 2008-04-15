;; programming-project.el - A simple programming project type
;; for simple-project-management.el
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
;;   (add-hook 'after-init-hook 'programming-project-init)
;;



(defconst programming-project-tags-file-name                   "TAGS")
(defconst programming-project-file-cache-file-name             "FILECACHE")
(defconst programming-project-id-database-file-name            "ID")
(defconst programming-project-config-file-name                 "programming-project-config.cfg")
(defconst programming-project-source-root-config-key           "source_root")
(defconst programming-project-binary-to-debug-config-key       "binary_to_debug")
(defconst programming-project-additional-tag-files-config-key  "additional_tag_files")
(defconst programming-project-default-file-cache-file-name     ".simple-project-management-file-cache")
(defconst programming-project-grep-template-command-format     "grep -rn \"%s\" -e ")
(defconst programming-project-lid-template-command-format      "%s lid -f \"%s\" --result=grep ")
(defconst programming-project-gdb-command-format               "libtool --mode=execute gdb --annotate=3 --args \"%s\" ")
(defconst programming-project-default-compile-command-format   "make -k -j3 -C \"%s\"")
(defconst programming-project-type                             "Programming Project")
(defconst programming-project-ctags-command-format             "ctags -e -o \"%s\" --recurse \"%s\" %s")
(defconst programming-project-mkid-command-format              "cd \"%s\" && mkid -o \"%s\" %s")



(defun programming-project-init ()
  (let ((project-type (make-simple-project-management-project-type-entry
                       :name                      programming-project-type
                       :create-project-function  'programming-project-create
                       :load-project-function    'programming-project-load
                       :unload-project-function  'programming-project-unload
                       :destroy-project-function 'programming-project-destroy)))
    (simple-project-management-register-project-type project-type)))


(defun programming-project-create (project-name)
  (let (source-root)
    (setq source-root (read-directory-name "Source code root directory: "))
    (programming-project-create-config-file project-name source-root)))


(defun programming-project-batch-create ()
  "To create a new project from the commandline, use this
command: `emacs -batch -l ~/.emacs -f
programming-project-batch-create PROJECTNAME'"
  (let (project-name)
    (setq project-name (car command-line-args-left))
    (if project-name
        (programming-project-create-complete-project-at-working-directory project-name)
      (error "programming-project-batch-create-project: Must pass `project-name' on the commandline"))))

(defun programming-project-load (project-name)

  (when (not (file-exists-p (programming-project-get-config-file project-name)))
    (error "Failed to find project file %s, project corrupt, please recreate"))

  ;; Setup the new project
  (desktop-change-dir (simple-project-management-get-project-directory project-name))

  (let (source-root)
    (setq source-root (programming-project-get-source-root project-name))
    (setq compile-command (format programming-project-default-compile-command-format source-root)))

  (if (file-regular-p (programming-project-get-file-cache-file project-name))
      (file-cache-read-cache-from-file (programming-project-get-file-cache-file project-name)))

  (let (tags-file
        additional-tag-files)

    (setq tags-file            (programming-project-get-tags-file            project-name)
          additional-tag-files (programming-project-get-additional-tag-files project-name))

    (if (not additional-tag-files)
        (setq tags-file-name tags-file)
      (setq tags-table-list (cons tags-file additional-tag-files)))))


(defun programming-project-debug-binary ()
  (interactive)
  (gdb (format programming-project-gdb-command-format
               (programming-project-get-debug-binary (simple-project-management-get-current-project)))))


(defun programming-project-get-debug-binary (project-name)
  (programming-project-get-config project-name
                                  programming-project-binary-to-debug-config-key))

(defun programming-project-unload (project-name)
  (save-some-buffers) 
  (programming-project-save-file-cache project-name)
  (file-cache-clear-cache)
  (setq tags-file-name  nil
        tags-table-list nil))


(defun programming-project-destroy (project-name)
  (delete-file-if-exists (programming-project-get-config-file      project-name))
  (delete-file-if-exists (programming-project-get-file-cache-file  project-name))
  (delete-file-if-exists (programming-project-get-tags-file        project-name))
  (delete-file-if-exists (programming-project-get-id-database-file project-name))
  (delete-file-if-exists (programming-project-get-desktop-file     project-name)))


(defun programming-project-gdb-wrapper ()
  (interactive))
  

(defun programming-project-get-additional-tag-files (project-name)
  (let (tag-files-string)
    (setq tag-files-string
          (programming-project-get-config project-name
                                          programming-project-additional-tag-files-config-key))
    (if tag-files-string
        (split-string tag-files-string ":")
      nil)))


(defun programming-project-recreate-tags-command (output-file
                                                  source-root
                                                  project-name
                                                  synchroniously)
  (shell-command (format programming-project-ctags-command-format
                         output-file
                         source-root
                         (if synchroniously "" "&"))
                 (format "*ctags (%s) Shell Command*"
                         project-name)))


(defun programming-project-recreate-id-database (project-name)
  (let (output-file
        source-root)
    (setq output-file )))


(defun programming-project-create-config-file (project-name source-root)
  (with-temp-buffer
    (insert programming-project-source-root-config-key " = " source-root "\n")
    (write-file (programming-project-get-config-file project-name))))
  

(defun programming-project-recreate-id-dabatabse-command (output-file
                                                          source-root
                                                          project-name
                                                          synchroniously)
  (shell-command (format programming-project-mkid-command-format
                         source-root
                         output-file
                         (if synchroniously "" "&"))
                 (format "*mkid (%s) Shell Command*" project-name)))


(defun programming-project-get-source-root (project-name)
  (programming-project-get-config project-name
                                  programming-project-source-root-config-key))


(defun programming-project-get-config (project-name variable)
  (get-config-string (programming-project-get-config-file project-name)
                     variable))

(defun programming-project-get-config-file (project-name)
  (expand-file-name programming-project-config-file-name
                    (simple-project-management-get-project-directory project-name)))


(defun programming-project-update-tags (project-name async)
  (let ((output-file (expand-file-name programming-project-tags-file-name
                                       (simple-project-management-get-project-directory project-name)))
        (source-root (programming-project-get-source-root project-name)))
    (programming-project-recreate-tags-command output-file
                                               source-root
                                               project-name)))


(defun programming-project-get-desktop-file (project-name)
  (expand-file-name desktop-base-file-name
                    (simple-project-management-get-project-directory project-name)))


(defun programming-project-get-file-cache-file (project-name)
  (expand-file-name programming-project-file-cache-file-name
                    (simple-project-management-get-project-directory project-name)))


(defun programming-project-get-id-database-file (project-name)
  (expand-file-name programming-project-id-database-file-name
                    (simple-project-management-get-project-directory project-name)))


(defun programming-project-get-tags-file (project-name)
  (expand-file-name programming-project-tags-file-name
                    (simple-project-management-get-project-directory project-name)))


(defun programming-project-save-file-cache (project-name)
  (file-cache-save-cache-to-file (programming-project-get-file-cache-file project-name)))


(defun programming-project-create-complete-project-at-working-directory (project-name)
  (let (tag-output-file
        id-database-output-file
        source-root)
    (setq tag-output-file         (programming-project-get-tags-file        project-name)
          id-database-output-file (programming-project-get-id-database-file project-name)
          source-root             (expand-file-name "."))

    (message "\n\n\n==== BATCH CREATE PROJECT `%s' ====" project-name)

    (message "Making sure project directory %s exists" (simple-project-management-get-project-directory project-name))
    (simple-project-management-create-project-directory project-name)

    (message "Creating %s" simple-project-management-config-file-name)
    (simple-project-management-create-config-file project-name programming-project-type)

    (message "Creating %s" programming-project-config-file-name)
    (programming-project-create-config-file project-name source-root)

    (message "Creating %s" programming-project-tags-file-name)
    (programming-project-recreate-tags-command tag-output-file
                                               source-root
                                               project-name
                                               t)

    (message "Creating %s" programming-project-id-database-file-name)
    (programming-project-recreate-id-dabatabse-command id-database-output-file
                                                       source-root
                                                       project-name
                                                       t)

    (message "Creating %s" programming-project-file-cache-file-name)
    (programming-project-recreate-file-cache project-name)))


(defun programming-project-recreate-file-cache (project-name)
  (file-cache-clear-cache)
  (file-cache-add-directory-recursively (programming-project-get-source-root project-name))
  (programming-project-save-file-cache project-name))


(defun programming-project-recreate-current-project-file-cache ()
  (interactive)
  (programming-project-recreate-file-cache (simple-project-management-get-current-project)))
  

(defun programming-project-svn-status-current-source-root ()
  (interactive)
  (svn-status (programming-project-get-source-root (simple-project-management-get-current-project))))


(defun programming-project-add-current-project-source-root-to-file-cache ()
  (interactive)
  (let (current-project)
    (setq current-project (simple-project-management-get-current-project))
    (file-cache-add-directory-recursively (programming-project-get-source-root current-project))))


(defun programming-project-edit-current-project-file ()
  (interactive)
  (find-file (programming-project-get-config-file (simple-project-management-get-current-project))))

(defun programming-project-grep-frontend (arg)
  (interactive "P")
  (let (project-name
        source-root)

    (setq project-name (simple-project-management-get-current-project)
          source-root  (programming-project-get-source-root project-name))
    
    (if (not arg)
        (setq grep-command
              (format programming-project-lid-template-command-format
                      source-root
                      (programming-project-get-id-database-file project-name)))
      (setq grep-command
            (format programming-project-grep-template-command-format
                    source-root)))
    (call-interactively 'grep)))



(provide 'programming-project)
